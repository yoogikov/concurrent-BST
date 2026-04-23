(* optimisticLazyList.ml
 *
 * Optimistic lazy BST implementation.
 *
 * The filename is a legacy project name, but the structure implemented here is
 * an optimistic, leaf-oriented BST. Traversals run without locks; updates then
 * lock a small window, validate the observed structure, and perform the change.
 *
 * Design decisions:
 * - External (leaf-oriented) BST with the same sentinel shape as Bst.ml
 * - Keys are ints derived via the supplied hash function
 * - Per-node locks protect local update windows
 * - Deletes are lazy: mark the doomed leaf and its parent before unlinking
 * - Structural equality (=) is used for value comparison at leaves
 *)

let inf0 = max_int - 2
let inf1 = max_int - 1
let inf2 = max_int

type 'a node = {
  item : 'a option;
  key : int;
  is_leaf : bool;
  mutable left : 'a node option;
  mutable right : 'a node option;
  mutable marked : bool;
  lock : Mutex.t;
}

type 'a t = {
  hash : 'a -> int;
  to_string : 'a -> string;
  root : 'a node;
  size_lock : Mutex.t;
  mutable size_value : int;
}

type 'a seek_record = {
  grandparent : 'a node;
  parent : 'a node;
  leaf : 'a node;
}

let make_leaf key item =
  {
    item;
    key;
    is_leaf = true;
    left = None;
    right = None;
    marked = false;
    lock = Mutex.create ();
  }

let make_internal key left right =
  {
    item = None;
    key;
    is_leaf = false;
    left = Some left;
    right = Some right;
    marked = false;
    lock = Mutex.create ();
  }

let get_left node =
  match node.left with
  | Some child -> child
  | None -> failwith "OptimisticLazyBST invariant violated: missing left child"

let get_right node =
  match node.right with
  | Some child -> child
  | None -> failwith "OptimisticLazyBST invariant violated: missing right child"

let child node key = if key < node.key then get_left node else get_right node

let set_child node key new_child =
  if key < node.key then node.left <- Some new_child else node.right <- Some new_child

let child_is node candidate =
  match (node.left, node.right) with
  | Some left, _ when left == candidate -> true
  | _, Some right when right == candidate -> true
  | _ -> false

let sibling parent leaf =
  match (parent.left, parent.right) with
  | Some left, Some right when left == leaf -> right
  | Some left, Some right when right == leaf -> left
  | _ -> failwith "OptimisticLazyBST invariant violated: leaf not found under parent"

let with_mutex mutex f =
  Mutex.lock mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock mutex) f

let with_three_locks a b c f =
  Mutex.lock a;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock a)
    (fun () ->
      Mutex.lock b;
      Fun.protect
        ~finally:(fun () -> Mutex.unlock b)
        (fun () ->
          Mutex.lock c;
          Fun.protect ~finally:(fun () -> Mutex.unlock c) f))

let change_size tree delta =
  with_mutex tree.size_lock (fun () -> tree.size_value <- tree.size_value + delta)

let sentinel_name k =
  if k = inf0 then Some "INF0"
  else if k = inf1 then Some "INF1"
  else if k = inf2 then Some "INF2"
  else None

let key_str k = match sentinel_name k with Some name -> name | None -> string_of_int k

let create hash_fn to_string_fn =
  let leaf_inf0 = make_leaf inf0 None in
  let leaf_inf1 = make_leaf inf1 None in
  let leaf_inf2 = make_leaf inf2 None in
  let s = make_internal inf1 leaf_inf0 leaf_inf1 in
  let r = make_internal inf2 s leaf_inf2 in
  {
    hash = hash_fn;
    to_string = to_string_fn;
    root = r;
    size_lock = Mutex.create ();
    size_value = 0;
  }

let seek tree value =
  let key = tree.hash value in
  let grandparent = tree.root in
  let parent = get_left tree.root in
  let leaf = child parent key in
  let rec loop grandparent parent leaf =
    if leaf.is_leaf then { grandparent; parent; leaf }
    else
      let next = child leaf key in
      loop parent leaf next
  in
  loop grandparent parent leaf

let validate_insert record =
  (not record.parent.marked)
  && (not record.leaf.marked)
  && (not record.parent.is_leaf)
  && record.leaf.is_leaf
  && child_is record.parent record.leaf

let validate_delete record =
  (not record.grandparent.marked)
  && (not record.parent.marked)
  && (not record.leaf.marked)
  && (not record.parent.is_leaf)
  && record.leaf.is_leaf
  && child_is record.grandparent record.parent
  && child_is record.parent record.leaf

let search tree value =
  let record = seek tree value in
  record.leaf.key = tree.hash value
  && not record.leaf.marked
  &&
  match record.leaf.item with
  | Some existing -> existing = value
  | None -> false

let insert tree value =
  let key = tree.hash value in
  let rec attempt () =
    let record = seek tree value in
    let result =
    with_mutex tree.size_lock (fun () ->
      with_mutex record.parent.lock (fun () ->
          with_mutex record.leaf.lock (fun () ->
              if not (validate_insert record) then `Retry
              else if record.leaf.key = key then
                match record.leaf.item with
                | Some existing when existing = value -> `Present
                | _ -> `Present
              else
                let new_leaf = make_leaf key (Some value) in
                let new_internal =
                  if key < record.leaf.key then
                    make_internal record.leaf.key new_leaf record.leaf
                  else
                    make_internal key record.leaf new_leaf
                in
                set_child record.parent record.leaf.key new_internal;
                change_size tree 1;
                `Inserted)))
    in
    match result with
    | `Retry -> attempt ()
    | `Present -> false
    | `Inserted -> true
  in
  attempt ()

let delete tree value =
  let key = tree.hash value in
  let rec attempt () =
    let record = seek tree value in
    let result =
      with_three_locks record.grandparent.lock record.parent.lock record.leaf.lock
        (fun () ->
          if not (validate_delete record) then `Retry
          else if record.leaf.key <> key then `Missing
          else
            match record.leaf.item with
            | Some existing when existing = value ->
                record.leaf.marked <- true;
                record.parent.marked <- true;
                let sibling_node = sibling record.parent record.leaf in
                set_child record.grandparent record.parent.key sibling_node;
                tree.size_value <- tree.size_value - 1;
                `Deleted
            | _ -> `Missing)
    in
    match result with
    | `Retry -> attempt ()
    | `Missing -> false
    | `Deleted -> true
  in
  attempt ()

let size tree = with_mutex tree.size_lock (fun () -> tree.size_value)

let to_string tree =
  let buf = Buffer.create 256 in
  let rec walk prefix is_last is_root node =
    let branch =
      if is_root then ""
      else if is_last then "\\-- "
      else "|-- "
    in
    let label =
      if node.is_leaf then
        match node.item with
        | Some value -> Printf.sprintf "L:%s" (tree.to_string value)
        | None -> Printf.sprintf "L:%s" (key_str node.key)
      else key_str node.key
    in
    let mark_suffix = if node.marked then " x" else "" in
    Buffer.add_string buf prefix;
    Buffer.add_string buf branch;
    Buffer.add_char buf '[';
    Buffer.add_string buf label;
    Buffer.add_string buf mark_suffix;
    Buffer.add_char buf ']';
    Buffer.add_char buf '\n';
    if not node.is_leaf then
      let child_prefix =
        if is_root then ""
        else if is_last then prefix ^ "    "
        else prefix ^ "|   "
      in
      let right = get_right node in
      let left = get_left node in
      walk child_prefix false false right;
      walk child_prefix true false left
  in
  walk "" false true tree.root;
  Buffer.contents buf
