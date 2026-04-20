(* coarseGrainedBST.ml
 *
 * Coarse-grained BST implementation.
 *)

type key = int

type 'a node = {
  key : key;
  mutable value : 'a;
  mutable left : 'a node option;
  mutable right : 'a node option;
}

type 'a t = {
  mutex : Mutex.t;
  mutable root : 'a node option;
  mutable size : int;
}

let create () =
  {
    mutex = Mutex.create ();
    root = None;
    size = 0;
  }

let search bst item =
  let key = Hashtbl.hash item in
  Mutex.lock bst.mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock bst.mutex)
    (fun () ->
      let rec loop curr =
        match curr with
        | None -> false
        | Some node ->
            if key = node.key then true
            else if key < node.key then loop node.left
            else loop node.right
      in
      loop bst.root)

let insert bst item =
  let key = Hashtbl.hash item in
  Mutex.lock bst.mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock bst.mutex)
    (fun () ->
      let rec loop curr =
        match curr with
        | None ->
            bst.root <-
              Some { key; value = item; left = None; right = None };
            bst.size <- bst.size + 1;
            true
        | Some node ->
            if key = node.key then
              false
            else if key < node.key then
              begin
                match node.left with
                | None ->
                    node.left <- Some { key; value = item; left = None; right = None };
                    bst.size <- bst.size + 1;
                    true
                | Some _ ->
                    loop node.left
              end
            else
              begin
                match node.right with
                | None ->
                    node.right <- Some { key; value = item; left = None; right = None };
                    bst.size <- bst.size + 1;
                    true
                | Some _ ->
                    loop node.right
              end
      in
      loop bst.root)

let delete bst item =
  let key = Hashtbl.hash item in
  Mutex.lock bst.mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock bst.mutex)
    (fun () ->
      let rec extract_min = function
        | None -> failwith "extract_min on empty tree"
        | Some node ->
            begin
              match node.left with
              | None ->
                  (node, node.right)
              | Some _ ->
                  let (min_node, new_left) = extract_min node.left in
                  node.left <- new_left;
                  (min_node, Some node)
            end
      in
      let rec remove = function
        | None -> (None, false)
        | Some node ->
            if key < node.key then
              let (new_left, deleted) = remove node.left in
              node.left <- new_left;
              (Some node, deleted)
            else if key > node.key then
              let (new_right, deleted) = remove node.right in
              node.right <- new_right;
              (Some node, deleted)
            else
              begin
                match (node.left, node.right) with
                | None, None ->
                    (None, true)
                | Some left, None ->
                    (Some left, true)
                | None, Some right ->
                    (Some right, true)
                | Some _, Some _ ->
                    let (successor, new_right) = extract_min node.right in
                    successor.left <- node.left;
                    successor.right <- new_right;
                    (Some successor, true)
              end
      in
      let (new_root, deleted) = remove bst.root in
      bst.root <- new_root;
      if deleted then (bst.size <- bst.size - 1;true)
      else false)

let size bst =
  Mutex.lock bst.mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock bst.mutex)
    (fun () -> bst.size)

(** to_string is a helper function to visualize the tree structure *)
let to_string bst =
  Mutex.lock bst.mutex;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock bst.mutex)
    (fun () ->
      let buf = Buffer.create 256 in
      let rec walk prefix is_last is_root node =
        match node with
        | None -> ()
        | Some n ->
            let branch =
              if is_root
              then ""
              else if is_last
              then "└── "
              else "├── "
            in
            let label = Printf.sprintf "[%d]" n.key in
            Buffer.add_string buf prefix;
            Buffer.add_string buf branch;
            Buffer.add_string buf label;
            Buffer.add_char buf '\n';
            
            let child_prefix =
              if is_root
              then ""
              else if is_last
              then prefix ^ "    "
              else prefix ^ "│   "
            in
            
            (* Traverse left then right *)
            walk child_prefix false false n.left;
            walk child_prefix true false n.right
      in
      walk "" false true bst.root;
      Buffer.contents buf)