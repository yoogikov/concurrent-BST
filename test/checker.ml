(* checker.ml *)
(* A simple BST checker that reads test cases and pretty-prints the tree structure *)

type tree =
  | Empty
  | Node of { key: int; left: tree; right: tree }

module BST = struct
  let rec search tree key =
    match tree with
    | Empty -> false
    | Node { key = k; left; right } ->
      if key = k then true
      else if key < k then search left key
      else search right key

  let rec insert tree key =
    match tree with
    | Empty -> Node { key; left = Empty; right = Empty }
    | Node { key = k; left; right } ->
      if key = k then Node { key = k; left; right }
      else if key < k then
        Node { key = k; left = insert left key; right }
      else
        Node { key = k; left; right = insert right key }

  let rec delete tree key =
    match tree with
    | Empty -> Empty
    | Node { key = k; left; right } ->
      if key = k then
        match left, right with
        | Empty, Empty -> Empty
        | Empty, _ -> right
        | _, Empty -> left
        | _, _ ->
          (* Find min in right subtree *)
          let rec find_min = function
            | Empty -> failwith "should not happen"
            | Node { key = mk; left = Empty; _ } -> mk
            | Node { left; _ } -> find_min left
          in
          let min_key = find_min right in
          Node { key = min_key; left; right = delete right min_key }
      else if key < k then
        Node { key = k; left = delete left key; right }
      else
        Node { key = k; left; right = delete right key }
end

(* Pretty printing the tree *)
let rec tree_to_string ?(prefix = "") ?(is_tail = true) tree =
  match tree with
  | Empty -> ""
  | Node { key; left; right } ->
    let current = 
      prefix ^ (if is_tail then "└── " else "├── ") ^ string_of_int key ^ "\n"
    in
    let prefix' = 
      prefix ^ (if is_tail then "    " else "│   ")
    in
    let left_str = tree_to_string ~prefix:prefix' ~is_tail:false left in
    let right_str = tree_to_string ~prefix:prefix' ~is_tail:true right in
    current ^ left_str ^ right_str

let print_tree tree =
  match tree with
  | Empty -> print_endline "Tree is empty"
  | Node { key; _ } as t ->
    Printf.printf "Root: %d\n" key;
    print_endline (tree_to_string t)

(* Parse and execute commands *)
let process_file filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  let lines = read_lines [] in
  let lines = List.map String.trim lines in
  let lines = List.filter (fun s -> s <> "" && s <> "START" && s <> "END") lines in
  
  let rec process_commands tree step = function
    | [] -> ()
    | line :: rest ->
      Printf.printf "\n=== Step %d ===\n" step;
      Printf.printf "Command: %s\n" line;
      
      let new_tree =
        if line = "CREATE" then begin
          Printf.printf "Creating new tree\n";
          Empty
        end
        else if String.starts_with ~prefix:"INSERT " line then begin
          let key = int_of_string (String.sub line 7 (String.length line - 7)) in
          Printf.printf "Inserting: %d\n" key;
          BST.insert tree key
        end
        else if String.starts_with ~prefix:"DELETE " line then begin
          let key = int_of_string (String.sub line 7 (String.length line - 7)) in
          Printf.printf "Deleting: %d\n" key;
          BST.delete tree key
        end
        else if String.starts_with ~prefix:"SEARCH " line then begin
          let key = int_of_string (String.sub line 7 (String.length line - 7)) in
          Printf.printf "Searching: %d\n" key;
          let found = BST.search tree key in
          Printf.printf "Result: %b\n" found;
          tree
        end
        else tree
      in
      
      Printf.printf "\nTree structure:\n";
      print_tree new_tree;
      process_commands new_tree (step + 1) rest
  in
  
  process_commands Empty 0 lines

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <test_file>\n" Sys.argv.(0);
    exit 1
  end;
  process_file Sys.argv.(1)