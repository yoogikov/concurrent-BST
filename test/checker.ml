(* checker.ml *)
(* A simple BST checker that reads test cases and verifies operations *)

module BST = Bst
(* module BST = CoarseGrainedBST *)

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
      Printf.printf "\n=== Step %d ===\n%!" step;
      Printf.printf "Command: %s\n%!" line;
      
      let new_tree =
        if line = "CREATE" then begin
          Printf.printf "Creating new tree\n%!";
          BST.create ()
        end
        else if String.starts_with ~prefix:"INSERT " line then begin
          let key = int_of_string (String.sub line 7 (String.length line - 7)) in
          let changed = BST.insert tree key in
          Printf.printf "Inserting: %d (changed: %b)\n%!" key changed;
          tree
        end
        else if String.starts_with ~prefix:"DELETE " line then begin
          let key = int_of_string (String.sub line 7 (String.length line - 7)) in
          let changed = BST.delete tree key in
          Printf.printf "Deleting: %d (changed: %b)\n%!" key changed;
          tree
        end
        else if String.starts_with ~prefix:"SEARCH " line then begin
          let key = int_of_string (String.sub line 7 (String.length line - 7)) in
          let found = BST.search tree key in
          Printf.printf "Searching: %d (found: %b)\n%!" key found;
          tree
        end
        else tree
      in
      
      Printf.printf "\nTree structure:\n%!";
      Printf.printf "%s\n%!" (BST.to_string new_tree);
      (* Printf.printf "Size: %d\n%!" (size new_tree); *)
      process_commands new_tree (step + 1) rest
  in
  
  process_commands (BST.create ()) 0 lines

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <test_file>\n%!" Sys.argv.(0);
    exit 1
  end;
  process_file Sys.argv.(1)