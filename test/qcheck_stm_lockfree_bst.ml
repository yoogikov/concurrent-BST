(** QCheck-STM State Machine Test for Lock-Free BST

    This test uses QCheck-STM to verify the lock-free BST's correctness.
    
    Tests demonstrate:
    - Sequential operations work correctly
    - Concurrent insert/delete/search maintain BST invariants
    - Operations are linearizable under concurrent execution
*)

open QCheck
open STM

module BST = Bst

(** Utility function for repeating tests *)
let rec repeat n f x =
  if n <= 0 then true
  else f x && repeat (n - 1) f x

module Spec = struct
  type cmd =
    | Insert of int
    | Delete of int
    | Search of int
    | Size

  let show_cmd c =
    match c with
    | Insert i -> "Insert " ^ string_of_int i
    | Delete i -> "Delete " ^ string_of_int i
    | Search i -> "Search " ^ string_of_int i
    | Size -> "Size"

  (** State: (current_size, contents_as_sorted_list) *)
  type state = int * int list
  type sut = int BST.t

  (** Commands for insertion-focused domain *)
  let insert_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof_weighted [
        (7, Gen.map (fun i -> Insert i) int_gen);
        (2, Gen.map (fun i -> Search i) int_gen);
        (1, Gen.return Size);
      ])

  (** Commands for deletion-focused domain *)
  let delete_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof_weighted [
        (7, Gen.map (fun i -> Delete i) int_gen);
        (2, Gen.map (fun i -> Search i) int_gen);
        (1, Gen.return Size);
      ])

  (** All commands for sequential testing *)
  let arb_cmd _s =
    let int_gen = Gen.nat in
    QCheck.make ~print:show_cmd
      (Gen.oneof [
        Gen.map (fun i -> Insert i) int_gen;
        Gen.map (fun i -> Delete i) int_gen;
        Gen.map (fun i -> Search i) int_gen;
        Gen.return Size;
      ])

  let init_state = (0, [])
  let init_sut () = BST.create (fun x -> x) string_of_int
  let cleanup _ = ()

  (** Helper to insert into sorted list (no duplicates) *)
  let rec insert_sorted x lst =
    match lst with
    | [] -> [x]
    | h :: t ->
        if x = h then lst  (* No duplicates *)
        else if x < h then x :: lst
        else h :: insert_sorted x t

  (** Helper to remove from sorted list *)
  let rec remove_sorted x lst =
    match lst with
    | [] -> []
    | h :: t ->
        if x = h then t
        else h :: remove_sorted x t

  (** Update the model state based on the command *)
  let next_state c (size, contents) =
    match c with
    | Insert i ->
        if List.mem i contents then
          (size, contents)  (* Already present, no change *)
        else
          (size + 1, insert_sorted i contents)
    | Delete i ->
        if List.mem i contents then
          (size - 1, remove_sorted i contents)
        else
          (size, contents)  (* Not present, no change *)
    | Search _ ->
        (size, contents)  (* Search doesn't change state *)
    | Size ->
        (size, contents)  (* Size query doesn't change state *)

  let precond _ _ = true

  (** Execute the command on the real implementation *)
  let run c d =
    match c with
    | Insert i ->
        Res (bool, BST.insert d i)
    | Delete i ->
        Res (bool, BST.delete d i)
    | Search i ->
        Res (bool, BST.search d i)
    | Size ->
        Res (int, BST.size d)

  (** Check if the actual result matches expectations from the model *)
  let postcond c ((size, contents) : state) res =
    match (c, res) with
    | Insert i, Res ((Bool, _), actual_res) ->
        (* Insert returns true if tree changed (key not already present) *)
        actual_res = (not (List.mem i contents))
    | Delete i, Res ((Bool, _), actual_res) ->
        (* Delete returns true if tree changed (key was present) *)
        actual_res = (List.mem i contents)
    | Search i, Res ((Bool, _), actual_res) ->
        (* Search returns true iff key is present *)
        actual_res = (List.mem i contents)
    | Size, Res ((Int, _), actual_res) ->
        (* Size should match the number of keys *)
        actual_res = size
    | _, _ -> false
end

(** Run tests based on command-line argument *)
let run_test test_name =
  let module Seq = STM_sequential.Make(Spec) in
  let module Dom = STM_domain.Make(Spec) in

  match test_name with
  | "sequential" | "seq" ->
      Printf.printf "Running sequential test (should pass)...\n\n%!";
      let seq_test = Seq.agree_test ~count:1000 ~name:"Lockfree_bst sequential" in
      QCheck_base_runner.run_tests ~verbose:true [seq_test]

  | "concurrent" ->
      Printf.printf "Running concurrent test - Insertions vs Deletions (should pass)...\n\n%!";
      let arb_cmds_par_concurrent =
        Dom.arb_triple 20 12
          Spec.arb_cmd       (* Sequential prefix *)
          Spec.insert_cmd    (* Insertion domain *)
          Spec.delete_cmd    (* Deletion domain *)
      in
      let concurrent_test =
        let rep_count = 20 in
        Test.make ~retries:10 ~count:100 ~name:"Lockfree_bst concurrent" arb_cmds_par_concurrent
        @@ fun triple ->
        assume (Dom.all_interleavings_ok triple);
        repeat rep_count Dom.agree_prop_par_asym triple
      in
      QCheck_base_runner.run_tests ~verbose:true [concurrent_test]

  | "all_insert" ->
      Printf.printf "Running test with concurrent insertions...\n\n%!";
      let arb_cmds_par_insert =
        Dom.arb_triple 15 10 Spec.arb_cmd Spec.insert_cmd Spec.insert_cmd
      in
      let insert_test =
        Test.make ~count:100 ~name:"Lockfree_bst concurrent inserts" arb_cmds_par_insert
        @@ fun triple ->
        assume (Dom.all_interleavings_ok triple);
        repeat 10 Dom.agree_prop_par_asym triple
      in
      QCheck_base_runner.run_tests ~verbose:true [insert_test]

  | "all_delete" ->
      Printf.printf "Running test with concurrent deletions...\n\n%!";
      let arb_cmds_par_delete =
        Dom.arb_triple 15 10 Spec.arb_cmd Spec.delete_cmd Spec.delete_cmd
      in
      let delete_test =
        Test.make ~count:100 ~name:"Lockfree_bst concurrent deletes" arb_cmds_par_delete
        @@ fun triple ->
        assume (Dom.all_interleavings_ok triple);
        repeat 10 Dom.agree_prop_par_asym triple
      in
      QCheck_base_runner.run_tests ~verbose:true [delete_test]

  | "all" ->
      Printf.printf "Running all tests...\n\n%!";
      let tests = [
        Seq.agree_test ~count:1000 ~name:"Lockfree_bst sequential";
        Test.make ~retries:10 ~count:100 ~name:"Lockfree_bst concurrent"
          (Dom.arb_triple 20 12 Spec.arb_cmd Spec.insert_cmd Spec.delete_cmd)
          (fun triple -> assume (Dom.all_interleavings_ok triple); repeat 20 Dom.agree_prop_par_asym triple);
        Test.make ~count:100 ~name:"Lockfree_bst concurrent inserts"
          (Dom.arb_triple 15 10 Spec.arb_cmd Spec.insert_cmd Spec.insert_cmd)
          (fun triple -> assume (Dom.all_interleavings_ok triple); repeat 10 Dom.agree_prop_par_asym triple);
        Test.make ~count:100 ~name:"Lockfree_bst concurrent deletes"
          (Dom.arb_triple 15 10 Spec.arb_cmd Spec.delete_cmd Spec.delete_cmd)
          (fun triple -> assume (Dom.all_interleavings_ok triple); repeat 10 Dom.agree_prop_par_asym triple);
      ] in
      QCheck_base_runner.run_tests ~verbose:true tests

  | _ ->
      Printf.eprintf "Error: Unknown test '%s'\n\n" test_name;
      exit 1

let print_help () =
  Printf.printf "QCheck-STM Tests for Lock-Free BST\n\n";
  Printf.printf "Usage: %s <test>\n\n" Sys.argv.(0);
  Printf.printf "Available tests:\n";
  Printf.printf "  sequential, seq   - Sequential operations only (should pass)\n";
  Printf.printf "  concurrent        - Concurrent inserts vs deletes (should pass)\n";
  Printf.printf "  all_insert        - Concurrent inserts only\n";
  Printf.printf "  all_delete        - Concurrent deletes only\n";
  Printf.printf "  all               - Run all tests\n\n";
  exit 0

let () =
  if Array.length Sys.argv < 2 then
    print_help ()
  else
    let exit_code = run_test Sys.argv.(1) in
    exit exit_code