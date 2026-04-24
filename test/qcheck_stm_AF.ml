(** QCheck-STM State Machine Test for AtomicFlagTag

    This test uses QCheck-STM to verify the AtomicFlagTag module's correctness.
    
    Tests demonstrate:
    - Sequential operations work correctly
    - Concurrent get/set/cas maintain atomicity
    - Operations are linearizable under concurrent execution

    Usage
        dune exec ./qcheck_stm_AF.exe sequential
        dune exec ./qcheck_stm_AF.exe concurrent
        dune exec ./qcheck_stm_AF.exe all
*)

open QCheck
open STM

module AFT = AtomicFlagTag

(** Utility function for repeating tests *)
let rec repeat n f x =
  if n <= 0 then true
  else f x && repeat (n - 1) f x

module Spec = struct
  type cmd =
    | Get_value
    | Get_flag
    | Get_tag
    | Set_flag of bool
    | Set_tag of bool
    | Set_value of int
    | Cas of bool * bool * int * bool * bool * int  (* exp_flag, exp_tag, exp_val, new_flag, new_tag, new_val *)

  let show_cmd c =
    match c with
    | Get_value -> "Get_value"
    | Get_flag -> "Get_flag"
    | Get_tag -> "Get_tag"
    | Set_flag b -> "Set_flag " ^ string_of_bool b
    | Set_tag b -> "Set_tag " ^ string_of_bool b
    | Set_value i -> "Set_value " ^ string_of_int i
    | Cas (ef, et, ev, nf, nt, nv) ->
        Printf.sprintf "Cas(%b,%b,%d -> %b,%b,%d)" ef et ev nf nt nv

  (** State: current snapshot (flag, tag, value) *)
  type state = bool * bool * int
  type sut = int AFT.t

  (** All commands for testing *)
  let arb_cmd _s =
    QCheck.make ~print:show_cmd
      (Gen.oneof [
        Gen.return Get_value;
        Gen.return Get_flag;
        Gen.return Get_tag;
        Gen.map (fun b -> Set_flag b) Gen.bool;
        Gen.map (fun b -> Set_tag b) Gen.bool;
        Gen.map (fun i -> Set_value i) Gen.nat;
        Gen.map (fun (ef, et, ev, nf, nt, nv) -> Cas (ef, et, ev, nf, nt, nv))
          (Gen.tup6 Gen.bool Gen.bool Gen.nat Gen.bool Gen.bool Gen.nat);
      ])

  let init_state = (false, false, 0)
  let init_sut () = AFT.make ~flag:false ~tag:false 0
  let cleanup _ = ()

  (** Update the model state based on the command *)
  let next_state c (flag, tag, value) =
    match c with
    | Get_value | Get_flag | Get_tag ->
        (flag, tag, value)  (* Getters don't change state *)
    | Set_flag new_flag ->
        (new_flag, tag, value)
    | Set_tag new_tag ->
        (flag, new_tag, value)
    | Set_value new_value ->
        (flag, tag, new_value)
    | Cas (exp_flag, exp_tag, exp_val, new_flag, new_tag, new_val) ->
        if flag = exp_flag && tag = exp_tag && value = exp_val then
          (new_flag, new_tag, new_val)  (* CAS succeeds *)
        else
          (flag, tag, value)  (* CAS fails, state unchanged *)

  let precond _ _ = true

  (** Execute the command on the real implementation *)
  let run c d =
    match c with
    | Get_value ->
        Res (int, AFT.get_value d)
    | Get_flag ->
        Res (bool, AFT.get_flag d)
    | Get_tag ->
        Res (bool, AFT.get_tag d)
    | Set_flag b ->
        AFT.set_flag d b;
        Res (unit, ())
    | Set_tag b ->
        AFT.set_tag d b;
        Res (unit, ())
    | Set_value i ->
        AFT.set_value d i;
        Res (unit, ())
    | Cas (ef, et, ev, nf, nt, nv) ->
        Res (bool, AFT.cas d ~exp_flag:ef ~exp_tag:et ~exp_val:ev
                           ~new_flag:nf ~new_tag:nt ~new_val:nv)

  (** Check if the actual result matches expectations from the model *)
  let postcond c ((flag, tag, value) : state) res =
    match (c, res) with
    | Get_value, Res ((Int, _), actual_val) ->
        actual_val = value
    | Get_flag, Res ((Bool, _), actual_flag) ->
        actual_flag = flag
    | Get_tag, Res ((Bool, _), actual_tag) ->
        actual_tag = tag
    | Set_flag _, Res ((Unit, _), ()) ->
        true  (* Setters always succeed *)
    | Set_tag _, Res ((Unit, _), ()) ->
        true
    | Set_value _, Res ((Unit, _), ()) ->
        true
    | Cas (ef, et, ev, _, _, _), Res ((Bool, _), actual_success) ->
        actual_success = (flag = ef && tag = et && value = ev)
    | _, _ -> false
end

(** Run tests based on command-line argument *)
let run_test test_name =
  let module Seq = STM_sequential.Make(Spec) in
  let module Dom = STM_domain.Make(Spec) in

  match test_name with
  | "sequential" | "seq" ->
      Printf.printf "Running sequential test (should pass)...\n\n%!";
      let seq_test = Seq.agree_test ~count:1000 ~name:"AtomicFlagTag sequential" in
      QCheck_base_runner.run_tests ~verbose:true [seq_test]

  | "concurrent" ->
      Printf.printf "Running concurrent test (should pass)...\n\n%!";
      let arb_cmds_par =
        Dom.arb_triple 20 12 Spec.arb_cmd Spec.arb_cmd Spec.arb_cmd
      in
      let concurrent_test =
        let rep_count = 20 in
        Test.make ~retries:10 ~count:100 ~name:"AtomicFlagTag concurrent" arb_cmds_par
        @@ fun triple ->
        assume (Dom.all_interleavings_ok triple);
        repeat rep_count Dom.agree_prop_par_asym triple
      in
      QCheck_base_runner.run_tests ~verbose:true [concurrent_test]

  | "all" ->
      Printf.printf "Running all tests...\n\n%!";
      let tests = [
        Seq.agree_test ~count:1000 ~name:"AtomicFlagTag sequential";
        Test.make ~retries:10 ~count:1000 ~name:"AtomicFlagTag concurrent"
          (Dom.arb_triple 20 12 Spec.arb_cmd Spec.arb_cmd Spec.arb_cmd)
          (fun triple -> assume (Dom.all_interleavings_ok triple); repeat 20 Dom.agree_prop_par_asym triple);
      ] in
      QCheck_base_runner.run_tests ~verbose:true tests

  | _ ->
      Printf.eprintf "Error: Unknown test '%s'\n\n" test_name;
      exit 1

let print_help () =
  Printf.printf "QCheck-STM Tests for AtomicFlagTag\n\n";
  Printf.printf "Usage: %s <test>\n\n" Sys.argv.(0);
  Printf.printf "Available tests:\n";
  Printf.printf "  sequential, seq   - Sequential operations only (should pass)\n";
  Printf.printf "  concurrent        - Concurrent operations (should pass)\n";
  Printf.printf "  all               - Run all tests\n\n";
  exit 0

let () =
  if Array.length Sys.argv < 2 then
    print_help ()
  else
    let exit_code = run_test Sys.argv.(1) in
    exit exit_code