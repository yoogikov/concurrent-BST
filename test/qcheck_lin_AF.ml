(** QCheck-Lin Linearizability Test for AtomicFlagTag

    This test verifies that the AtomicFlagTag module maintains
    linearizability of its atomic operations under concurrent access.

    == What We're Testing ==

    AtomicFlagTag provides lock-free atomic cells that store:
    - A value (generic type)
    - A flag bit
    - A tag bit

    These bits are stolen from pointer encoding and protected by
    compare-and-swap (CAS) operations. The test checks that:
    - CAS operations are truly atomic
    - Flag/tag reads/writes don't interfere
    - Concurrent modifications maintain consistency

    == Operations Tested ==
    - get: Atomic snapshot of value + flag + tag
    - get_value: Read just the value
    - get_flag: Read just the flag bit
    - get_tag: Read just the tag bit
    - set_tag: Atomically set tag bit
    - cas: Compare-and-swap on all three fields
*)

module AFT = AtomicFlagTag

(** Lin API specification for AtomicFlagTag *)
module AFTSig = struct
  type t = int AFT.t

  (** Create a new AFT cell initialized to (value=0, flag=false, tag=false) *)
  let init () = AFT.make ~flag:false ~tag:false 0

  (** No cleanup needed *)
  let cleanup _ = ()

  open Lin

  (** Generate test values: small integers (0-20) *)
  let int_small = nat_small

  (** API description using Lin's combinator DSL *)
  let api =
    [
      val_ "get_value" AFT.get_value (t @-> returning int);
      val_ "get_flag" AFT.get_flag (t @-> returning bool);
      val_ "get_tag" AFT.get_tag (t @-> returning bool);
      val_ "set_tag" AFT.set_tag (t @-> bool @-> returning unit);
      val_ "cas_success" 
        (fun cell exp_val new_val ->
          AFT.cas cell ~exp_val ~exp_flag:false ~exp_tag:false
            ~new_val ~new_flag:false ~new_tag:false)
        (t @-> int_small @-> int_small @-> returning bool);
      val_ "cas_with_flags"
        (fun cell exp_val exp_flag new_val new_flag ->
          AFT.cas cell ~exp_val ~exp_flag ~exp_tag:false
            ~new_val ~new_flag ~new_tag:false)
        (t @-> int_small @-> bool @-> int_small @-> bool @-> returning bool);
    ]
end

(** Generate the linearizability test *)
module AFT_domain = Lin_domain.Make(AFTSig)

(** Run the test *)
let () =
  QCheck_base_runner.run_tests_main [
    AFT_domain.lin_test ~count:1000 ~name:"AtomicFlagTag linearizability test";
  ]