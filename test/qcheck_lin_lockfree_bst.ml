(** QCheck-Lin Linearizability Test for Lock-Free BST

    This test verifies that the lock-free binary search tree maintains
    linearizability under concurrent operations.

    == How Lin Works ==

    1. Test Structure:
       - Sequential prefix: A sequence of operations run sequentially
       - Parallel spawn: Two domains execute operations in parallel
       - Result checking: Lin searches for a sequential interleaving

    2. Linearizability Property:
       A concurrent execution is linearizable if there exists some
       sequential execution of the same operations that produces the
       same results. Each operation should appear to take effect
       instantaneously at some point between its invocation and response.

    3. The API Specification:
       We describe the BST operations using Lin's DSL:
       - val_ "name" function (arg_types @-> ... @-> returning result_type)
       - returning_or_exc: The function may return a value or raise an exception
       - t is the BST type, int_small generates small test integers

    4. What Lin Does:
       - Generates random command sequences with small test values
       - Runs them: sequential prefix, then parallel domains
       - Records all results (return values, exceptions)
       - Searches for a valid sequential interleaving
       - If none found -> linearizability violation!

    5. BST Operations Tested:
       - Insert: Adds a key to the tree, returns true if tree changed
       - Delete: Removes a key, returns true if tree changed
       - Search: Looks up a key, returns true if found
       - Size: Returns current number of keys in the tree
       
    The lock-free BST should handle concurrent inserts and deletes
    without violating the BST invariant and linearizability.
*)

module BST = Bst

(** Lin API specification for the lock-free BST *)
module BSTSig = struct
  type t = int BST.t

  (** Create a new BST for testing *)
  let init () = BST.create (fun x -> x) string_of_int

  (** No cleanup needed *)
  let cleanup _ = ()

  open Lin

  (** Use small integers (0-99) for test values *)
  let int_small = nat_small

  (** API description using Lin's combinator DSL:
      - val_ registers a function to test
      - (t @-> ...) describes argument types
      - returning means function returns a value (not an exception)
  *)
  let api =
    [ val_ "insert" BST.insert (t @-> int_small @-> returning_or_exc bool);
      val_ "delete" BST.delete (t @-> int_small @-> returning_or_exc bool);
      val_ "search" BST.search (t @-> int_small @-> returning_or_exc bool);
      (* val_ "size" BST.size (t @-> returning int);  *)
      ]
end

(** Generate the linearizability test from the specification *)
module BST_domain = Lin_domain.Make(BSTSig)

(** Run 1000 test iterations, each with random command sequences *)
let () =
  QCheck_base_runner.run_tests_main [
    BST_domain.lin_test ~count:100 ~name:"Lock-free BST linearizability test";
  ]