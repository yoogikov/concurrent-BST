(** test_bst_concurrent.ml * * Concurrent stress tests for the lock-free BST
    (Natarajan-Mittal). * * Test categories: * 1. Basic sanity – single-domain
    correctness before going parallel * 2. Concurrent inserts only * 3.
    Concurrent deletes only * 4. Concurrent insert/delete mix * 5. Insert +
    search (readers never block writers) * 6. ABA / re-insert after delete * 7.
    High contention on a tiny key space * 8. Large key space / low contention *
    9. Many domains (> 2) * 10. Idempotency invariants (insert returns false on
    duplicate, etc.) * * Each test is self-contained and prints PASS / FAIL. *
    Run with: dune exec ./test_bst_concurrent.exe * or: ocamlfind ocamlopt
    -package bst,threads.posix \ * -linkpkg test_bst_concurrent.ml -o
    test_bst_concurrent *)

module BST = Bst

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let make () = BST.create (fun x -> x) string_of_int

(** Run [f] in a fresh domain and join it. *)
let in_domain f =
  let d = Domain.spawn f in
  Domain.join d

(** Spawn [n] domains each running [f i], collect results. *)
let spawn_n n f =
  let domains = Array.init n (fun i -> Domain.spawn (fun () -> f i)) in
  Array.map Domain.join domains

(** Assert with a label. *)
let check label cond =
  if cond then Printf.printf "  PASS  %s\n%!" label
  else Printf.printf "  FAIL  %s\n%!" label

(** Sort and deduplicate an int list. *)
let sort_uniq lst = List.sort_uniq compare lst

(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)
(* 1. Basic single-domain sanity                                        *)
(* ------------------------------------------------------------------ *)
(* ------------------------------------------------------------------ *)

let test_single_domain_insert_search_delete () =
  Printf.printf "\n[1] Single-domain sanity\n%!";
  let t = make () in

  (* Insert fresh key *)
  check "insert 42 -> true" (BST.insert t 42 = true);

  (* Duplicate insert *)
  check "insert 42 -> false (duplicate)" (BST.insert t 42 = false);

  (* Search present *)
  check "search 42 -> true" (BST.search t 42 = true);

  (* Search absent *)
  check "search 99 -> false" (BST.search t 99 = false);

  (* Delete present *)
  check "delete 42 -> true" (BST.delete t 42 = true);

  (* Delete absent *)
  check "delete 42 -> false (already gone)" (BST.delete t 42 = false);

  (* Search after delete *)
  check "search 42 -> false after delete" (BST.search t 42 = false);

  (* Multiple keys *)
  List.iter (fun k -> ignore (BST.insert t k)) [ 1; 2; 3; 4; 5 ];
  check "all inserted keys are found"
    (List.for_all (fun k -> BST.search t k) [ 1; 2; 3; 4; 5 ]);
  check "non-inserted keys not found"
    (List.for_all (fun k -> not (BST.search t k)) [ 6; 7; 8 ]);

  (* Delete subset *)
  List.iter (fun k -> ignore (BST.delete t k)) [ 2; 4 ];
  check "deleted keys gone" ((not (BST.search t 2)) && not (BST.search t 4));
  check "remaining keys intact"
    (BST.search t 1 && BST.search t 3 && BST.search t 5)

(* ------------------------------------------------------------------ *)
(* 2. Concurrent inserts — disjoint key sets                            *)
(* ------------------------------------------------------------------ *)

let test_concurrent_inserts_disjoint () =
  Printf.printf "\n[2] Concurrent inserts – disjoint key sets\n%!";
  let n_domains = 4 in
  let keys_per_domain = 500 in
  let t = make () in
  (* Domain i inserts keys [i*500 .. (i+1)*500 - 1] *)
  let results =
    spawn_n n_domains (fun i ->
        let base = i * keys_per_domain in
        Array.init keys_per_domain (fun j -> BST.insert t (base + j)))
  in
  (* Every insert on a disjoint key must return true *)
  check "all disjoint inserts return true"
    (Array.for_all (Array.for_all (fun r -> r)) results);
  (* Every key must be searchable afterwards *)
  let all_found =
    let ok = ref true in
    for i = 0 to n_domains - 1 do
      let base = i * keys_per_domain in
      for j = 0 to keys_per_domain - 1 do
        if not (BST.search t (base + j)) then ok := false
      done
    done;
    !ok
  in
  check "all inserted keys found after concurrent inserts" all_found

(* ------------------------------------------------------------------ *)
(* 3. Concurrent inserts — shared key space (high contention)           *)
(* ------------------------------------------------------------------ *)

let test_concurrent_inserts_shared () =
  Printf.printf "\n[3] Concurrent inserts – shared key space\n%!";
  let n_domains = 4 in
  let key_range = 20 in
  (* tiny range → heavy contention *)
  let iters = 2000 in
  let t = make () in
  let _results =
    spawn_n n_domains (fun _i ->
        for k = 0 to iters - 1 do
          ignore (BST.insert t (k mod key_range))
        done)
  in
  (* After all inserts, every key in [0..key_range) must be present *)
  check "all keys in shared range are present"
    (let ok = ref true in
     for k = 0 to key_range - 1 do
       if not (BST.search t k) then ok := false
     done;
     !ok)

(* ------------------------------------------------------------------ *)
(* 4. Concurrent deletes only                                           *)
(* ------------------------------------------------------------------ *)

let test_concurrent_deletes () =
  Printf.printf "\n[4] Concurrent deletes only\n%!";
  let n_domains = 4 in
  let n_keys = 1000 in
  let t = make () in
  (* Pre-populate *)
  for k = 0 to n_keys - 1 do
    ignore (BST.insert t k)
  done;
  (* Each domain deletes the whole range; each key should be deleted exactly once *)
  let results =
    spawn_n n_domains (fun _i -> Array.init n_keys (fun k -> BST.delete t k))
  in
  (* For each key, exactly one domain should have gotten true *)
  let true_counts = Array.make n_keys 0 in
  Array.iter
    (fun arr ->
      Array.iteri
        (fun k r -> if r then true_counts.(k) <- true_counts.(k) + 1)
        arr)
    results;
  check "each key deleted exactly once across all domains"
    (Array.for_all (fun c -> c = 1) true_counts);
  (* Tree must now be empty of user keys *)
  check "all keys absent after concurrent deletes"
    (let ok = ref true in
     for k = 0 to n_keys - 1 do
       if BST.search t k then ok := false
     done;
     !ok)

(* ------------------------------------------------------------------ *)
(* 5. Concurrent insert + delete (interleaved, same key space)          *)
(* ------------------------------------------------------------------ *)

let test_concurrent_insert_delete () =
  Printf.printf "\n[5] Concurrent insert + delete mix\n%!";
  let n_domains = 4 in
  let iters = 1000 in
  let key_range = 50 in
  let t = make () in
  (* Odd domains insert; even domains delete *)
  let _r =
    spawn_n n_domains (fun i ->
        for j = 0 to iters - 1 do
          let k = j mod key_range in
          if i mod 2 = 0 then ignore (BST.insert t k)
          else ignore (BST.delete t k)
        done)
  in
  (* No structural assertion can be made about final state, but the
     tree must not crash, and search must not raise. *)
  check "no crash during concurrent insert/delete mix" true;
  check "search is safe after insert/delete mix"
    (let ok = ref true in
     for k = 0 to key_range - 1 do
       try ignore (BST.search t k) with _ -> ok := false
     done;
     !ok)

(* ------------------------------------------------------------------ *)
(* 6. Readers never block — concurrent inserts + searches               *)
(* ------------------------------------------------------------------ *)

let test_concurrent_insert_search () =
  Printf.printf "\n[6] Concurrent inserts + concurrent searches\n%!";
  let n_writers = 2 in
  let n_readers = 2 in
  let key_range = 200 in
  let iters = 1000 in
  let t = make () in
  (* Pre-insert half the keys so readers have something to find *)
  for k = 0 to (key_range / 2) - 1 do
    ignore (BST.insert t k)
  done;
  let writer_domains =
    Array.init n_writers (fun i ->
        Domain.spawn (fun () ->
            for j = 0 to iters - 1 do
              ignore (BST.insert t (((i * iters) + j) mod key_range))
            done))
  in
  let reader_domains =
    Array.init n_readers (fun _i ->
        Domain.spawn (fun () ->
            let errors = ref 0 in
            for j = 0 to iters - 1 do
              try ignore (BST.search t (j mod key_range))
              with _ -> incr errors
            done;
            !errors))
  in
  Array.iter Domain.join writer_domains;
  let reader_errors = Array.map Domain.join reader_domains in
  check "readers never raise during concurrent inserts"
    (Array.for_all (fun e -> e = 0) reader_errors)

(* ------------------------------------------------------------------ *)
(* 7. ABA: re-insert after delete                                       *)
(* ------------------------------------------------------------------ *)

let test_reinsert_after_delete () =
  Printf.printf "\n[7] Re-insert after delete (ABA pattern)\n%!";
  let key = 42 in
  let rounds = 500 in
  let t = make () in
  (* Two domains: one repeatedly inserts, one repeatedly deletes *)
  let inserter =
    Domain.spawn (fun () ->
        for _ = 1 to rounds do
          ignore (BST.insert t key)
        done)
  in
  let deleter =
    Domain.spawn (fun () ->
        for _ = 1 to rounds do
          ignore (BST.delete t key)
        done)
  in
  Domain.join inserter;
  Domain.join deleter;
  check "no crash in ABA insert/delete cycle" true;
  (* State is either present or absent — both valid. Search must not raise. *)
  check "search safe after ABA cycle"
    (try
       ignore (BST.search t key);
       true
     with _ -> false)

(* ------------------------------------------------------------------ *)
(* 8. Idempotency: concurrent duplicate inserts                         *)
(* ------------------------------------------------------------------ *)

let test_idempotent_insert () =
  Printf.printf
    "\n[8] Idempotency: concurrent duplicate inserts of same key\n%!";
  let n_domains = 8 in
  let key = 7 in
  let t = make () in
  let results = spawn_n n_domains (fun _i -> BST.insert t key) in
  (* Exactly one domain must have gotten true *)
  let true_count =
    Array.fold_left (fun acc r -> if r then acc + 1 else acc) 0 results
  in
  check "exactly one concurrent insert of same key returns true" (true_count = 1);
  check "key is present after concurrent duplicate inserts" (BST.search t key)

(* ------------------------------------------------------------------ *)
(* 9. Idempotency: concurrent duplicate deletes                         *)
(* ------------------------------------------------------------------ *)

let test_idempotent_delete () =
  Printf.printf
    "\n[9] Idempotency: concurrent duplicate deletes of same key\n%!";
  let n_domains = 8 in
  let key = 7 in
  let t = make () in
  ignore (BST.insert t key);
  let results = spawn_n n_domains (fun _i -> BST.delete t key) in
  let true_count =
    Array.fold_left (fun acc r -> if r then acc + 1 else acc) 0 results
  in
  check "exactly one concurrent delete of same key returns true" (true_count = 1);
  check "key is absent after concurrent duplicate deletes"
    (not (BST.search t key))

(* ------------------------------------------------------------------ *)
(* 10. Many domains (> 2) – high-fan-out stress                         *)
(* ------------------------------------------------------------------ *)

let test_many_domains () =
  Printf.printf "\n[10] Many domains – high fan-out\n%!";
  let n_domains = 16 in
  let keys_per_domain = 100 in
  let t = make () in
  (* Phase A: every domain inserts its own disjoint slice *)
  let _ =
    spawn_n n_domains (fun i ->
        let base = i * keys_per_domain in
        for j = 0 to keys_per_domain - 1 do
          ignore (BST.insert t (base + j))
        done)
  in
  let total_keys = n_domains * keys_per_domain in
  let found_after_insert =
    let c = ref 0 in
    for k = 0 to total_keys - 1 do
      if BST.search t k then incr c
    done;
    !c
  in
  check "all keys present after many-domain inserts"
    (found_after_insert = total_keys);

  (* Phase B: every domain deletes the entire range *)
  let delete_results =
    spawn_n n_domains (fun _i ->
        let c = ref 0 in
        for k = 0 to total_keys - 1 do
          if BST.delete t k then incr c
        done;
        !c)
  in
  let total_deletions = Array.fold_left ( + ) 0 delete_results in
  check "total successful deletes equals total keys across all domains"
    (total_deletions = total_keys);
  let found_after_delete =
    let c = ref 0 in
    for k = 0 to total_keys - 1 do
      if BST.search t k then incr c
    done;
    !c
  in
  check "no keys remain after many-domain deletes" (found_after_delete = 0)

(* ------------------------------------------------------------------ *)
(* 11. Large key space – low contention                                  *)
(* ------------------------------------------------------------------ *)

let test_large_key_space () =
  Printf.printf "\n[11] Large key space – low contention\n%!";
  let n_domains = 4 in
  let n_keys = 10_000 in
  let t = make () in
  let _ =
    spawn_n n_domains (fun i ->
        let chunk = n_keys / n_domains in
        let lo = i * chunk in
        let hi = if i = n_domains - 1 then n_keys else lo + chunk in
        for k = lo to hi - 1 do
          ignore (BST.insert t k)
        done)
  in
  let all_found =
    let ok = ref true in
    for k = 0 to n_keys - 1 do
      if not (BST.search t k) then ok := false
    done;
    !ok
  in
  check "all keys found in large key space after concurrent inserts" all_found

(* ------------------------------------------------------------------ *)
(* 12. Interleaved insert/delete/search on a single key (linearizability *)
(*     sanity: final state must be consistent)                           *)
(* ------------------------------------------------------------------ *)

let test_single_key_linearizability () =
  Printf.printf "\n[12] Single-key linearizability sanity\n%!";
  let key = 1 in
  let iters = 2000 in
  let t = make () in
  let d1 =
    Domain.spawn (fun () ->
        for _ = 1 to iters do
          ignore (BST.insert t key)
        done)
  in
  let d2 =
    Domain.spawn (fun () ->
        for _ = 1 to iters do
          ignore (BST.delete t key)
        done)
  in
  let d3 =
    Domain.spawn (fun () ->
        let bad = ref 0 in
        for _ = 1 to iters do
          try ignore (BST.search t key) with _ -> incr bad
        done;
        !bad)
  in
  Domain.join d1;
  Domain.join d2;
  let search_errors = Domain.join d3 in
  check "search never raises during single-key insert/delete storm"
    (search_errors = 0);
  (* Final state: either present or absent is fine, but must be consistent *)
  let present = BST.search t key in
  let present2 = BST.search t key in
  check "repeated search returns consistent result" (present = present2)

(* ------------------------------------------------------------------ *)
(* 13. Delete returns false for keys never inserted                      *)
(* ------------------------------------------------------------------ *)

let test_delete_absent_concurrent () =
  Printf.printf "\n[13] Delete absent keys – concurrent\n%!";
  let n_domains = 4 in
  let key_range = 100 in
  let t = make () in
  (* Insert only even keys *)
  for k = 0 to key_range - 1 do
    if k mod 2 = 0 then ignore (BST.insert t k)
  done;
  (* All domains try to delete odd (absent) keys *)
  let results =
    spawn_n n_domains (fun _i ->
        Array.init (key_range / 2) (fun j -> BST.delete t ((2 * j) + 1)))
  in
  check "delete of never-inserted keys always returns false"
    (Array.for_all (Array.for_all (fun r -> not r)) results)

(* ------------------------------------------------------------------ *)
(* 14. Concurrent independent trees do not interfere                     *)
(* ------------------------------------------------------------------ *)

let test_independent_trees () =
  Printf.printf "\n[14] Concurrent independent trees\n%!";
  let n = 4 in
  let keys = 200 in
  (* Each domain operates on its own tree *)
  let results =
    spawn_n n (fun _i ->
        let t = make () in
        for k = 0 to keys - 1 do
          ignore (BST.insert t k)
        done;
        let found = ref 0 in
        for k = 0 to keys - 1 do
          if BST.search t k then incr found
        done;
        !found)
  in
  check "each independent tree contains exactly its own keys"
    (Array.for_all (fun c -> c = keys) results)

(* ------------------------------------------------------------------ *)
(* 15. Snapshot consistency: search results do not go backwards          *)
(*     (once a key is returned as present, a later search from the same  *)
(*      domain may or may not see it — but must not raise)               *)
(* ------------------------------------------------------------------ *)

let test_search_no_raise_under_churn () =
  Printf.printf "\n[15] Search never raises under heavy churn\n%!";
  let key_range = 30 in
  let iters = 3000 in
  let t = make () in
  let churn =
    Domain.spawn (fun () ->
        for j = 0 to iters - 1 do
          let k = j mod key_range in
          if j mod 2 = 0 then ignore (BST.insert t k)
          else ignore (BST.delete t k)
        done)
  in
  let searcher =
    Domain.spawn (fun () ->
        let bad = ref 0 in
        for j = 0 to iters - 1 do
          try ignore (BST.search t (j mod key_range)) with _ -> incr bad
        done;
        !bad)
  in
  Domain.join churn;
  let errs = Domain.join searcher in
  check "search never raises under insert/delete churn" (errs = 0)

(* ------------------------------------------------------------------ *)
(* Entry point                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  Printf.printf "=== Lock-free BST concurrent test suite ===\n%!";
  test_single_domain_insert_search_delete ();
  test_concurrent_inserts_disjoint ();
  test_concurrent_inserts_shared ();
  test_concurrent_deletes ();
  test_concurrent_insert_delete ();
  test_concurrent_insert_search ();
  test_reinsert_after_delete ();
  test_idempotent_insert ();
  test_idempotent_delete ();
  test_many_domains ();
  test_large_key_space ();
  test_single_key_linearizability ();
  test_delete_absent_concurrent ();
  test_independent_trees ();
  test_search_no_raise_under_churn ();
  Printf.printf "\n=== Done ===\n%!"
