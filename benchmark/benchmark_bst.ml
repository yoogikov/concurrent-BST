(* Benchmark for concurrent BST implementations

   Measures throughput (ops/sec) for different BST implementations
   under various workload ratios (search/insert/delete mix) and thread counts.
*)

module type BST = sig
  type 'a t
  val create : ('a -> int) -> ('a -> string) -> 'a t
  val search : 'a t -> 'a -> bool
  val insert : 'a t -> 'a -> bool
  val delete : 'a t -> 'a -> bool
end

module type BST_WITH_STATS = sig
  include BST
  val sequential_stats : 'a t -> int * int * float
end

(* Adapt the monomorphic int skiplist to the generic BST signature by storing
   the key projection function inside the wrapper record. *)
module SkiplistBST : BST = struct
  type 'a t = { sl: Lockfree_skiplist.t; key: 'a -> int }
  let create key_fn _sf = { sl = Lockfree_skiplist.create 32; key = key_fn }
  let search t x = Lockfree_skiplist.search t.sl (t.key x)
  let insert t x = Lockfree_skiplist.insert t.sl (t.key x)
  let delete t x = Lockfree_skiplist.delete t.sl (t.key x)
end

(* Key distribution for generated operand keys *)
type key_dist = Uniform | Normal | Skewed | Degenerate

(* Shared counter used only in Degenerate mode to issue monotonically
   increasing keys, which forces sorted insertion and an O(n)-height chain. *)
let degenerate_counter = Atomic.make 0

let sample_key dist rng value_range =
  match dist with
  | Uniform -> Random.State.int rng value_range
  | Normal ->
    (* Box-Muller: mean at value_range/2, stddev = range/6 so ~99.7% in-range *)
    let u1 = max 1e-10 (Random.State.float rng 1.0) in
    let u2 = Random.State.float rng 1.0 in
    let pi = 4.0 *. atan 1.0 in
    let z  = sqrt (-2.0 *. log u1) *. cos (2.0 *. pi *. u2) in
    let v  = (float_of_int value_range /. 2.0)
             +. (float_of_int value_range /. 6.0) *. z in
    max 0 (min (value_range - 1) (int_of_float v))
  | Skewed ->
    (* Squaring a uniform variate concentrates mass near 0 (power-law-like) *)
    let u = Random.State.float rng 1.0 in
    int_of_float (float_of_int value_range *. u *. u)
  | Degenerate ->
    (* Always issues the next integer: forces right-chain degenerate tree *)
    Atomic.fetch_and_add degenerate_counter 1 mod value_range

(* Populate a BST according to the chosen distribution.
   For Degenerate mode the keys are inserted in strict ascending order so the
   tree immediately becomes a right-leaning chain, and the counter is set so
   that subsequent worker-thread samples continue from there. *)
let populate_bst insert dist rng initial_size value_range =
  match dist with
  | Degenerate ->
    for i = 0 to initial_size - 1 do
      ignore (insert (i mod value_range))
    done;
    Atomic.set degenerate_counter initial_size
  | _ ->
    for _ = 1 to initial_size do
      ignore (insert (sample_key dist rng value_range))
    done

(* Atomic counter for total operations *)
let total_ops = Atomic.make 0

(* Run benchmark for a given duration *)
let benchmark_bst
    (module B : BST)
    ~num_threads
    ~duration_sec
    ~search_pct
    ~insert_pct
    ~initial_size
    ~value_range
    ~dist =

  let bst = B.create (fun x -> x) string_of_int in
  let rng = Random.State.make [|42|] in
  populate_bst (B.insert bst) dist rng initial_size value_range;

  Atomic.set total_ops 0;
  let stop = Atomic.make false in

  let worker () =
    let local_rng = Random.State.make_self_init () in
    let local_ops = ref 0 in

    while not (Atomic.get stop) do
      let op_type = Random.State.int local_rng 100 in
      let value = sample_key dist local_rng value_range in

      (if op_type < search_pct then
         B.search bst value
      else if op_type < search_pct + insert_pct then
         B.insert bst value
      else
         B.delete bst value) |> ignore;

      incr local_ops
    done;

    Atomic.fetch_and_add total_ops !local_ops |> ignore
  in

  (* Start worker domains *)
  let start_time = Unix.gettimeofday () in
  let domains = List.init num_threads (fun _ -> Domain.spawn worker) in

  (* Run for specified duration *)
  Unix.sleepf duration_sec;
  Atomic.set stop true;

  (* Wait for all domains to finish *)
  List.iter Domain.join domains;
  let end_time = Unix.gettimeofday () in

  let elapsed = end_time -. start_time in
  let ops = Atomic.get total_ops in
  let throughput = float_of_int ops /. elapsed in

  (ops, elapsed, throughput)

(* Main benchmark runner *)
let run_benchmark impl_name num_threads search_pct insert_pct duration initial_size value_range runs dist =
  let module_of_name = function
    | "coarse"   -> (module CoarseGrainedBST : BST)
    | "lockfree" -> (module Bst : BST)
    | "lazy"     -> (module OptimisticLazyBST : BST)
    | "skiplist" -> (module SkiplistBST : BST)
    | _ -> failwith "Unknown implementation"
  in

  let impl_module = module_of_name impl_name in
  let results = ref [] in

  Printf.printf "Running %s with %d threads, %d%% search / %d%% insert / %d%% delete...\n%!"
    impl_name num_threads search_pct insert_pct (100 - search_pct - insert_pct);

  for run = 1 to runs do
    Printf.printf "  Run %d/%d%s... %!" run runs (if run = 1 then " (warmup)" else "");
    if run > 1 then Gc.compact ();
    let (ops, elapsed, throughput) =
      benchmark_bst impl_module ~num_threads ~duration_sec:duration
        ~search_pct ~insert_pct ~initial_size ~value_range ~dist
    in
    Printf.printf "%d ops in %.2fs (%.0f ops/sec)\n%!" ops elapsed throughput;
    if run > 1 then results := throughput :: !results
  done;

  (* Calculate statistics over measured runs only (run 1 was warmup) *)
  let sorted = List.sort compare !results in
  let median = List.nth sorted (List.length sorted / 2) in
  let avg = (List.fold_left (+.) 0.0 !results) /. float_of_int (List.length !results) in

  Printf.printf "  Median: %.0f ops/sec, Avg: %.0f ops/sec\n\n%!" median avg;
  (median, avg)

(* ── Latency benchmark ──────────────────────────────────────────────────── *)

type latency_result = {
  elapsed            : float;
  total_ops          : int;
  throughput         : float;
  search_hit_avg_us  : float;
  search_miss_avg_us : float;
  insert_hit_avg_us  : float;
  insert_miss_avg_us : float;
  delete_hit_avg_us  : float;
  delete_miss_avg_us : float;
}

let avg_us count total_time =
  if count = 0 then Float.nan
  else total_time /. float_of_int count *. 1e6

(* Like benchmark_bst but also records per-operation average latency,
   split by whether each call returned true (hit) or false (miss). *)
let benchmark_bst_latency
    (module B : BST)
    ~num_threads
    ~duration_sec
    ~search_pct
    ~insert_pct
    ~initial_size
    ~value_range
    ~dist =

  let bst = B.create (fun x -> x) string_of_int in
  let rng = Random.State.make [|42|] in
  populate_bst (B.insert bst) dist rng initial_size value_range;

  let stop = Atomic.make false in

  let worker () =
    let local_rng = Random.State.make_self_init () in
    let sh_c = ref 0 and sh_t = ref 0.0 in
    let sm_c = ref 0 and sm_t = ref 0.0 in
    let ih_c = ref 0 and ih_t = ref 0.0 in
    let im_c = ref 0 and im_t = ref 0.0 in
    let dh_c = ref 0 and dh_t = ref 0.0 in
    let dm_c = ref 0 and dm_t = ref 0.0 in
    while not (Atomic.get stop) do
      let op = Random.State.int local_rng 100 in
      let v  = sample_key dist local_rng value_range in
      let t0 = Unix.gettimeofday () in
      let ok =
        if op < search_pct then B.search bst v
        else if op < search_pct + insert_pct then B.insert bst v
        else B.delete bst v
      in
      let dt = Unix.gettimeofday () -. t0 in
      if op < search_pct then
        (if ok then (incr sh_c; sh_t := !sh_t +. dt)
         else       (incr sm_c; sm_t := !sm_t +. dt))
      else if op < search_pct + insert_pct then
        (if ok then (incr ih_c; ih_t := !ih_t +. dt)
         else       (incr im_c; im_t := !im_t +. dt))
      else
        (if ok then (incr dh_c; dh_t := !dh_t +. dt)
         else       (incr dm_c; dm_t := !dm_t +. dt))
    done;
    (!sh_c, !sh_t, !sm_c, !sm_t, !ih_c, !ih_t, !im_c, !im_t, !dh_c, !dh_t, !dm_c, !dm_t)
  in

  let start_time = Unix.gettimeofday () in
  let domains = List.init num_threads (fun _ -> Domain.spawn worker) in
  Unix.sleepf duration_sec;
  Atomic.set stop true;
  let worker_results = List.map Domain.join domains in
  let elapsed = Unix.gettimeofday () -. start_time in

  (* Aggregate per-thread counts and total times *)
  let sh_c = ref 0 and sh_t = ref 0.0 in
  let sm_c = ref 0 and sm_t = ref 0.0 in
  let ih_c = ref 0 and ih_t = ref 0.0 in
  let im_c = ref 0 and im_t = ref 0.0 in
  let dh_c = ref 0 and dh_t = ref 0.0 in
  let dm_c = ref 0 and dm_t = ref 0.0 in
  List.iter (fun (a,b,c,d,e,f,g,h,i,j,k,l) ->
    sh_c := !sh_c + a; sh_t := !sh_t +. b;
    sm_c := !sm_c + c; sm_t := !sm_t +. d;
    ih_c := !ih_c + e; ih_t := !ih_t +. f;
    im_c := !im_c + g; im_t := !im_t +. h;
    dh_c := !dh_c + i; dh_t := !dh_t +. j;
    dm_c := !dm_c + k; dm_t := !dm_t +. l)
    worker_results;

  let ops = !sh_c + !sm_c + !ih_c + !im_c + !dh_c + !dm_c in
  { elapsed;
    total_ops          = ops;
    throughput         = float_of_int ops /. elapsed;
    search_hit_avg_us  = avg_us !sh_c !sh_t;
    search_miss_avg_us = avg_us !sm_c !sm_t;
    insert_hit_avg_us  = avg_us !ih_c !ih_t;
    insert_miss_avg_us = avg_us !im_c !im_t;
    delete_hit_avg_us  = avg_us !dh_c !dh_t;
    delete_miss_avg_us = avg_us !dm_c !dm_t }

let run_benchmark_latency impl_name num_threads search_pct insert_pct duration initial_size value_range runs dist =
  let module_of_name = function
    | "coarse"   -> (module CoarseGrainedBST : BST)
    | "lockfree" -> (module Bst : BST)
    | "lazy"     -> (module OptimisticLazyBST : BST)
    | "skiplist" -> (module SkiplistBST : BST)
    | _          -> failwith "Unknown implementation"
  in
  let impl_module = module_of_name impl_name in

  Printf.printf "Running latency benchmark: %s, %d threads, %d%% search / %d%% insert / %d%% delete...\n%!"
    impl_name num_threads search_pct insert_pct (100 - search_pct - insert_pct);

  let results = ref [] in
  for run = 1 to runs do
    Printf.printf "  Run %d/%d%s... %!" run runs (if run = 1 then " (warmup)" else "");
    if run > 1 then Gc.compact ();
    let r = benchmark_bst_latency impl_module
              ~num_threads ~duration_sec:duration
              ~search_pct ~insert_pct ~initial_size ~value_range ~dist in
    Printf.printf "%d ops in %.2fs (%.0f ops/sec)\n%!" r.total_ops r.elapsed r.throughput;
    if run > 1 then results := r :: !results
  done;

  let n = float_of_int (runs - 1) in
  let mean f = List.fold_left (fun acc r -> acc +. f r) 0.0 !results /. n in
  let fmt v = if Float.is_nan v then "    n/a" else Printf.sprintf "%7.2f" v in

  Printf.printf "\n  Average latencies over %d run(s) (excluding warmup):\n" (runs - 1);
  Printf.printf "  %-8s  hit: %s us  miss: %s us\n" "search"
    (fmt (mean (fun r -> r.search_hit_avg_us)))
    (fmt (mean (fun r -> r.search_miss_avg_us)));
  Printf.printf "  %-8s  hit: %s us  miss: %s us\n" "insert"
    (fmt (mean (fun r -> r.insert_hit_avg_us)))
    (fmt (mean (fun r -> r.insert_miss_avg_us)));
  Printf.printf "  %-8s  hit: %s us  miss: %s us\n\n%!" "delete"
    (fmt (mean (fun r -> r.delete_hit_avg_us)))
    (fmt (mean (fun r -> r.delete_miss_avg_us)))

(* ── Iteration-count benchmark (lockfree only) ──────────────────────────── *)

(* Measures average number of seek() calls per insert/delete invocation.
   Uses the global atomic counters in Bst, so only valid for lockfree. *)
let benchmark_bst_iters ~num_threads ~duration_sec ~search_pct ~insert_pct ~initial_size ~value_range ~dist =
  let bst = Bst.create (fun x -> x) string_of_int in
  let rng = Random.State.make [|42|] in
  populate_bst (Bst.insert bst) dist rng initial_size value_range;

  Bst.reset_iter_counts ();
  Atomic.set total_ops 0;
  let stop = Atomic.make false in

  let worker () =
    let local_rng = Random.State.make_self_init () in
    let local_ops = ref 0 in
    let insert_calls = ref 0 in
    let delete_calls = ref 0 in
    while not (Atomic.get stop) do
      let op = Random.State.int local_rng 100 in
      let v  = sample_key dist local_rng value_range in
      (if op < search_pct then Bst.search bst v
       else if op < search_pct + insert_pct then (incr insert_calls; Bst.insert bst v)
       else (incr delete_calls; Bst.delete bst v)) |> ignore;
      incr local_ops
    done;
    Atomic.fetch_and_add total_ops !local_ops |> ignore;
    (!insert_calls, !delete_calls)
  in

  let start_time = Unix.gettimeofday () in
  let domains = List.init num_threads (fun _ -> Domain.spawn worker) in
  Unix.sleepf duration_sec;
  Atomic.set stop true;
  let worker_results = List.map Domain.join domains in
  let elapsed = Unix.gettimeofday () -. start_time in

  let total_insert_calls = List.fold_left (fun acc (ic, _) -> acc + ic) 0 worker_results in
  let total_delete_calls = List.fold_left (fun acc (_, dc) -> acc + dc) 0 worker_results in
  let ops = Atomic.get total_ops in
  let insert_iters = Bst.get_insert_iters () in
  let delete_iters = Bst.get_delete_iters () in
  let avg f n = if n = 0 then Float.nan else float_of_int f /. float_of_int n in

  (ops, elapsed, float_of_int ops /. elapsed,
   avg insert_iters total_insert_calls,
   avg delete_iters total_delete_calls)

let run_benchmark_iters num_threads search_pct insert_pct duration initial_size value_range runs dist =
  Printf.printf "Running iter benchmark: lockfree, %d threads, %d%% search / %d%% insert / %d%% delete...\n%!"
    num_threads search_pct insert_pct (100 - search_pct - insert_pct);

  let throughputs = ref [] in
  let avg_inserts = ref [] in
  let avg_deletes = ref [] in

  for run = 1 to runs do
    Printf.printf "  Run %d/%d%s... %!" run runs (if run = 1 then " (warmup)" else "");
    if run > 1 then Gc.compact ();
    let (ops, elapsed, throughput, avg_insert, avg_delete) =
      benchmark_bst_iters ~num_threads ~duration_sec:duration
        ~search_pct ~insert_pct ~initial_size ~value_range ~dist
    in
    Printf.printf "%d ops in %.2fs (%.0f ops/sec)\n%!" ops elapsed throughput;
    Printf.printf "    insert: %.3f seeks/call  delete: %.3f seeks/call\n%!"
      avg_insert avg_delete;
    if run > 1 then begin
      throughputs := throughput :: !throughputs;
      avg_inserts := avg_insert :: !avg_inserts;
      avg_deletes := avg_delete :: !avg_deletes
    end
  done;

  let n = float_of_int (runs - 1) in
  let mean xs = List.fold_left (+.) 0.0 xs /. n in
  let sorted = List.sort compare !throughputs in
  let median = List.nth sorted ((runs - 1) / 2) in

  Printf.printf "\n  Median throughput: %.0f ops/sec\n" median;
  Printf.printf "  Avg seeks/call — insert: %.3f  delete: %.3f\n\n%!"
    (mean !avg_inserts) (mean !avg_deletes)

(* ── Sequential-stats snapshot benchmark ────────────────────────────────── *)

(* Runs the concurrent workload for [duration_sec] seconds. Every
   [stats_interval] total operations the worker domains are all paused so that
   [sequential_stats] can be called safely (no concurrent ops in flight).
   Returns the collected snapshots as a list of (ops_at_snapshot, n, height, ratio). *)
let benchmark_seqstats
    (module B : BST_WITH_STATS)
    ~num_threads
    ~duration_sec
    ~search_pct
    ~insert_pct
    ~initial_size
    ~value_range
    ~stats_interval
    ~dist =

  let bst = B.create (fun x -> x) string_of_int in
  let rng = Random.State.make [|42|] in
  populate_bst (B.insert bst) dist rng initial_size value_range;

  let ops_done   = Atomic.make 0 in
  let stop       = Atomic.make false in
  let pause_req  = Atomic.make false in
  let paused_cnt = Atomic.make 0 in

  let worker () =
    let local_rng = Random.State.make_self_init () in
    while not (Atomic.get stop) do
      if Atomic.get pause_req then begin
        Atomic.fetch_and_add paused_cnt 1 |> ignore;
        while Atomic.get pause_req && not (Atomic.get stop) do () done;
        Atomic.fetch_and_add paused_cnt (-1) |> ignore
      end;
      if not (Atomic.get stop) then begin
        let op = Random.State.int local_rng 100 in
        let v  = sample_key dist local_rng value_range in
        ignore (
          if op < search_pct then B.search bst v
          else if op < search_pct + insert_pct then B.insert bst v
          else B.delete bst v);
        Atomic.fetch_and_add ops_done 1 |> ignore
      end
    done
  in

  let domains = List.init num_threads (fun _ -> Domain.spawn worker) in

  let snapshots      = ref [] in
  let next_checkpoint = ref stats_interval in
  let start          = Unix.gettimeofday () in

  while Unix.gettimeofday () -. start < duration_sec do
    let cur = Atomic.get ops_done in
    if cur >= !next_checkpoint then begin
      Atomic.set pause_req true;
      while Atomic.get paused_cnt < num_threads do () done;
      let (n, h, ratio) = B.sequential_stats bst in
      snapshots := (cur, n, h, ratio) :: !snapshots;
      next_checkpoint := !next_checkpoint + stats_interval;
      Atomic.set pause_req false
    end;
    Unix.sleepf 0.001
  done;

  Atomic.set stop true;
  Atomic.set pause_req false;
  List.iter Domain.join domains;
  List.rev !snapshots

let run_benchmark_seqstats impl_name num_threads search_pct insert_pct duration initial_size value_range stats_interval dist =
  let module_of_name = function
    | "coarse"   -> (module CoarseGrainedBST   : BST_WITH_STATS)
    | "lockfree" -> (module Bst                : BST_WITH_STATS)
    | "lazy"     -> (module OptimisticLazyBST : BST_WITH_STATS)
    | _          -> failwith "Unknown implementation"
  in
  let impl_module = module_of_name impl_name in

  Printf.printf "Running seqstats: %s, %d threads, %d%% search / %d%% insert / %d%% delete, interval=%d ops...\n%!"
    impl_name num_threads search_pct insert_pct (100 - search_pct - insert_pct) stats_interval;

  let snapshots = benchmark_seqstats impl_module
      ~num_threads ~duration_sec:duration
      ~search_pct ~insert_pct ~initial_size ~value_range
      ~stats_interval ~dist
  in

  Printf.printf "  Collected %d snapshot(s):\n%!" (List.length snapshots);
  List.iter (fun (ops, n, h, ratio) ->
    Printf.printf "SNAPSHOT %d %d %d %f\n%!" ops n h ratio
  ) snapshots

let () =
  let impl = ref "coarse" in
  let threads = ref 4 in
  let search = ref 90 in
  let insert = ref (-1) in
  let delete = ref (-1) in
  let duration = ref 3.0 in
  let initial_size = ref 1000 in
  let value_range = ref 10000 in
  let runs = ref 5 in
  let csv_output = ref None in
  let latency = ref false in
  let iters = ref false in
  let seqstats = ref false in
  let seqstats_interval = ref 10000 in
  let dist_str = ref "uniform" in

  let speclist = [
    ("--impl", Arg.Set_string impl,
     "Implementation: coarse, lockfree, lazy, skiplist, all (default: coarse)");
    ("--threads", Arg.Set_int threads,
     "Number of threads (default: 4)");
    ("--search", Arg.Set_int search,
     "Percentage of search operations (default: 90)");
    ("--insert", Arg.Set_int insert,
     "Percentage of insert operations (default: half of remaining after search)");
    ("--delete", Arg.Set_int delete,
     "Percentage of delete operations (default: half of remaining after search)");
    ("--duration", Arg.Set_float duration,
     "Duration in seconds (default: 2.0)");
    ("--initial-size", Arg.Set_int initial_size,
     "Initial BST size (default: 1000)");
    ("--value-range", Arg.Set_int value_range,
     "Range of values [0, N) (default: 10000)");
    ("--runs", Arg.Set_int runs,
     "Number of runs (default: 3)");
    ("--csv", Arg.String (fun s -> csv_output := Some s),
     "Output CSV file (optional)");
    ("--latency", Arg.Set latency,
     "Also measure avg hit/miss latency per operation (default: false)");
    ("--iters", Arg.Set iters,
     "Measure avg seek iterations per insert/delete call (lockfree only)");
    ("--seqstats", Arg.Set seqstats,
     "Collect sequential_stats snapshots periodically during the concurrent run");
    ("--seqstats-interval", Arg.Set_int seqstats_interval,
     "Ops between sequential_stats snapshots (default: 10000; only with --seqstats)");
    ("--dist", Arg.Set_string dist_str,
     "Key distribution: uniform, normal, skewed, degenerate (default: uniform). \
      'degenerate' inserts keys in ascending order to force an O(n)-height right chain.");
  ] in

  Arg.parse speclist (fun _ -> ())
    "Benchmark concurrent BST implementations";

  let dist =
    match !dist_str with
    | "uniform"    -> Uniform
    | "normal"     -> Normal
    | "skewed"     -> Skewed
    | "degenerate" -> Degenerate
    | s ->
      Printf.eprintf "Error: unknown --dist value '%s' (choose uniform|normal|skewed|degenerate)\n%!" s;
      exit 1
  in

  let insert_pct =
    if !insert >= 0 then !insert
    else (100 - !search) / 2
  in
  let delete_pct =
    if !delete >= 0 then !delete
    else 100 - !search - insert_pct
  in
  if insert_pct + delete_pct + !search <> 100 then (
    Printf.eprintf
      "Error: search (%d) + insert (%d) + delete (%d) must equal 100\n%!"
      !search insert_pct delete_pct;
    exit 1);

  let all_impls = ["coarse"; "lockfree"; "lazy"; "skiplist"] in
  let impls = if !impl = "all" then all_impls else [!impl] in

  Printf.printf "=== BST Benchmark ===\n";
  Printf.printf "Implementation: %s\n" !impl;
  Printf.printf "Threads: %d\n" !threads;
  Printf.printf "Workload: %d%% search, %d%% insert, %d%% delete\n"
    !search insert_pct delete_pct;
  Printf.printf "Duration: %.1fs per run\n" !duration;
  Printf.printf "Initial size: %d items\n" !initial_size;
  Printf.printf "Value range: [0, %d)\n" !value_range;
  Printf.printf "Key distribution: %s\n" !dist_str;
  Printf.printf "Runs: %d\n" !runs;
  Printf.printf "Latency mode: %b  Iter mode: %b  Seqstats mode: %b\n\n%!"
    !latency !iters !seqstats;

  if !seqstats then begin
    if List.mem "skiplist" impls then (
      Printf.eprintf "Error: --seqstats is not supported for skiplist (no sequential_stats)\n%!";
      exit 1);
    List.iter (fun impl_name ->
      run_benchmark_seqstats impl_name !threads !search insert_pct !duration
        !initial_size !value_range !seqstats_interval dist
    ) impls
  end else if !iters then begin
    if !impl <> "lockfree" && !impl <> "all" then (
      Printf.eprintf "Error: --iters is only valid for --impl lockfree (or all)\n%!";
      exit 1);
    run_benchmark_iters !threads !search insert_pct !duration !initial_size !value_range !runs dist
  end else
    List.iter (fun impl_name ->
      if !latency then
        run_benchmark_latency impl_name !threads !search insert_pct !duration
          !initial_size !value_range !runs dist
      else begin
        let (median, avg) = run_benchmark impl_name !threads !search insert_pct !duration
          !initial_size !value_range !runs dist in
        match !csv_output with
        | Some filename ->
            let oc = open_out_gen [Open_append; Open_creat] 0o644 filename in
            Printf.fprintf oc "%s,%d,%d,%d,%s,%.0f,%.0f\n"
              impl_name !threads !search insert_pct !dist_str median avg;
            close_out oc;
            Printf.printf "Results appended to %s\n%!" filename
        | None -> ()
      end
    ) impls