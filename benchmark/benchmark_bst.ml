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


(* Atomic counter for total operations *)
let total_ops = Atomic.make 0

(* Run benchmark for a given duration *)
let benchmark_bst
    (module B : BST)
    ~num_threads
    ~duration_sec
    ~search_pct
    ~initial_size
    ~value_range =

  (* Create and populate BST *)
  let bst = B.create (fun x -> x) string_of_int in
  let rng = Random.State.make [|42|] in
  for _ = 1 to initial_size do
    let value = Random.State.int rng value_range in
    let _ = B.insert bst value in
    ()
  done;

  (* Reset counter *)
  Atomic.set total_ops 0;
  let stop = Atomic.make false in

  (* Worker thread function *)
  let worker () =
    let local_rng = Random.State.make_self_init () in
    let local_ops = ref 0 in

    while not (Atomic.get stop) do
      let op_type = Random.State.int local_rng 100 in
      let value = Random.State.int local_rng value_range in

      (if op_type < search_pct then
         B.search bst value
      else if op_type < search_pct + ((100 - search_pct) / 2) then
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
let run_benchmark impl_name num_threads search_pct duration initial_size value_range runs =
  let module_of_name = function
    | "coarse" -> (module CoarseGrainedBST : BST)
    | "lockfree" -> (module Bst : BST)
    | "lazy" -> (module OptimisticLazyList: BST)
    | _ -> failwith "Unknown implementation"
  in

  let impl_module = module_of_name impl_name in
  let results = ref [] in

  Printf.printf "Running %s with %d threads, %d%% search...\n%!"
    impl_name num_threads search_pct;

  for run = 1 to runs do
    Printf.printf "  Run %d/%d... %!" run runs;
    (* Compact heap between runs for consistent memory state *)
    if run > 1 then Gc.compact ();
    let (ops, elapsed, throughput) =
      benchmark_bst impl_module ~num_threads ~duration_sec:duration
        ~search_pct ~initial_size ~value_range
    in
    Printf.printf "%d ops in %.2fs (%.0f ops/sec)\n%!" ops elapsed throughput;
    results := throughput :: !results
  done;

  (* Calculate statistics *)
  let sorted = List.sort compare !results in
  let median = List.nth sorted (List.length sorted / 2) in
  let avg = (List.fold_left (+.) 0.0 !results) /. float_of_int (List.length !results) in

  Printf.printf "  Median: %.0f ops/sec, Avg: %.0f ops/sec\n\n%!" median avg;
  (median, avg)

let () =
  let impl = ref "coarse" in
  let threads = ref 4 in
  let search = ref 90 in
  let duration = ref 2.0 in
  let initial_size = ref 1000 in
  let value_range = ref 10000 in
  let runs = ref 3 in
  let csv_output = ref None in

  let speclist = [
    ("--impl", Arg.Set_string impl,
     "Implementation: coarse, lockfree (default: coarse)");
    ("--threads", Arg.Set_int threads,
     "Number of threads (default: 4)");
    ("--search", Arg.Set_int search,
     "Percentage of search operations (default: 90)");
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
  ] in

  Arg.parse speclist (fun _ -> ())
    "Benchmark concurrent BST implementations";

  Printf.printf "=== BST Benchmark ===\n";
  Printf.printf "Implementation: %s\n" !impl;
  Printf.printf "Threads: %d\n" !threads;
  Printf.printf "Workload: %d%% search, %d%% insert, %d%% delete\n"
    !search ((100 - !search)/2) ((100 - !search)/2);
  Printf.printf "Duration: %.1fs per run\n" !duration;
  Printf.printf "Initial size: %d items\n" !initial_size;
  Printf.printf "Value range: [0, %d)\n" !value_range;
  Printf.printf "Runs: %d\n\n%!" !runs;

  let (median, avg) = run_benchmark !impl !threads !search !duration
    !initial_size !value_range !runs in

  (* Output CSV if requested *)
  begin match !csv_output with
  | Some filename ->
      let oc = open_out_gen [Open_append; Open_creat] 0o644 filename in
      Printf.fprintf oc "%s,%d,%d,%.0f,%.0f\n" !impl !threads !search median avg;
      close_out oc;
      Printf.printf "Results appended to %s\n%!" filename
  | None -> ()
  end