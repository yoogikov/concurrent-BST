#!/bin/bash
# Run comprehensive benchmarks for BST implementations

set -e

BENCHMARK_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_DIR="$BENCHMARK_DIR/results"
mkdir -p "$RESULTS_DIR"

DURATION=3.0
RUNS=3
IMPLEMENTATIONS="coarse lockfree"
THREAD_COUNTS="1 2 4 8 12 16 20 24 28"

echo "=== Starting Comprehensive BST Benchmarks ==="
echo "Duration: ${DURATION}s per run"
echo "Runs: $RUNS"
echo "Implementations: $IMPLEMENTATIONS"
echo "Thread counts: $THREAD_COUNTS"
echo ""

cd "$BENCHMARK_DIR"

# Experiment 1: High search ratio (90%)
echo "Experiment 1: High Search Ratio (90% read, 5% insert, 5% delete)"
CSV_FILE="$RESULTS_DIR/high_search.csv"
rm -f "$CSV_FILE"
echo "impl,threads,search_pct,median,avg" > "$CSV_FILE"

for impl in $IMPLEMENTATIONS; do
  for threads in $THREAD_COUNTS; do
    echo "  Running $impl with $threads threads..."
    dune exec benchmark/benchmark_bst.exe -- \
      --impl "$impl" \
      --threads "$threads" \
      --search 90 \
      --duration "$DURATION" \
      --runs "$RUNS" \
      --csv "$CSV_FILE" \
      2>&1 | tail -1
  done
done

echo ""
echo "Results saved to $CSV_FILE"
echo ""

# Experiment 2: Low search ratio (50%)
echo "Experiment 2: Low Search Ratio (50% read, 25% insert, 25% delete)"
CSV_FILE="$RESULTS_DIR/low_search.csv"
rm -f "$CSV_FILE"
echo "impl,threads,search_pct,median,avg" > "$CSV_FILE"

for impl in $IMPLEMENTATIONS; do
  for threads in $THREAD_COUNTS; do
    echo "  Running $impl with $threads threads..."
    dune exec benchmark/benchmark_bst.exe -- \
      --impl "$impl" \
      --threads "$threads" \
      --search 50 \
      --duration "$DURATION" \
      --runs "$RUNS" \
      --csv "$CSV_FILE" \
      2>&1 | tail -1
  done
done

echo ""
echo "Results saved to $CSV_FILE"
echo ""

# Experiment 3: Varying search ratio (fixed 28 threads)
echo "Experiment 3: Varying Search Ratio (28 threads, varying search %)"
CSV_FILE="$RESULTS_DIR/varying_search.csv"
rm -f "$CSV_FILE"
echo "impl,threads,search_pct,median,avg" > "$CSV_FILE"

SEARCH_PCTS="10 30 50 70 90"

for impl in $IMPLEMENTATIONS; do
  for search_pct in $SEARCH_PCTS; do
    echo "  Running $impl with 28 threads, ${search_pct}% search..."
    dune exec benchmark/benchmark_bst.exe -- \
      --impl "$impl" \
      --threads 28 \
      --search "$search_pct" \
      --duration "$DURATION" \
      --runs "$RUNS" \
      --csv "$CSV_FILE" \
      2>&1 | tail -1
  done
done

echo ""
echo "Results saved to $CSV_FILE"
echo ""

echo "=== Benchmarking Complete ==="
echo "Results are in $RESULTS_DIR"
echo "Run 'python3 benchmark/plot_results.py' to generate plots"