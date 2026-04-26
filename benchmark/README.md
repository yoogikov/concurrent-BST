# BST Benchmarks

Comprehensive benchmarking suite for concurrent BST implementations.

## Implementations

The benchmark suite tests 2 different BST implementations:

1. **coarse** - Coarse-grained locking (single global lock)
2. **lockfree** - Lock-free BST based on Natarajan and Mittal

## Usage

### Run a single benchmark

```bash
dune exec benchmark/benchmark_bst.exe -- \
  --impl lockfree \
  --threads 24 \
  --search 90 \
  --duration 3.0 \
  --runs 3
```

### Run comprehensive benchmark suite

```bash
cd benchmark
chmod +x run_benchmarks.sh
./run_benchmarks.sh
```

This will:
- Test both implementations
- Use thread counts: 1, 2, 4, 8, 12, 16, 20, 24, 28
- Run 3 experiments:
  1. High search ratio (90% read, 5% insert, 5% delete)
  2. Low search ratio (50% read, 25% insert, 25% delete)
  3. Varying search ratio (10-90% in 20% increments at 28 threads)
- Save results to `../results/` directory

### Generate plots

```bash
python3 plot_results.py
```

This generates three plots:
- `plot_high_search.png` - Thread scaling with 90% search
- `plot_low_search.png` - Thread scaling with 50% search
- `plot_varying_search.png` - Performance vs search ratio (28 threads)

## Parameters

- `--impl` - Implementation to test (coarse, lockfree)
- `--threads` - Number of concurrent threads
- `--search` - Percentage of search operations (0-100)
- `--duration` - Duration of each run in seconds
- `--initial-size` - Initial number of elements in BST
- `--value-range` - Range of values [0, N)
- `--runs` - Number of runs per configuration (for statistical stability)
- `--csv` - Output CSV file path (optional)

## Results

Results are saved as CSV files with columns:
- `impl` - Implementation name
- `threads` - Number of threads
- `search_pct` - Percentage of search operations
- `median` - Median throughput (ops/sec)
- `avg` - Average throughput (ops/sec)

## Building

From the concurrent-BST root directory:

```bash
dune build benchmark
```

## Notes

- Values are integers in range [0, value_range)
- Initial BST is populated with `initial_size` random elements
- Workload consists of search/insert/delete operations with specified ratios
- Throughput is measured in operations per second