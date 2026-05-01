# Lock-free Binary Search Tree

A concurrent lock-free BST implementation in OCaml, based on the algorithm by Natarajan and Mittal (PPoPP 2014).

## Video Submission Link
https://drive.google.com/drive/folders/1u7lm32U_ibzI56fWDup667QOX58De_Dc


https://drive.google.com/file/d/1Lr6R6GuDVN8xFDYI5bAKA-cx_AsLr95N/view?usp=sharing

## Goal

The main goal of this project is to implement a lock-free binary search tree that allows multiple threads to safely perform search, insert, and delete operations concurrently without using traditional locks. Instead, we use compare-and-swap (CAS) operations to ensure thread safety.

## Implementation

The lock-free BST is implemented in `lib/Bst.ml` and follows the algorithm from:

> Natarajan and Mittal, "Fast Concurrent Lock-Free Binary Search Trees", PPoPP 2014

### Key Design Decisions

- **External (leaf-oriented) BST**: Only leaf nodes store actual keys. Internal nodes act as guides.
- **Sentinel nodes**: The tree starts with a fixed sentinel structure (inf0 < inf1 < inf2) to simplify edge-based modifications.
- **Edge-based marking**: Each edge (pointer to a child) has flag and tag bits stored in `AtomicFlagTag` cells. This helps detect concurrent modifications.
- **CAS operations**: All modifications use compare-and-swap to avoid blocking other threads.

### How it Works

1. **Search**: Traverses the tree without any locking. Reads edges atomically to check if they're marked for deletion.

2. **Insert**: 
   - Finds the appropriate leaf position
   - Creates a new internal node and two leaf nodes
   - Uses CAS to replace the old leaf with the new subtree

3. **Delete**:
   - Marks the target leaf as "to be deleted" (logical deletion)
   - Marks the edge from its parent (physical deletion)
   - Uses CAS to physically remove the node

The edge marking scheme (flag + tag bits) helps detect when another thread is in the middle of modifying the same edge, allowing safe retry.

## Project Structure

```
concurrent-BST/
├── lib/                    # Core implementations
│   ├── Bst.ml / Bst.mli   # Lock-free BST (main implementation)
│   ├── coarseGrainedBST.ml # Coarse-grained locking version
│   ├── optimisticLazyBST.ml # Optimistic lazy BST
│   ├── lockfree_skiplist.ml # Lock-free skip list
│   ├── atomic_markable_ref.ml # Atomic markable reference
│   └── AtomicFlagTag.ml    # Flag/tag bits for edges
├── benchmark/              # Benchmarking code
│   ├── benchmark_bst.ml   # Main benchmark driver
│   ├── run_benchmarks.sh  # Script to run full suite
│   └── plot_results.py    # Plot generation
├── test/                   # Property-based tests
│   ├── qcheck_lin_lockfree_bst.ml
│   ├── qcheck_stm_lockfree_bst.ml
│   └── test_bst_concurrent.ml
└── results/                # Benchmark output
```

## Building

```bash
dune build
```

## Running Tests

```bash
dune test
```

## Benchmarks

Run a single benchmark:

```bash
dune exec benchmark/benchmark_bst.exe -- \
  --impl lockfree \
  --threads 24 \
  --search 90 \
  --duration 3.0
```

Key parameters:
- `--impl` — Implementation to test (coarse, lockfree, lazy, skiplist)
- `--threads` — Number of concurrent threads
- `--search` — Percentage of search operations (0-100)
- `--duration` — Duration per run in seconds
- `--dist` — Key distribution (uniform, normal, skewed, degenerate)

Other parameters can be found in `benchmark/benchmark_bst.ml`.


## Other Implementations

For comparison, the project also includes:

- **Coarse-grained BST** (`lib/coarseGrainedBST.ml`) — Single global mutex
- **Optimistic lazy BST** (`lib/optimisticLazyBST.ml`) — Per-node locking with lazy deletion
- **Lock-free skip list** (`lib/lockfree_skiplist.ml`) — Alternative lock-free data structure

## References

- Natarajan, A., & Mittal, N. (2014). Fast Concurrent Lock-Free Binary Search Trees. PPoPP 2014.
- CS6868_s26 — https://github.com/fplaunchpad/cs6868_s26
- Lock-free skip list implementation — https://github.com/agentcreatormax/Concurrent_SkipLists
