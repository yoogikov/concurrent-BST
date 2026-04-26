#!/usr/bin/env python3
"""
Plot benchmark results for BST implementations.
Generates three plots matching the slide format:
1. High Search Ratio (90%)
2. Low Search Ratio (50%)
3. Varying Search Ratio (32 threads)
"""

import csv
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path

# Style configuration for BST implementations
IMPL_STYLES = {
    'lockfree': {'label': 'Lock-free BST', 'marker': 's', 'color': 'black', 'linestyle': '--'},
    'coarse': {'label': 'Coarse-grained BST', 'marker': 'D', 'color': 'orange', 'linestyle': ':'},
}

def read_csv(filename):
    """Read CSV file and return data grouped by implementation."""
    data = {}
    with open(filename, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            impl = row['impl']
            if impl not in data:
                data[impl] = {'threads': [], 'search_pct': [], 'median': [], 'avg': []}
            data[impl]['threads'].append(int(row['threads']))
            data[impl]['search_pct'].append(int(row['search_pct']))
            data[impl]['median'].append(float(row['median']))
            data[impl]['avg'].append(float(row['avg']))
    return data

def plot_thread_scaling(csv_file, title, output_file):
    """Plot throughput vs thread count."""
    data = read_csv(csv_file)

    fig, ax = plt.subplots(figsize=(8, 6))

    # Plot each implementation
    for impl in ['coarse', 'lockfree']:
        if impl in data:
            style = IMPL_STYLES[impl]
            threads = data[impl]['threads']
            throughput = data[impl]['median']

            ax.plot(threads, throughput,
                   marker=style['marker'],
                   color=style['color'],
                   linestyle=style['linestyle'],
                   markersize=8,
                   linewidth=2,
                   label=style['label'])

    ax.set_xlabel('threads', fontsize=12)
    ax.set_ylabel('Ops/sec', fontsize=12)
    ax.set_title(title, fontsize=14, fontweight='bold')
    ax.legend(loc='upper left', fontsize=10)
    ax.grid(True, alpha=0.3)

    # Format y-axis with scientific notation
    ax.ticklabel_format(style='scientific', axis='y', scilimits=(0,0))

    plt.tight_layout()
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Saved {output_file}")
    plt.close()

def plot_varying_search(csv_file, output_file):
    """Plot throughput vs search percentage (fixed 28 threads)."""
    data = read_csv(csv_file)

    fig, ax = plt.subplots(figsize=(8, 6))

    # Plot each implementation
    for impl in ['coarse', 'lockfree']:
        if impl in data:
            style = IMPL_STYLES[impl]
            search_pct = data[impl]['search_pct']
            throughput = data[impl]['median']

            ax.plot(search_pct, throughput,
                   marker=style['marker'],
                   color=style['color'],
                   linestyle=style['linestyle'],
                   markersize=8,
                   linewidth=2,
                   label=style['label'])

    ax.set_xlabel('% Search()', fontsize=12)
    ax.set_ylabel('Ops/sec', fontsize=12)
    ax.set_title('As Search Ratio Increases', fontsize=14, fontweight='bold')
    ax.legend(loc='upper left', fontsize=10)
    ax.grid(True, alpha=0.3)

    # Format y-axis with scientific notation
    ax.ticklabel_format(style='scientific', axis='y', scilimits=(0,0))

    plt.tight_layout()
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Saved {output_file}")
    plt.close()

def main():
    # Get script directory and construct results path relative to it
    script_dir = Path(__file__).parent.absolute()
    results_dir = script_dir.parent / 'results'

    if not results_dir.exists():
        print(f"Error: Results directory not found: {results_dir}")
        return

    print("Generating plots...")

    # Plot 1: High Search Ratio
    high_csv = results_dir / 'high_search.csv'
    if high_csv.exists():
        plot_thread_scaling(
            high_csv,
            '90% Search Ratio',
            results_dir / 'plot_high_search.png'
        )
    else:
        print(f"Warning: {high_csv} not found")

    # Plot 2: Low Search Ratio
    low_csv = results_dir / 'low_search.csv'
    if low_csv.exists():
        plot_thread_scaling(
            low_csv,
            '50% Search Ratio',
            results_dir / 'plot_low_search.png'
        )
    else:
        print(f"Warning: {low_csv} not found")

    # Plot 3: Varying Search Ratio
    varying_csv = results_dir / 'varying_search.csv'
    if varying_csv.exists():
        plot_varying_search(
            varying_csv,
            results_dir / 'plot_varying_search.png'
        )
    else:
        print(f"Warning: {varying_csv} not found")

    print("\nAll plots generated successfully!")

if __name__ == '__main__':
    main()