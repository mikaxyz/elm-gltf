#!/usr/bin/env bash
#
# Snapshot the library's src/ into benchmarks/generated/ under a parallel module
# namespace: Gltf -> BenchmarkGltf, Internal -> BenchmarkInternal, Common ->
# BenchmarkCommon. The copy compiles side by side with the live code, so the
# benchmark harness can compare current vs. frozen implementations in one run.
#
# Run this to (re)set the comparison baseline: it captures whatever is currently
# committed in src/. Applying an optimization afterwards makes the benchmark show
# the delta against this frozen copy.
#
# Only runs on a clean working tree, so a snapshot always reflects committed code
# (and an in-progress optimization can never clobber the baseline).
set -euo pipefail

cd "$(dirname "$0")"

if [ -n "$(git -C .. status --porcelain)" ]; then
    echo "Working tree not clean — commit or stash before snapshotting the baseline." >&2
    exit 1
fi

rm -rf generated
mkdir generated

find ../src -name '*.elm' | while read -r file; do
    rel="${file#../src/}"                 # e.g. Gltf/Animation.elm, Common.elm
    out="generated/Benchmark${rel}"       # e.g. generated/BenchmarkGltf/Animation.elm
    mkdir -p "$(dirname "$out")"
    perl -pe '
        s/^module /module Benchmark/;
        s/^import (Common|Gltf|Internal)\b/import Benchmark$1/;
        s/ as (Common|Gltf|Internal)\b/ as Benchmark$1/;
        s/(?<![\w.])(Common|Gltf|Internal)\./Benchmark$1./g;
    ' "$file" > "$out"
done

echo "Mirrored $(find ../src -name '*.elm' | wc -l | tr -d ' ') modules into benchmarks/generated/"
