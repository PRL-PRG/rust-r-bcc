#!/bin/bash

echo "Running the R implementation"
for i in {1..10}; do
    scripts/bench.R 2>/dev/null >> origtimings.csv
done

echo "Running the Java implementation"
for i in {1..10}; do
    (cd "/Users/jakobeha/Documents/grad/research/r-compile-server/server" && mvn test -q -Dtest=BCCompilerBenchmarkTest#testBenchmark) 2>/dev/null >> javatimings.csv
done

echo "Running the Rust implementation"
for i in {1..10}; do
    NOCHECK=1 cargo run --quiet --release --package test_build --bin test_build -- -b 2>/dev/null >> mytimings.csv
done

