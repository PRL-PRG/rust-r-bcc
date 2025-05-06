#!/bin/bash

# Set default number of iterations
ITERATIONS=${1:-10}

echo "Running $ITERATIONS iterations of the benchmarks"

echo "Running the R implementation"
echo "r" > r-timings.csv
for i in $(seq 1 $ITERATIONS); do
    echo "- R $i"
    scripts/bench.R >> r-timings.csv
done


echo "Running the Java implementation"
JAVA_SERVER_PATH=${JAVA_SERVER_PATH:-"/Users/jakobeha/Documents/grad/research"}
echo "java" > java-timings.csv
for i in $(seq 1 $ITERATIONS); do
echo "- Java $i"
    (cd "$JAVA_SERVER_PATH/r-compile-server/server" && mvn test -q -Dtest=BCCompilerBenchmarkTest#testBenchmark) >> java-timings.csv
done


echo "Running the Rust implementation"
echo "rust" > rust-timings.csv
for i in {1..10}; do
    echo "- Rust $i"
    NOCHECK=1 cargo run --quiet --release --package test_build --bin test_build -- -b >> rust-timings.csv
done

paste -d ',' r-timings.csv java-timings.csv rust-timings.csv > timings.csv

./scripts/compute-timings.R .