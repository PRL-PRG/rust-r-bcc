#!/bin/bash

# Set default number of iterations
ITERATIONS=${1:-10}

echo "Running $ITERATIONS iterations of the benchmarks"

echo "Running the R implementation"
for i in $(seq 1 $ITERATIONS); do
    scripts/bench.R 2>/dev/null >> origtimings.csv
done

echo "Running the Java implementation"
JAVA_SERVER_PATH=${JAVA_SERVER_PATH:-"/Users/jakobeha/Documents/grad/research"}
for i in $(seq 1 $ITERATIONS); do
    (cd "$JAVA_SERVER_PATH/r-compile-server/server" && mvn test -q -Dtest=BCCompilerBenchmarkTest#testBenchmark) 2>/dev/null >> javatimings.csv
done

echo "Running the Rust implementation"
for i in $(seq 1 $ITERATIONS); do
    NOCHECK=1 cargo run --quiet --release --package test_build --bin test_build -- -b 2>/dev/null >> rusttimings.csv
done

