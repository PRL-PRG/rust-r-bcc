#!/bin/bash

echo "Running the R implementation"
for i in {1..10}; do
    scripts/bench.R 2>/dev/null >> origtimings.csv
done

echo "Running the Java implementation"
JAVA_SERVER_PATH=${JAVA_SERVER_PATH:-"/Users/jakobeha/Documents/grad/research"}
for i in {1..10}; do
<<<<<<< HEAD
    (cd "$JAVA_SERVER_PATH/r-compile-server/server" && mvn test -q -Dtest=BCCompilerBenchmarkTest#testBenchmark) 2>/dev/null >> javatimings.csv
=======
    (cd "/home/jakob/r-compile-server/server" && mvn test -q -Dtest=BCCompilerBenchmarkTest#testBenchmark) 2>/dev/null >> javatimings.csv
>>>>>>> 66ed4132506b2f27f5f7b5dc998e76190cfd10e3
done

echo "Running the Rust implementation"
for i in {1..10}; do
    NOCHECK=1 cargo run --quiet --release --package test_build --bin test_build -- -b 2>/dev/null >> mytimings.csv
done

