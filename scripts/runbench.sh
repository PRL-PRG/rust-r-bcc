#!/bin/bash

echo  "rds_sec rds comp_sec comp" > mytimings.csv
echo  "" > origtimings.csv

echo "Running the Rust implementation"
for i in {1..100}; do
    target/release/test_build -b 2> /dev/null >> mytimings.csv
done

echo "Running the R implementation"
for i in {1..100}; do
    ./bench.R 2> /dev/null >> origtimings.csv
done
