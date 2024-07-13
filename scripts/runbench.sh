#!/bin/bash

echo  "rds_sec rds comp_sec comp" > mytimings.csv
echo  "" > origtimings.csv

for i in {1..100}; do
    target/release/test_build -b 2> /dev/null >> mytimings.csv
done

for i in {1..100}; do
    ./bench.R 2> /dev/null >> origtimings.csv
done
