#!/usr/bin/python

import sys
import subprocess

r_obj = sys.argv[1]

batcmd= f"Rscript -e 'serialize({r_obj}, NULL)'"
result : bytes = subprocess.check_output(batcmd, shell=True)


lines = result.split(b"\n")[:-1]
lines = list(map(lambda x: x.split(b"] ")[1], lines))

all_data : list[bytes] = []

for line in lines:
    all_data += line.split(b" ")

print("let data : Vec<u8> = vec![", end="")
for i in all_data:
    print(f"0x{str(i)[2:-1]}, ", end="")

print("];")

