#!/bin/bash

# Ensure a directory argument is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <directory>"
    exit 1
fi

DIR="$1"

# Ensure the provided argument is a valid directory
if [ ! -d "$DIR" ]; then
    echo "Error: '$DIR' is not a valid directory."
    exit 1
fi

# Find and process each Rust file containing #[cfg(test)]
rg -l '^\s*#\[cfg\(test\)\]' "$DIR" --glob '*.rs' | while read -r file; do
    rg -A 5000 '^\s*#\[cfg\(test\)\]' "$file" | \
    awk '
    /^\s*#\[cfg\(test\)\]/ { inside=1 } 
    inside && /^\s*}/ { inside=0 } 
    inside && !/^\s*(\/\/|\/\*|\*|$)/ { count++ } 
    END { print count }'
done | awk '{sum+=$1} END {print sum}'
