#!/bin/bash

# Check if a filename is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <filename.cl>"
  exit 1
fi

# Input filename
FILE=$1
BASE_NAME=$(basename "$FILE" .cl)

# Compile main.ml to an executable named main
ocamlc -o main main.ml

# Parse the COOL file using ./cool --parse
./cool --type "$FILE"

# Run the COOL file and save the output to a.out
./cool "$FILE" > a.out

# Run main with the COOL file and save output to -test.cl-tac
./main "$FILE"-ast > "$BASE_NAME"-test.s

chmod +x "$BASE_NAME"-test.s
./"$BASE_NAME"-test.s > b.out

# Compare a.out and b.out and display if there are differences
if diff a.out b.out > /dev/null; then
  echo "No differences found"
else
  echo "Differences found"
fi


