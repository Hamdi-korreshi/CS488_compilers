#!/bin/bash

dir="testcase"

if [ ! -d "$dir" ]; then
  echo "Directory $dir does not exist."
  exit 1
fi

# Compile the OCaml file (main.ml)
ocamlc -o main main.ml
if [ $? -ne 0 ]; then
  echo "Failed to compile main.ml"
  exit 1
fi

# Loop over all .cl files in the testcase directory
for file in "$dir"/*.cl; do
  filename="${file%.cl}"  # Remove the extension
  base_filename=$(basename "$filename")  # Get the base filename without the directory

  # Step 1: Run ./cool --parse to generate .cl-ast
  ./cool --parse "$file"
  if [ $? -ne 0 ]; then
    echo "Failed to parse $file"
    continue
  fi

  # Step 2: Run ./cool and generate .out file
  ./cool "$file" > "$dir/${base_filename}.out"
  if [ $? -ne 0 ]; then
    echo "Failed to run ./cool on $file"
    continue
  fi

  # Step 3: Run the OCaml program on the generated .cl-ast file
  ./main "$dir/${base_filename}.cl-ast" > "$dir/my_${base_filename}.out"
  if [ $? -ne 0 ]; then
    echo "Failed to run ./main on ${base_filename}.cl-ast"
    continue
  fi

  # Step 4: Compare the outputs using diff
  diff "$dir/${base_filename}.out" "$dir/my_${base_filename}.out"
  if [ $? -ne 0 ]; then
    echo "Difference found for $file"
  else
    echo "No differences for $file"
  fi
done
