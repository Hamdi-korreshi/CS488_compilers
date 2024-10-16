#!/bin/bash

dir="testcase"

if [ ! -d "$dir" ]; then
  echo "Directory $dir does not exist."
  exit 1
fi

ocamlc -o main main.ml
if [ $? -ne 0 ]; then
  echo "Failed to compile main.ml"
  exit 1
fi

failed_files=()

for file in "$dir"/*.cl; do
  filename="${file%.cl}"
  base_filename=$(basename "$filename")

  ./cool --parse "$file"
  if [ $? -ne 0 ]; then
    echo "Failed to parse $file"
    continue
  fi

  ./cool "$file" > "$dir/${base_filename}.out"
  if [ $? -ne 0 ]; then
    echo "Failed to run ./cool on $file"
    continue
  fi

  ./main "$dir/${base_filename}.cl-ast" > "$dir/my_${base_filename}.out"
  if [ $? -ne 0 ]; then
    echo "Failed to run ./main on ${base_filename}.cl-ast"
    continue
  fi

  diff "$dir/${base_filename}.out" "$dir/my_${base_filename}.out"
  if [ $? -ne 0 ]; then
    echo "Difference found for $file"
    failed_files+=("$file")
  else
    echo "No differences for $file"
  fi
done

if [ ${#failed_files[@]} -ne 0 ]; then
  echo -e "\nFiles with differences:"
  for failed_file in "${failed_files[@]}"; do
    echo "$failed_file"
  done
else
  echo -e "\nAll files passed the diff check."
fi
