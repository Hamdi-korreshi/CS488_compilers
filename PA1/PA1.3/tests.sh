#!/usr/bin/env bash

if [[ -f "endresults.txt" ]]; then
  echo "Removing endresults.txt"
  rm "endresults.txt"
fi

echo "Creating endresults.txt"
touch endresults.txt

answers=("100words.answer" "ambig.answer" "austen.answer" "century.answer" "cycle.answer")
tests=("100words.list" "ambig.list" "austen.list" "century.list" "cycle.list")
hamdi=("hamdi-100words.answer" "hamdi-ambig.answer" "hamdi-austen.answer" "hamdi-century.answer" "hamdi-cycle.answer")
counter=0
for file in "${tests[@]}"; do
  ./rosetta < "PA1 Test Cases"/$file > MyAnswers/"${hamdi[counter]}"
  counter=$((counter+1))
done
counter=0
for ans in "${answers[@]}"; do
  if diff -u "PA1 Test Cases"/$ans MyAnswers/"${hamdi[counter]}" >> endresults.txt; then
    echo "Passed test: $ans"
  else
    echo "Failed test: $ans"
  fi
  counter=$((counter+1))
done