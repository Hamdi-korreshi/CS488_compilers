#!/usr/bin/env bash

if [[ -f "endresults.txt" ]]; then
  echo "Removing endresults.txt"
  rm "endresults.txt"
fi

echo "Creating endresults.txt"
touch endresults.txt

testingfiles=("badoverridebuiltin" "cycle" "objectcheck" "redefobj" "rettypedne" "selfinformparam" "selftypeparam" "selftyperedef" "sigchange" "attr_init_not_conf" "case" "class-disconform-static-dispatch" "cycle" "inherit_int" "mainmissing" "other_attr" "other_inheritance_problem" "redef_attr" "redef_method_class" "self_err" "string_int" "substring" "test12" "test13" "test15" "test17" "test18" "wrongnumberargs")
counter=0
for file in "${testingfiles[@]}"; do
  make checker "CURRFILE=${testingfiles[counter]}" >> endresults.txt;
  counter=$((counter+1))
done