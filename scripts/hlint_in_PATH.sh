#!/bin/bash

[[ -d ".bin" ]] || { echo ".bin must be a directory"; return 1; }
cd .bin
hlint=$(ls hlint-*)
[[ -e "$hlint" ]] || { echo "hlint not found"; return 1; }
if [[ `readlink hlint` != "$hlint" ]]; then
  [[ ! -e "hlint" ]]  || rm hlint
  ln -s $hlint hlint
fi
echo "Using $hlint"
export PATH="$PATH:`pwd`"
