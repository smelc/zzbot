#!/bin/bash

function run() {
  echo $@
  $@
}

which pandoc &> /dev/null || { echo "pandoc must be installed. Exiting"; exit 1; }
[[ -d "build" ]] || run mkdir "build"
run pandoc src/index.md -o build/index.html