#! /bin/bash
set -e

cargo build

for file in ./examples/*; do
  printf "$file: "
  if ./target/debug/clara "$file" &>/dev/null; then
    echo "OK"
  else
    echo "ERR"
    exit 1
  fi
done
