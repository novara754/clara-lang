#! /bin/bash
set -e

cargo build

for file in ./examples/*; do
  printf "$file: "
  if ./target/debug/clara "$file" &>/dev/null; then
    if gcc -no-pie -o ./build/a.out ./build/out.o ./lib/clara.c &>/dev/null; then
      echo "OK"
    else
      echo "ERR"
      exit 1
    fi
  else
    echo "ERR"
    exit 1
  fi
done
