#! /bin/bash
mkdir -p ./build

cargo build
if [ $? -eq 1 ]; then
  exit 1
fi

TESTS_RUN=0
TESTS_SUCCESSFUL=0
EXIT_CODE=0

for file in ./examples/*; do
  ((TESTS_RUN += 1))

  printf "$file: "

  CLARAC_OUT=`./target/debug/clara "$file" 2>&1`

  if [ $? -eq 0 ]; then
    GCC_OUT=`gcc -no-pie -o ./build/a.out ./build/out.o ./lib/clara.c  2>&1`
    if [ $? -eq 0 ]; then
     ((TESTS_SUCCESSFUL += 1))
      echo "OK"
    else
      printf "LINK ERR\n$GCC_OUT\n"
      EXIT_CODE=1
    fi
  else
    printf "COMPILE ERR\n$CLARAC_OUT"
    EXIT_CODE=1
  fi
done

printf "\n$TESTS_SUCCESSFUL/$TESTS_RUN tests succesfull.\n"

exit $EXIT_CODE
