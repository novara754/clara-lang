#! /bin/bash
mkdir -p ./build

cargo build
if [ $? -eq 1 ]; then
  exit 1
fi

TESTS_RUN=0
TESTS_SUCCESSFUL=0
EXIT_CODE=0

for FILE in ./examples/*.clara; do
  ((TESTS_RUN += 1))

  printf "$FILE: "

  CLARAC_OUT=`./target/debug/clara "$FILE" 2>&1`

  if [ $? -eq 0 ]; then
    GCC_OUT=`gcc -no-pie -o ./build/a.out ./build/out.o ./lib/clara.c  2>&1`
    if [ $? -eq 0 ]; then
      EXPECTED_STDOUT_FILE="${FILE%.*}.stdout"
      if [ -f "$EXPECTED_STDOUT_FILE" ]; then
        EXPECTED_STDOUT=`cat "$EXPECTED_STDOUT_FILE"`
        ACTUAL_STDOUT=`./build/a.out`

        if [ "$EXPECTED_STDOUT" = "$ACTUAL_STDOUT" ]; then
          ((TESTS_SUCCESSFUL += 1))
          echo "OK"
        else
          printf "INCORRECT OUTPUT\n"
          printf "$ACTUAL_STDOUT\n" | diff - "$EXPECTED_STDOUT_FILE"
        fi
      else
        ((TESTS_SUCCESSFUL += 1))
        echo "OK"
      fi
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
