#ifndef CLARA_LIB_H
#define CLARA_LIB_H

#include <stdlib.h>
#include <stdio.h>

/**
 * Reference to a string that does not own its data.
 */
typedef struct
{
  char *data;
  size_t length;
} StringSlice;

/**
 * Function to print a StringSlice to stdout.
 * Automatically prints a newline character after the given StringSlice.
 */
void print(StringSlice string)
{
  for (size_t i = 0; i < string.length; ++i)
  {
    putchar(string.data[i]);
  }
  putchar('\n');
}

#endif // CLARA_LIB_H
