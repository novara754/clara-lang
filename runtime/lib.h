#ifndef CLARA_LIB_H
#define CLARA_LIB_H

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

typedef char c_char;
typedef int c_int;

/**
 * A string with associated data and length.
 */
typedef struct
{
  char *data;
  size_t length;
} string;

/**
 * Get `->c_char` from a `string`.
 */
c_char *c_string(string s)
{
  return s.data;
}

/**
 * Function to print a string to stdout.
 * Automatically prints a newline character after the given string.
 */
void print(string msg)
{
  for (size_t i = 0; i < msg.length; ++i)
  {
    putchar(msg.data[i]);
  }
  putchar('\n');
}

#endif // CLARA_LIB_H
