/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 */

#include <cstdio>
#include <unistd.h>

namespace lisp
{
/*
 * Read a characters from a terminal.  Returns 0 if
 * no character was read.  The character is returned in
 * the single character buffer cp.
 */
bool readchar(FILE* file, char* cp)
{
  int i = read(fileno(file), cp, 1);
  if(i != 1)
    return false;
  return true;
}

} // namespace lisp
