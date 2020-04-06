/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 * $Id$
 */
#include <stdio.h>
#include <unistd.h>

#ifndef lint
static char rcsid[] = "$Id$";
#endif

/*
 * Read a characters from a terminal.  Returns 0 if
 * no character was read.  The character is returned in
 * the single character buffer cp.
 */
int
readchar(file, cp)
  FILE *file;
  char *cp;
{
  int i;

  i = read(fileno(file), cp, 1);
  if (i != 1) return 0;
  return 1;
}
