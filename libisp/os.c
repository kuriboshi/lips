/*
 * Lips, lisp shell.
 * Copyright 1989, Krister Joas
 *
 * $Id$
 */
#include <stdio.h>
#ifdef SARGASSO
#include <tops20.h>
#endif SARGASSO

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

#ifdef SARGASSO
  if (ejsys(BIN, _PRIIN)) return 0;
  *c = jsac[2];
#else
  i = read(fileno(file), cp, 1);
  if (i != 1) return 0;
#endif SARGASSO
  return 1;
}
