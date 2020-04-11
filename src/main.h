/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 * $Id$
 */

#pragma once

#include <setjmp.h>
#include <lisp.h>

extern jmp_buf toplevel;
extern int interrupt;
extern int brkflg;
extern struct options options;
extern LISPT path;
extern LISPT home;
extern LISPT globsort;

struct options
{
  int debug;       /* Debugging */
  int interactive; /* Force interactive mode */
  int command;     /* Command string */
  int version;     /* Print version */
  int fast;        /* Fast start, don't read init file */
};
