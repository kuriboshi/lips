/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 * $Id$
 */

#pragma once

#include <lisp.hh>

extern int interrupt;
extern int brkflg;
extern struct options options;
extern lisp::LISPT path;
extern lisp::LISPT home;
extern lisp::LISPT globsort;

struct options
{
  int debug;       /* Debugging */
  int interactive; /* Force interactive mode */
  int command;     /* Command string */
  int version;     /* Print version */
  int fast;        /* Fast start, don't read init file */
};
