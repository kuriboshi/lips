/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <lisp.hh>

struct options_t
{
  bool debug;       /* Debugging */
  bool interactive; /* Force interactive mode */
  bool command;     /* Command string */
  bool version;     /* Print version */
  bool fast;        /* Fast start, don't read init file */
};

extern options_t options;
extern lisp::LISPT path;
extern lisp::LISPT home;
extern lisp::LISPT globsort;

extern lisp::LISPT C_ALIAS;
extern lisp::LISPT C_AMPER;
extern lisp::LISPT C_BACK;
extern lisp::LISPT C_BAR;
extern lisp::LISPT C_EXCL;
extern lisp::LISPT C_EXEC;
extern lisp::LISPT C_FROM;
extern lisp::LISPT C_GGT;
extern lisp::LISPT C_GT;
extern lisp::LISPT C_LT;
extern lisp::LISPT C_OLDVAL;
extern lisp::LISPT C_PIPE;
extern lisp::LISPT C_PROGN;
extern lisp::LISPT C_SEMI;
extern lisp::LISPT C_TO;
extern lisp::LISPT C_TOTO;
