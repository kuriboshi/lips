//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include <memory>
#include <lisp/lisp.hh>
#include "env.hh"

struct options_t
{
  bool debug = false;       // Debugging
  bool interactive = false; // Force interactive mode
  bool command = false;     // Command string
  bool version = false;     // Print version
  bool fast = false;        // Fast start, don't read init file
  bool test = false;        // Run unit tests
};

extern options_t options;
extern std::unique_ptr<lisp::environment> env;

extern lisp::LISPT C_ALIAS;
extern lisp::LISPT C_AMPER;
extern lisp::LISPT C_BACK;
extern lisp::LISPT C_BAR;
extern lisp::LISPT C_EXCL;
extern lisp::LISPT C_EXEC;
extern lisp::LISPT C_GGT;
extern lisp::LISPT C_GT;
extern lisp::LISPT C_LT;
extern lisp::LISPT C_OLDVAL;
extern lisp::LISPT C_PIPE;
extern lisp::LISPT C_PROGN;
extern lisp::LISPT C_REDIR_APPEND;
extern lisp::LISPT C_REDIR_FROM;
extern lisp::LISPT C_REDIR_TO;
extern lisp::LISPT C_SEMI;
