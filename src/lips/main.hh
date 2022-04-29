//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#ifndef LIPS_MAIN_HH
#define LIPS_MAIN_HH

#include <memory>
#include <lisp/lisp.hh>
#include "env.hh"

inline lisp::file_t& primout() { return lisp::lisp::current().primout(); }
inline lisp::file_t& primin() { return lisp::lisp::current().primin(); }
inline lisp::file_t& primerr() { return lisp::lisp::current().primerr(); }

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

#endif
