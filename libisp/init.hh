//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
/*
 * All lisp constants used internally.
 */
extern LISPT C_T;
extern LISPT CE_NIL;
extern LISPT CE_T;
extern LISPT C_ALIAS;
extern LISPT C_AMPER;
extern LISPT C_APPEND;
extern LISPT C_AUTOLOAD;
extern LISPT C_BACK;
extern LISPT C_BAR;
extern LISPT C_BIGNUM;
extern LISPT C_BROKEN;
extern LISPT C_BT;
extern LISPT C_CLOSURE;
extern LISPT C_CONS;
extern LISPT C_DOT;
extern LISPT C_ENDOFFILE;
extern LISPT C_ENVIRON;
extern LISPT C_EOF;
extern LISPT C_ERROR;
extern LISPT C_EXCL;
extern LISPT C_EXEC;
extern LISPT C_FILE;
extern LISPT C_FLOAT;
extern LISPT C_FREE;
extern LISPT C_FROM;
extern LISPT C_FSUBR;
extern LISPT C_GGT;
extern LISPT C_GO;
extern LISPT C_GT;
extern LISPT C_INDIRECT;
extern LISPT C_INTEGER;
extern LISPT C_LAMBDA;
extern LISPT C_LT;
extern LISPT C_NLAMBDA;
extern LISPT C_OLDDEF;
extern LISPT C_OLDVAL;
extern LISPT C_PIPE;
extern LISPT C_PROGN;
extern LISPT C_QUOTE;
extern LISPT C_READ;
extern LISPT C_REDEFINED;
extern LISPT C_RESET;
extern LISPT C_RETURN;
extern LISPT C_SEMI;
extern LISPT C_STRING;
extern LISPT C_SUBR;
extern LISPT C_SYMBOL;
extern LISPT C_TO;
extern LISPT C_TOTO;
extern LISPT C_UNBOUND;
extern LISPT C_WRITE;

// Variables
extern LISPT currentbase;
extern LISPT topprompt;
extern LISPT promptform;
extern LISPT brkprompt;
extern LISPT interactive;
extern LISPT version;

/* functions */
extern void init_lisp();

} // namespace lisp
