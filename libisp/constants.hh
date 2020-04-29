/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 */

#pragma once

#include "lisp.hh"

namespace lisp
{
/*
 * All lisp constants needed internally.
 */
extern LISPT CE_NIL, CE_T;
extern LISPT C_ALIAS, C_APPEND, C_AUTOLOAD, C_BACK, C_BIGNUM, C_BROKEN;
extern LISPT C_BT, C_CLOSURE, C_CONS, C_DOT, C_ENDOFFILE, C_ENVIRON;
extern LISPT C_EOF, C_ERROR, C_EXEC, C_EXCL, C_FILE, C_FLOAT, C_FREE;
extern LISPT C_FROM, C_FSUBR, C_GO, C_INDIRECT, C_INTEGER;
extern LISPT C_LAMBDA, C_NLAMBDA, C_OLDDEF, C_OLDVAL, C_PIPE;
extern LISPT C_QUOTE, C_READ, C_REDEFINED, C_RESET, C_RETURN, C_STRING;
extern LISPT C_SUBR, C_SYMBOL, C_TO, C_TOTO, C_UNBOUND, C_WRITE;
extern LISPT C_BAR, C_GT, C_GGT, C_LT, C_SEMI, C_PROGN, C_AMPER;

} // namespace lisp
