/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 * $Id$
 */
#pragma once

#include "lisp.hh"
#include "constants.hh"
#include "init.hh"

namespace lisp {

extern LISPT top, rstack, fun, expression;
extern LISPT args, verboseflg, promptform, brkprompt, topprompt;
extern LISPT interactive, version;

}
