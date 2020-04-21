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

extern LISPT top, rstack, history, histnum, fun, expression;
extern LISPT args, path, home, verboseflg, promptform, brkprompt, topprompt;
extern LISPT alias_expanded, interactive, version;
