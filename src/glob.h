/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 * $Id$
 */
#pragma once

#include <lisp.h>

extern char* extilde(char*, int);
extern LISPT expandfiles(char*, int, int, int);
extern LISPT glob(LISPT);

extern LISPT expand(LISPT, LISPT, LISPT);
