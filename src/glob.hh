/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 * $Id$
 */
#pragma once

#include <libisp.hh>

extern const char* extilde(const char*, int);
extern LISPT expandfiles(const char*, int, int, int);
extern LISPT glob(LISPT);

extern LISPT expand(LISPT, LISPT, LISPT);
