/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 * $Id$
 */
#pragma once

#include <libisp.hh>

extern const char* extilde(const char*, int);
extern lisp::LISPT expandfiles(const char*, int, int, int);
extern lisp::LISPT glob(lisp::LISPT);

extern lisp::LISPT expand(lisp::LISPT, lisp::LISPT, lisp::LISPT);
