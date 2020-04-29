/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include "lisp.hh"

extern char current_prompt[];
extern lisp::LISPT history;
extern lisp::LISPT histnum;
extern lisp::LISPT histmax;
extern lisp::LISPT input_exp;
extern lisp::LISPT topexp;
extern lisp::LISPT alias_expanded;
extern lisp::LISPT (*transformhook)(lisp::LISPT);
extern void (*beforeprompt)();

extern lisp::LISPT histget(int, lisp::LISPT);
extern lisp::LISPT findalias(lisp::LISPT);
extern bool toploop(lisp::LISPT*, int (*)(lisp::LISPT*));
extern void init_hist();
