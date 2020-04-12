/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 * $Id$
 */

#pragma once

#include "lisp.hh"

extern char current_prompt[];
extern LISPT history;
extern LISPT histnum;
extern LISPT histmax;
extern LISPT input_exp;
extern LISPT topexp;
extern LISPT alias_expanded;
extern LISPT (*transformhook)(LISPT);
extern void (*beforeprompt)(void);

extern LISPT histget(long, LISPT);
extern LISPT findalias(LISPT);
extern void toploop(LISPT*, int (*)(LISPT*));
extern void init_hist(void);
