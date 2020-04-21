//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

/* variables */
extern LISPT fun;
extern LISPT expression;
extern LISPT args;
extern struct destblock* env;
extern struct destblock* dest;
extern CONTROL control;
extern int toctrl;
extern int (*undefhook)(LISPT, LISPT*);
extern void (*breakhook)(void);
/* functions */
extern void unwind(void);
extern void init_ev(void);
extern void bt(void);
