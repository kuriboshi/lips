//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

/* functions */
extern void mkprim(const char*, LISPT (*fname)(void), short, lisp_type);
extern void mkprim(const char*, LISPT (*fname)(LISPT), short, lisp_type);
extern void mkprim(const char*, LISPT (*fname)(LISPT, LISPT), short, lisp_type);
extern void mkprim(const char*, LISPT (*fname)(LISPT, LISPT, LISPT), short, lisp_type);
extern LISPT nth(LISPT list, long n);
extern LISPT closobj(LISPT vars);
extern LISPT mklambda(LISPT args, LISPT def, lisp_type);
extern void init_prim(void);
