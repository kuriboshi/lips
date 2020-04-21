//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

/* variables */
extern LISPT top;
extern LISPT rstack;
extern long printlevel;
extern long thisplevel;
extern int echoline;
extern struct rtinfo currentrt;
/* functions */
extern LISPT ratom(FILE*);
extern LISPT lispread(FILE*, int);
extern LISPT readline(FILE*);
extern LISPT patom(LISPT, FILE*, int);
extern LISPT terpri(FILE*);
extern LISPT prinbody(LISPT, FILE*, int);
extern LISPT prin0(LISPT, FILE*, int);
extern LISPT print(LISPT, FILE*);
