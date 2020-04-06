/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 * $Id$
 */
/*
 * alloc.c
 */
#pragma once

/* variables */
extern LISPT savearray[];
extern int savept;
extern OBARRAY *obarray[];
extern LISPT freelist;
/* functions */
extern LISPT intern();
extern LISPT getobject ();
extern struct destblock *dalloc();
extern void dfree();
extern void dzero();
extern void init_alloc();
extern char *realmalloc();

/*
 * arith.c
 */
/* functions */
extern void init_arith();

/*
 * eval.c
 */
/* variables */
extern LISPT fun;
extern LISPT expression;
extern LISPT args;
extern struct destblock *env;
extern struct destblock *dest;
extern CONTROL control;
extern int toctrl;
/* functions */
extern void unwind();
extern void init_ev();
extern void bt();

/*
 * file.c
 */
/* variables */
extern FILE *primin;
extern FILE *primout;
extern FILE *primerr;
/* functions */
/*extern void init_file();*/

/*
 * init.c
 */
/* variables */
extern LISPT currentbase;
extern LISPT topprompt;
extern LISPT promptform;
extern LISPT brkprompt;
extern LISPT interactive;
extern LISPT version;
/* functions */
extern void init_lisp();

/*
 * logic.c
 */
/* functions */
extern void init_logic();

/*
 * low.c
 */
/* variables */
extern LISPT verboseflg;
/* functions */
extern void init_low();

/*
 * map.c
 */
/* functions */
extern void init_map();

/*
 * misc.c
 */
/* variables */
extern long trace;
/* functions */
extern LISPT error();
extern LISPT syserr();
extern LISPT break0();
extern void init_debug();

/*
 * pred.c
 */
/* functions */
extern void init_pred();

/*
 * prim.c
 */
/* functions */
extern void mkprim();
extern LISPT nth();
extern LISPT closobj();
extern LISPT mklambda();
extern void init_prim();

/*
 * prop.c
 */
/* functions */
extern void init_prop();

/*
 * read.c
 */
/* variables */
extern LISPT top;
extern LISPT rstack;
extern long printlevel;
extern long thisplevel;
extern int echoline;
extern struct rtinfo currentrt;
/* functions */
extern LISPT ratom();
extern LISPT lispread();
extern LISPT readline();
extern LISPT patom();
extern LISPT terpri();
extern LISPT prinbody();
extern LISPT prin0();
extern LISPT print();

/*
 * string.c
 */
/* functions */
extern void init_string();

/*
 * user.c
 */
/* functions */
extern LISPT funeq();
extern void init_user();
