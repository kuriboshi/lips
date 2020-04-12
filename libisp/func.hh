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
extern OBARRAY* obarray[];
extern LISPT freelist;
/* functions */
extern LISPT intern(const char*);
extern LISPT getobject(void);
extern struct destblock* dalloc(int);
extern void dfree(struct destblock*);
extern void dzero(void);
extern void init_alloc(void);
extern char* realmalloc(unsigned int);

/*
 * arith.c
 */
/* functions */
extern void init_arith(void);

/*
 * eval.c
 */
/* variables */
extern LISPT fun;
extern LISPT expression;
extern LISPT args;
extern struct destblock* env;
extern struct destblock* dest;
extern CONTROL control;
extern int toctrl;
/* functions */
extern void unwind(void);
extern void init_ev(void);
extern void bt(void);

/*
 * file.c
 */
/* variables */
extern FILE* primin;
extern FILE* primout;
extern FILE* primerr;
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
extern void init_lisp(void);

/*
 * logic.c
 */
/* functions */
extern void init_logic(void);

/*
 * low.c
 */
/* variables */
extern LISPT verboseflg;
/* functions */
extern void init_low(void);

/*
 * map.c
 */
/* functions */
extern void init_map(void);

/*
 * misc.c
 */
/* variables */
extern long trace;
/* functions */
extern LISPT error(int, LISPT);
extern LISPT syserr(LISPT);
extern LISPT break0(LISPT);
extern void init_debug(void);

/*
 * pred.c
 */
/* functions */
extern void init_pred(void);

/*
 * prim.c
 */
/* functions */
extern void mkprim0(const char*, LISPT (*fname)(void), short, enum lisp_type);
extern void mkprim1(const char*, LISPT (*fname)(LISPT), short, enum lisp_type);
extern void mkprim2(const char*, LISPT (*fname)(LISPT, LISPT), short, enum lisp_type);
extern void mkprim3(const char*, LISPT (*fname)(LISPT, LISPT, LISPT), short, enum lisp_type);
extern LISPT nth(LISPT list, long n);
extern LISPT closobj(LISPT vars);
extern LISPT mklambda(LISPT args, LISPT def, enum lisp_type);
extern void init_prim(void);

/*
 * prop.c
 */
/* functions */
extern void init_prop(void);

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
extern LISPT ratom(FILE*);
extern LISPT lispread(FILE*, int);
extern LISPT readline(FILE*);
extern LISPT patom(LISPT, FILE*, int);
extern LISPT terpri(FILE*);
extern LISPT prinbody(LISPT, FILE*, int);
extern LISPT prin0(LISPT, FILE*, int);
extern LISPT print(LISPT, FILE*);

/*
 * string.c
 */
/* functions */
extern void init_string(void);

/*
 * user.c
 */
/* functions */
extern LISPT funeq(LISPT, LISPT);
extern void init_user(void);
