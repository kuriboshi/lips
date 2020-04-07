/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 * $Id$
 */
#include <setjmp.h>
#include "lisp.h"

/*
 * exec.c
 */
extern int insidefork;

extern char *strsave(char *);
extern void printdone(void);
extern char *ltoa(long);
extern void checkfork(void);
extern int execcommand(LISPT, LISPT*);
extern void init_exec(void);

/*
 * glob.c
 */
extern char *extilde(char*, int);
extern LISPT expandfiles(char*, int, int, int);
extern LISPT glob(LISPT);

extern LISPT expand(LISPT, LISPT, LISPT);

/*
 * main.c
 */
struct options {
  int debug;                            /* Debugging */
  int interactive;                      /* Force interactive mode */
  int command;                          /* Command string */
  int version;                          /* Print version */
  int fast;                             /* Fast start, don't read init file */
};

extern jmp_buf toplevel;
extern char *progname;
extern int brkflg;
extern int interrupt;
extern int mypgrp;
extern struct options options;
extern LISPT path;
extern LISPT home;
extern LISPT globsort;

/*
 * top.c
 */
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
extern void toploop(LISPT*, int(*)(LISPT*));
extern void init_hist(void);

/*
 * version.c
 */
extern char *VERSION;
