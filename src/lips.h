/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 * $Id$
 */
#include "lisp.h"
#include <setjmp.h>

/*
 * exec.c
 */
extern int insidefork;

extern char *strsave();
extern void printdone();
extern char *ltoa();
extern void checkfork();
extern int execcommand();
extern void init_exec();

/*
 * glob.c
 */
extern char *extilde();
extern LISPT expandfiles();
extern LISPT glob();

extern LISPT expand();

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
extern LISPT (*transformhook)();
extern void (*beforeprompt)();

extern LISPT histget();
extern LISPT findalias();
extern void toploop();
extern void init_hist();

/*
 * version.c
 */
extern char *VERSION;
