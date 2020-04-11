/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 * $Id$
 */

#pragma once

#include <lisp.h>

extern int insidefork;

extern char* strsave(char*);
extern void printdone(void);
extern char* ltoa(long);
extern void checkfork(void);
extern int execcommand(LISPT, LISPT*);
extern void init_exec(void);
