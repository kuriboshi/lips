/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 * $Id$
 */

#pragma once

#include <lisp.hh>

extern int insidefork;

extern char* strsave(const char*);
extern void printdone();
extern char* ltoa(int);
extern void checkfork();
extern int execcommand(lisp::LISPT, lisp::LISPT*);
extern void init_exec();
