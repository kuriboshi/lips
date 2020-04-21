//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

/* variables */
extern LISPT savearray[];
extern int savept;
extern OBARRAY* obarray[];
extern LISPT freelist;
/* functions */
extern LISPT intern(const char*);
extern LISPT getobject(void);
extern LISPT mkstring(const char*);
extern LISPT mknumber(long);
extern LISPT mkatom(char*);
extern LISPT mkfloat(double);
extern struct destblock* dalloc(int);
extern void dfree(struct destblock*);
extern void dzero(void);
extern void init_alloc(void);
extern char* realmalloc(unsigned int);
