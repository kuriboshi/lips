//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

/*
 * The control stack.
 */
enum control_type
{
  CTRL_LISP,
  CTRL_FUNC,
  CTRL_POINT,
};

struct control
{
  enum control_type type;
  union
  {
    int (*f_point)(void);
    lisp::alloc::destblock_t* point;
    LISPT lisp;
  } u;
};

static const int CTRLBLKSIZE = 4000;
typedef struct control CONTROL[CTRLBLKSIZE];

/* variables */
extern LISPT fun;
extern LISPT expression;
extern LISPT args;
extern lisp::alloc::destblock_t* env;
extern lisp::alloc::destblock_t* dest;
extern CONTROL control;
extern int toctrl;
extern int (*undefhook)(LISPT, LISPT*);
extern void (*breakhook)(void);
/* functions */
extern void unwind(void);
extern void init_ev(void);
extern void bt(void);
