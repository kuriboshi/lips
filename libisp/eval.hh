//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{

class evaluator
{
public:
  evaluator();
  ~evaluator() = default;

public:
  using continuation_t = int(*)();

  /*
   * The control stack.
   */
  enum control_type
  {
    CTRL_LISP,
    CTRL_FUNC,
    CTRL_POINT,
  };

  struct control_t
  {
    enum control_type type;
    union
    {
      continuation_t f_point;
      alloc::destblock_t* point;
      LISPT lisp;
    } u;
  };

  static const int CTRLBLKSIZE = 4000;
  static control_t control[CTRLBLKSIZE];

  static PRIMITIVE eval(LISPT expr);
  static PRIMITIVE apply(LISPT f, LISPT a);
  static PRIMITIVE baktrace();

  static void bt();
  static void unwind();
  static void init_ev();
  static int toctrl;
  static LISPT fun;
  static LISPT expression;
  static LISPT args;
  static alloc::destblock_t* env;
  static alloc::destblock_t* dest;

  using undefhook_t = int (*)(LISPT, LISPT*);
  static undefhook_t undefhook;
  using breakhook_t = void (*)();
  static breakhook_t breakhook;

private:
  static void push_lisp(LISPT);
  static void push_point(lisp::alloc::destblock_t*);
  static void push_func(continuation_t);
  static LISPT pop_lisp();
  static alloc::destblock_t* pop_point();
  static alloc::destblock_t* pop_env();
  static continuation_t pop_func();
  static void xbreak(int mess, LISPT fault, continuation_t next);
  static alloc::destblock_t* mkdestblock(int);
  static void storevar(LISPT v, int i);
  static void send(LISPT a);
  static LISPT receive();
  static void next();
  static LISPT call(LISPT fun);
  static int evalhook(LISPT exp);
  static void do_unbound(continuation_t continuation);
  static int do_default(continuation_t continuation);
  static void link();
  static void restore_env();

  static int peval();
  static int peval1();
  static int peval2();
  static int ev0();
  static int ev1();
  static int ev2();
  static int ev3();
  static int ev4();
  static int evlam1();
  static int evlam0();
  static int ev9();
  static int ev11();
  static int ev3p();
  static int evalargs();
  static int noevarg();
  static int evlam();
  static int spread();
  static int evlis();
  static int evlis1();
  static int evlis2();
  static int evlis3();
  static int evlis4();
  static int noev9();
  static int evsequence();
  static int evseq1();
  static int evseq3();
  static int evclosure();
  static int evclosure1();
  static int eval0();
  static int apply0();
  static int everr();
  static int lookup();

  static LISPT printwhere();
  static void abort(int m, LISPT v);
  static void overflow();

  static bool noeval;        /* Don't evaluate arguments. */
  static continuation_t cont; /* Current continuation. */
};

inline void unwind() { evaluator::unwind(); }
inline void init_ev() { evaluator::init_ev(); }
inline void bt() { evaluator::bt(); }

inline PRIMITIVE eval(LISPT expr) { return evaluator::eval(expr); }
inline PRIMITIVE apply(LISPT f, LISPT a) { return evaluator::apply(f, a); }
inline PRIMITIVE baktrace() { return evaluator::baktrace(); }

}
