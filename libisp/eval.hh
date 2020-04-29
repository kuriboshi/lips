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
  evaluator() = delete;
  ~evaluator() = delete;

public:
  using continuation_t = bool (*)();

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

  static void reset();

  static PRIMITIVE eval(LISPT expr);
  static PRIMITIVE apply(LISPT f, LISPT a);
  static PRIMITIVE baktrace();

  static void bt();
  static void unwind();
  static void init_ev();

  static int toctrl;
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
  static bool evalhook(LISPT exp);
  static void do_unbound(continuation_t continuation);
  static bool do_default(continuation_t continuation);
  static void link();
  static void restore_env();

  // Continuations
  static bool peval();
  static bool peval1();
  static bool peval2();
  static bool ev0();
  static bool ev1();
  static bool ev2();
  static bool ev3();
  static bool ev4();
  static bool evlam0();
  static bool evlam1();
  static bool ev9();
  static bool ev11();
  static bool ev3p();
  static bool evalargs();
  static bool noevarg();
  static bool evlam();
  static bool spread();
  static bool evlis();
  static bool evlis1();
  static bool evlis2();
  static bool evlis3();
  static bool evlis4();
  static bool noev9();
  static bool evsequence();
  static bool evseq1();
  static bool evseq3();
  static bool evclosure();
  static bool evclosure1();
  static bool eval0();
  static bool apply0();
  static bool everr();
  static bool lookup();

  static LISPT printwhere();
  static void abort(int m, LISPT v);
  static void overflow();

  static LISPT fun;
  static LISPT expression;
  static LISPT args;
  static bool noeval;         // Don't evaluate arguments.
  static continuation_t cont; // Current continuation.
  static alloc::destblock_t* env;
};

inline void unwind() { evaluator::unwind(); }
inline void init_ev() { evaluator::init_ev(); }
inline void bt() { evaluator::bt(); }

inline PRIMITIVE eval(LISPT expr) { return evaluator::eval(expr); }
inline PRIMITIVE apply(LISPT f, LISPT a) { return evaluator::apply(f, a); }
inline PRIMITIVE baktrace() { return evaluator::baktrace(); }

} // namespace lisp
