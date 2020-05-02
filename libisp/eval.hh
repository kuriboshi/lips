//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class evaluator: public base
{
public:
  evaluator(lisp&);
  ~evaluator() = default;

  using continuation_t = bool (evaluator::*)();

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

  static constexpr int CTRLBLKSIZE = 4000;
  control_t control[CTRLBLKSIZE]; // Control-stack
  int toctrl = 0;                 // Control-stack stack pointer

  void reset();

  PRIMITIVE eval(lisp&, LISPT);
  PRIMITIVE apply(lisp&, LISPT, LISPT);
  PRIMITIVE baktrace(lisp&);

  void bt();
  void unwind();
  void init();
  int trace() const { return _trace; }
  void trace(int t) { _trace = t; }

  alloc::destblock_t* dest = nullptr; // Current destination being built.

  using undefhook_t = int (*)(LISPT, LISPT*);
  undefhook_t undefhook = nullptr; // Called in case of undefined function.

  using breakhook_t = void (*)();
  breakhook_t breakhook = nullptr; // Called before going into break.

private:
  void push_lisp(LISPT);
  void push_point(alloc::destblock_t*);
  void push_func(continuation_t);
  LISPT pop_lisp();
  alloc::destblock_t* pop_point();
  alloc::destblock_t* pop_env();
  continuation_t pop_func();
  void xbreak(int mess, LISPT fault, continuation_t next);
  alloc::destblock_t* mkdestblock(int);
  void storevar(LISPT v, int i);
  void send(LISPT a);
  LISPT receive();
  void next();
  LISPT call(LISPT fun);
  bool evalhook(LISPT exp);
  void do_unbound(continuation_t continuation);
  bool do_default(continuation_t continuation);
  void link();
  void restore_env();

  // Continuations
  bool peval();
  bool peval1();
  bool peval2();
  bool ev0();
  bool ev1();
  bool ev2();
  bool ev3();
  bool ev4();
  bool evlam0();
  bool evlam1();
  bool ev9();
  bool ev11();
  bool ev3p();
  bool evalargs();
  bool noevarg();
  bool evlam();
  bool spread();
  bool evlis();
  bool evlis1();
  bool evlis2();
  bool evlis3();
  bool evlis4();
  bool noev9();
  bool evsequence();
  bool evseq1();
  bool evseq3();
  bool evclosure();
  bool evclosure1();
  bool eval0();
  bool apply0();
  bool everr();
  bool lookup();

  LISPT printwhere();
  void abort(int m, LISPT v);
  void overflow();

  LISPT fun = nullptr;               // Store current function being evaluated.
  LISPT expression = nullptr;        // Current expression.
  LISPT args = nullptr;              // Current arguments.
  bool noeval = false;               // Don't evaluate arguments.
  continuation_t cont = nullptr;     // Current continuation.
  alloc::destblock_t* env = nullptr; // Current environment.
  int _trace = 0;
};

#if 0
inline void unwind() { evaluator::unwind(); }
inline void bt() { evaluator::bt(); }
#endif

inline LISPT eval(lisp& l, LISPT expr) { return l.e().eval(l, expr); }
inline LISPT apply(lisp& l, LISPT f, LISPT a) { return l.e().apply(l, f, a); }
inline LISPT baktrace(lisp& l) { return l.e().baktrace(l); }

} // namespace lisp
