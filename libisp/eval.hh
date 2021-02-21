//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"
#include "io.hh"

namespace lisp
{
inline constexpr auto PN_E = "e";               // noeval version of eval
inline constexpr auto PN_EVAL = "eval";         // evaluate exp
inline constexpr auto PN_APPLY = "apply";       // apply function on args
inline constexpr auto PN_APPLYSTAR = "apply*";  // apply nospread
inline constexpr auto PN_BAKTRACE = "baktrace"; // control stack backtrace

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
      destblock_t* point;
      LISPT lisp;
    } u;
  };

  static constexpr int CTRLBLKSIZE = 4000;
  control_t control[CTRLBLKSIZE]; // Control-stack
  int toctrl = 0;                 // Control-stack stack pointer

  void reset();

  PRIMITIVE eval(LISPT);
  PRIMITIVE apply(LISPT, LISPT);
  PRIMITIVE baktrace();
  PRIMITIVE topofstack();
  PRIMITIVE envget(LISPT, LISPT);

  void bt();
  void unwind();
  void init();
  int trace() const { return _trace; }
  void trace(int t) { _trace = t; }

  destblock_t* dest = nullptr; // Current destination being built.

  using undefhook_t = int (*)(LISPT, LISPT*);
  undefhook_t undefhook = nullptr; // Called in case of undefined function.

  using breakhook_t = void (*)();
  breakhook_t breakhook = nullptr; // Called before going into break.

  struct destblock_t* environment() const { return env; }

private:
  void push_lisp(LISPT);
  LISPT pop_lisp();
  void push_point(destblock_t*);
  destblock_t* pop_point();
  void push_func(continuation_t);
  continuation_t pop_func();
  destblock_t* pop_env();

  void xbreak(int mess, LISPT fault, continuation_t next);
  destblock_t* mkdestblock(int);
  void storevar(LISPT v, int i);
  void send(LISPT a);
  LISPT receive();
  void next();
  LISPT call(LISPT fun);
  bool evalhook(LISPT exp);
  void do_unbound(continuation_t);
  bool do_default(continuation_t);
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

  LISPT fun = nullptr;           // Store current function being evaluated.
  LISPT expression = nullptr;    // Current expression.
  LISPT args = nullptr;          // Current arguments.
  bool noeval = false;           // Don't evaluate arguments.
  continuation_t cont = nullptr; // Current continuation.
  destblock_t* env = nullptr;    // Current environment.
  int _trace = 0;
};

inline LISPT eval(lisp& l, LISPT expr) { return l.e().eval(expr); }
inline LISPT eval(LISPT expr) { return eval(lisp::current(), expr); }
inline LISPT eval(lisp& l, std::string expr)
{
  file_t in(expr);
  auto e = lispread(l, in, false);
  return eval(l, e);
}
inline LISPT eval(std::string expr) { return eval(lisp::current(), expr); }
inline LISPT apply(lisp& l, LISPT fun, LISPT args) { return l.e().apply(fun, args); }
inline LISPT apply(LISPT fun, LISPT args) { return apply(lisp::current(), fun, args); }
inline LISPT baktrace(lisp& l) { return l.e().baktrace(); }
inline LISPT baktrace() { return baktrace(lisp::current()); }
inline LISPT topofstack(lisp& l) { return l.e().topofstack(); }
inline LISPT topofstack() { return topofstack(lisp::current()); }
inline LISPT envget(lisp& l, LISPT a, LISPT b) { return l.e().envget(a, b); }
inline LISPT envget(LISPT a, LISPT b) { return envget(lisp::current(), a, b); }

} // namespace lisp
