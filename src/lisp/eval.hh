//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#ifndef LISP_EVAL_HH
#define LISP_EVAL_HH

#include <array>
#include <functional>
#include <variant>

#include "lisp.hh"

namespace lisp
{
class evaluator
{
public:
  evaluator(lisp&);
  ~evaluator() = default;

  void reset();

  LISPT eval(LISPT);
  LISPT apply(LISPT, LISPT);
  LISPT baktrace();
  /// @brief Return the current environment.
  LISPT topofstack();
  /// @brief Convert an environment to a list.
  LISPT destblock(LISPT);

  void bt();
  void unwind();
  int trace() const { return _trace; }
  void trace(int t) { _trace = t; }
  void interactive(bool b) { _interactive = b; }

  using undefhook_t = std::function<int(LISPT, LISPT*)>;
  void undefhook(undefhook_t fun) { _undefhook = fun; }
  using breakhook_t = std::function<void()>;
  void breakhook(breakhook_t fun) { _breakhook = fun; }

  destblock_t* environment() const { return env; }

private:
  destblock_t* dest = nullptr; // Current destination being built.

  //
  // The control stack.
  //
  using continuation_t = bool (evaluator::*)();
  using control_t = std::variant<std::monostate, continuation_t, destblock_t*, LISPT>;
  static constexpr int CTRLBLKSIZE = 4000;
  std::array<control_t, CTRLBLKSIZE> control; // Control-stack
  int toctrl = 0;                             // Control-stack stack pointer

  // @brief Pushes continuations, destinations, or LISPT objects on the control
  // stack.
  template<typename T>
  void push(T t)
  {
    control[toctrl++] = t;
    if(toctrl >= CTRLBLKSIZE)
      overflow();
  }
  // @brief Pops continuations, destinations, or LISPT objects from the control
  // stack.
  template<typename T>
  void pop(T& t)
  {
    t = std::get<T>(control[--toctrl]);
  }
  void pop_env();

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
  LISPT destblock(lisp&, const destblock_t*);

  lisp& l;
  alloc& a;
  undefhook_t _undefhook;        // Called in case of undefined function.
  breakhook_t _breakhook;        // Called before going into break.
  LISPT fun;                     // Store current function being evaluated.
  LISPT expression;              // Current expression.
  LISPT args;                    // Current arguments.
  bool noeval = false;           // Don't evaluate arguments.
  continuation_t cont = nullptr; // Current continuation.
  destblock_t* env = nullptr;    // Current environment.
  int _trace = 0;
  bool _interactive = false;
};

inline void breakhook(lisp& l, evaluator::breakhook_t fun) { l.e().breakhook(fun); }
inline void breakhook(evaluator::breakhook_t fun) { lisp::current().e().breakhook(fun); }
inline void undefhook(lisp& l, evaluator::undefhook_t fun) { l.e().undefhook(fun); }
inline void undefhook(evaluator::undefhook_t fun) { lisp::current().e().undefhook(fun); }
inline void unwind(lisp& l) { l.e().unwind(); }
inline void unwind() { lisp::current().e().unwind(); }

} // namespace lisp

#endif
