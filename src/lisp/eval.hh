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
#include <cstdint>
#include <functional>
#include <variant>

#include "error.hh"
#include "types.hh"

namespace lisp
{
/// @brief Destination block is used to collect the parameters to a function.
///
/// @details The destblock_t is used to store variables and their values.  Each
/// block of variable/value pairs is proceeded by a control block which
/// contains the following pieces of information: The size of the block, the
/// index of the variable/value pair currently being set, and a link to another
/// destblock_t in a chain of blocks.
///
class destblock_t
{
private:
  struct control_block
  {
    std::int8_t size;
    std::int8_t index;
    destblock_t* link;
  };
  struct var_val_pair
  {
    LISPT var;
    LISPT val;
  };
  std::variant<control_block, var_val_pair> u;

public:
  void reset() { u = var_val_pair{NIL, NIL}; }

  void num(std::int8_t size) { u = control_block{size, size, nullptr}; }
  int size() const { return std::get<control_block>(u).size; }
  int index() const { return std::get<control_block>(u).index; }
  destblock_t* link() const { return std::get<control_block>(u).link; }
  void link(destblock_t* dest) { std::get<control_block>(u).link = dest; }
  void decr()
  {
    if(std::get<control_block>(u).index > 0)
      --std::get<control_block>(u).index;
  }

  void var(LISPT x) { std::get<var_val_pair>(u).var = x; }
  LISPT var() const { return std::get<var_val_pair>(u).var; }
  void val(LISPT x) { std::get<var_val_pair>(u).val = x; }
  LISPT val() const { return std::get<var_val_pair>(u).val; }
};

class evaluator
{
public:
  evaluator(context&);
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
  undefhook_t undefhook(undefhook_t fun)
  {
    auto f = _undefhook;
    _undefhook = fun;
    return f;
  }
  using breakhook_t = std::function<void()>;
  breakhook_t breakhook(breakhook_t fun)
  {
    auto f = _breakhook;
    _breakhook = fun;
    return f;
  }

  destblock_t* environment() const { return _env; }

private:
  destblock_t* _dest = nullptr; // Current destination being built.

  //
  // The control stack.
  //
  using continuation_t = bool (evaluator::*)();
  using control_t = std::variant<std::monostate, continuation_t, destblock_t*, LISPT>;
  static constexpr int CTRLBLKSIZE = 4000;
  std::array<control_t, CTRLBLKSIZE> _control; // Control-stack
  int _toctrl = 0;                             // Control-stack stack pointer

  // @brief Pushes continuations, destinations, or LISPT objects on the control
  // stack.
  template<typename T>
  void push(T t)
  {
    _control[_toctrl++] = t;
    if(_toctrl >= CTRLBLKSIZE)
      overflow();
  }
  // @brief Pops continuations, destinations, or LISPT objects from the control
  // stack.
  template<typename T>
  void pop(T& t)
  {
    t = std::get<T>(_control[--_toctrl]);
  }
  void pop_env();

  void xbreak(std::error_code, LISPT fault, continuation_t next);
  destblock_t* mkdestblock(int);
  void storevar(LISPT v, int i);
  void send(LISPT a);
  LISPT receive();
  void next();
  LISPT call(LISPT fun);
  bool evalhook(LISPT exp);
  void do_unbound(continuation_t);
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
  void abort(std::error_code, LISPT v);
  void overflow();
  LISPT destblock(const destblock_t*);

  context& _ctx;
  undefhook_t _undefhook;         // Called in case of undefined function.
  breakhook_t _breakhook;         // Called before going into break.
  LISPT _fun;                     // Store current function being evaluated.
  LISPT _expression;              // Current expression.
  LISPT _args;                    // Current arguments.
  bool _noeval = false;           // Don't evaluate arguments.
  continuation_t _cont = nullptr; // Current continuation.
  destblock_t* _env = nullptr;    // Current environment.
  int _trace = 0;
  bool _interactive = false;

  /// @brief Allocate a destination block.
  ///
  /// @param size The size of the destination block.
  ///
  destblock_t* dalloc(int size);

  /// @brief Free a destination block.
  ///
  /// @details The destination blocks are freed in the reverse order of their
  /// allocation.
  ///
  /// @param The block to free.
  ///
  void dfree(destblock_t* block);

  /// @brief Release all destination blocks.
  ///
  void dzero();

  /// @brief Size of destination block area
  static constexpr int DESTBLOCKSIZE = 3000;
  /// @brief Destination block area.
  std::array<destblock_t, DESTBLOCKSIZE> _destblock;
  /// @brief Index to last slot in destblock.
  int _destblockused = 0;
};

inline evaluator::breakhook_t breakhook(evaluator::breakhook_t fun) { return context::current().e().breakhook(fun); }
inline evaluator::undefhook_t undefhook(evaluator::undefhook_t fun) { return context::current().e().undefhook(fun); }
inline void unwind() { context::current().e().unwind(); }

inline LISPT eval(LISPT expr) { return context::eval(context::current(), expr); }
LISPT eval(const std::string& expr);
inline LISPT apply(LISPT fun, LISPT args) { return context::apply(context::current(), fun, args); }
inline LISPT baktrace() { return context::baktrace(context::current()); }
inline LISPT topofstack() { return context::topofstack(context::current()); }
inline LISPT destblock(LISPT a) { return context::destblock(context::current(), a); }

} // namespace lisp

#endif
