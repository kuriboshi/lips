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

#include "context.hh"
#include "error.hh"
#include "io.hh"
#include "types.hh"
#include "details/vm.hh"

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
    lisp_t var;
    lisp_t val;
  };
  std::variant<control_block, var_val_pair> u;

public:
  void reset() { u = var_val_pair{nil, nil}; }

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

  void var(lisp_t x) { std::get<var_val_pair>(u).var = x; }
  lisp_t var() const { return std::get<var_val_pair>(u).var; }
  void val(lisp_t x) { std::get<var_val_pair>(u).val = x; }
  lisp_t val() const { return std::get<var_val_pair>(u).val; }
};

class vm
{
public:
  vm(context&);
  ~vm() = default;

  void reset();

  /// @brief This is the LISP vm.
  ///
  /// @details The vm allocates a destination slot for the result and
  /// starts munching continuations.
  lisp_t eval(lisp_t);
  lisp_t apply(lisp_t, lisp_t);
  lisp_t backtrace();
  /// @brief Return the current environment.
  lisp_t topofstack() const;
  /// @brief Convert an environment to a list.
  lisp_t destblock(lisp_t);

  /// @brief Prints a backtrace of all expressions on the stack.
  void bt();
  void unwind();
  bool trace() const { return _trace; }
  void trace(bool t) { _trace = t; }
  void interactive(bool b) { _interactive = b; }

  using undefhook_t = std::function<int(lisp_t, lisp_t*)>;
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
  using continuation_t = bool (vm::*)();
  using control_t = std::variant<std::monostate, continuation_t, destblock_t*, lisp_t>;
  static constexpr int CTRLBLKSIZE = 4000;
  std::array<control_t, CTRLBLKSIZE> _control; // Control-stack
  int _toctrl = 0;                             // Control-stack stack pointer

  // @brief Pushes continuations, destinations, or lisp_t objects on the control
  // stack.
  template<typename T>
  void push(T t)
  {
    _control[_toctrl++] = t;
    if(_toctrl >= CTRLBLKSIZE)
      overflow();
  }
  // @brief Pops continuations, destinations, or lisp_t objects from the control
  // stack.
  template<typename T>
  void pop(T& t)
  {
    t = std::get<T>(_control[--_toctrl]);
  }
  void pop_env();

  /// @brief This function prints an error message, and sets up a call to everr
  /// that handles breaks.
  void xbreak(std::error_code, lisp_t fault, continuation_t next);
  void storevar(lisp_t v, int i);
  void send(lisp_t a);
  lisp_t receive();
  void next();
  /// @brief Make a call to the function in parameter `fun'.
  ///
  /// @details It can handle functions with up to three arguments.
  lisp_t call(lisp_t fun);
  bool evalhook(lisp_t exp);
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

  lisp_t printwhere();
  /// @brief Print error message, abort current evaluation, and return to top
  /// level.
  void abort(std::error_code);
  void overflow();
  lisp_t destblock(const destblock_t*);

  ///
  /// @brief Allocates a destination block of size size.
  ///
  /// @param size The size of the destination block.
  /// @returns A destblock or nullptr if no more space available.
  destblock_t* mkdestblock(int);
  ///
  /// @brief Free a destination block.
  ///
  /// @details The destination blocks are freed in the reverse order of their
  /// allocation.
  ///
  /// @param The destination block to free.
  void free(destblock_t* block);

  context& _ctx;
  undefhook_t _undefhook;         // Called in case of undefined function.
  breakhook_t _breakhook;         // Called before going into break.
  lisp_t _fun;                    // Store current function being evaluated.
  lisp_t _expression;             // Current expression.
  lisp_t _args;                   // Current arguments.
  bool _noeval = false;           // Don't evaluate arguments.
  continuation_t _cont = nullptr; // Current continuation.
  destblock_t* _env = nullptr;    // Current environment.
  bool _trace = false;
  bool _interactive = false;

  /// @brief Size of destination block area
  static constexpr int DESTBLOCKSIZE = 3000;
  /// @brief Destination block area.
  std::array<destblock_t, DESTBLOCKSIZE> _destblock;
  /// @brief Index to last slot in destblock.
  int _destblockused = 0;
};

inline vm::breakhook_t breakhook(vm::breakhook_t fun) { return context::current().vm().breakhook(fun); }
inline vm::undefhook_t undefhook(vm::undefhook_t fun) { return context::current().vm().undefhook(fun); }
inline void unwind() { context::current().vm().unwind(); }

inline lisp_t eval(lisp_t expr) { return details::vm::eval(context::current(), expr); }
inline lisp_t eval(const std::string& expr) { return details::vm::eval(context::current(), lispread(expr)); }
inline lisp_t apply(lisp_t fun, lisp_t args) { return details::vm::apply(context::current(), fun, args); }
inline lisp_t backtrace() { return details::vm::backtrace(context::current()); }
inline lisp_t topofstack() { return details::vm::topofstack(context::current()); }
inline lisp_t destblock(lisp_t a) { return details::vm::destblock(context::current(), a); }

} // namespace lisp

#endif
