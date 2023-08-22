//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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

#ifndef LISP_VM_HH
#define LISP_VM_HH

#include <iostream>
#include <fmt/format.h>

#include <array>
#include <cstdint>
#include <functional>
#include <variant>

#include "types.hh"

namespace lisp
{
class vm final
{
public:
  /// @brief Lisp virtual machine constructor.
  ///
  /// @details The lisp virtual machine doesn't store the context. The
  /// parameter is only there to ensure that the context is created since it's
  /// used implicitly by the virtual machine.
  ///
  /// @param ctx The context object.
  vm(context& ctx);
  /// @brief The destructor.
  ~vm();

  static vm& get()
  {
    if(_current == nullptr)
      throw std::runtime_error("lisp::vm has not been created");
    return *_current;
  }

  /// @brief This is the LISP vm.
  ///
  /// @details The vm allocates a destination slot for the result and
  /// starts munching continuations.
  lisp_t eval(lisp_t);
  lisp_t eval(const std::string&);
  lisp_t apply(lisp_t, lisp_t);
  lisp_t backtrace();
  /// @brief Return the current environment.
  lisp_t topofstack() const;
  /// @brief Convert an environment to a list.
  static lisp_t destblock(lisp_t);

  /// @brief Prints a backtrace of all expressions on the stack.
  void bt();
  /// @brief Unwinds the stack, restoring the shallow bindings, and then calls
  /// reset.
  void unwind();
  bool trace() const { return _trace; }
  bool trace(bool t)
  {
    bool old = _trace;
    _trace = t;
    return old;
  }
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

  lisp_t break0(lisp_t) const;

  enum class break_return
  {
    RETURN,  // Return from recursive repl
    PROCEED, // Proceed with repl
    SKIP,    // Skip eval
  };
  using repl_fun_t = std::function<lisp_t(lisp_t)>;
  repl_fun_t repl;

  bool brkflg = false;
  bool interrupt = false;

  destblock_t* environment() const { return _env; }

private:
  //
  // The control stack.
  //
  destblock_t* _dest = nullptr; // Current destination being built.
  using continuation_t = bool (vm::*)();
  struct expr_t
  {
    lisp_t value;
    operator lisp_t() const noexcept { return value; }
    lisp_t operator->() const noexcept { return value; }
    lisp_t operator=(lisp_t v)
    {
      value = v;
#ifdef LIPS_ENABLE_TRACE
      if(vm::get()._trace)
        std::cerr << fmt::format("set {}\n", vm::to_string(*this));
#endif
      return *this;
    }
  };
  struct fun_t
  {
    lisp_t value;
    operator lisp_t() const noexcept { return value; }
    lisp_t operator->() const noexcept { return value; }
    lisp_t operator=(lisp_t v)
    {
      value = v;
#ifdef LIPS_ENABLE_TRACE
      if(vm::get()._trace)
        std::cerr << fmt::format("set {}\n", vm::to_string(*this));
#endif
      return *this;
    }
  };
  struct args_t
  {
    lisp_t value;
    operator lisp_t() const noexcept { return value; }
    lisp_t operator->() const noexcept { return value; }
    lisp_t operator=(lisp_t v)
    {
      value = v;
#ifdef LIPS_ENABLE_TRACE
      if(vm::get()._trace)
        std::cerr << fmt::format("set {}\n", vm::to_string(*this));
#endif
      return *this;
    }
  };
  using control_t = std::variant<std::monostate, destblock_t*, continuation_t, expr_t, fun_t, args_t>;
  static constexpr int CTRLBLKSIZE = 4000;
  std::array<control_t, CTRLBLKSIZE> _control; // Control-stack
  int _toctrl = 0;                             // Control-stack stack pointer

  // @brief Pushes continuations, destinations, or lisp_t objects on the control
  // stack.
  template<typename T>
  void push(T t)
  {
#ifdef LIPS_ENABLE_TRACE
    if(_trace)
      std::cerr << fmt::format("push({}): {}\n", _toctrl, to_string(t));
#endif
    _control[_toctrl++] = std::move(t);
    if(_toctrl >= CTRLBLKSIZE)
      overflow();
  }
  // @brief Pops continuations, destinations, or lisp_t objects from the control
  // stack.
  template<typename T>
  void pop(T& t)
  {
    t = std::move(std::get<T>(_control[--_toctrl]));
#ifdef LIPS_ENABLE_TRACE
    if(_trace)
      std::cerr << fmt::format("pop ({}): {}\n", _toctrl, to_string(t));
#endif
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
  static std::string to_string(const expr_t& v) { return "expr: " + to_string(v.value); }
  static std::string to_string(const args_t& v) { return "args: " + to_string(v.value); }
  static std::string to_string(const fun_t& v) { return "fun: " + to_string(v.value); }
  /// @brief Translate a lisp_t object to a string
  static std::string to_string(const lisp_t&);
  /// @brief Translate a destblock_t to a string
  static std::string to_string(const destblock_t*);
  /// @brief Translate a continuation member function pointer to a string
  static std::string to_string(continuation_t);

  // Continuations
  bool eval_expr();
  bool eval0();
  bool eval_apply();
  bool apply0();
  bool eval_end();
  bool eval_func();
  bool ev1();
  bool eval_prim();
  bool ev3();
  bool ev4();
  bool evlam0();
  bool evlam1();
  bool eval_args1();
  bool eval_args2();
  bool ev3p();
  bool eval_args();
  bool noevarg();
  bool eval_lambda();
  bool spread();
  bool eval_list();
  bool evlis1();
  bool eval_list_end();
  bool evlis3();
  bool evlis4();
  bool noeval_args1();
  bool eval_sequence();
  bool eval_seq1();
  bool eval_seq2();
  bool eval_closure();
  bool eval_closure1();
  bool everr();
  bool eval_lookup();

  lisp_t printwhere();
  /// @brief Print error message, abort current evaluation, and return to top
  /// level.
  void abort(std::error_code);
  /// @brief Resets the vm to it's initial state.
  void reset();
  void overflow();
  static lisp_t destblock(const destblock_t*);

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

  undefhook_t _undefhook;         // Called in case of undefined function.
  breakhook_t _breakhook;         // Called before going into break.
  expr_t _expression;             // Stores urrent expression being evaluated.
  fun_t _fun;                     // Current function.
  args_t _args;                   // Current arguments.
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

  static vm* _current;
};

inline vm::breakhook_t breakhook(vm::breakhook_t fun) { return vm::get().breakhook(fun); }
inline vm::undefhook_t undefhook(vm::undefhook_t fun) { return vm::get().undefhook(fun); }
inline void unwind() { vm::get().unwind(); }

inline lisp_t eval(lisp_t expr) { return vm::get().eval(expr); }
inline lisp_t eval(const std::string& expr) { return vm::get().eval(expr); }
inline lisp_t apply(lisp_t fun, lisp_t args) { return vm::get().apply(fun, args); }
inline lisp_t backtrace() { return vm::get().backtrace(); }
inline lisp_t topofstack() { return vm::get().topofstack(); }
inline lisp_t destblock(lisp_t a) { return vm::destblock(a); }

} // namespace lisp

#endif
