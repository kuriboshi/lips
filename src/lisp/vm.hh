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

#include "context.hh"
#include "syntax.hh"
#include "types.hh"
#include "version.hh"

namespace lisp
{
class vm
{
public:
  vm() = default;
  virtual ~vm() = default;

  vm(const vm&) = delete;
  vm(vm&&) = delete;
  vm& operator=(const vm&) = delete;
  vm& operator=(vm&&) = delete;

  static ref_file_t primout() { return get().do_primout(); }
  static ref_file_t primerr() { return get().do_primerr(); }
  static ref_file_t primin() { return get().do_primin(); }
  static ref_file_t primout(ref_file_t f) { return get().do_primout(f); }
  static ref_file_t primerr(ref_file_t f) { return get().do_primerr(f); }
  static ref_file_t primin(ref_file_t f) { return get().do_primin(f); }
  static ref_file_t stdout() { return get().do_stdout(); }
  static ref_file_t stderr() { return get().do_stderr(); }
  static ref_file_t stdin() { return get().do_stdin(); }

  static syntax& read_table() { return get().do_read_table(); }
  static void read_table(std::unique_ptr<syntax> syntax) { get().do_read_table(std::move(syntax)); }

  static lisp_t perror(std::error_code code) { return get().do_perror(code); }
  static lisp_t perror(std::error_code code, lisp_t a) { return get().do_perror(code, std::move(a)); }
  static lisp_t error(std::error_code code, lisp_t a) { return get().do_error(code, std::move(a)); }
  static void fatal(std::error_code code) { get().do_fatal(code); }
  template<typename... Ts>
  static void fatal(std::error_code code, const Ts&... a) { get().do_fatal(code, concat(a...)); }

  static const cvariable_t& currentbase() { return get().do_currentbase(); }
  static const cvariable_t& verbose() { return get().do_verbose(); }
  static const cvariable_t& loadpath() { return get().do_loadpath(); }
  static void loadpath(lisp_t path) { get().do_loadpath(std::move(path)); }
  static const char* version() { return lisp::version(); }

  static std::int64_t printlevel() { return get().do_printlevel(); }
  static void printlevel(std::int64_t pl) { get().do_printlevel(pl); }

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
    const bool old = _trace;
    _trace = t;
    return old;
  }
  void interactive(bool b) { _interactive = b; }

  using undefhook_t = std::function<int(lisp_t, lisp_t*)>;
  undefhook_t undefhook(undefhook_t fun)
  {
    auto f = _undefhook;
    _undefhook = std::move(fun);
    return f;
  }

  using breakhook_t = std::function<void()>;
  breakhook_t breakhook(breakhook_t fun)
  {
    auto f = _breakhook;
    _breakhook = std::move(fun);
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

  static vm& get()
  {
    if(set(nullptr) == nullptr)
      throw std::runtime_error("lisp::vm has not been created");
    return *set(nullptr);
  }

protected:
  static vm* set(vm* ptr);

  virtual ref_file_t do_primout() = 0;
  virtual ref_file_t do_primerr() = 0;
  virtual ref_file_t do_primin() = 0;
  virtual ref_file_t do_primout(ref_file_t f) = 0;
  virtual ref_file_t do_primerr(ref_file_t f) = 0;
  virtual ref_file_t do_primin(ref_file_t f) = 0;
  virtual ref_file_t do_stdout() = 0;
  virtual ref_file_t do_stderr() = 0;
  virtual ref_file_t do_stdin() = 0;

  virtual syntax& do_read_table() = 0;
  virtual void do_read_table(std::unique_ptr<syntax> syntax) = 0;

  virtual lisp_t do_perror(std::error_code code) = 0;
  virtual lisp_t do_perror(std::error_code code, lisp_t a) = 0;
  virtual lisp_t do_error(std::error_code code, lisp_t a) = 0;
  virtual void do_fatal(std::error_code code) = 0;
  virtual void do_fatal(std::error_code code, const std::string& a) = 0;

  virtual const cvariable_t& do_currentbase() = 0;
  virtual const cvariable_t& do_verbose() = 0;
  virtual const cvariable_t& do_loadpath() = 0;
  virtual void do_loadpath(lisp_t path) = 0;

  virtual std::int64_t do_printlevel() const = 0;
  virtual void do_printlevel(std::int64_t) = 0;

private:
  template<typename T, typename... Ts>
  static std::string concat(const T& first, const Ts&... args)
  {
    if constexpr(sizeof...(Ts) > 0)
    {
      return fmt::format("{} {}", first, concat(args...));
    }
    return fmt::format("{}", first);
  }

  //
  // The control stack.
  //
  using continuation_t = bool (vm::*)();
  struct expr_t
  {
    lisp_t value;
    operator lisp_t() const noexcept { return value; }
    lisp_t operator->() const noexcept { return value; }
    expr_t& operator=(lisp_t v)
    {
      value = std::move(v);
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
    fun_t& operator=(lisp_t v)
    {
      value = std::move(v);
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
    args_t& operator=(lisp_t v)
    {
      value = std::move(v);
#ifdef LIPS_ENABLE_TRACE
      if(vm::get()._trace)
        std::cerr << fmt::format("set {}\n", vm::to_string(*this));
#endif
      return *this;
    }
  };
  using control_t = std::variant<std::monostate, destblock_t*, continuation_t, expr_t, fun_t, args_t>;
  static constexpr int CTRLBLKSIZE{4000};
  std::array<control_t, CTRLBLKSIZE> _control; // Control-stack
  int _toctrl{0};                              // Control-stack stack pointer

  // @brief Pushes continuations, destinations, or lisp_t objects on the control
  // stack.
  template<typename T>
  void push(T t)
  {
#ifdef LIPS_ENABLE_TRACE
    if(_trace)
      std::cerr << fmt::format("push({}): {}\n", _toctrl, to_string(t));
#endif
    _control.at(_toctrl++) = std::move(t);
    if(_toctrl >= CTRLBLKSIZE)
      overflow();
  }
  // @brief Pops continuations, destinations, or lisp_t objects from the control
  // stack.
  template<typename T>
  void pop(T& t)
  {
    t = std::move(std::get<T>(_control.at(--_toctrl)));
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
  bool _noeval{false};            // Don't evaluate arguments.
  continuation_t _cont{nullptr};  // Current continuation.
  destblock_t* _env{nullptr};     // Current environment.

  bool _trace{false};
  bool _interactive{false};

  /// @brief Size of destination block area
  static constexpr int DESTBLOCKSIZE{3000};
  /// @brief Destination block area.
  std::array<destblock_t, DESTBLOCKSIZE> _destblock;
  // @brief Current destination being built.
  destblock_t* _dest{nullptr};
  /// @brief Index to last slot in destblock.
  int _destblockused{0};
};

template<typename Context>
class vm_t final : public vm
{
public:
  // using type = vm_t<Context>;
  // using Context = context_t;
  /// @brief Lisp virtual machine constructor.
  ///
  /// @param context The context object.
  explicit vm_t(std::shared_ptr<Context> context)
    : _context(std::move(context))
  {
    vm::set(this);
  }

  std::shared_ptr<Context> context() const { return _context; }

private:
  ref_file_t do_primout() override { return _context->primout(); }
  ref_file_t do_primerr() override { return _context->primerr(); }
  ref_file_t do_primin() override { return _context->primin(); }
  ref_file_t do_primout(ref_file_t f) override { return _context->primout(f); }
  ref_file_t do_primerr(ref_file_t f) override { return _context->primerr(f); }
  ref_file_t do_primin(ref_file_t f) override { return _context->primin(f); }
  ref_file_t do_stdout() override { return _context->stdout(); }
  ref_file_t do_stderr() override { return _context->stderr(); }
  ref_file_t do_stdin() override { return _context->stdin(); }

  syntax& do_read_table() override { return _context->read_table(); }
  void do_read_table(std::unique_ptr<syntax> syntax) override { _context->read_table(std::move(syntax)); }

  lisp_t do_perror(std::error_code code) override { return _context->perror(code); }
  lisp_t do_perror(std::error_code code, lisp_t a) override { return _context->perror(code, a); }
  lisp_t do_error(std::error_code code, lisp_t a) override { return _context->error(code, a); }
  void do_fatal(std::error_code code) override { _context->fatal(code); }
  void do_fatal(std::error_code code, const std::string& a) override
  {
    _context->fatal(code, a);
  }

  const cvariable_t& do_currentbase() override { return _context->currentbase(); }
  const cvariable_t& do_verbose() override { return _context->verbose(); }
  const cvariable_t& do_loadpath() override { return _context->loadpath(); }
  void do_loadpath(lisp_t path) override { _context->loadpath(path); }

  std::int64_t do_printlevel() const override { return _context->printlevel(); }
  void do_printlevel(std::int64_t pl) override { _context->printlevel(pl); }

  std::shared_ptr<Context> _context;
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

inline lisp_t perror(std::error_code code, lisp_t a) { return vm::perror(code, a); }
inline lisp_t error(std::error_code code, lisp_t a) { return vm::error(code, a); }
template<typename... Ts>
inline void fatal(std::error_code code, const Ts&... a)
{
  return vm::fatal(code, a...);
}

} // namespace lisp

#endif
