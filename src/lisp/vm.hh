//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

/// @file vm.hh
///
/// # Function Evaluation
///
/// Functions in this section relate to the evaluation of lisp expressions.

#include <array>
#include <concepts>
#include <cstdint>
#include <functional>
#include <iostream>
#include <variant>

#include <fmt/format.h>

#include "context.hh"
#include "syntax.hh"
#include "types.hh"
#include "version.hh"
#include "details/vm.hh"

namespace lisp
{
class vm
{
public:
  /// @brief Default constructor.
  vm() = default;
  /// @brief Default destructor.
  virtual ~vm() = default;

  /// @brief Disable copy constructor.
  vm(const vm&) = delete;
  /// @brief Disable move constructor.
  vm(vm&&) = delete;
  /// @brief Disable assignment operator.
  vm& operator=(const vm&) = delete;
  /// @brief Disable move assignment operator.
  vm& operator=(vm&&) = delete;

  /// @brief Returns the current primary output file.
  static ref_file_t primout() { return get().do_primout(); }
  /// @brief Returns the current primary error file.
  static ref_file_t primerr() { return get().do_primerr(); }
  /// @brief Returns the current primary input file.
  static ref_file_t primin() { return get().do_primin(); }
  /// @brief Sets the primary output file.
  ///
  /// @param file Set the primary output file to this file.
  /// @returns The previous output file.
  static ref_file_t primout(ref_file_t file) { return get().do_primout(std::move(file)); }
  /// @brief Sets the primary output file.
  ///
  /// @param file Set the primary output file to this file.
  /// @returns The previous output file.
  static ref_file_t primerr(ref_file_t file) { return get().do_primerr(std::move(file)); }
  /// @brief Sets the primary output file.
  ///
  /// @param file Set the primary output file to this file.
  /// @returns The previous output file.
  static ref_file_t primin(ref_file_t file) { return get().do_primin(std::move(file)); }
  /// @brief Returns the standard output file.
  static ref_file_t stdout() { return get().do_stdout(); }
  /// @brief Returns the standard error file.
  static ref_file_t stderr() { return get().do_stderr(); }
  /// @brief Returns the standard input file.
  static ref_file_t stdin() { return get().do_stdin(); }

  /// @brief Returns the current read table.
  static syntax& read_table() { return get().do_read_table(); }
  /// @brief Sets the read table.
  static void read_table(std::unique_ptr<syntax> syntax) { get().do_read_table(std::move(syntax)); }

  /// @brief Prints an error associated with the error code.
  static lisp_t perror(std::error_code code) { return get().do_perror(code); }
  /// @brief Prints an error associated with the error code and the lisp expression.
  static lisp_t perror(std::error_code code, lisp_t a) { return get().do_perror(code, std::move(a)); }
  /// @brief Prints an error associated with the error code and throw a lisp_error exception.
  static lisp_t error(std::error_code code, lisp_t a) { return get().do_error(code, std::move(a)); }
  /// @brief Throw a lisp_error exception with the error code.
  static void fatal(std::error_code code) { get().do_fatal(code); }
  /// @brief Throw a lisp_error exception with the error code and the concatenation of the strings.
  template<typename... Ts>
  static void fatal(std::error_code code, const Ts&... a)
  {
    get().do_fatal(code, concat(a...));
  }

  /// @brief Return the current base used for numeric output as a cvariable.
  static const cvariable_t& currentbase() { return get().do_currentbase(); }
  /// @brief Return the current setting of the verbose flag as a cvariable.
  static const cvariable_t& verbose() { return get().do_verbose(); }
  /// @brief Return the current load path as a cvariable.
  static const cvariable_t& loadpath() { return get().do_loadpath(); }
  /// @brief Sets the load path to the list of strings or symbols.
  static void loadpath(lisp_t path) { get().do_loadpath(std::move(path)); }

  /// @brief Return the program version string.
  static const std::tuple<unsigned, unsigned, unsigned, const char*> version() { return lisp::version(); }

  /// @brief Return the current print level.
  static integer_t::value_type printlevel() { return get().do_printlevel(); }
  /// @brief Set the print level.
  static void printlevel(integer_t::value_type pl) { get().do_printlevel(pl); }

  /// @brief This is the LISP vm.
  ///
  /// The vm allocates a destination slot for the result and starts munching
  /// continuations.
  lisp_t eval(const lisp_t&);
  /// @brief Evaluate the string as a lisp expression.
  lisp_t eval(const std::string&);
  /// @brief Apply a function to arguments.
  ///
  /// @param fun The function to apply. Can be any type of function.
  /// @param args A list of arguments.
  lisp_t apply(const lisp_t& fun, const lisp_t& args);
  /// @brief Print a backtrace of the internal evaluation stack.
  lisp_t backtrace();
  /// @brief Return the current environment.
  lisp_t topofstack() const;
  /// @brief Convert an environment to a list.
  static lisp_t destblock(const lisp_t&);

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

  virtual integer_t::value_type do_printlevel() const = 0;
  virtual void do_printlevel(integer_t::value_type) = 0;

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
  void xbreak(std::error_code, const lisp_t& fault, continuation_t next);
  void storevar(const lisp_t& v, int i);
  void send(const lisp_t& a);
  lisp_t receive();
  void next();
  /// @brief Make a call to the function in parameter `fun'.
  ///
  /// It can handle functions with up to three arguments.
  lisp_t call(const lisp_t& fun);
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
  /// @brief Called when the stack overflows.
  void overflow();
  static lisp_t destblock(const destblock_t*);

  ///
  /// @brief Allocates a destination block of size size.
  ///
  /// @param size The size of the destination block.
  /// @returns A pointer to a destblock_t or nullptr if no more space
  /// available.
  destblock_t* mkdestblock(int);
  ///
  /// @brief Free a destination block.
  ///
  /// The destination blocks are freed in the reverse order of their
  /// allocation.
  ///
  /// @param block The destination block to free.
  void free(destblock_t* block);

  undefhook_t _undefhook;        // Called in case of undefined function.
  breakhook_t _breakhook;        // Called before going into break.
  expr_t _expression;            // Stores current expression being evaluated.
  fun_t _fun;                    // Current function.
  args_t _args;                  // Current arguments.
  bool _noeval{false};           // Don't evaluate arguments.
  continuation_t _cont{nullptr}; // Current continuation.
  destblock_t* _env{nullptr};    // Current environment.

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

/// @internal Provides functions to get and set the read_table.
template<typename T>
concept syntax_table = requires(T v, std::unique_ptr<syntax> syn) {
  { v.read_table() } -> std::same_as<syntax&>;
  v.read_table(std::move(syn));
};

/// @internal Provides getting and setting standard file streams.
template<typename T>
concept standard_streams = requires(T v, ref_file_t file) {
  { v.primout() } -> std::same_as<ref_file_t>;
  { v.primerr() } -> std::same_as<ref_file_t>;
  { v.primin() } -> std::same_as<ref_file_t>;
  { v.primout(file) } -> std::same_as<ref_file_t>;
  { v.primerr(file) } -> std::same_as<ref_file_t>;
  { v.primin(file) } -> std::same_as<ref_file_t>;
  { v.stdout() } -> std::same_as<ref_file_t>;
  { v.stderr() } -> std::same_as<ref_file_t>;
  { v.stdin() } -> std::same_as<ref_file_t>;
};

/// @internal Provides functions to get and set the print level.
template<typename T>
concept print_level = requires(T v, integer_t::value_type i) {
  v.printlevel(i);
  { v.printlevel() } -> std::same_as<integer_t::value_type>;
};

/// @internal Provides access to some miscellaneous global variables.
template<typename T>
concept misc_globals = requires(T v, lisp_t exp) {
  { v.currentbase() } -> std::same_as<const cvariable_t&>;
  { v.verbose() } -> std::same_as<const cvariable_t&>;
  { v.loadpath() } -> std::same_as<const cvariable_t&>;
  { v.loadpath(exp) };
};

/// @internal Provides some error reporing functions.
template<typename T>
concept error_functions = requires(T v, std::error_code code, const std::string& str, lisp_t exp) {
  { v.perror(code) } -> std::same_as<lisp_t>;
  { v.perror(code, exp) } -> std::same_as<lisp_t>;
  { v.error(code, exp) } -> std::same_as<lisp_t>;

  { T::fatal(code) } -> std::same_as<lisp_t>;
  { T::fatal(code, str) } -> std::same_as<lisp_t>;
};

/// @internal The concept required by the Context parameter to vm_t.
template<typename T>
concept Context = syntax_table<T> && standard_streams<T> && print_level<T> && misc_globals<T> && error_functions<T>;

/// @brief Templated derived class which implements the virtual functions of
/// vm.
///
/// The vm_t template provides access to some global symbols like the primary
/// input and output file, the stdin and stdout, and some other global
/// symbols. It also provides access to the implementation of error functions.
///
/// The Context needs to satisfy the _Context_ concept. The type
/// `lisp::context_t` provides a default implementation.
///
/// @tparam Context The _context_ holds the actual implementation.
template<Context Context>
class vm_t final: public vm
{
public:
  /// @brief Lisp virtual machine constructor.
  ///
  /// @param context The context object.
  explicit vm_t(std::unique_ptr<Context> context)
    : _context(std::move(context))
  {
    vm::set(this);
  }

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
  void do_fatal(std::error_code code, const std::string& a) override { _context->fatal(code, a); }

  const cvariable_t& do_currentbase() override { return _context->currentbase(); }
  const cvariable_t& do_verbose() override { return _context->verbose(); }
  const cvariable_t& do_loadpath() override { return _context->loadpath(); }
  void do_loadpath(lisp_t path) override { _context->loadpath(path); }

  integer_t::value_type do_printlevel() const override { return _context->printlevel(); }
  void do_printlevel(integer_t::value_type pl) override { _context->printlevel(pl); }

  std::unique_ptr<Context> _context;
};

inline vm::breakhook_t breakhook(vm::breakhook_t fun) { return vm::get().breakhook(std::move(fun)); }
inline vm::undefhook_t undefhook(vm::undefhook_t fun) { return vm::get().undefhook(std::move(fun)); }
inline void unwind() { vm::get().unwind(); }

/// @brief Evaluate a lisp expression.
/// @lisp{(eval fn),Function}
///
/// @param expr The lisp expression to evaluate.
///
/// @returns Returns the result of the evaluation.
inline lisp_t eval(const lisp_t& expr) { return vm::get().eval(expr); }
/// @brief Evaluate a lisp expression read from a string.
inline lisp_t eval(const std::string& expr) { return vm::get().eval(expr); }
/// @brief Apply a function to a list of arguments.
/// @lisp{(apply fn lis),Function}
///
/// Applies the function _fn_ to the arguments in the list _list_ as if _fn_ is
/// called with the list as its arguments.
///
/// ```lisp
/// (apply car '((a b c)))
///   => a
/// ```
inline lisp_t apply(const lisp_t& fn, const lisp_t& list) { return vm::get().apply(fn, list); }
/// @brief A nospread version of `apply`.
/// @lisp{(apply* fn args...),NoSpread Function}
///
/// ```lisp
/// (apply* car '(a b c))
///   => a
/// ```

/// @brief Returns _x_ unevaluated.
/// @lisp{(quote a),NLambda Function}
inline lisp_t quote(const lisp_t& a) { return details::vm::quote(a); }
/// @brief Creates a lambda object.
/// @lisp{(lambda x . y),NoSpread Function}
///
/// The parameter _x_ is the parameters of the function being defined. If it's
/// a list of atoms the function is a spread function, if it's a single atoms
/// the function is a nospread function, if it's dotted pair the function is a
/// half spread function.
///
/// A _spread_ function binds each formal parameter to the actual parameters
/// when the function is called. Any excess parameter is ignored and any
/// missing actual parameter is bound to `nil`.
///
/// A _nospread_ function binds the formal parameter to a list of all actual
/// parameters when called.
///
/// A _half spread_ function is a combination of the above where the actual
/// parameters are bound to each formal parameter and any excess actual
/// parameters are bound to the formal parameter in the symbol in the `cdr` of
/// the list of formal parameters.
inline lisp_t lambda(const lisp_t& x, const lisp_t& y) { return details::vm::lambda(x, y); }
/// @brief Creates an nlambda function object.
/// @lisp{(nlambda x . y)
///
/// Same as `lambda` except that the function object is an nlambda function
/// object and parameters are not evaluated when the function is called.
inline lisp_t nlambda(const lisp_t& x, const lisp_t& y) { return details::vm::nlambda(x, y); }
/// @brief Eval function that forms the closure of function _f_, with variables
/// listed in _v_ statically bound.
/// @lisp{(closure f v),Function}
///
/// This is close to function in other lisp dialects. Any closure that is
/// created within another closure and lists a variable contained in that
/// closure refers to the same variable. This makes it possible for two
/// closures to share one or more variables.
///
/// Here is an example of defining a simple function which maintains the
/// balance of a bank account.
///
/// ```lisp
/// (defineq
///   (make-account
///    (lambda (balance)
///      ((closure
///           '(progn
///             (setq withdraw
///              (closure
///                  (lambda (amount)
///                    (setq balance (difference balance amount)))
///                  '(balance)))
///             (setq deposit
///              (closure
///                  (lambda (amount)
///                    (setq balance (plus balance amount)))
///                  '(balance)))
///             (lambda ()
///               (closure
///                   (lambda (m)
///                     (cond
///                       ((eq m 'withdraw) withdraw)
///                       ((eq m 'deposit) deposit)
///                       (t nil)))
///                   '(withdraw deposit))))
///           '(balance withdraw deposit))))))
/// ```
///
/// The function `make-account` creates and returns a closure object which
/// binds the three symbols on line 24 in their lexical scope. It sets the
/// symbols `withdraw` and `deposit` each to a closure over `balance` with a
/// lambda expression which subtracts or adds an `amount` to the `balance`.
inline lisp_t closure(const lisp_t& a, const lisp_t& b) { return details::vm::closure(a, b); }

/// @brief Print a backtrace of the control stack.
/// @lisp{(backtrace),Function}
///
/// Subject to change between any version of the interpeter.
///
/// @returns `nil`
inline lisp_t backtrace() { return vm::get().backtrace(); }
/// @brief Retrieves the most recent environment.
/// @lisp{(topofstack),Function}
///
/// Subject to change between any version of the interpeter.
///
/// @returns An object of type environment.
inline lisp_t topofstack() { return vm::get().topofstack(); }
/// @brief Converts an object of type environment to a list.
/// @lisp{(destblock),Function}
///
/// Subject to change between any version of the interpeter.
///
/// @returns A list representing a destblock.
inline lisp_t destblock(const lisp_t& a) { return vm::destblock(a); }
/// @brief Exits with an error code.
/// @lisp{(error code),Function}
inline lisp_t error(const lisp_t& code) { return details::vm::error(code); }

inline lisp_t perror(std::error_code code, const lisp_t& a) { return vm::perror(code, a); }
inline lisp_t error(std::error_code code, const lisp_t& a) { return vm::error(code, a); }
template<typename... Ts>
inline void fatal(std::error_code code, const Ts&... a)
{
  return vm::fatal(code, a...);
}

} // namespace lisp

#endif
