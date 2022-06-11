//
// Lips, lisp shell.
// Copyright 1989, 2020-2022 Krister Joas
//

#ifndef LISP_LISP_HH
#define LISP_LISP_HH

#include <array>
#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "error.hh"
#include "except.hh"
#include "pool.hh"
#include "ref_ptr.hh"
#include "symbol.hh"

namespace lisp
{
class lisp;
class evaluator;
class alloc;
class file_t;
class lisp_t;
using LISPT = ref_ptr<lisp_t>;

///
/// @brief Puts the lisp_t object back on the freelist.
///
/// @details The definition can be found in alloc.hh. See also ref_ptr.hh.
///
/// @param obj The object to be returned to the freelist.
///
void ref_deleter(lisp_t* obj);

template<typename Enum>
constexpr auto to_underlying(Enum e) noexcept
{
  return static_cast<std::underlying_type_t<Enum>>(e);
}

enum class type
{
  NIL = 0,   // so that nullptr also becomes NIL
  T,         // the truth object
  EMPTY,     // the empty object, contains no value
  SYMBOL,    // an atomic symbol
  INTEGER,   // 24 bit integer in same word
  FLOAT,     // a double
  INDIRECT,  // used when a value is stored in a closure
  CONS,      // a pair
  STRING,    // character strings
  SUBR,      // eval type primitive function
  FSUBR,     // noeval
  LAMBDA,    // lambda function
  NLAMBDA,   // noeval
  CLOSURE,   // static binding object
  UNBOUND,   // unbound indicator
  ENVIRON,   // environment stack type for gc use
  FILET,     // file pointer
  FREE,      // an object on the freelist, used for consistency checks
  ENDOFFILE, // returned from read at end of file
  ERROR,     // returned from primitive when an error occured
  CVARIABLE  // is a pointer to c-variable
};

inline constexpr auto NIL = nullptr;

/// @brief The cons cell.
///
/// @details A cons cell contains two pieces of data: The head (traditionall
/// called car) and the tail (traditionally called cdr).
///
struct cons_t
{
  LISPT car = NIL;
  LISPT cdr = NIL;
};

/// @brief Structure describing a built-in function.
///
/// @details Built-in function can have zero, one, two, or three parameters.
/// They can either evaluate their parameters or not (special forms).  Function
/// can be either spread (fixed number of arguments) or nospread (variable
/// number of arguments).
///
struct subr_t
{
  enum class subr
  {
    EVAL,
    NOEVAL
  };
  enum class spread
  {
    SPREAD,
    NOSPREAD
  };

  using func0_t = std::function<LISPT(lisp&)>;
  using func1_t = std::function<LISPT(lisp&, LISPT)>;
  using func2_t = std::function<LISPT(lisp&, LISPT, LISPT)>;
  using func3_t = std::function<LISPT(lisp&, LISPT, LISPT, LISPT)>;

  subr_t(const std::string& pname, func0_t fun, enum subr subr, enum spread spread)
    : name(pname), f(fun), subr(subr), spread(spread)
  {}
  subr_t(const std::string& pname, func1_t fun, enum subr subr, enum spread spread)
    : name(pname), f(fun), subr(subr), spread(spread)
  {}
  subr_t(const std::string& pname, func2_t fun, enum subr subr, enum spread spread)
    : name(pname), f(fun), subr(subr), spread(spread)
  {}
  subr_t(const std::string& pname, func3_t fun, enum subr subr, enum spread spread)
    : name(pname), f(fun), subr(subr), spread(spread)
  {}
  constexpr std::size_t argcount() const noexcept { return f.index(); }

  LISPT operator()(lisp& l) const { return std::get<func0_t>(f)(l); }
  LISPT operator()(lisp& l, LISPT a) const { return std::get<func1_t>(f)(l, a); }
  LISPT operator()(lisp& l, LISPT a, LISPT b) const { return std::get<func2_t>(f)(l, a, b); }
  LISPT operator()(lisp& l, LISPT a, LISPT b, LISPT c) const { return std::get<func3_t>(f)(l, a, b, c); }

  std::string name;
  std::variant<func0_t, func1_t, func2_t, func3_t> f;
  enum subr subr = subr::EVAL;
  enum spread spread = spread::SPREAD;

  using subr_vector = std::vector<subr_t>;
  using subr_index = subr_vector::size_type;
  static std::unordered_map<std::string, subr_index> subr_map;
  static subr_vector subr_store;
  static const subr_t& get(subr_index index) { return subr_store[index]; }
  static subr_index put(const subr_t& subr)
  {
    auto p = subr_map.find(subr.name);
    if(p != subr_map.end())
      throw lisp_error("redefinition of subr not allowed");
    auto index = subr_store.size();
    subr_store.push_back(subr);
    subr_map.insert(std::pair(subr.name, index));
    return index;
  }
};

/// @brief Lambda representation.
///
struct lambda_t
{
  /// @brief The S-expression representation of the lambda function.
  LISPT body = NIL;
  /// @brief The list of arguments.
  LISPT args = NIL;
  /// @brief The number of arguments.
  std::int8_t count = 0;
};

/// @brief A closure (static binding).
///
class closure_t : public ref_count<closure_t>
{
public:
  LISPT cfunction = NIL;
  LISPT closed = NIL;
  LISPT cvalues = NIL;
  std::uint8_t count = 0;

  void* operator new(std::size_t)
  {
    return _pool.allocate();
  }
  void operator delete(void* x)
  {
    _pool.deallocate(x);
  }
  void operator delete(closure_t* x, std::destroying_delete_t)
  {
    _pool.deallocate(x);
  }

private:
  using pool_t = pool<closure_t, 256>;
  static pool_t _pool;
};

using ref_closure_t = ref_ptr<closure_t>;
using ref_file_t = ref_ptr<file_t>;

struct subr_index
{
  subr_t::subr_index index;
};

/// @brief A representation of a C++ variable linked to a lisp variable.
///
/// @details Wrapps a LISPT value in such a way that the value can be changed
/// from either the C++ context of the lisp context and have the value be
/// reflected to both.
///
class cvariable_t
{
public:
  explicit cvariable_t(LISPT value): _value(value) {}
  cvariable_t() {}
  ~cvariable_t() = default;
  cvariable_t(const cvariable_t& other) = delete;
  cvariable_t(cvariable_t&& other) noexcept
  {
    _value = std::move(other._value);
  }
  cvariable_t& operator=(cvariable_t&& other) noexcept
  {
    std::swap(_value, other._value);
    return *this;
  }
  cvariable_t& operator=(const cvariable_t& other) = delete;
  cvariable_t& operator=(LISPT value)
  {
    _value = value;
    return *this;
  }

  /// @brief Automatically convert to the LISPT value in a LISPT context.
  operator LISPT () const noexcept { return _value; }
  /// @brief Dereference the wrapped LISPT value.
  LISPT operator*() const noexcept { return _value; }
  /// @brief Dereference the wrapped LISPT value.
  LISPT operator->() const noexcept { return _value; }

private:
  /// @brief The wrapped LISPT value.
  LISPT _value;
};

struct indirect_t
{
  LISPT value;
};

class destblock_t;

// ^# start of line comment
// #' Function quote
// ' Quote
// " String
// ! Repeat
// ?? History
// >
// >>
// <
// |
// &
// *
// `
// ,@
// ;

class syntax
{
public:
  syntax() { reset(); }
  enum class type
  {
    OTHER = 0,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    STRING_DELIM,
    ESCAPE,
    BREAKCHAR,
    SEPARATOR,
    //
    QUOTE,
    // Integers and floating point numbers
    EXPONENT,
    SIGN,
    DIGIT,
    DECIMAL_POINT,
    // Comments
    COMMENT,
    SHELL_COMMENT,
    NEWLINE,
    // Macros
    MACRO,
    SPLICE,
    INFIX
  };
  type get(std::uint8_t index) const { return _table[index]; }
  void set(std::uint8_t index, type value) { _table[index] = value; }
  void set(std::uint8_t index, LISPT value)
  {
    set(index, type::MACRO);
    _macro[index] = value;
  }
  LISPT macro(lisp& lisp, ref_file_t source, std::uint8_t index);
  /// @brief Reset read table to the defaults.
  void reset()
  {
    set('(', type::LEFT_PAREN);
    set(')', type::RIGHT_PAREN);
    set('[', type::LEFT_BRACKET);
    set(']', type::RIGHT_BRACKET);
    set('"', type::STRING_DELIM);
    set('\\', type::ESCAPE);
    set(' ', type::SEPARATOR);
    set('\t', type::SEPARATOR);
    set('\n', type::NEWLINE);
    set('0', type::DIGIT);
    set('1', type::DIGIT);
    set('2', type::DIGIT);
    set('3', type::DIGIT);
    set('4', type::DIGIT);
    set('5', type::DIGIT);
    set('6', type::DIGIT);
    set('7', type::DIGIT);
    set('8', type::DIGIT);
    set('9', type::DIGIT);
    set('+', type::SIGN);
    set('-', type::SIGN);
    set('.', type::DECIMAL_POINT);
    set('e', type::EXPONENT);
    set('E', type::EXPONENT);
    set(';', type::COMMENT);
    set('#', type::SHELL_COMMENT);
    set('\'', type::QUOTE);
  }
private:
  std::array<type, 256> _table = {type::OTHER};
  std::array<LISPT, 256> _macro = {NIL};
};

class lisp_t final : public ref_count<lisp_t>
{
public:
  lisp_t() = default;
  ~lisp_t() = default;
  lisp_t(const lisp_t&) = delete;

  void set() { _type = type::NIL; _u = {}; }
  void set(std::nullptr_t) { _type = type::EMPTY; _u = nullptr; }
  bool empty() const { return std::holds_alternative<std::nullptr_t>(_u); }
  auto symbol() -> symbol::symbol_t& { return symbol_collection().get(std::get<symbol::symbol_id>(_u)); }
  void set(const symbol::symbol_t& sym) { _type = type::SYMBOL; _u = sym.id; }
  auto value() const -> LISPT { return symbol_collection().get(std::get<symbol::symbol_id>(_u)).value; }
  void value(LISPT x) { symbol_collection().get(std::get<symbol::symbol_id>(_u)).value = x; }
  auto intval() const -> int { return std::get<int>(_u); }
  void set(int x)
  {
    _type = type::INTEGER;
    _u = x;
  }
  auto floatval() const -> double { return std::get<double>(_u); }
  void set(double f)
  {
    _type = type::FLOAT;
    _u = f;
  }
  auto indirectval() const -> LISPT { return std::get<indirect_t>(_u).value; }
  void set(indirect_t x) { _type = type::INDIRECT; _u = x; }
  auto cons() const -> const cons_t& { return std::get<cons_t>(_u); }
  void set(cons_t x) { _type = type::CONS; _u = x; }
  auto car() const -> LISPT { return std::get<cons_t>(_u).car; }
  auto cdr() const -> LISPT { return std::get<cons_t>(_u).cdr; }
  void car(LISPT x) { std::get<cons_t>(_u).car = x; }
  void cdr(LISPT x) { std::get<cons_t>(_u).cdr = x; }
  auto string() const -> const std::string& { return std::get<std::string>(_u); }
  void set(const std::string& s)
  {
    _type = type::STRING;
    _u = s;
  }
  auto subrval() const -> const subr_t& { return subr_t::get(std::get<subr_index>(_u).index); }
  void set(subr_index x)
  {
    _type = subr_t::get(x.index).subr == subr_t::subr::EVAL ? type::SUBR : type::FSUBR;
    _u = x;
  }
  auto lambda() -> lambda_t& { return std::get<lambda_t>(_u); }
  void set(lambda_t x, bool lambda) { _type = lambda ? type::LAMBDA : type::NLAMBDA; _u = x; }
  auto closure() -> ref_closure_t& { return std::get<ref_closure_t>(_u); }
  void set(ref_closure_t x) { _type = type::CLOSURE; _u = x; }
  auto envval() -> destblock_t* { return std::get<destblock_t*>(_u); }
  void set(destblock_t* env) { _type = type::ENVIRON; _u = env; }
  auto file() -> ref_file_t { return std::get<ref_file_t>(_u); }
  void set(ref_file_t f) { _type = type::FILET; _u = f; }
  auto cvarval() -> cvariable_t& { return std::get<cvariable_t>(_u); }
  void set(cvariable_t&& x) { _type = type::CVARIABLE; _u = std::move(x); }

  const std::string& getstr() const
  {
    return _type == type::STRING ? string() : symbol_collection().get(std::get<symbol::symbol_id>(_u)).pname;
  }

  type gettype() const { return _type; }
  void settype(type t) { _type = t; }

  static symbol::symbol_collection& symbol_collection()
  {
    static symbol::symbol_collection& all_symbols = *new symbol::symbol_collection;
    return all_symbols;
  }

  // The new and delete operators uses the global pool to create objects.
  void* operator new(std::size_t)
  {
    return _pool.allocate();
  }
  void operator delete(void* x)
  {
    _pool.deallocate(x);
  }
  void operator delete(lisp_t* x, std::destroying_delete_t)
  {
    _pool.deallocate(x);
  }

  static std::size_t freecount()
  {
    return _pool.size();
  }

private:
  using pool_t = pool<lisp_t, 256>;
  static pool_t _pool;

  type _type = type::NIL;

  // One entry for each type.  Types that has no, or just one, value are
  // indicated by a comment.
  std::variant<
    std::monostate,             // NIL
    std::nullptr_t,             // EMPTY
    symbol::symbol_id,          // SYMBOL
    int,                        // INTEGER
    double,                     // FLOAT
    indirect_t,                 // INDIRECT
    cons_t,                     // CONS
    std::string,                // STRING
    subr_index,                 // SUBR
    lambda_t,                   // LAMBDA
    ref_closure_t,              // CLOSURE
    destblock_t*,               // ENVIRON
    ref_file_t,                 // FILE
    cvariable_t                 // CVARIABLE
    > _u;
};

inline type type_of(LISPT a) { return a == nullptr ? type::NIL : a->gettype(); }
inline type type_of(lisp_t& a) { return a.gettype(); }
inline bool is_T(LISPT x) { return type_of(x) == type::T; }
inline bool is_NIL(LISPT x) { return type_of(x) == type::NIL; }

/// @brief The lisp interpreter.
///
class lisp
{
public:
  lisp();
  ~lisp();
  alloc& a() const { return _alloc; }
  evaluator& e() const { return _eval; };
  static lisp& current() { return *_current; }
  static void current(lisp& lisp) { _current = &lisp; }

  static LISPT eval(lisp& l, LISPT expr);
  static LISPT apply(lisp& l, LISPT fun, LISPT args);
  static LISPT baktrace(lisp& l);
  static LISPT topofstack(lisp& l);
  static LISPT destblock(lisp& l, LISPT a);

  static LISPT cons(lisp& l, LISPT a, LISPT b);
  static LISPT obarray(lisp& l);
  static LISPT freecount(lisp& l);

  syntax& read_table();
  void read_table(std::unique_ptr<syntax>);

  ref_file_t primout() const { return _primout; }
  ref_file_t primerr() const { return _primerr; }
  ref_file_t primin() const { return _primin; }
  void primout(ref_file_t);
  void primerr(ref_file_t);
  void primin(ref_file_t);
  ref_file_t stdout() const { return _stdout; }
  ref_file_t stderr() const { return _stderr; }
  ref_file_t stdin() const { return _stdin; }

  std::string geterror(int);
  LISPT perror(int, LISPT);
  LISPT error(int, LISPT);
  void fatal(int);
  LISPT syserr(LISPT);
  LISPT break0(LISPT);

  enum class break_return
  {
    RETURN,                     // Return from recursive repl
    PROCEED,                    // Proceed with repl
    SKIP,                       // Skip eval
  };
  using repl_fun_t = std::function<LISPT(LISPT)>;
  repl_fun_t repl;

  // Used by lisp::io
  LISPT top = NIL;
  LISPT rstack = NIL;
  int printlevel = 0;
  int thisplevel = 0;
  bool echoline = false;

  // Used by the interpreter
  bool brkflg = false;
  bool interrupt = false;

  cvariable_t& currentbase() const { return _variables->_currentbase; }
  cvariable_t& verbose() const { return _variables->_verbose; }
  cvariable_t& loadpath() const { return _variables->_loadpath; }
  void loadpath(LISPT newpath) { _variables->_loadpath = newpath; }
  const cvariable_t& version() const { return _version; }

private:
  alloc& _alloc;
  evaluator& _eval;
  std::unique_ptr<syntax> _syntax;

  ref_file_t _primout;
  ref_file_t _primerr;
  ref_file_t _primin;
  ref_file_t _stdout;
  ref_file_t _stderr;
  ref_file_t _stdin;
  static lisp* _current;

  class cvariables
  {
  public:
    cvariables(alloc&);
    cvariable_t& _currentbase;
    cvariable_t& _verbose;
    cvariable_t& _loadpath;
  };
  cvariable_t _version;
  std::unique_ptr<cvariables> _variables;

  static std::map<int, std::string> messages;
  // Some standard messages, all of them not necessarily used
  // clang-format off
  const std::vector<std::string> errmess = {
    "Not NIL",
    "Not T",
    "Not a symbol",
    "Not an integer",
    "Not a float",
    "Not indirect",
    "Not a cons cell",
    "Not a string",
    "Not SUBR",
    "Not FSUBR",
    "Not LAMBDA",
    "Not NLAMBDA",
    "Not a closure",
    "Not unbound",
    "Not an environment",
    "Not a file pointer",
    "Not free",
    "Not EOF",
    "Not an ERROR",
    "Not a c-variable"
  };
  // clang-format on
};

inline LISPT perror(lisp& l, int i, LISPT a) { return l.perror(i, a); }
inline LISPT perror(int i, LISPT a) { return perror(lisp::current(), i, a); }
inline LISPT error(lisp& l, int i, LISPT a) { return l.error(i, a); }
inline LISPT error(int i, LISPT a) { return error(lisp::current(), i, a); }
inline LISPT syserr(lisp& l, LISPT a) { return l.syserr(a); }
inline LISPT syserr(LISPT a) { return syserr(lisp::current(), a); }
inline LISPT break0(lisp& l, LISPT a) { return l.break0(a); }
inline LISPT break0(LISPT a) { return break0(lisp::current(), a); }

inline LISPT eval(lisp& l, LISPT expr) { return lisp::eval(l, expr); }
inline LISPT eval(LISPT expr) { return lisp::eval(lisp::current(), expr); }
LISPT eval(lisp&, const std::string&);
LISPT eval(const std::string& expr);
inline LISPT apply(lisp& l, LISPT fun, LISPT args) { return lisp::apply(l, fun, args); }
inline LISPT apply(LISPT fun, LISPT args) { return lisp::apply(lisp::current(), fun, args); }
inline LISPT baktrace(lisp& l) { return lisp::baktrace(l); }
inline LISPT baktrace() { return lisp::baktrace(lisp::current()); }
inline LISPT topofstack(lisp& l) { return lisp::topofstack(l); }
inline LISPT topofstack() { return lisp::topofstack(lisp::current()); }
inline LISPT destblock(lisp& l, LISPT a) { return lisp::destblock(l, a); }
inline LISPT destblock(LISPT a) { return lisp::destblock(lisp::current(), a); }

inline LISPT cons(lisp& l, LISPT a, LISPT b) { return lisp::cons(l, a, b); }
inline LISPT cons(LISPT a, LISPT b) { return lisp::cons(lisp::current(), a, b); }
inline LISPT obarray(lisp& l) { return lisp::obarray(l); }
inline LISPT obarray() { return lisp::obarray(lisp::current()); }
inline LISPT freecount(lisp& l) { return lisp::freecount(l); }
inline LISPT freecount() { return lisp::freecount(lisp::current()); }

//
// All lisp constants used internally.
//
extern LISPT T;
extern LISPT C_EMPTY;
extern LISPT C_AUTOLOAD;
extern LISPT C_BROKEN;
extern LISPT C_BT;
extern LISPT C_CLOSURE;
extern LISPT C_CONS;
extern LISPT C_DOT;
extern LISPT C_ENDOFFILE;
extern LISPT C_ENVIRON;
extern LISPT C_EOF;
extern LISPT C_ERROR;
extern LISPT C_FILE;
extern LISPT C_FLOAT;
extern LISPT C_FREE;
extern LISPT C_FSUBR;
extern LISPT C_GO;
extern LISPT C_INDIRECT;
extern LISPT C_INTEGER;
extern LISPT C_LAMBDA;
extern LISPT C_NLAMBDA;
extern LISPT C_OLDDEF;
extern LISPT C_QUOTE;
extern LISPT C_REDEFINED;
extern LISPT C_RESET;
extern LISPT C_RETURN;
extern LISPT C_STRING;
extern LISPT C_SUBR;
extern LISPT C_SYMBOL;
extern LISPT C_READ;
extern LISPT C_WRITE;
extern LISPT C_APPEND;
} // namespace lisp

#include "alloc.hh"
#include "arith.hh"
#include "debug.hh"
#include "eval.hh"
#include "file.hh"
#include "iter.hh"
#include "io.hh"
#include "logic.hh"
#include "low.hh"
#include "map.hh"
#include "pred.hh"
#include "prim.hh"
#include "prop.hh"
#include "repl.hh"
#include "rtable.hh"
#include "string.hh"
#include "user.hh"
#include "version.hh"

namespace lisp
{
class current
{
public:
  current(lisp& c)
  {
    previous = &lisp::current();
    lisp::current(c);
  }
  ~current()
  {
    lisp::current(*previous);
  }
  current(const current&) = delete;
private:
  lisp* previous = nullptr;
};

template<typename T>
void check(LISPT arg, T type)
{
  if(type_of(arg) != type)
    error(ILLEGAL_ARG, arg);
}

template<typename T, typename ...Ts>
void check(LISPT arg, T type, Ts ...types)
{
  if(type_of(arg) == type)
    return;
  check(arg, types...);
}

inline void break_flag(lisp& l, bool val) { l.brkflg = val; }
inline void break_flag(bool val) { lisp::current().brkflg = val; }

} // namespace lisp

#endif
