//
// Lips, lisp shell.
// Copyright 1989, 2020 Krister Joas
//

#ifndef LISP_LISP_HH
#define LISP_LISP_HH

//
// This header file is private to the libisp libray.  Applications using
// libisp should only include libisp.hh.
//

#include <cstdint>
#include <functional>
#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "error.hh"
#include "except.hh"
#include "symbol.hh"

namespace lisp
{
class lisp;
class evaluator;
class alloc;
class file_t;

//
// All lisp constants used internally.
//
extern LISPT T;
extern LISPT C_APPEND;
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
extern LISPT C_WRITE;

template<typename Enum>
constexpr auto to_underlying(Enum e) noexcept
{
  return static_cast<std::underlying_type_t<Enum>>(e);
}

enum class type
{
  NIL = 0,   // so that nullptr also becomes NIL
  T,         // the truth object
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
  CVARIABLE // is a pointer to c-variable
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
  LISPT lambdarep = NIL;
  /// @brief The list of arguments.
  LISPT arglist = NIL;
  /// @brief The number of arguments.
  std::int8_t argcnt = 0;
};

/// @brief A closure (static binding).
///
struct closure_t
{
  LISPT cfunction = NIL;
  LISPT closed = NIL;
  LISPT cvalues = NIL;
  std::uint8_t count = 0;
};

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
class cvariable
{
public:
  explicit cvariable(LISPT value): _value(value) {}
  cvariable(): _value(NIL) {}
  ~cvariable() = default;
  cvariable(const cvariable& other) = delete;
  cvariable(cvariable&& other) noexcept
  {
    _value = std::move(other._value);
  }
  cvariable& operator=(cvariable&& other) noexcept
  {
    std::swap(_value, other._value);
    return *this;
  }
  cvariable& operator=(const cvariable& other) = delete;
  cvariable& operator=(LISPT value)
  {
    _value = value;
    return *this;
  }

  /// @brief Automatically convert to the LISPT value in a LISPT context.
  operator LISPT () const noexcept { return _value; }
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

class lisp_t
{
public:
  lisp_t() = default;
  ~lisp_t() = default;
  lisp_t(const lisp_t&) = delete;

  void set() { _u = {}; }
  auto symbol() -> symbol::symbol_t& { return symbol_collection().get(std::get<symbol::print_name>(_u)); }
  void set(const symbol::symbol_t& sym) { _type = type::SYMBOL; _u = sym.pname; }
  auto symvalue() const -> LISPT { return symbol_collection().get(std::get<symbol::print_name>(_u)).value; }
  void symvalue(LISPT x) { symbol_collection().get(std::get<symbol::print_name>(_u)).value = x; }
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
  auto consval() const -> const cons_t& { return std::get<cons_t>(_u); }
  void set(cons_t x) { _type = type::CONS; _u = x; }
  auto car() const -> LISPT { return std::get<cons_t>(_u).car; }
  auto cdr() const -> LISPT { return std::get<cons_t>(_u).cdr; }
  void car(LISPT x) { std::get<cons_t>(_u).car = x; }
  void cdr(LISPT x) { std::get<cons_t>(_u).cdr = x; }
  auto stringval() const -> const std::string& { return std::get<std::string>(_u); }
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
  auto lamval() -> lambda_t& { return std::get<lambda_t>(_u); }
  void set(lambda_t x, bool lambda) { _type = lambda ? type::LAMBDA : type::NLAMBDA; _u = x; }
  auto closval() -> closure_t& { return std::get<closure_t>(_u); }
  void set(closure_t x) { _type = type::CLOSURE; _u = x; }
  auto envval() -> destblock_t* { return std::get<destblock_t*>(_u); }
  void set(destblock_t* env) { _type = type::ENVIRON; _u = env; }
  auto fileval() -> file_t& { return *std::get<std::shared_ptr<file_t>>(_u).get(); }
  void set(std::shared_ptr<file_t> f) { _type = type::FILET; _u = f; }
  auto cvarval() -> cvariable& { return std::get<cvariable>(_u); }
  void set(cvariable&& x) { _type = type::CVARIABLE; _u = std::move(x); }

  const std::string& getstr() const { return _type == type::STRING ? stringval() : std::get<symbol::print_name>(_u).name; }

  type gettype() const { return _type; }
  void settype(type t) { _type = t; }

  static symbol::symbol_collection& symbol_collection()
  {
    static symbol::symbol_collection& all_symbols = *new symbol::symbol_collection;
    return all_symbols;
  }

private:
  type _type = type::NIL;

  // One entry for each type.  Types that has no, or just one, value are
  // indicated by a comment.
  std::variant<
    std::monostate,             // NIL (0)
    symbol::print_name,         // SYMBOL (1)
    int,                        // INTEGER (2)
    double,                     // FLOAT (3)
    indirect_t,                 // INDIRECT (4)
    cons_t,                     // CONS (5)
    std::string,                // STRING (6)
    subr_index,                 // SUBR (7)
    lambda_t,                   // LAMBDA (8)
    closure_t,                  // CLOSURE (9)
    destblock_t*,               // ENVIRON (10)
    std::shared_ptr<file_t>,    // FILE (11)
    cvariable                   // CVARIABLE (12)
    > _u;
};

inline bool EQ(LISPT x, LISPT y) { return x == y; }
inline type type_of(LISPT a) { return a == nullptr ? type::NIL : a->gettype(); }
inline type type_of(lisp_t& a) { return a.gettype(); }
inline bool is_T(LISPT x) { return type_of(x) == type::T; }
inline bool is_NIL(LISPT x) { return type_of(x) == type::NIL; }

enum class char_class
{
  NONE = 0,
  SEPR,                         // seperator
  BRK,                          // break character
  CTRL,                         // control character - escaped when printed
  INSERT,                       // insert read macro
  SPLICE,                       // splice read macro
  INFIX                         // infix read macro
};

struct rtinfo
{
  enum char_class chclass[128];
  using rmacro_t = LISPT (*)(lisp&, file_t&, LISPT, char);
  rmacro_t rmacros[128];
};

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
  static LISPT envget(lisp& l, LISPT a, LISPT b);

  static LISPT cons(lisp& l, LISPT a, LISPT b);
  static LISPT reclaim(lisp& l, LISPT a);
  static LISPT obarray(lisp& l);
  static LISPT freecount(lisp& l);

  file_t& primout() const { return *_primout; }
  file_t& primerr() const { return *_primerr; }
  file_t& primin() const { return *_primin; }
  void primout(std::unique_ptr<file_t>);
  void primerr(std::unique_ptr<file_t>);
  void primin(std::unique_ptr<file_t>);
  file_t& stdout() const { return *_stdout; }
  file_t& stderr() const { return *_stderr; }
  file_t& stdin() const { return *_stdin; }

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

  cvariable& currentbase() const { return _variables->_currentbase; }
  cvariable& verbose() const { return _variables->_verbose; }
  cvariable& version() const { return _variables->_version; }

  // clang-format off
  rtinfo currentrt =
  {
    {
      // NUL SOH STX ETX
      char_class::CTRL, char_class::CTRL, char_class::CTRL, char_class::CTRL,
      // EOT ENQ ACK BEL
      char_class::CTRL, char_class::CTRL, char_class::CTRL, char_class::CTRL,
      // BS  HT  NL  VT
      char_class::CTRL, char_class::SEPR, char_class::SEPR, char_class::CTRL,
      // NP  CR  SO  SI
      char_class::CTRL, char_class::CTRL, char_class::CTRL, char_class::CTRL,
      // DLE DC1 DC2 DC3
      char_class::CTRL, char_class::CTRL, char_class::CTRL, char_class::CTRL,
      // DC4 NAK SYN ETB
      char_class::CTRL, char_class::CTRL, char_class::CTRL, char_class::CTRL,
      // CAN EM SUB ESC
      char_class::CTRL, char_class::CTRL, char_class::CTRL, char_class::CTRL,
      // FS GS RS US
      char_class::CTRL, char_class::CTRL, char_class::CTRL, char_class::CTRL,
      // SP  !   "   #
      char_class::SEPR, char_class::/*SPLICE*/NONE, char_class::/*INSERT*/NONE, char_class::NONE,
      // $   %   &   '
      char_class::NONE, char_class::NONE, char_class::BRK, char_class::/*INSERT*/NONE,
      // (   )   *   +
      char_class::BRK, char_class::BRK, char_class::NONE, char_class::NONE,
      // ,   -   .   /
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // 0   1   2   3
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // 4   5   6   7
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // 8   9   :   ;
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // <   =   >   ?
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // @   A   B   C
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // D   E   F   G
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // H   I   J   K
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // L   M   N   O
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // P   Q   R   S
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // T   U   V   W
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // X   Y   Z   [
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // \   ]   ^   _
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // `   a   b   c
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // d   e   f   g
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // h   i   j   k
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // l   m   n   o
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // p   q   r   s
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // t   u   v   w
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // x   y   z   {
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::NONE,
      // |   }   ~   DEL
      char_class::NONE, char_class::NONE, char_class::NONE, char_class::CTRL
    },
    {
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, /*io::rmexcl*/0, /*io::rmdquote*/0, 0, 0, 0, 0, /*io::rmsquote*/0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0
    }
  };
  // clang-format on
  void set_read_table(unsigned char c, enum char_class chcls, rtinfo::rmacro_t macro)
  {
    currentrt.chclass[static_cast<int>(c)] = chcls;
    currentrt.rmacros[static_cast<int>(c)] = macro;
  }

private:
  alloc& _alloc;
  evaluator& _eval;
  std::unique_ptr<file_t> _primout;
  std::unique_ptr<file_t> _primerr;
  std::unique_ptr<file_t> _primin;
  std::unique_ptr<file_t> _stdout;
  std::unique_ptr<file_t> _stderr;
  std::unique_ptr<file_t> _stdin;
  static lisp* _current;

  class cvariables
  {
  public:
    cvariables(alloc&);
    cvariable& _currentbase;
    cvariable& _verbose;
    cvariable& _version;
  };
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
inline LISPT envget(lisp& l, LISPT a, LISPT b) { return lisp::envget(l, a, b); }
inline LISPT envget(LISPT a, LISPT b) { return lisp::envget(lisp::current(), a, b); }

inline LISPT cons(lisp& l, LISPT a, LISPT b) { return lisp::cons(l, a, b); }
inline LISPT cons(LISPT a, LISPT b) { return lisp::cons(lisp::current(), a, b); }
inline LISPT reclaim(lisp& l, LISPT a) { return lisp::reclaim(l, a); }
inline LISPT reclaim(LISPT a) { return lisp::reclaim(lisp::current(), a); }
inline LISPT obarray(lisp& l) { return lisp::obarray(l); }
inline LISPT obarray() { return lisp::obarray(lisp::current()); }
inline LISPT freecount(lisp& l) { return lisp::freecount(l); }
inline LISPT freecount() { return lisp::freecount(lisp::current()); }

inline void check(LISPT arg, type type)
{
  if(type_of(arg) != type)
    error(NOT_A | to_underlying(type), arg);
}

inline void check(LISPT arg, type type0, type type1)
{
  if(type_of(arg) != type0 && type_of(arg) != type1)
    error(ILLEGAL_ARG, arg);
}

inline void check(LISPT arg, type type0, type type1, type type2)
{
  if(type_of(arg) != type0 && type_of(arg) != type1 && type_of(arg) != type2)
    error(ILLEGAL_ARG, arg);
}

inline void break_flag(lisp& l, bool val) { l.brkflg = val; }
inline void break_flag(bool val) { lisp::current().brkflg = val; }

} // namespace lisp

#endif
