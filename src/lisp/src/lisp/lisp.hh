//
// Lips, lisp shell.
// Copyright 1989, 2020 Krister Joas
//

#pragma once

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

namespace lisp
{
class lisp_t;
using LISPT = std::shared_ptr<lisp_t>;
inline constexpr auto NIL = nullptr;
extern LISPT C_UNBOUND;
}

#include "error.hh"
#include "symbol.hh"

namespace lisp
{
class lisp;
class evaluator;
class alloc;
class file_t;

//class lisp_t;
//using LISPT = std::shared_ptr<lisp_t>;

//
// All lisp constants used internally.
//
extern LISPT T;
extern LISPT C_APPEND;
extern LISPT C_AUTOLOAD;
extern LISPT C_BIGNUM;
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

// This is used to recognize c-functions for cpprint.
using PRIMITIVE = LISPT;

template<typename Enum>
constexpr auto to_underlying(Enum e) noexcept
{
  return static_cast<std::underlying_type_t<Enum>>(e);
}

enum class type
{
  NIL = 0,   // so that nullptr also becomes NIL
  SYMBOL,    // an atomic symbol
  INTEGER,   // 24 bit integer in same word
  BIGNUM,    // bigger than longs (NYI)
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
  T,         // the truth object
  FREE,      // an object on the freelist, used for
             // consistency checks
  ENDOFFILE, // returned from read at end of file
  ERROR,     // returned from primitive when an error
             // occured
  HASHTAB,   // contains hashed data table
  CVARIABLE, // is a pointer to c-variable
  USER,      // user defined type
  SYMBOL2
};

//inline constexpr auto NIL = nullptr;

// The cons cell
struct cons_t
{
  LISPT car = NIL;
  LISPT cdr = NIL;
};

struct symbol_t
{
  std::string pname;     // The printname of the atom
  bool constant = false; // If true this is a constant which can't be set
  LISPT value = NIL;     // Value
  LISPT plist = NIL;     // The property list
  LISPT topval = NIL;    // Holds top value (not used yet)
};

struct subr_t
{
  enum class subr {
    EVAL,
    NOEVAL
  } subr = subr::EVAL;
  enum class spread {
    SPREAD,
    NOSPREAD
  } spread = spread::SPREAD;

  using func0_t = LISPT (*)(lisp&);
  using func1_t = LISPT (*)(lisp&, LISPT);
  using func2_t = LISPT (*)(lisp&, LISPT, LISPT);
  using func3_t = LISPT (*)(lisp&, LISPT, LISPT, LISPT);

  subr_t(enum subr subr, enum spread spread, func0_t fun) : subr(subr), spread(spread), f(fun) {}
  subr_t(enum subr subr, enum spread spread, func1_t fun) : subr(subr), spread(spread), f(fun) {}
  subr_t(enum subr subr, enum spread spread, func2_t fun) : subr(subr), spread(spread), f(fun) {}
  subr_t(enum subr subr, enum spread spread, func3_t fun) : subr(subr), spread(spread), f(fun) {}
  constexpr std::size_t argcount() const noexcept { return f.index() - 1; }

  LISPT operator()(lisp& l) const { return std::get<func0_t>(f)(l); }
  LISPT operator()(lisp& l, LISPT a) const { return std::get<func1_t>(f)(l, a); }
  LISPT operator()(lisp& l, LISPT a, LISPT b) const { return std::get<func2_t>(f)(l, a, b); }
  LISPT operator()(lisp& l, LISPT a, LISPT b, LISPT c) const { return std::get<func3_t>(f)(l, a, b, c); }

  std::variant<std::monostate, func0_t, func1_t, func2_t, func3_t> f;
};

struct lambda_t
{
  LISPT lambdarep = NIL;
  LISPT arglist = NIL;
  short argcnt = 0;
};

struct closure_t
{
  LISPT cfunction = NIL;
  LISPT closed = NIL;
  LISPT cvalues = NIL;
  short count = 0;
};

struct cvariable_t
{
  LISPT value;
};

struct indirect_t
{
  LISPT value;
};

struct destblock_t;

class lisp_t
{
public:
  lisp_t() = default;
  ~lisp_t() = default;
  lisp_t(const lisp_t&) = delete;

  void setnil() { u = {}; }
  auto symbol() -> symbol_t& { return std::get<symbol_t>(u); }
  auto symbol(symbol_t x) -> void { _type = type::SYMBOL; u = x; }
  auto symvalue() const -> LISPT { return std::get<symbol_t>(u).value; }
  auto symvalue(LISPT x) -> void { std::get<symbol_t>(u).value = x; }

  // Symbol 2
  auto symbol2() -> symbol::symbol_t& { return symbol_collection().get(std::get<symbol::print_name>(u)); }
  // auto symbol2(symbol::symbol_t x) -> void { _type = type::SYMBOL2; u = symbol_collection().get(); }
  auto symvalue2() const -> LISPT { return symbol_collection().get(std::get<symbol::print_name>(u)).value; }
  auto symvalue2(LISPT x) -> void { symbol_collection().get(std::get<symbol::print_name>(u)).value = x; }

  auto intval() const -> int { return std::get<int>(u); }
  auto intval(int x) -> void
  {
    _type = type::INTEGER;
    u = x;
  }
  auto floatval() const -> double { return std::get<double>(u); }
  auto floatval(double f) -> void
  {
    _type = type::FLOAT;
    u = f;
  }
  auto indirectval() const -> LISPT { return std::get<indirect_t>(u).value; }
  void indirectval(LISPT x) { _type = type::INDIRECT; u = indirect_t{x}; }
  auto consval() const -> const cons_t& { return std::get<cons_t>(u); }
  void consval(cons_t x) { _type = type::CONS; u = x; }
  auto car() const -> LISPT { return std::get<cons_t>(u).car; }
  auto cdr() const -> LISPT { return std::get<cons_t>(u).cdr; }
  void car(LISPT x) { std::get<cons_t>(u).car = x; }
  void cdr(LISPT x) { std::get<cons_t>(u).cdr = x; }
  auto stringval() const -> const std::string& { return std::get<std::string>(u); }
  void stringval(const std::string& s)
  {
    _type = type::STRING;
    u = s;
  }
  auto subrval() const -> const subr_t& { return std::get<subr_t>(u); }
  void subrval(subr_t x) { _type = type::SUBR; u = x; }
  auto lamval() -> lambda_t& { return std::get<lambda_t>(u); }
  void lamval(lambda_t x) { _type = type::LAMBDA; u = x; }
  void nlamval(lambda_t x) { _type = type::NLAMBDA; u = x; }
  auto closval() -> closure_t& { return std::get<closure_t>(u); }
  void closval(closure_t x) { _type = type::CLOSURE; u = x; }
  auto envval() -> destblock_t* { return std::get<destblock_t*>(u); }
  void envval(destblock_t* env) { _type = type::ENVIRON; u = env; }
  auto fileval() -> file_t& { return *std::get<std::unique_ptr<file_t>>(u).get(); }
  void fileval(std::unique_ptr<file_t> f) { _type = type::FILET; u = std::move(f); }
  auto cvarval() -> LISPT { return std::get<cvariable_t>(u).value; }
  void cvarval(LISPT x) { _type = type::CVARIABLE; u.emplace<cvariable_t>(cvariable_t{x}); }

  const std::string& getstr() const { return _type == type::STRING ? stringval() : std::get<symbol_t>(u).pname; }

  //
  // Some more or less helpful functions
  //
  type gettype() const { return _type; }
  void settype(type t) { _type = t; }
  static symbol::symbol_collection& symbol_collection()
  {
    static symbol::symbol_collection& all_symbols = *new symbol::symbol_collection;
    return all_symbols;
  }

private:
  type _type = type::NIL;
  // One entry for each type.  Types that has no, or just one value are
  // indicated by a comment.
  std::variant<
    std::monostate,             // NIL (0)
    symbol_t,                   // SYMBOL (1)
    int,                        // INTEGER (2)
    double,                     // FLOAT (3)
    indirect_t,                 // INDIRECT (4)
    cons_t,                     // CONS (5)
    std::string,                // STRING (6)
    subr_t,                     // SUBR (7)
    lambda_t,                   // LAMBDA (8)
    closure_t,                  // CLOSURE (9)
    destblock_t*,               // ENVIRON (10)
    std::unique_ptr<file_t>,    // FILE (11)
    cvariable_t,                // CVARIABLE (12)
    symbol::print_name
    > u;
};

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

inline bool EQ(LISPT x, LISPT y) { return x == y; }
inline type type_of(LISPT a) { return a == nullptr ? type::NIL : a->gettype(); }
inline type type_of(lisp_t& a) { return a.gettype(); }

inline void set(LISPT& a, type t, LISPT p)
{
  a = p;
  a->settype(t);
}

inline bool is_T(LISPT x) { return type_of(x) == type::T; }
inline bool is_NIL(LISPT x) { return type_of(x) == type::NIL; }

class lisp
{
public:
  lisp();
  ~lisp();
  alloc& a() const { return _alloc; }
  evaluator& e() const { return _eval; };
  static lisp& current() { return *_current; }
  static void current(lisp& lisp) { _current = &lisp; }

  file_t& primout() const { return *_primout; }
  file_t& primerr() const { return *_primerr; }
  file_t& primin() const { return *_primin; }
  void primout(std::unique_ptr<file_t>);
  void primerr(std::unique_ptr<file_t>);
  void primin(std::unique_ptr<file_t>);
  file_t& stdout() const { return *_stdout; }
  file_t& stderr() const { return *_stderr; }
  file_t& stdin() const { return *_stdin; }

  LISPT perror(int, LISPT);
  LISPT error(int, LISPT);
  LISPT syserr(LISPT);
  LISPT break0(LISPT);

  void check(LISPT arg, type type)
  {
    if(type_of(arg) != type)
      error(NOT_A | to_underlying(type), arg);
  }

  void check(LISPT arg, type type0, type type1)
  {
    if(type_of(arg) != type0 && type_of(arg) != type1)
      error(ILLEGAL_ARG, arg);
  }

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

  LISPT currentbase = NIL;
  LISPT topprompt = NIL;
  LISPT brkprompt = NIL;
  LISPT verbose = NIL;
  LISPT version = NIL;

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

  std::map<int, std::string> messages;
  // Some standard messages, all of them not necessarily used
  // clang-format off
  const std::vector<std::string> errmess = {
    "Not NIL",
    "Not a symbol",
    "Not an integer",
    "Not a bignum",
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
    "Not T",
    "Not free",
    "Not EOF",
    "Not an ERROR",
    "Not a hash table"
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

inline void check(lisp& l, LISPT arg, type type) { l.check(arg, type); }
inline void check(LISPT arg, type type) { check(lisp::current(), arg, type); }
inline void check(lisp& l, LISPT arg, type type0, type type1) { l.check(arg, type0, type1); }
inline void check(LISPT arg, type type0, type type1) { check(lisp::current(), arg, type0, type1); }
inline void break_flag(lisp& l, bool val) { l.brkflg = val; }
inline void break_flag(bool val) { lisp::current().brkflg = val; }

} // namespace lisp
