//
// Lips, lisp shell.
// Copyright 1989, 2020 Krister Joas
//

#pragma once

//
// This header file is private to the libisp libray.  Applications using
// libisp should only include libisp.hh.
//

#include <map>
#include <memory>
#include <string>
#include <variant>
#include <vector>
#include "error.hh"

namespace lisp
{
class lisp;
class evaluator;
class alloc;
class file_t;

struct lisp_t;
using LISPT = struct lisp_t*;

//
// All lisp constants used internally.
//
extern LISPT C_T;
extern LISPT CE_NIL;
extern LISPT CE_T;
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
extern LISPT C_UNBOUND;
extern LISPT C_WRITE;

// This is used to recognize c-functions for cpprint.
using PRIMITIVE = LISPT;

template<typename Enum>
constexpr auto to_underlying(Enum e) noexcept
{
  return static_cast<std::underlying_type_t<Enum>>(e);
}

enum class lisp_type
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
  CPOINTER,  // general c pointer
  USER,      // user defined type
};

inline constexpr auto C_NIL = nullptr;

// The cons cell
struct cons_t
{
  LISPT car = C_NIL;
  LISPT cdr = C_NIL;
};

struct symbol_t
{
  std::string pname;  // The printname of the atom
  LISPT value = C_NIL;
  LISPT plist = C_NIL;          // The property list
  LISPT topval = C_NIL;         // Holds top value (not used yet)
};

using func0_t = LISPT (*)(lisp&);
using func1_t = LISPT (*)(lisp&, LISPT);
using func2_t = LISPT (*)(lisp&, LISPT, LISPT);
using func3_t = LISPT (*)(lisp&, LISPT, LISPT, LISPT);

struct subr_t
{
  enum subr_type {
    S_EVAL,
    S_NOEVAL
  };
  enum spread_type {
    S_SPREAD,
    S_NOSPREAD
  };

  subr_t(subr_type subr, spread_type spread) : subr(subr), spread(spread) {}
  constexpr std::size_t argcount() const noexcept { return f.index() - 1; }

  std::variant<std::monostate, func0_t, func1_t, func2_t, func3_t> f;
  subr_type subr;
  spread_type spread;
};

struct lambda_t
{
  LISPT lambdarep = C_NIL;
  LISPT arglist = C_NIL;
  short argcnt = 0;
};

struct closure_t
{
  LISPT cfunction = C_NIL;
  LISPT closed = C_NIL;
  LISPT cvalues = C_NIL;
  short count = 0;
};

enum class block_type
{
  EMPTY = 0,
  LISPT,
  ENVIRON
};

struct destblock_t
{
  block_type type = block_type::EMPTY;
  union
  {
    LISPT d_lisp;
    int d_integer;
    destblock_t* d_environ;
  } var, val;
};

using indirect_t = LISPT;
using free_t = LISPT;
using cvariable_t = LISPT*;

struct lisp_t
{
  lisp_t() = default;
  ~lisp_t() = default;
  lisp_t(const lisp_t&) = delete;

  bool gcmark = false;
  enum lisp_type type = lisp_type::NIL;
  lisp* interpreter = nullptr;

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
    subr_t*,                    // SUBR (7)
    lambda_t,                   // LAMBDA (8)
    closure_t,                  // CLOSURE (9)
    destblock_t*,               // ENVIRON (10)
    std::unique_ptr<file_t>,    // FILE (11)
    free_t,                     // FREE (12)
    cvariable_t,                // CVARIABLE (13)
    void*                       // CPOINTER (14)
    > u;
  symbol_t& symval() { return std::get<symbol_t>(u); }
  void symval(symbol_t x) { type = lisp_type::SYMBOL; u = x; }
  LISPT symvalue() { return std::get<symbol_t>(u).value; }
  void symvalue(LISPT x) { std::get<symbol_t>(u).value = x; }
  void setq(LISPT y) { std::get<symbol_t>(u).value = y; }
  void setopval(LISPT y) { setq(y); }
  LISPT getopval() { return symvalue(); }
  int intval() { return std::get<int>(u); }
  void intval(int x)
  {
    type = lisp_type::INTEGER;
    u = x;
  }
  double floatval() { return std::get<double>(u); }
  void floatval(double f)
  {
    type = lisp_type::FLOAT;
    u = f;
  }
  indirect_t& indirectval() { return std::get<4>(u); }
  cons_t& consval() { return std::get<cons_t>(u); }
  void consval(cons_t x) { type = lisp_type::CONS; u = x; }
  LISPT car() { return std::get<cons_t>(u).car; }
  LISPT cdr() { return std::get<cons_t>(u).cdr; }
  void car(LISPT x) { std::get<cons_t>(u).car = x; }
  void cdr(LISPT x) { std::get<cons_t>(u).cdr = x; }
  const std::string& stringval() const { return std::get<std::string>(u); }
  void stringval(const std::string& s)
  {
    type = lisp_type::STRING;
    u = s;
  }
  subr_t& subrval() { return *std::get<subr_t*>(u); }
  void subrval(subr_t* x) { type = lisp_type::SUBR; u = x; }
  lambda_t& lamval() { return std::get<lambda_t>(u); }
  void lamval(lambda_t x) { type = lisp_type::LAMBDA; u = x; }
  void nlamval(lambda_t x) { type = lisp_type::NLAMBDA; u = x; }
  closure_t& closval() { return std::get<closure_t>(u); }
  destblock_t* envval() { return std::get<destblock_t*>(u); }
  void envval(destblock_t* env) { type = lisp_type::ENVIRON; u = env; }
  file_t& fileval() { return *std::get<std::unique_ptr<file_t>>(u).get(); }
  void fileval(std::unique_ptr<file_t> f) { type = lisp_type::FILET; u = std::move(f); }
  free_t& freeval() { return std::get<12>(u); }
  void freeval(LISPT x) { type = lisp_type::FREE; u.emplace<12>(x); }
  cvariable_t cvarval() const { return std::get<cvariable_t>(u); }
  void cvarval(cvariable_t x) { u.emplace<cvariable_t>(x); }
  void* cpointval() { return std::get<void*>(u); }

  const std::string& getstr() const { return type == lisp_type::STRING ? stringval() : std::get<symbol_t>(u).pname; }

  //
  // Some more or less helpfull functions
  //
  void settype(lisp_type t) { type = t; }
  bool marked() { return gcmark; }
  void mark() { gcmark = true; }
  void unmark() { gcmark = false; }
};

enum class char_class
{
  NONE = 0,
  SEPR,                         // seperator
  BRK,                          // break character
  CTRL,                         // controll character - escaped when printed
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
inline lisp_type type_of(LISPT a) { return a == nullptr ? lisp_type::NIL : a->type; }

inline void set(LISPT& a, lisp_type t, LISPT p)
{
  a = p;
  a->type = t;
  a->unmark();
}

inline bool is_T(LISPT x) { return type_of(x) == lisp_type::T; }
inline bool is_NIL(LISPT x) { return type_of(x) == lisp_type::NIL; }

class lisp
{
public:
  lisp();
  ~lisp();
  alloc& a() const { return _alloc; }
  evaluator& e() const { return _eval; };
  static lisp& current() { return *_current; }

  file_t& primout() const { return *_primout; }
  file_t& primerr() const { return *_primerr; }
  file_t& primin() const { return *_primin; }
  void primout(file_t&);
  void primerr(file_t&);
  void primin(file_t&);
  file_t& stdout() const { return *_stdout; }
  file_t& stderr() const { return *_stderr; }
  file_t& stdin() const { return *_stdin; }

  LISPT perror(int, LISPT);
  LISPT error(int, LISPT);
  LISPT syserr(LISPT);
  LISPT break0(LISPT);

  void check(LISPT arg, lisp_type type)
  {
    if(type_of(arg) != type)
      error(NOT_A | to_underlying(type), arg);
  }

  void check(LISPT arg, lisp_type type0, lisp_type type1)
  {
    if(type_of(arg) != type0 && type_of(arg) != type1)
      error(ILLEGAL_ARG, arg);
  }

  using breakfun_t = int (*)(lisp&, LISPT*);
  void repl(LISPT prompt, breakfun_t f);
  LISPT pexp = nullptr;

  // Used by lisp::io
  LISPT top = nullptr;
  LISPT rstack = nullptr;
  int printlevel = 0;
  int thisplevel = 0;
  bool echoline = false;

  // Used by the interpreter
  bool brkflg = false;
  bool interrupt = false;

  LISPT currentbase = nullptr;
  LISPT topprompt = nullptr;
  LISPT brkprompt = nullptr;
  LISPT verbose = nullptr;
  LISPT version = nullptr;

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
  file_t* _primout = nullptr;
  file_t* _primerr = nullptr;
  file_t* _primin = nullptr;
  file_t* _stdout = nullptr;
  file_t* _stderr = nullptr;
  file_t* _stdin = nullptr;
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

inline LISPT perror(lisp& l, int i, LISPT a) { return l.perror(i, a); }
inline LISPT perror(int i, LISPT a) { return perror(lisp::current(), i, a); }
inline LISPT error(lisp& l, int i, LISPT a) { return l.error(i, a); }
inline LISPT error(int i, LISPT a) { return error(lisp::current(), i, a); }
inline LISPT syserr(lisp& l, LISPT a) { return l.syserr(a); }
inline LISPT syserr(LISPT a) { return syserr(lisp::current(), a); }
inline LISPT break0(lisp& l, LISPT a) { return l.break0(a); }
inline LISPT break0(LISPT a) { return break0(lisp::current(), a); }

inline void check(lisp& l, LISPT arg, lisp_type type) { l.check(arg, type); }
inline void check(LISPT arg, lisp_type type) { check(lisp::current(), arg, type); }
inline void check(lisp& l, LISPT arg, lisp_type type0, lisp_type type1) { l.check(arg, type0, type1); }
inline void check(LISPT arg, lisp_type type0, lisp_type type1) { check(lisp::current(), arg, type0, type1); }
inline void break_flag(lisp& l, bool val) { l.brkflg = val; }
inline void break_flag(bool val) { lisp::current().brkflg = val; }

} // namespace lisp
