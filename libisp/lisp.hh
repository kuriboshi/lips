/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 */

#pragma once

/* 
 * This header file is private to the libisp libray.  Applications using
 * libisp should only include libisp.h.
 */

#include <variant>
#include <cstdio>
#include "config.hh"
#include "error.hh"

#ifdef TRUE
#undef TRUE
#endif

namespace lisp
{
class lisp;
class evaluator;
class alloc;
class file_t;

struct lisp_t;
using LISPT = struct lisp_t*;

/*
 * All lisp constants used internally.
 */
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
extern LISPT C_READ;
extern LISPT C_REDEFINED;
extern LISPT C_RESET;
extern LISPT C_RETURN;
extern LISPT C_STRING;
extern LISPT C_SUBR;
extern LISPT C_SYMBOL;
extern LISPT C_UNBOUND;
extern LISPT C_WRITE;

/* This is used to recognize c-functions for cpprint */
using PRIMITIVE = LISPT;

enum lisp_type
{
  NIL = 0,   /* so that nullptr also becomes NIL */
  SYMBOL,    /* an atomic symbol */
  INTEGER,   /* 24 bit integer in same word */
  BIGNUM,    /* bigger than longs (NYI) */
  FLOAT,     /* a double */
  INDIRECT,  /* used when a value is stored in a closure */
  CONS,      /* a pair */
  STRING,    /* character strings */
  SUBR,      /* eval type primitive function */
  FSUBR,     /* noeval */
  LAMBDA,    /* lambda function */
  NLAMBDA,   /* noeval */
  CLOSURE,   /* static binding object */
  UNBOUND,   /* unbound indicator */
  ENVIRON,   /* environment stack type for gc use */
  FILET,     /* file pointer */
  TRUE,      /* the truth object */
  FREE,      /* an object on the freelist, used for
                consistency checks */
  ENDOFFILE, /* returned from read at end of file */
  ERROR,     /* returned from primitive when an error
                occured */
  HASHTAB,   /* contains hashed data table */
  CVARIABLE, /* is a pointer to c-variable */
  CPOINTER,  /* general c pointer */
  USER,      /* user defined type */
};

inline constexpr auto C_NIL = nullptr;

using BITS32 = int;

// The cons cell
struct cons_t
{
  LISPT car = C_NIL;
  LISPT cdr = C_NIL;
};

struct symbol_t
{
  const char* pname = nullptr;  // The printname of the atom
  LISPT value = C_NIL;
  LISPT plist = C_NIL;          // The property list
  LISPT topval = C_NIL;         // Holds top value (not used yet)
};

struct subr_t
{
  // The type of internal c-functions
  LISPT (*function0)(lisp&);
  LISPT (*function1)(lisp&, LISPT);
  LISPT (*function2)(lisp&, LISPT, LISPT);
  LISPT (*function3)(lisp&, LISPT, LISPT, LISPT);
  short argcount = 0; // Negative argcount indicates that arguments should not
                      // be evaluated
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
  lisp_t() {}
  ~lisp_t() {}
  lisp_t(const lisp_t&) = delete;

  bool gcmark = false;
  enum lisp_type type = NIL;
  // One entry for each type.  Types that has no, or just one value are
  // indicated by a comment.
  std::variant<
    std::monostate,             // NIL (0)
    symbol_t,                   // SYMBOL (1)
    int,                        // INTEGER (2)
    double,                     // FLOAT (3)
    indirect_t,                 // INDIRECT (4)
    cons_t,                     // CONS (5)
    char*,                      // STRING (6)
    subr_t,                     // SUBR (7)
    lambda_t,                   // LAMBDA (8)
    closure_t,                  // CLOSURE (9)
    destblock_t*,               // ENVIRON (10)
    file_t*,                    // FILE (11)
    free_t,                     // FREE (12)
    cvariable_t,                // CVARIABLE (13)
    void*                       // CPOINTER (14)
    > u;
  symbol_t& symval() { return std::get<symbol_t>(u); }
  void symval(symbol_t x) { type = SYMBOL; u = x; }
  LISPT symvalue() { return std::get<symbol_t>(u).value; }
  void symvalue(LISPT x) { std::get<symbol_t>(u).value = x; }
  void setq(LISPT y) { std::get<symbol_t>(u).value = y; }
  void setopval(LISPT y) { setq(y); }
  LISPT getopval() { return symvalue(); }
  int intval() { return std::get<int>(u); }
  void intval(int x)
  {
    type = INTEGER;
    u = x;
  }
  double floatval() { return std::get<double>(u); }
  void floatval(double f)
  {
    type = FLOAT;
    u = f;
  }
  indirect_t& indirectval() { return std::get<4>(u); }
  cons_t& consval() { return std::get<cons_t>(u); }
  void consval(cons_t x) { type = CONS; u = x; }
  LISPT car() { return std::get<cons_t>(u).car; }
  LISPT cdr() { return std::get<cons_t>(u).cdr; }
  void car(LISPT x) { std::get<cons_t>(u).car = x; }
  void cdr(LISPT x) { std::get<cons_t>(u).cdr = x; }
  const char* stringval() const { return std::get<char*>(u); }
  void stringval(char* s)
  {
    type = STRING;
    u = s;
  }
  subr_t& subrval() { return std::get<subr_t>(u); }
  void subrval(subr_t x, lisp_type t) { type = t; u = x; }
  lambda_t& lamval() { return std::get<lambda_t>(u); }
  void lamval(lambda_t x) { type = LAMBDA; u = x; }
  closure_t& closval() { return std::get<closure_t>(u); }
  destblock_t* envval() { return std::get<destblock_t*>(u); }
  void envval(destblock_t* env) { type = ENVIRON; u = env; }
  file_t* fileval() { return std::get<file_t*>(u); }
  void fileval(file_t* f) { type = FILET; u = f; }
  free_t& freeval() { return std::get<12>(u); }
  void freeval(LISPT x) { type = FREE; u.emplace<12>(x); }
  cvariable_t cvarval() const { return std::get<cvariable_t>(u); }
  void cvarval(cvariable_t x) { u.emplace<cvariable_t>(x); }
  void* cpointval() { return std::get<void*>(u); }

  const char* getstr() const { return type == STRING ? stringval() : std::get<symbol_t>(u).pname; }

  /*
   * Some more or less helpfull functions
   */
  void settype(lisp_type t) { type = t; }
  bool marked() { return gcmark; }
  void mark() { gcmark = true; }
  void unmark() { gcmark = false; }
};

enum char_class
{
  NONE = 0,
  SEPR = 001,   // seperator
  BRK = 002,    // break character
  INSERT = 004, // insert read macro
  SPLICE = 010, // splice read macro
  INFIX = 014,  // infix read macro
  RMACRO = 014  // read macro mask
};

struct rtinfo
{
  enum char_class chclass[128];
  using rmacro_t = LISPT (*)(lisp&, file_t&, LISPT, char);
  rmacro_t rmacros[128];
};

inline bool EQ(LISPT x, LISPT y) { return x == y; }
inline lisp_type type_of(LISPT a) { return a == nullptr ? NIL : a->type; }

inline void set(LISPT& a, lisp_type t, LISPT p)
{
  a = p;
  a->type = t;
  a->unmark();
}

inline bool is_T(LISPT x) { return type_of(x) == TRUE; }
inline bool is_NIL(LISPT x) { return type_of(x) == NIL; }

class lisp
{
public:
  lisp();
  ~lisp();
  alloc& a() const { return _alloc; }
  evaluator& e() const { return _eval; };

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
      error(NOT_A | type, arg);
  }

  void check2(LISPT arg, lisp_type type0, lisp_type type1)
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

  // Used by the interprete
  bool brkflg = false;
  bool interrupt = false;

  /* clang-format off */
  rtinfo currentrt =
  {
    {
      /* NUL SOH STX ETX EOT ENQ ACK BEL */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* BS  HT  NL  VT  NP  CR  SO  SI  */
      NONE, SEPR, SEPR, NONE, NONE, NONE, NONE, NONE,
      /* DLE DC1 DC2 DC3 DC4 NAK SYN ETB */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* CAN EM  SUB ESC FS  GS  RS  US  */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* SP  !   "   #   $   %   &   '   */
      SEPR, /*SPLICE*/NONE, /*INSERT*/NONE, NONE, NONE, NONE, BRK, /*INSERT*/NONE,
      /* (   )   *   +   ,   -   .   /   */
      BRK, BRK, NONE, NONE, NONE, NONE, NONE, NONE,
      /* 0   1   2   3   4   5   6   7   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* 8   9   :   ;   <   =   >   ?   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* @   A   B   C   D   E   F   G   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* H   I   J   K   L   M   N   O   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* P   Q   R   S   T   U   V   W   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* X   Y   Z   [   \   ]   ^   _   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* `   a   b   c   d   e   f   g   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* h   i   j   k   l   m   n   o   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* p   q   r   s   t   u   v   w   */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE,
      /* x   y   z   {   |   }   ~   DEL */
      NONE, NONE, NONE, NONE, NONE, NONE, NONE, NONE },
    { 0, 0, 0, 0, 0, 0, 0, 0,
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
      0, 0, 0, 0, 0, 0, 0, 0 }
  };
  /* clang-format on */
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
};

inline LISPT perror(lisp& l, int i, LISPT a) { return l.perror(i, a); }
inline LISPT error(lisp& l, int i, LISPT a) { return l.error(i, a); }
inline LISPT syserr(lisp& l, LISPT a) { return l.syserr(a); }
inline LISPT break0(lisp& l, LISPT a) { return l.break0(a); }

inline void check(lisp& l, LISPT arg, lisp_type type) { l.check(arg, type); }
inline void check2(lisp& l, LISPT arg, lisp_type type0, lisp_type type1) { l.check2(arg, type0, type1); }

// Variables
extern LISPT currentbase;
extern LISPT topprompt;
extern LISPT brkprompt;
extern LISPT interactive;
extern LISPT version;

} // namespace lisp
