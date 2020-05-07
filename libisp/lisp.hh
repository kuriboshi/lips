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
struct lisp_t;
struct file_t;
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
  HASHTAB,   /* contains hased data table */
  CVARIABLE, /* is a pointer to c-variable */
  CPOINTER,  /* general c pointer */
  USER,      /* user defined type */
};

inline constexpr auto C_NIL = nullptr;

using BITS32 = int;

// The cons cell
struct cons_t
{
  LISPT car;
  LISPT cdr;
};

struct symbol_t
{
  const char* pname; /* The printname of the atom */
  LISPT value;
  LISPT plist;  /* The property list */
  LISPT topval; /* Holds top value (not used yet) */
};

struct subr_t
{
  // The type of internal c-functions
  LISPT (*function0)(lisp&);
  LISPT (*function1)(lisp&, LISPT);
  LISPT (*function2)(lisp&, LISPT, LISPT);
  LISPT (*function3)(lisp&, LISPT, LISPT, LISPT);
  short argcount; // Negative argcount indicates that arguments should not be
                  // evaluated
};

struct lambda_t
{
  LISPT lambdarep;
  LISPT arglist;
  short argcnt;
};

struct closure_t
{
  LISPT cfunction;
  LISPT closed;
  LISPT cvalues;
  short count;
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

struct lisp_t
{
  lisp_t() {}
  ~lisp_t() {}
  lisp_t(const lisp_t&) = delete;

  bool gcmark = false;
  enum lisp_type type = NIL;
  union
  {
    // One entry for each type.  Types that has no, or just one value are
    // indicated by a comment.

    // NIL
    symbol_t l_symbol;
    int l_integer;
    int* l_bignum;
    double l_float;
    LISPT l_indirect;
    cons_t l_cons;
    char* l_string;
    subr_t l_subr;
    lambda_t l_lambda;
    closure_t l_closure;
    // UNBOUND
    destblock_t* l_environ;
    file_t* l_filet;
    // TRUE
    LISPT l_free;
    // ENDOFFILE
    // ERROR
    // HASHTAB
    LISPT* l_cvariable;
    void* l_cpointer;
    // USER
  } u;
  symbol_t& symval() { return u.l_symbol; }
  LISPT& freeval() { return u.l_free; }
  LISPT* cvarval() { return u.l_cvariable; }
  void cvarval(LISPT* x) { u.l_cvariable = x; }
  LISPT& indirectval() { return u.l_indirect; }
  LISPT car() { return u.l_cons.car; }
  LISPT cdr() { return u.l_cons.cdr; }
  void car(LISPT x) { u.l_cons.car = x; }
  void cdr(LISPT x) { u.l_cons.cdr = x; }
  lambda_t& lamval() { return u.l_lambda; }
  closure_t& closval() { return u.l_closure; }
  subr_t& subrval() { return u.l_subr; }
  LISPT symvalue() { return u.l_symbol.value; }
  void symvalue(LISPT x) { u.l_symbol.value = x; }
  const char* stringval() { return u.l_string; }
  void stringval(char* s)
  {
    u.l_string = s;
    type = STRING;
  }
  int intval() { return u.l_integer; }
  void intval(int x)
  {
    u.l_integer = x;
    type = INTEGER;
  }
  double floatval() { return u.l_float; }
  void floatval(double f)
  {
    u.l_float = f;
    type = FLOAT;
  }
  cons_t& consval() { return u.l_cons; }
  file_t* fileval() { return u.l_filet; }
  void fileval(file_t* f) { u.l_filet = f; }
  void* cpointval() { return u.l_cpointer; }
  void setq(LISPT y) { u.l_symbol.value = y; }
  void setopval(LISPT y) { setq(y); }
  LISPT getopval() { return symvalue(); }
  const char* getstr() { return type == STRING ? stringval() : symval().pname; }
  destblock_t* envval() { return u.l_environ; }
  void envval(destblock_t* env) { u.l_environ = env; }

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
