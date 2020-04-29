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

namespace lisp
{
struct lisp_t;
using LISPT = struct lisp_t*;
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
extern LISPT C_T;

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
  LISPT (*function0)();
  LISPT (*function1)(LISPT);
  LISPT (*function2)(LISPT, LISPT);
  LISPT (*function3)(LISPT, LISPT, LISPT);
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
    // alloc::destblock_t* l_environ;
    FILE* l_filet;
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
  FILE* fileval() { return u.l_filet; }
  void fileval(FILE* f) { u.l_filet = f; }
  void* cpointval() { return u.l_cpointer; }
  void setq(LISPT y) { u.l_symbol.value = y; }
  void setopval(LISPT y) { setq(y); }
  LISPT getopval() { return symvalue(); }
  const char* getstr() { return type == STRING ? stringval() : symval().pname; }

  /*
   * Some more or less helpfull functions
   */
  void settype(lisp_type t) { type = t; }
  bool marked() { return gcmark; }
  void mark() { gcmark = true; }
  void unmark() { gcmark = false; }
};

inline bool EQ(LISPT x, LISPT y) { return x == y; }
inline lisp_type type_of(LISPT a) { return a == nullptr ? NIL : a->type; }

#if 0
inline alloc::destblock_t* ENVVAL(LISPT e) { return e->u.l_environ; }
#endif

inline void SET(LISPT& a, lisp_type t, LISPT p)
{
  a = p;
  a->type = t;
  a->unmark();
}

inline bool is_T(LISPT x) { return type_of(x) == TRUE; }
inline bool is_NIL(LISPT x) { return type_of(x) == NIL; }

} // namespace lisp
