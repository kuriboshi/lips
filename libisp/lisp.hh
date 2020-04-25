/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 * $Id$
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

#define C_NIL nullptr
extern LISPT C_T;

using BITS32 = int;

typedef struct
{ /* The cons cell */
  LISPT car;
  LISPT cdr;
} CONST;

typedef struct
{
  const char* pname; /* The printname of the atom */
  LISPT value;
  LISPT plist;  /* The property list */
  LISPT topval; /* Holds top value (not used yet) */
} SYMBOLT;

typedef struct
{
  // The type of internal c-functions
  LISPT (*function0)(void);
  LISPT (*function1)(LISPT);
  LISPT (*function2)(LISPT, LISPT);
  LISPT (*function3)(LISPT, LISPT, LISPT);
  short argcount; // Negative argcount indicates that arguments should not be
                  // evaluated
} SUBRT;

typedef struct
{
  LISPT lambdarep;
  LISPT arglist;
  short argcnt;
} LAMBDAT;

typedef struct
{
  LISPT cfunction;
  LISPT closed;
  LISPT cvalues;
  short count;
} CLOSURET;

struct lisp_t
{
  bool gcmark = false;
  enum lisp_type type;
  union
  {
    // One entry for each type.  Types that has no, or just one value are
    // indicated by a comment.
    /* NIL */
    SYMBOLT l_symbol;
    int l_integer;
    int* l_bignum;
    double l_float;
    LISPT l_indirect;
    CONST l_cons;
    char* l_string;
    SUBRT l_subr;
    LAMBDAT l_lambda;
    CLOSURET l_closure;
    /* UNBOUND */
    // alloc::destblock_t* l_environ;
    FILE* l_filet;
    /* TRUE */
    LISPT l_free;
    /* ENDOFFILE */
    /* ERROR */
    /* HASHTAB */
    LISPT* l_cvariable;
    void* l_cpointer;
    /* USER */
  } u;
};

/*
 * Some more or less helpfull functions
 */
inline lisp_type TYPEOF(LISPT a) { return a == nullptr ? NIL : a->type; }
inline void SETTYPE(LISPT a, lisp_type t) { a->type = t; }
inline bool MARKED(LISPT a) { return a->gcmark; }
inline void MARK(LISPT a) { a->gcmark = true; }
inline void UNMARK(LISPT a) { a->gcmark = false; }

inline char*& STRINGVAL(LISPT s) { return s->u.l_string; }
inline int& INTVAL(LISPT i) { return i->u.l_integer; }
inline SYMBOLT& SYMVAL(LISPT s) { return s->u.l_symbol; }
inline CONST& CONSVAL(LISPT c) { return c->u.l_cons; }
inline SUBRT& SUBRVAL(LISPT s) { return s->u.l_subr; }
inline LAMBDAT& LAMVAL(LISPT l) { return l->u.l_lambda; }
inline CLOSURET& CLOSVAL(LISPT c) { return c->u.l_closure; }
inline double& FLOATVAL(LISPT f) { return f->u.l_float; }
inline FILE*& FILEVAL(LISPT f) { return f->u.l_filet; }
inline LISPT& INDIRECTVAL(LISPT i) { return i->u.l_indirect; }
inline LISPT*& CVARVAL(LISPT v) { return v->u.l_cvariable; }
inline void*& CPOINTVAL(LISPT c) { return c->u.l_cpointer; }
inline LISPT& FREEVAL(LISPT f) { return f->u.l_free; }

#if 0
inline alloc::destblock_t*& ENVVAL(LISPT e) { return e->u.l_environ; }
#endif

inline void SET(LISPT& a, lisp_type t, LISPT p) { a = p; a->type = t; UNMARK(a); }

inline bool IST(LISPT x) { return TYPEOF(x) == TRUE; }
inline bool ISNIL(LISPT x) { return x == nullptr || TYPEOF(x) == NIL; }

inline LISPT& CAR(LISPT x) { return CONSVAL(x).car; }
inline LISPT& CDR(LISPT x) { return CONSVAL(x).cdr; }
inline bool EQ(LISPT x, LISPT y) { return x == y; }
inline void SETQ(LISPT x, LISPT y) { SYMVAL(x).value = y; }
inline LISPT& SYMVALUE(LISPT x) { return SYMVAL(x).value; }
inline void SETOPVAL(LISPT x, LISPT y) { SETQ(x, y); }
inline LISPT GETOPVAL(LISPT x) { return SYMVAL(x).value; }
inline const char* GETSTR(LISPT s) { return TYPEOF(s) == STRING ? STRINGVAL(s) : SYMVAL(s).pname; }

}
