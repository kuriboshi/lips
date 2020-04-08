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
#include <stdio.h>
#include <memory.h>
#include "config.h"

#include <libisp.h>

/* This is used to recognize c-functions for cpprint */
#define PRIMITIVE LISPT

enum lisp_type
{
  NIL = 0,   /* so that NULL also becomes NIL */
  SYMBOL,    /* an atomic symbol */
  INTEGER,   /* 24 bit integer in same word */
  BIGNUM,    /* bigger than longs (NYI) */
  FLOAT,     /* a double */
  INDIRECT,  /* used when a value is stored in a closure */
  LONG,      /* a long i.e. 32 bits of integer data (NYI) */
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

/*
 * Some more or less helpfull macros.
 */
#define TYPEOF(a) ((a) == NULL ? NIL : (a)->type)
#define SETTYPE(a, t) ((a)->type = (t))
#define MARKED(a) ((a)->gcmark)
#define MARK(a) ((a)->gcmark = 1)
#define UNMARK(a) ((a)->gcmark = 0)

#define C_NIL NULL
extern LISPT C_T;

#define STRINGVAL(s) ((s)->u.l_string)
#define INTVAL(i) ((i)->u.l_integer)
#define SYMVAL(s) ((s)->u.l_symbol)
#define CONSVAL(c) ((c)->u.l_cons)
#define SUBRVAL(s) ((s)->u.l_subr)
#define LAMVAL(l) ((l)->u.l_lambda)
#define CLOSVAL(c) ((c)->u.l_closure)
#define FLOATVAL(f) ((f)->u.l_float)
#define FILEVAL(f) ((f)->u.l_filet)
#define INDIRECTVAL(i) ((i)->u.l_indirect)
#define CVARVAL(v) ((v)->u.l_cvariable)
#define CPOINTVAL(c) ((c)->u.l_cpointer)
#define FREEVAL(f) ((f)->u.l_free)
#define ENVVAL(e) ((e)->u.l_environ)

#define SET(a, t, p) ((a) = (p), (a)->type = (t), UNMARK(a))

#define ISNIL(x) ((x) == NULL || TYPEOF(x) == NIL)
#define IST(x) (TYPEOF(x) == TRUE)

#define CAR(x) (CONSVAL(x).car)
#define CDR(x) (CONSVAL(x).cdr)
#define EQ(x, y) ((x) == (y))
#define SETQ(x, y) (SYMVAL(x).value = (y))
#define SYMVALUE(x) (SYMVAL(x).value)
#define SETOPVAL(x, y) SETQ(x, y)
#define GETOPVAL(x) (SYMVAL(x).value)
#define GETSTR(s) TYPEOF(s) == STRING ? STRINGVAL(s) : SYMVAL(s).pname

/*
 * A simple way of protecting internal lisp objects from
 * the garbage collector.
 */
#define USESAVE \
  extern LISPT savearray[]; \
  extern int savept;
#define SAVE(v) savearray[savept++] = v;
#define UNSAVE(v) v = savearray[--savept];

#define BITS32 int

typedef struct
{ /* The cons cell */
  LISPT car;
  LISPT cdr;
} CONST;

typedef struct
{
  char* pname; /* The printname of the atom */
  LISPT value;
  LISPT plist;  /* The property list */
  LISPT topval; /* Holds top value (not used yet) */
} SYMBOLT;

typedef struct
{ /* The type of internal c-functions */
  LISPT (*function0)(void);
  LISPT (*function1)(LISPT);
  LISPT (*function2)(LISPT, LISPT);
  LISPT (*function3)(LISPT, LISPT, LISPT);
  short argcount; /* Negative argcount indicates that
				   arguments should not be evaluated */
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

struct lispt
{
  unsigned int gcmark : 1;
  enum lisp_type type;
  union
  { /* One entry for each type.  Types that
				   has no, or just one value are indicated
				   by a comment. */
    /* NIL */
    SYMBOLT l_symbol;
    int l_integer;
    int* l_bignum;
    float l_float;
    LISPT l_indirect;
    CONST l_cons;
    char* l_string;
    SUBRT l_subr;
    LAMBDAT l_lambda;
    CLOSURET l_closure;
    /* UNBOUND */
    struct destblock* l_environ;
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
 * Each hashbucket contains a symbol and a pointer to the next
 * symbol in that bucket.
 */
typedef struct obarr
{
  LISPT sym;
  struct obarr* onext;
} OBARRAY;

/*
 * The control stack.
 */
enum control_type
{
  CTRL_LISP,
  CTRL_FUNC,
  CTRL_POINT,
};

struct control
{
  enum control_type type;
  union
  {
    int (*f_point)(void);
    void* point;
    LISPT lisp;
  } u;
};
#define CTRLBLKSIZE 4000
typedef struct control CONTROL[CTRLBLKSIZE];

struct destblock
{
  char type;
  union
  {
    LISPT d_lisp;
    int d_integer;
    struct destblock* d_environ;
  } var, val;
};

/*
 * Just to avoid lint complaints about pointers returned by
 * `malloc' beeing unaligned.
 */
#ifdef lint
#define safemalloc(x) ((void) realmalloc(x), 0)
#else
#define safemalloc(x) realmalloc(x)
#endif

#define MAXHASH 255 /* The number of hash buckets */

#include "defs.h"
#include "io.h"
#include "constants.h"
#include "error.h"
#include "func.h"
