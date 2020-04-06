/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */
#include <errno.h>
#include "lisp.h"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

extern void toploop();
long trace;

static char *messages[MAXMESSAGE];
/* Some standard messages, all of them not necessarily used */
static char *errmess[] = {
  "Not NIL",            "Not a symbol",
  "Not an integer",     "Not a bignum",
  "Not a float",        "Not indirect",
  "Not a long",         "Not a list",
  "Not a string",       "Not SUBR",
  "Not FSUBR",          "Not LAMBDA",
  "Not NLAMBDA",        "Not a closure",
  "Not unbound",        "Not an environment",
  "Not a file pointer", "Not T",
  "Not free",           "Not EOF",
  "Not an ERROR",       "Not a hash table"
};

PRIMITIVE xobarray()
{
  int i;
  LISPT o;
  OBARRAY *l;

  o = C_NIL;
  for (i=0; i<MAXHASH; i++)
    for (l=obarray[i]; l; l = l->onext)
      o = cons(l->sym,o);
  return o;
}

PRIMITIVE evaltrace(state)
LISPT state;
{
  long i = trace;

  if (!ISNIL(state))
    {
      CHECK(state,INTEGER);
      trace = INTVAL(state);
    }
  return mknumber(i);
}

PRIMITIVE freecount()
{
  int i;
  LISPT l;
  
  i = 0;
  for (l = freelist; INTVAL(l); l = CDR(l))
    i++;
  return mknumber((long) i);
}

LISPT
error(messnr, arg)
  int messnr;
  LISPT arg;
{
  if (NOT_A & messnr)
    (void) fprintf(primerr, "%s ", errmess[ERRNO(messnr)]);
  else
    (void) fprintf(primerr, "%s ", messages[ERRNO(messnr)]);
  if (messnr & (PRINT_ARG | NOT_A)) (void) prin2(arg, C_T);
  return C_ERROR;
}

LISPT
syserr(fault)
  LISPT fault;
{
  if (!ISNIL(fault))
    {
      (void) prin2(fault, C_T);
      (void) fprintf(stderr, ": ");
    }
  (void) fprintf(stderr, "%s", sys_errlist[errno]);
  return C_ERROR;
}

#include <setjmp.h>
static LISPT pexp;
extern jmp_buf toplevel;

static int
dobreak(com)
  LISPT *com;
{
/* OK, EVAL, ^, ... */
  if (TYPEOF(*com) != CONS)
    {
      unwind();
      longjmp(toplevel, 2);
    }
  else if (EQ(CAR(*com), C_GO))
    {
      pexp = xprint(eval(pexp), C_NIL);
      return 0;
    }
  else if (EQ(CAR(*com), C_RESET))
    {
      unwind();
      longjmp(toplevel, 2);
    }
  else if (EQ(CAR(*com), C_BT))
    {
      bt();
      return 2;
    }
  else if (EQ(CAR(*com), C_RETURN))
    {
      pexp = ISNIL(CDR(*com)) ? C_NIL : CAR(CDR(*com));
      return 0;
    }
  return 1;
}

LISPT
break0(exp)
  LISPT exp;
{
  pexp = exp;
  toploop(&brkprompt, dobreak);
  return pexp;
}

void
init_debug()
{
  mkprim(PN_FREECOUNT, freecount, 0, SUBR);
  mkprim(PN_EVALTRACE, evaltrace, 1, SUBR);
  mkprim(PN_OBARRAY,   xobarray,  0, SUBR);
  messages[ERRNO(NO_MESSAGE)] = "";
  messages[ERRNO(ILLEGAL_ARG)] = "Illegal argument";
  messages[ERRNO(DIVIDE_ZERO)] = "Divide by zero";
  messages[ERRNO(BUG)] = "Internal bug";
  messages[ERRNO(NO_MATCH)] = "No match for";
  messages[ERRNO(CANT_CREATE)] = "Can't create file";
  messages[ERRNO(CANT_CREATE_OPEN)] = "Can't create or open file";
  messages[ERRNO(CANT_OPEN)] = "Can't open file";
  messages[ERRNO(NO_SUCH_JOB)] = "No such job";
  messages[ERRNO(NOT_PRINTABLE)] = "Not printable";
  messages[ERRNO(NO_DIRECTORY)] = "No directory";
  messages[ERRNO(NO_USER)] = "No such user";
  messages[ERRNO(ATTEMPT_TO_RESET)] = "Attempt to clobber";
  messages[ERRNO(OUT_OF_MEMORY)] = "Out of memory";
  messages[ERRNO(UNEXPECTED_EOF)] = "Unexpected end of file";
  messages[ERRNO(EVENT_NOT_FOUND)] = "Event not found";
  messages[ERRNO(UNKNOWN_REQUEST)] = "Unknown request";
  messages[ERRNO(ILLEGAL_SIGNAL)] = "Illegal signal";
  messages[ERRNO(STACK_OVERFLOW)] = "Stack overflow";
  messages[ERRNO(CORRUPT_DATA)] = "Bug: corrupt data";
  messages[ERRNO(COMMAND_ABORTED)] = "Command aborted";
  messages[ERRNO(ALIAS_LOOP)] = "Alias loop";
  messages[ERRNO(ILLEGAL_FUNCTION)] = "Illegal function";
  messages[ERRNO(UNDEF_FUNCTION)] = "Undefined function";
  messages[ERRNO(UNBOUND_VARIABLE)] = "Unbound variable";
  messages[ERRNO(KBD_BREAK)] = "Break";
  messages[ERRNO(USER_ERROR)] = "";
}
