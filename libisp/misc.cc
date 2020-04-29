/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <cerrno>

#include "libisp.hh"

using namespace lisp;

extern bool toploop(LISPT*, int (*)(LISPT*));

static const char* messages[MAXMESSAGE];
/* Some standard messages, all of them not necessarily used */
static const char* errmess[] = {"Not NIL", "Not a symbol", "Not an integer", "Not a bignum", "Not a float",
  "Not indirect", "Not a long", "Not a list", "Not a string", "Not SUBR", "Not FSUBR", "Not LAMBDA", "Not NLAMBDA",
  "Not a closure", "Not unbound", "Not an environment", "Not a file pointer", "Not T", "Not free", "Not EOF",
  "Not an ERROR", "Not a hash table"};

namespace lisp
{
int trace;

PRIMITIVE evaltrace(LISPT state)
{
  auto i = trace;

  if(!ISNIL(state))
  {
    check(state, INTEGER);
    trace = state->intval();
  }
  return mknumber(i);
}

LISPT perror(int messnr, LISPT arg)
{
  if(NOT_A & messnr)
    fprintf(primerr, "%s ", errmess[ERRNO(messnr)]);
  else
    fprintf(primerr, "%s ", messages[ERRNO(messnr)]);
  if(messnr & (PRINT_ARG | NOT_A))
    prin2(arg, C_T);
  return C_ERROR;
}

LISPT error(int messnr, LISPT arg)
{
  perror(messnr, arg);
  throw lisp_error("lisp_error");
}

LISPT syserr(LISPT fault)
{
  if(!ISNIL(fault))
  {
    prin2(fault, C_T);
    fprintf(stderr, ": ");
  }
  fprintf(stderr, "%s", strerror(errno));
  return C_ERROR;
}

static LISPT pexp;

static int dobreak(LISPT* com)
{
  /* OK, EVAL, ^, ... */
  if(TYPEOF(*com) != CONS)
  {
    unwind();
    throw lisp_error("bad command");
  }
  else if(EQ((**com).car(), C_GO))
  {
    pexp = xprint(evaluator::eval(pexp), C_NIL);
    return 0;
  }
  else if(EQ((**com).car(), C_RESET))
  {
    unwind();
    throw lisp_reset();
  }
  else if(EQ((**com).car(), C_BT))
  {
    bt();
    return 2;
  }
  else if(EQ((**com).car(), C_RETURN))
  {
    pexp = ISNIL((**com).cdr()) ? C_NIL : (**com).cdr()->car();
    return 0;
  }
  return 1;
}

LISPT break0(LISPT exp)
{
  pexp = exp;
  toploop(&brkprompt, dobreak);
  return pexp;
}

void init_debug()
{
  mkprim(PN_EVALTRACE, evaltrace, 1, SUBR);
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

} // namespace lisp
