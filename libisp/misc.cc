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
    fprintf(primerr, "%s ", errmess[error_code(messnr)]);
  else
    fprintf(primerr, "%s ", messages[error_code(messnr)]);
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

debug::debug()
{
  mkprim(PN_EVALTRACE, evaltrace, 1, SUBR);
  messages[error_code(NO_MESSAGE)] = "";
  messages[error_code(ILLEGAL_ARG)] = "Illegal argument";
  messages[error_code(DIVIDE_ZERO)] = "Divide by zero";
  messages[error_code(BUG)] = "Internal bug";
  messages[error_code(NO_MATCH)] = "No match for";
  messages[error_code(CANT_CREATE)] = "Can't create file";
  messages[error_code(CANT_CREATE_OPEN)] = "Can't create or open file";
  messages[error_code(CANT_OPEN)] = "Can't open file";
  messages[error_code(NO_SUCH_JOB)] = "No such job";
  messages[error_code(NOT_PRINTABLE)] = "Not printable";
  messages[error_code(NO_DIRECTORY)] = "No directory";
  messages[error_code(NO_USER)] = "No such user";
  messages[error_code(ATTEMPT_TO_RESET)] = "Attempt to clobber";
  messages[error_code(OUT_OF_MEMORY)] = "Out of memory";
  messages[error_code(UNEXPECTED_EOF)] = "Unexpected end of file";
  messages[error_code(EVENT_NOT_FOUND)] = "Event not found";
  messages[error_code(UNKNOWN_REQUEST)] = "Unknown request";
  messages[error_code(ILLEGAL_SIGNAL)] = "Illegal signal";
  messages[error_code(STACK_OVERFLOW)] = "Stack overflow";
  messages[error_code(CORRUPT_DATA)] = "Bug: corrupt data";
  messages[error_code(COMMAND_ABORTED)] = "Command aborted";
  messages[error_code(ALIAS_LOOP)] = "Alias loop";
  messages[error_code(ILLEGAL_FUNCTION)] = "Illegal function";
  messages[error_code(UNDEF_FUNCTION)] = "Undefined function";
  messages[error_code(UNBOUND_VARIABLE)] = "Unbound variable";
  messages[error_code(KBD_BREAK)] = "Break";
  messages[error_code(USER_ERROR)] = "";
}

} // namespace lisp
