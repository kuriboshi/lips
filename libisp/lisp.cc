//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#include <cstdio>
#include <cerrno>
#include "libisp.hh"
#include "except.hh"
#include "error.hh"

namespace lisp
{
const char* messages[MAXMESSAGE];
/* Some standard messages, all of them not necessarily used */
const char* errmess[] = {"Not NIL", "Not a symbol", "Not an integer", "Not a bignum", "Not a float",
  "Not indirect", "Not a long", "Not a list", "Not a string", "Not SUBR", "Not FSUBR", "Not LAMBDA", "Not NLAMBDA",
  "Not a closure", "Not unbound", "Not an environment", "Not a file pointer", "Not T", "Not free", "Not EOF",
  "Not an ERROR", "Not a hash table"};
}

namespace lisp
{
lisp::lisp(): _alloc(*new alloc(*this)), _eval(*new evaluator(*this))
{
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

  auto intern = [this](const char* s) { return a().intern(s); };

  set(C_T, TRUE, a().getobject());
  CE_NIL = intern("nil");
  CE_NIL->setq(C_NIL);
  CE_T = intern("t");
  CE_T->setq(C_T);
  C_APPEND = intern(PN_APPEND);
  C_AUTOLOAD = intern("autoload");
  C_BIGNUM = intern("bignum");
  C_BROKEN = intern("broken");
  C_BT = intern("bt");
  C_CLOSURE = intern("closure");
  C_CONS = intern(PN_CONS);
  C_DOT = intern(".");
  C_ENDOFFILE = intern("endoffile");
  C_ENVIRON = intern("environ");
  set(C_EOF, ENDOFFILE, a().getobject());
  C_ERROR = intern(PN_ERROR);
  C_ERROR->type = ERROR;
  C_FILE = intern("file");
  C_FLOAT = intern("float");
  C_FREE = intern("free");
  C_FSUBR = intern("fsubr");
  C_GO = intern("go");
  C_INDIRECT = intern("indirect");
  C_INTEGER = intern("integer");
  C_LAMBDA = intern(PN_LAMBDA);
  C_NLAMBDA = intern(PN_NLAMBDA);
  C_OLDDEF = intern("olddef");
  C_QUOTE = intern(PN_QUOTE);
  C_READ = intern(PN_READ);
  C_REDEFINED = intern("redefined");
  C_RESET = intern("reset");
  C_RETURN = intern("return");
  C_STRING = intern("string");
  C_SUBR = intern("subr");
  C_SYMBOL = intern("symbol");
  C_UNBOUND = intern("unbound");
  C_WRITE = intern("write");

  initcvar(&topprompt, "prompt", a().mkstring("!_"));
  initcvar(&brkprompt, "brkprompt", a().mkstring("!:"));
  initcvar(&currentbase, "base", a().mknumber(10L));
  initcvar(&interactive, "interactive", C_NIL);
  initcvar(&version, "version", a().mkstring(VERSION));

  _primout = new file_t(new io::filesink(::stdout));
  _primerr = new file_t(new io::filesink(::stderr));
  _primin = new file_t(new io::filesource(::stdin));
  _stdout = new file_t(new io::filesink(::stdout));
  _stderr = new file_t(new io::filesink(::stderr));
  _stdin = new file_t(new io::filesource(::stdin));

  a().add_mark_object(&top);
  a().add_mark_object(&rstack);

  arith::init();
  debug::init();
  file::init();
  logic::init();
  low::init();
  map::init();
  pred::init();
  prim::init();
  prop::init();
  string::init();
  posix::init();
  user::init();
  io::init(*this);

  e().undefhook = nullptr;
  e().breakhook = nullptr;
}

lisp::~lisp() {}

void lisp::primout(file_t& f) { delete _primout; _primout = &f; }
void lisp::primerr(file_t& f) { delete _primerr; _primerr = &f; }
void lisp::primin(file_t& f) { delete _primin; _primin = &f; }

LISPT lisp::perror(int messnr, LISPT arg)
{
  if(NOT_A & messnr)
    primerr().printf("%s ", errmess[error_code(messnr)]);
  else
    primerr().printf("%s ", messages[error_code(messnr)]);
  if(messnr & (PRINT_ARG | NOT_A))
    prin2(*this, arg, C_T);
  return C_ERROR;
}

LISPT lisp::error(int messnr, LISPT arg)
{
  perror(messnr, arg);
  throw lisp_error("lisp_error");
}

LISPT lisp::syserr(LISPT fault)
{
  if(!is_NIL(fault))
  {
    prin2(*this, fault, C_T);
    primerr().printf(": ");
  }
  primerr().printf("%s", strerror(errno));
  return C_ERROR;
}

static int dobreak(lisp& l, LISPT* com)
{
  /* OK, EVAL, ^, ... */
  if(type_of(*com) != CONS)
  {
    l.e().unwind();
    throw lisp_error("bad command");
  }
  else if(EQ((**com).car(), C_GO))
  {
    l.pexp = xprint(l, eval(l, l.pexp), C_NIL);
    return 0;
  }
  else if(EQ((**com).car(), C_RESET))
  {
    l.e().unwind();
    throw lisp_reset();
  }
  else if(EQ((**com).car(), C_BT))
  {
    l.e().bt();
    return 2;
  }
  else if(EQ((**com).car(), C_RETURN))
  {
    l.pexp = is_NIL((**com).cdr()) ? C_NIL : (**com).cdr()->car();
    return 0;
  }
  return 1;
}

LISPT lisp::break0(LISPT exp)
{
  repl(brkprompt, [](lisp& lisp, LISPT* com) -> int { return dobreak(lisp, com); });
  return pexp;
}

void lisp::repl(LISPT prompt, breakfun_t f)
{
  while(true)
  {
    prin0(*this, prompt, primout());
    auto* buf = primin().source->getline();
    if(buf == nullptr)
      break;
    auto in = std::make_unique<file_t>(new io::stringsource(buf));
    auto expr = lispread(*this, *in.get(), false);
    print(*this, eval(*this, expr), primout());
  }
}

//
// All lisp constants needed internally.
//
LISPT C_T;
LISPT CE_NIL;
LISPT CE_T;
LISPT C_APPEND;
LISPT C_AUTOLOAD;
LISPT C_BIGNUM;
LISPT C_BROKEN;
LISPT C_BT;
LISPT C_CLOSURE;
LISPT C_CONS;
LISPT C_DOT;
LISPT C_ENDOFFILE;
LISPT C_ENVIRON;
LISPT C_EOF;
LISPT C_ERROR;
LISPT C_FILE;
LISPT C_FLOAT;
LISPT C_FREE;
LISPT C_FSUBR;
LISPT C_GO;
LISPT C_INDIRECT;
LISPT C_INTEGER;
LISPT C_LAMBDA;
LISPT C_NLAMBDA;
LISPT C_OLDDEF;
LISPT C_QUOTE;
LISPT C_READ;
LISPT C_REDEFINED;
LISPT C_RESET;
LISPT C_RETURN;
LISPT C_STRING;
LISPT C_SUBR;
LISPT C_SYMBOL;
LISPT C_UNBOUND;
LISPT C_WRITE;

LISPT currentbase; // Conversion base for print of integer.
LISPT topprompt;   // Normal prompt.
LISPT brkprompt;   // Prompt in break.
LISPT interactive; // Nonnil if interactive lips.
LISPT version;     // Is set to the version string.

} // namespace lisp
