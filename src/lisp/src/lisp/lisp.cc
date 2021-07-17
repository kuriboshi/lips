//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#include <doctest/doctest.h>
#include <cstring>              // For strerror
#include <cerrno>               // For errno
#include <iostream>
#include "libisp.hh"
#include "except.hh"
#include "error.hh"

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
  messages[error_code(ATTEMPT_TO_CLOBBER)] = "Attempt to clobber constant";
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
  messages[error_code(AMBIGUOUS)] = "Ambiguous";
  messages[error_code(USER_ERROR)] = "";

  auto intern = [this](const auto s) { return a().intern(s); };

  set(T, type::T, a().getobject());

  auto nil = intern("nil");
  nil->symvalue(NIL);
  nil->symbol().constant = true;
  
  auto t = intern("t");
  t->symvalue(T);
  t->symbol().constant = true;

  C_AUTOLOAD = intern("autoload");
  C_BIGNUM = intern("bignum");
  C_BROKEN = intern("broken");
  C_BT = intern("bt");
  C_CLOSURE = intern("closure");
  C_CONS = intern(pn::CONS);
  C_DOT = intern(".");
  C_ENDOFFILE = intern("endoffile");
  C_ENVIRON = intern("environ");
  set(C_EOF, type::ENDOFFILE, a().getobject());
  C_FILE = intern("file");
  C_FLOAT = intern("float");
  C_FREE = intern("free");
  C_FSUBR = intern("fsubr");
  C_GO = intern("go");
  C_INDIRECT = intern("indirect");
  C_INTEGER = intern("integer");
  C_OLDDEF = intern("olddef");
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
  initcvar(&version, "version", a().mkstring(VERSION));

  _primout = std::make_unique<file_t>(std::cout);
  _primerr = std::make_unique<file_t>(std::cerr);
  _primin = std::make_unique<file_t>(std::cin);
  _stdout = std::make_unique<file_t>(std::cout);
  _stderr = std::make_unique<file_t>(std::cerr);
  _stdin = std::make_unique<file_t>(std::cin);

  a().gcprotect(top);
  a().gcprotect(rstack);

  arith::init();
  debug::init();
  file::init();
  logic::init();
  low::init();
  Map::init();
  pred::init();
  prim::init();
  prop::init();
  string::init();
  posix::init();
  user::init();
  io::init(*this);

  e().undefhook(nullptr);
  e().breakhook(nullptr);

  if(_current == nullptr)
    _current = this;
}

lisp::~lisp()
{
  if(_current == this)
    _current = nullptr;
}

void lisp::primout(std::unique_ptr<file_t> f)
{
  _primout = std::move(f);
}
void lisp::primerr(std::unique_ptr<file_t> f)
{
  _primerr = std::move(f);
}
void lisp::primin(std::unique_ptr<file_t> f)
{
  _primin = std::move(f);
}

LISPT lisp::perror(int messnr, LISPT arg)
{
  if(NOT_A & messnr)
    primerr().format("{} ", errmess[error_code(messnr)]);
  else
    primerr().format("{} ", messages[error_code(messnr)]);
  if(messnr & (PRINT_ARG | NOT_A))
    prin2(*this, arg, T);
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
    prin2(*this, fault, T);
    primerr().format(": ");
  }
  primerr().format("{}", strerror(errno));
  return C_ERROR;
}

LISPT lisp::break0(LISPT exp)
{
  return repl(exp);
}

lisp* lisp::_current = nullptr;

//
// All lisp constants needed internally.
//
LISPT T;
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
LISPT C_FILE;
LISPT C_FLOAT;
LISPT C_FREE;
LISPT C_FSUBR;
LISPT C_GO;
LISPT C_INDIRECT;
LISPT C_INTEGER;
LISPT C_OLDDEF;
LISPT C_REDEFINED;
LISPT C_RESET;
LISPT C_RETURN;
LISPT C_STRING;
LISPT C_SUBR;
LISPT C_SYMBOL;
LISPT C_UNBOUND;
LISPT C_WRITE;

} // namespace lisp
