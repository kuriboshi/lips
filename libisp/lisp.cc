//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#include "libisp.hh"

namespace lisp
{
lisp::lisp(): _alloc(*new alloc(*this)), _eval(*new evaluator(*this))
{
  set(C_T, TRUE, a().getobject());

  auto intern = [this](const char* s) { return a().intern(s); };

  CE_NIL = intern("nil");
  CE_T = intern("t");
  C_ALIAS = intern("alias");
  C_AMPER = intern("&");
  C_APPEND = intern(PN_APPEND);
  C_AUTOLOAD = intern("autoload");
  C_BACK = intern(PN_BACK);
  C_BAR = intern("|");
  C_BIGNUM = intern("bignum");
  C_BROKEN = intern("broken");
  C_BT = intern("bt");
  C_CLOSURE = intern("closure");
  C_CONS = intern(PN_CONS);
  C_DOT = intern(".");
  C_ENDOFFILE = intern("endoffile");
  C_ENVIRON = intern("environ");
  C_ERROR = intern(PN_ERROR);
  C_EXCL = intern("!");
  C_EXEC = intern(PN_EXEC);
  C_FILE = intern("file");
  C_FLOAT = intern("float");
  C_FREE = intern("free");
  C_FROM = intern(PN_FROM);
  C_FSUBR = intern("fsubr");
  C_GGT = intern(">>");
  C_GO = intern("go");
  C_GT = intern(">");
  C_INDIRECT = intern("indirect");
  C_INTEGER = intern("integer");
  C_LAMBDA = intern(PN_LAMBDA);
  C_LT = intern("<");
  C_NLAMBDA = intern(PN_NLAMBDA);
  C_OLDDEF = intern("olddef");
  C_OLDVAL = intern("oldval");
  C_PIPE = intern(PN_PIPECMD);
  C_PROGN = intern(PN_PROGN);
  C_QUOTE = intern(PN_QUOTE);
  C_READ = intern(PN_READ);
  C_REDEFINED = intern("redefined");
  C_RESET = intern("reset");
  C_RETURN = intern("return");
  C_SEMI = intern(";");
  C_STRING = intern("string");
  C_SUBR = intern("subr");
  C_SYMBOL = intern("symbol");
  C_TO = intern(PN_TO);
  C_TOTO = intern(PN_TOTO);
  C_UNBOUND = intern("unbound");
  C_WRITE = intern("write");

  C_ERROR->type = ERROR;
  set(C_EOF, ENDOFFILE, a().getobject());
  CE_NIL->setq(C_NIL);
  CE_T->setq(C_T);

  initcvar(*this, &topprompt, "prompt", a().mkstring("!_"));
  initcvar(*this, &promptform, "promptform", C_NIL);
  initcvar(*this, &brkprompt, "brkprompt", a().mkstring("!:"));
  initcvar(*this, &currentbase, "base", a().mknumber(10L));
  initcvar(*this, &interactive, "interactive", C_NIL);
  initcvar(*this, &version, "version", a().mkstring(VERSION));

#if 0
  rstack = C_NIL;
  top = C_NIL;
#endif

  // new arith(l);
  new debug(*this);
  new file(*this);
  // new logic(l);
  // new low(l);
  // new map(l);
  // new pred(l);
  // new prim(l);
  // new prop(l);
  // new string(l);
  // new unix(l);
  // new user(l);

  e().undefhook = nullptr;
  e().breakhook = nullptr;
}

lisp::~lisp() {}

//
// All lisp constants needed internally.
//
LISPT C_T;
LISPT CE_NIL;
LISPT CE_T;
LISPT C_ALIAS;
LISPT C_AMPER;
LISPT C_APPEND;
LISPT C_AUTOLOAD;
LISPT C_BACK;
LISPT C_BAR;
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
LISPT C_EXCL;
LISPT C_EXEC;
LISPT C_FILE;
LISPT C_FLOAT;
LISPT C_FREE;
LISPT C_FROM;
LISPT C_FSUBR;
LISPT C_GGT;
LISPT C_GO;
LISPT C_GT;
LISPT C_INDIRECT;
LISPT C_INTEGER;
LISPT C_LAMBDA;
LISPT C_LT;
LISPT C_NLAMBDA;
LISPT C_OLDDEF;
LISPT C_OLDVAL;
LISPT C_PIPE;
LISPT C_PROGN;
LISPT C_QUOTE;
LISPT C_READ;
LISPT C_REDEFINED;
LISPT C_RESET;
LISPT C_RETURN;
LISPT C_SEMI;
LISPT C_STRING;
LISPT C_SUBR;
LISPT C_SYMBOL;
LISPT C_TO;
LISPT C_TOTO;
LISPT C_UNBOUND;
LISPT C_WRITE;

LISPT currentbase; // Conversion base for print of integer.
LISPT topprompt;   // Normal prompt.
LISPT promptform;  // Evaluated before printing the prompt.
LISPT brkprompt;   // Prompt in break.
LISPT interactive; // Nonnil if interactive lips.
LISPT version;     // Is set to the version string.

} // namespace lisp
