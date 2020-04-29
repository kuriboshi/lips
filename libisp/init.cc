/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
/*
 * All lisp constants needed internally.
 */
LISPT C_T;
LISPT CE_NIL, CE_T;
LISPT C_ALIAS, C_APPEND, C_AUTOLOAD, C_BACK, C_BIGNUM, C_BROKEN;
LISPT C_BT, C_CLOSURE, C_CONS, C_DOT, C_ENDOFFILE, C_ENVIRON;
LISPT C_EOF, C_ERROR, C_EXEC, C_EXCL, C_FILE, C_FLOAT, C_FREE;
LISPT C_FROM, C_FSUBR, C_GO, C_INDIRECT, C_INTEGER;
LISPT C_LAMBDA, C_NLAMBDA, C_OLDDEF, C_OLDVAL, C_PIPE;
LISPT C_QUOTE, C_READ, C_REDEFINED, C_RESET, C_RETURN, C_STRING;
LISPT C_SUBR, C_SYMBOL, C_TO, C_TOTO, C_UNBOUND, C_WRITE;
LISPT C_BAR, C_GT, C_GGT, C_LT, C_SEMI, C_PROGN, C_AMPER;

LISPT currentbase; /* Conversion base for print of integer. */
LISPT topprompt;   /* Normal prompt. */
LISPT promptform;  /* Evaluated before printing the prompt. */
LISPT brkprompt;   /* Prompt in break. */
LISPT interactive; /* Nonnil if interactive lips. */
LISPT version;     /* Is set to the version string. */

/*
 * Initializes a lisp symbol with the pname NAME to contain the same
 * value as the C variable that CVAR points to. CVAR is set to VAL.
 * Whenever CVAR is changed the corresponding lisp variable changes
 * and vice versa.
 */
void initcvar(LISPT* cvar, const char* name, LISPT val)
{
  LISPT t = intern(name);
  set(t->symval().value, CVARIABLE, getobject());
  t->symval().value->cvarval(cvar);
  *cvar = val;
}

void init_lisp()
{
  alloc::init();

  set(C_T, TRUE, getobject());

  CE_NIL = intern("nil");
  CE_T = intern("t");
  C_ALIAS = intern("alias");
  C_APPEND = intern(PN_APPEND);
  C_AUTOLOAD = intern("autoload");
  C_BACK = intern(PN_BACK);
  C_BIGNUM = intern("bignum");
  C_BROKEN = intern("broken");
  C_BT = intern("bt");
  C_CLOSURE = intern("closure");
  C_CONS = intern(PN_CONS);
  C_DOT = intern(".");
  C_ENDOFFILE = intern("endoffile");
  C_ENVIRON = intern("environ");
  C_ERROR = intern(PN_ERROR);
  C_EXEC = intern(PN_EXEC);
  C_EXCL = intern("!");
  C_FILE = intern("file");
  C_FLOAT = intern("float");
  C_FREE = intern("free");
  C_FROM = intern(PN_FROM);
  C_FSUBR = intern("fsubr");
  C_GO = intern("go");
  C_INDIRECT = intern("indirect");
  C_INTEGER = intern("integer");
  C_LAMBDA = intern(PN_LAMBDA);
  C_NLAMBDA = intern(PN_NLAMBDA);
  C_OLDDEF = intern("olddef");
  C_OLDVAL = intern("oldval");
  C_PIPE = intern(PN_PIPECMD);
  C_QUOTE = intern(PN_QUOTE);
  C_READ = intern(PN_READ);
  C_REDEFINED = intern("redefined");
  C_RESET = intern("reset");
  C_RETURN = intern("return");
  C_STRING = intern("string");
  C_SUBR = intern("subr");
  C_SYMBOL = intern("symbol");
  C_TO = intern(PN_TO);
  C_TOTO = intern(PN_TOTO);
  C_UNBOUND = intern("unbound");
  C_WRITE = intern("write");
  C_PROGN = intern(PN_PROGN);
  C_BAR = intern("|");
  C_GT = intern(">");
  C_GGT = intern(">>");
  C_LT = intern("<");
  C_SEMI = intern(";");
  C_AMPER = intern("&");

  C_ERROR->type = ERROR;
  set(C_EOF, ENDOFFILE, getobject());
  CE_NIL->setq(C_NIL);
  CE_T->setq(C_T);

  initcvar(&topprompt, "prompt", mkstring("!_"));
  initcvar(&promptform, "promptform", C_NIL);
  initcvar(&brkprompt, "brkprompt", mkstring("!:"));
  initcvar(&currentbase, "base", mknumber(10L));
  initcvar(&interactive, "interactive", C_NIL);
  initcvar(&version, "version", mkstring(VERSION));

  rstack = C_NIL;
  top = C_NIL;

  evaluator::init();

  new arith;
  new debug;
  new file;
  new logic;
  new low;
  new map;
  new pred;
  new prim;
  new prop;
  new string;
  new unix;
  new user;

  evaluator::undefhook = nullptr;
  evaluator::breakhook = nullptr;
}

} // namespace lisp
