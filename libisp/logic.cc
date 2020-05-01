/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
PRIMITIVE logic::p_and(LISPT l)
{
  LISPT foo = C_T;
  while(!is_NIL(l))
  {
    foo = eval(_lisp, l->car());
    if(is_NIL(foo))
      return foo;
    l = l->cdr();
  }
  return foo;
}

PRIMITIVE logic::p_or(LISPT l)
{
  LISPT foo = C_NIL;
  while(!is_NIL(l))
  {
    foo = eval(_lisp, l->car());
    if(!is_NIL(foo))
      return foo;
    l = l->cdr();
  }
  return foo;
}

PRIMITIVE logic::p_not(LISPT x)
{
  if(is_NIL(x))
    return C_T;
  return C_NIL;
}

PRIMITIVE logic::xif(LISPT pred, LISPT true_expr, LISPT false_expr)
{
  LISPT foo = eval(_lisp, pred);
  if(is_NIL(foo))
    return progn(_lisp, false_expr);
  return eval(_lisp, true_expr);
}

logic::logic(lisp& lisp) : base(lisp) {}

void logic::init()
{
  alloc::mkprim(PN_AND, ::lisp::p_and, -1, FSUBR);
  alloc::mkprim(PN_OR, ::lisp::p_or, -1, FSUBR);
  alloc::mkprim(PN_NOT, ::lisp::p_not, 1, SUBR);
  alloc::mkprim(PN_IF, ::lisp::xif, -3, FSUBR);
}

} // namespace lisp
