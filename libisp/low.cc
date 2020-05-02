/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
LISPT verboseflg;

/*
Dummy definition for cpprint.
PRIMITIVE low::set(var, val)
*/
PRIMITIVE low::set(LISPT var, LISPT val)
{
  _lisp.check(var, SYMBOL);
  if(EQ(var, CE_NIL) || EQ(var, CE_T))
    return _lisp.error(ATTEMPT_TO_RESET, var);
  if(type_of(var->symval().value) == INDIRECT)
    var->symval().value->indirectval() = val;
  else if(type_of(var->symval().value) == CVARIABLE)
    *var->symval().value->cvarval() = val;
  else
    var->symvalue(val);
  return val;
}

PRIMITIVE low::setq(LISPT var, LISPT val) { return set(var, eval(_lisp, val)); }

PRIMITIVE low::progn(LISPT lexp)
{
  if(is_NIL(lexp))
    return C_NIL;
  while(!is_NIL(lexp->cdr()))
  {
    eval(_lisp, lexp->car());
    lexp = lexp->cdr();
  }
  return eval(_lisp, lexp->car());
}

PRIMITIVE low::cond(LISPT args)
{
  LISPT res = C_NIL;
  if(is_NIL(args))
    return C_NIL;
  while(is_NIL(res))
  {
    LISPT alt = args->car();
    _lisp.check(alt, CONS);
    res = eval(_lisp, alt->car());
    if(!is_NIL(res))
    {
      if(is_NIL(alt->cdr()))
        return res;
      else
        return progn(alt->cdr());
    }
    args = args->cdr();
    if(is_NIL(args))
      break;
  }
  return C_NIL;
}

PRIMITIVE low::xwhile(LISPT pred, LISPT exp)
{
  LISPT res = eval(_lisp, pred);
  while(!is_NIL(res))
  {
    progn(exp);
    res = eval(_lisp, pred);
  }
  return C_NIL;
}

PRIMITIVE low::prog1(LISPT a1, LISPT) { return a1; }

PRIMITIVE low::prog2(LISPT, LISPT a2, LISPT) { return a2; }

#if 0
PRIMITIVE low::topofstack()
{
  return env;
}
#endif

PRIMITIVE low::envget(LISPT e, LISPT n)
{
#if 0
  LISPT foo;
  
  CHECK(e, ENVIRON);
  CHECK(n, INTEGER);
  if (INTVAL(n) <= 0)
    foo = cons(CAR(ENVVAL(e)), mknumber(CDR(ENVVAL(e))));
  else
    if (INTVAL(n) <= INTVAL(CDR(e)))
      foo = cons (CAR(ENVVAL(e) + INTVAL(n)),
		  CDR(ENVVAL(e) + INTVAL(n)));
    else
      foo = C_NIL;
  return foo;
#else
  return e;
#endif
}

low::low(lisp& lisp): base(lisp) {}

void low::init()
{
  alloc::mkprim(PN_SET, ::lisp::set, 2, SUBR);
  alloc::mkprim(PN_SETQ, ::lisp::setq, 2, FSUBR);
  alloc::mkprim(PN_SETQQ, ::lisp::set, 2, FSUBR);
  alloc::mkprim(PN_COND, ::lisp::cond, -1, FSUBR);
  alloc::mkprim(PN_WHILE, ::lisp::xwhile, -2, FSUBR);
  alloc::mkprim(PN_PROGN, ::lisp::progn, -1, FSUBR);
  alloc::mkprim(PN_PROG1, ::lisp::prog1, -2, SUBR);
  alloc::mkprim(PN_PROG2, ::lisp::prog2, -3, SUBR);
#if 0
  alloc::mkprim(PN_TOPOFSTACK, ::lisp::topofstack,  0, SUBR);
#endif
  alloc::mkprim(PN_ENVGET, ::lisp::envget, 2, SUBR);
  alloc::initcvar(&verboseflg, "verboseflg", C_NIL);
}

} // namespace lisp
