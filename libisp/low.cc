/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
/*
Dummy definition for cpprint.
PRIMITIVE low::set(var, val)
*/
PRIMITIVE low::set(LISPT var, LISPT val)
{
  l.check(var, SYMBOL);
  if(EQ(var, CE_NIL) || EQ(var, CE_T))
    return l.error(ATTEMPT_TO_RESET, var);
  if(type_of(var->symval().value) == INDIRECT)
    var->symval().value->indirectval() = val;
  else if(type_of(var->symval().value) == CVARIABLE)
    *var->symval().value->cvarval() = val;
  else
    var->symvalue(val);
  return val;
}

PRIMITIVE low::setq(LISPT var, LISPT val) { return set(var, eval(l, val)); }

PRIMITIVE low::progn(LISPT lexp)
{
  if(is_NIL(lexp))
    return C_NIL;
  while(!is_NIL(lexp->cdr()))
  {
    eval(l, lexp->car());
    lexp = lexp->cdr();
  }
  return eval(l, lexp->car());
}

PRIMITIVE low::cond(LISPT args)
{
  LISPT res = C_NIL;
  if(is_NIL(args))
    return C_NIL;
  while(is_NIL(res))
  {
    LISPT alt = args->car();
    l.check(alt, CONS);
    res = eval(l, alt->car());
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
  LISPT res = eval(l, pred);
  while(!is_NIL(res))
  {
    progn(exp);
    res = eval(l, pred);
  }
  return C_NIL;
}

PRIMITIVE low::prog1(LISPT a1, LISPT) { return a1; }

PRIMITIVE low::prog2(LISPT, LISPT a2, LISPT) { return a2; }

low::low(lisp& lisp): base(lisp) {}

void low::init()
{
  // clang-format off
  mkprim(PN_SET,   ::lisp::set,    subr_t::S_EVAL,   subr_t::S_NOSPREAD);
  mkprim(PN_SETQ,  ::lisp::setq,   subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_SETQQ, ::lisp::set,    subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_COND,  ::lisp::cond,   subr_t::S_NOEVAL, subr_t::S_SPREAD);
  mkprim(PN_WHILE, ::lisp::xwhile, subr_t::S_NOEVAL, subr_t::S_SPREAD);
  mkprim(PN_PROGN, ::lisp::progn,  subr_t::S_NOEVAL, subr_t::S_SPREAD);
  mkprim(PN_PROG1, ::lisp::prog1,  subr_t::S_EVAL,   subr_t::S_SPREAD);
  mkprim(PN_PROG2, ::lisp::prog2,  subr_t::S_EVAL,   subr_t::S_SPREAD);
  // clang-format on
}

} // namespace lisp
