/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "low.hh"
#include "alloc.hh"
#include "eval.hh"

namespace lisp
{
low::low(): base() {}
low::low(lisp& lisp): base(lisp) {}

/*
Dummy definition for cpprint.
PRIMITIVE low::set(var, val)
*/
PRIMITIVE low::set(LISPT var, LISPT val)
{
  l.check(var, type::SYMBOL);
  if(var->symbol().constant)
    return l.error(ATTEMPT_TO_CLOBBER, var);
  if(type_of(var->symvalue()) == type::INDIRECT)
    var->symvalue()->indirectval(val);
  else if(type_of(var->symvalue()) == type::CVARIABLE)
    var->symvalue()->cvarval() = val;
  else
    var->symvalue(val);
  return val;
}

PRIMITIVE low::setq(LISPT var, LISPT val) { return set(var, eval(l, val)); }

PRIMITIVE low::progn(LISPT lexp)
{
  if(is_NIL(lexp))
    return NIL;
  while(!is_NIL(lexp->cdr()))
  {
    eval(l, lexp->car());
    lexp = lexp->cdr();
  }
  return eval(l, lexp->car());
}

PRIMITIVE low::cond(LISPT args)
{
  LISPT res = NIL;
  if(is_NIL(args))
    return NIL;
  while(is_NIL(res))
  {
    LISPT alt = args->car();
    l.check(alt, type::CONS);
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
  return NIL;
}

PRIMITIVE low::xwhile(LISPT pred, LISPT exp)
{
  LISPT res = eval(l, pred);
  while(!is_NIL(res))
  {
    progn(exp);
    res = eval(l, pred);
  }
  return NIL;
}

PRIMITIVE low::prog1(LISPT a1, LISPT) { return a1; }

PRIMITIVE low::prog2(LISPT, LISPT a2, LISPT) { return a2; }

namespace pn
{
inline constexpr auto SET = "set";     // set variable
inline constexpr auto SETQ = "setq";   // set quoted variable
inline constexpr auto SETQQ = "setqq"; // noeval set
inline constexpr auto COND = "cond";   // cond
inline constexpr auto WHILE = "while"; // while t
inline constexpr auto PROGN = "progn"; // return last expression
inline constexpr auto PROG1 = "prog1"; // return first expression
inline constexpr auto PROG2 = "prog2"; // return second expression
} // namespace pn

void low::init()
{
  // clang-format off
  mkprim(pn::SET,   ::lisp::set,    subr_t::subr::EVAL,   subr_t::spread::NOSPREAD);
  mkprim(pn::SETQ,  ::lisp::setq,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::SETQQ, ::lisp::set,    subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::COND,  ::lisp::cond,   subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::WHILE, ::lisp::xwhile, subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::PROGN, ::lisp::progn,  subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::PROG1, ::lisp::prog1,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  mkprim(pn::PROG2, ::lisp::prog2,  subr_t::subr::EVAL,   subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp
