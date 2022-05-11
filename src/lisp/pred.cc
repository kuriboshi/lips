//
// Lips, lisp shell.
// Copyright 1988, 2020-2022 Krister Joas
//

#include "pred.hh"

namespace lisp::pred
{
LISPT numberp(lisp& l, LISPT a)
{
  switch(type_of(a))
  {
    case type::INTEGER:
    case type::FLOAT:
      return a;
    default:
      return NIL;
  }
}

LISPT listp(lisp& l, LISPT a)
{
  if(type_of(a) == type::CONS)
    return a;
  return NIL;
}

LISPT memb(lisp& l, LISPT x, LISPT ls)
{
  while(!eq(ls, NIL))
  {
    if(eq(x, ls->car()))
      return ls;
    ls = ls->cdr();
  }
  return NIL;
}

LISPT equal(lisp& l, LISPT l1, LISPT l2)
{
  if(type_of(l1) != type_of(l2))
    return NIL;
  if(l1 == l2)
    return T;
  switch(type_of(l1))
  {
    case type::CONS:
      while(type_of(l1) == type_of(l2))
      {
        if(pred::equal(l, l1->car(), l2->car()) != NIL)
          return pred::equal(l, l1->cdr(), l2->cdr());
        return NIL;
      }
    case type::STRING:
      return (l1->string() == l2->string()) ? T : NIL;
    case type::LAMBDA:
    case type::NLAMBDA:
      return user::funeq(l, l1, l2);
    case type::INTEGER:
      return (l1->intval() == l2->intval() ? T : NIL);
    default:
      break;
  }
  return NIL;
}

LISPT nlistp(lisp& l, LISPT a)
{
  if(a == NIL)
    return T;
  if(type_of(a) != type::CONS)
    return a;
  return NIL;
}

LISPT neq(lisp& l, LISPT a, LISPT b)
{
  if(a != b)
    return T;
  return NIL;
}

LISPT boundp(lisp& l, LISPT a)
{
  if(type_of(a) != type::SYMBOL)
    return NIL;
  else if(type_of(a->value()) != type::UNBOUND)
    return T;
  return NIL;
}

LISPT litatom(lisp& l, LISPT a)
{
  if(type_of(a) == type::SYMBOL || type_of(a) == type::T)
    return T;
  return NIL;
}

LISPT xtypeof(lisp& l, LISPT a)
{
  switch(type_of(a))
  {
    case type::NIL:
      return NIL;
    case type::SYMBOL:
      return C_SYMBOL;
    case type::INTEGER:
      return C_INTEGER;
    case type::FLOAT:
      return C_FLOAT;
    case type::INDIRECT:
      return C_INDIRECT;
    case type::CONS:
      return C_CONS;
    case type::STRING:
      return C_STRING;
    case type::SUBR:
      return C_SUBR;
    case type::FSUBR:
      return C_FSUBR;
    case type::LAMBDA:
      return C_LAMBDA;
    case type::NLAMBDA:
      return C_NLAMBDA;
    case type::CLOSURE:
      return C_CLOSURE;
    case type::UNBOUND:
      return C_UNBOUND;
    case type::ENVIRON:
      return C_ENVIRON;
    case type::T:
      return T;
    case type::FREE:
      return C_FREE;
    case type::ENDOFFILE:
      return C_ENDOFFILE;
    case type::ERROR:
      return C_ERROR;
    case type::FILET:
      return C_FILE;
    default:
      return NIL;
  }
  return NIL;
}

namespace pn
{
inline constexpr auto LISTP = "listp";     // t if cons
inline constexpr auto NLISTP = "nlistp";   // not listp
inline constexpr auto NEQ = "neq";         // not eq
inline constexpr auto NUMBERP = "numberp"; // integer of float
inline constexpr auto MEMB = "memb";       // t if a in l
inline constexpr auto EQUAL = "equal";     // equal
inline constexpr auto BOUNDP = "boundp";   // t if var bound
inline constexpr auto LITATOM = "litatom"; // t if is a literal atom (Interlisp)
inline constexpr auto SYMBOLP = "symbolp"; // t if is a symbol (CL)
inline constexpr auto TYPEOF = "typeof";   // return type as an atom
} // namespace pn

void init()
{
  // clang-format off
  mkprim(pn::LISTP,   listp,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::NLISTP,  nlistp,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::NEQ,     neq,     subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::NUMBERP, numberp, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::MEMB,    memb,    subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::EQUAL,   equal,   subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::BOUNDP,  boundp,  subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::LITATOM, litatom, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::SYMBOLP, litatom, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  mkprim(pn::TYPEOF,  xtypeof, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format on
}

} // namespace lisp::pred
