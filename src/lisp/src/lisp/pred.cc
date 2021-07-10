/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
pred::pred(): base() {}
pred::pred(lisp& lisp): base(lisp) {}

PRIMITIVE pred::numberp(LISPT a)
{
  switch(type_of(a))
  {
    case type::INTEGER:
    case type::FLOAT:
    case type::BIGNUM:
      return a;
    default:
      return NIL;
  }
}

PRIMITIVE pred::listp(LISPT a)
{
  if(type_of(a) == type::CONS)
    return a;
  else
    return NIL;
}

PRIMITIVE pred::memb(LISPT x, LISPT l)
{
  while(!EQ(l, NIL))
  {
    if(EQ(x, l->car()))
      return l;
    l = l->cdr();
  }
  return NIL;
}

PRIMITIVE pred::equal(LISPT l1, LISPT l2)
{
  LISPT x = NIL;

  if(type_of(l1) != type_of(l2))
    return NIL;
  if(EQ(l1, l2))
    return C_T;
  switch(type_of(l1))
  {
    case type::CONS:
      while(!EQ(l1, NIL) && !EQ(l2, NIL))
      {
        x = equal(l1->car(), l2->car());
        if(EQ(x, C_T))
        {
          l1 = l1->cdr();
          l2 = l2->cdr();
        }
        else
          return NIL;
      }
      return x;
    case type::STRING:
      return (l1->stringval() == l2->stringval()) ? C_T : NIL;
      break;
    case type::LAMBDA:
    case type::NLAMBDA:
      return funeq(l, l1, l2);
      break;
    case type::INTEGER:
      return (l1->intval() == l2->intval() ? C_T : NIL);
      break;
    default:
      break;
  }
  return NIL;
}

PRIMITIVE pred::nlistp(LISPT a)
{
  if(type_of(a) != type::CONS)
    return a;
  else
    return NIL;
}

PRIMITIVE pred::neq(LISPT a, LISPT b)
{
  if(!EQ(a, b))
    return C_T;
  else
    return NIL;
}

PRIMITIVE pred::boundp(LISPT a)
{
  if(type_of(a) != type::SYMBOL)
    return NIL;
  else if(type_of(a->symvalue()) != type::UNBOUND)
    return C_T;
  else
    return NIL;
}

PRIMITIVE pred::litatom(LISPT a)
{
  if(type_of(a) == type::SYMBOL)
    return C_T;
  else
    return NIL;
}

PRIMITIVE pred::xtypeof(LISPT a)
{
  switch(type_of(a))
  {
    case type::NIL:
      return NIL;
    case type::SYMBOL:
      return C_SYMBOL;
    case type::INTEGER:
      return C_INTEGER;
    case type::BIGNUM:
      return C_BIGNUM;
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
      return C_T;
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
inline constexpr auto LITATOM = "litatom"; // t if literal atom
inline constexpr auto TYPEOF = "typeof";   // return type as an atom
} // namespace pn

void pred::init()
{
  // clang-format off
  mkprim(pn::LISTP,   ::lisp::listp,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NLISTP,  ::lisp::nlistp,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NEQ,     ::lisp::neq,     subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::NUMBERP, ::lisp::numberp, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::MEMB,    ::lisp::memb,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::EQUAL,   ::lisp::equal,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::BOUNDP,  ::lisp::boundp,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::LITATOM, ::lisp::litatom, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::TYPEOF,  ::lisp::xtypeof, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp
