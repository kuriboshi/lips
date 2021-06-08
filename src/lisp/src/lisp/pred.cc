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
    case lisp_type::INTEGER:
    case lisp_type::FLOAT:
    case lisp_type::BIGNUM:
      return a;
    default:
      return C_NIL;
  }
}

PRIMITIVE pred::listp(LISPT a)
{
  if(type_of(a) == lisp_type::CONS)
    return a;
  else
    return C_NIL;
}

PRIMITIVE pred::memb(LISPT x, LISPT l)
{
  while(!EQ(l, C_NIL))
  {
    if(EQ(x, l->car()))
      return l;
    l = l->cdr();
  }
  return C_NIL;
}

PRIMITIVE pred::equal(LISPT l1, LISPT l2)
{
  LISPT x;

  if(type_of(l1) != type_of(l2))
    return C_NIL;
  if(EQ(l1, l2))
    return C_T;
  switch(type_of(l1))
  {
    case lisp_type::CONS:
      while(!EQ(l1, C_NIL) && !EQ(l2, C_NIL))
      {
        x = equal(l1->car(), l2->car());
        if(EQ(x, C_T))
        {
          l1 = l1->cdr();
          l2 = l2->cdr();
        }
        else
          return C_NIL;
      }
      return x;
    case lisp_type::STRING:
      return (l1->stringval() == l2->stringval()) ? C_T : C_NIL;
      break;
    case lisp_type::LAMBDA:
    case lisp_type::NLAMBDA:
      return funeq(l, l1, l2);
      break;
    case lisp_type::INTEGER:
      return (l1->intval() == l2->intval() ? C_T : C_NIL);
      break;
    default:
      break;
  }
  return C_NIL;
}

PRIMITIVE pred::nlistp(LISPT a)
{
  if(type_of(a) != lisp_type::CONS)
    return a;
  else
    return C_NIL;
}

PRIMITIVE pred::neq(LISPT a, LISPT b)
{
  if(!EQ(a, b))
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE pred::boundp(LISPT a)
{
  if(type_of(a) != lisp_type::SYMBOL)
    return C_NIL;
  else if(type_of(a->symval().value) != lisp_type::UNBOUND)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE pred::litatom(LISPT a)
{
  if(type_of(a) == lisp_type::SYMBOL)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE pred::xtypeof(LISPT a)
{
  switch(type_of(a))
  {
    case lisp_type::NIL:
      return C_NIL;
    case lisp_type::SYMBOL:
      return C_SYMBOL;
    case lisp_type::INTEGER:
      return C_INTEGER;
    case lisp_type::BIGNUM:
      return C_BIGNUM;
    case lisp_type::FLOAT:
      return C_FLOAT;
    case lisp_type::INDIRECT:
      return C_INDIRECT;
    case lisp_type::CONS:
      return C_CONS;
    case lisp_type::STRING:
      return C_STRING;
    case lisp_type::SUBR:
      return C_SUBR;
    case lisp_type::FSUBR:
      return C_FSUBR;
    case lisp_type::LAMBDA:
      return C_LAMBDA;
    case lisp_type::NLAMBDA:
      return C_NLAMBDA;
    case lisp_type::CLOSURE:
      return C_CLOSURE;
    case lisp_type::UNBOUND:
      return C_UNBOUND;
    case lisp_type::ENVIRON:
      return C_ENVIRON;
    case lisp_type::T:
      return C_T;
    case lisp_type::FREE:
      return C_FREE;
    case lisp_type::ENDOFFILE:
      return C_ENDOFFILE;
    case lisp_type::ERROR:
      return C_ERROR;
    case lisp_type::FILET:
      return C_FILE;
    default:
      return C_NIL;
  }
  return C_NIL;
}

inline constexpr auto PN_LISTP = "listp";     // t if cons
inline constexpr auto PN_NLISTP = "nlistp";   // not listp
inline constexpr auto PN_NEQ = "neq";         // not eq
inline constexpr auto PN_NUMBERP = "numberp"; // integer of float
inline constexpr auto PN_MEMB = "memb";       // t if a in l
inline constexpr auto PN_EQUAL = "equal";     // equal
inline constexpr auto PN_BOUNDP = "boundp";   // t if var bound
inline constexpr auto PN_LITATOM = "litatom"; // t if literal atom
inline constexpr auto PN_TYPEOF = "typeof";   // return type as an atom

void pred::init()
{
  // clang-format off
  mkprim(PN_LISTP,   ::lisp::listp,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_NLISTP,  ::lisp::nlistp,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_NEQ,     ::lisp::neq,     subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_NUMBERP, ::lisp::numberp, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_MEMB,    ::lisp::memb,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_EQUAL,   ::lisp::equal,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_BOUNDP,  ::lisp::boundp,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_LITATOM, ::lisp::litatom, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_TYPEOF,  ::lisp::xtypeof, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format on
}

} // namespace lisp
