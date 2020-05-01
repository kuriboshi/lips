/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <cstring>

#include "libisp.hh"

namespace lisp
{
PRIMITIVE pred::numberp(LISPT a)
{
  switch(type_of(a))
  {
    case INTEGER:
    case FLOAT:
    case BIGNUM:
#ifdef FLOATING
    case LONG:
      return a;
#endif /* FLOATING */
    default:
      return C_NIL;
  }
}

PRIMITIVE pred::listp(LISPT a)
{
  if(type_of(a) == CONS)
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
    case CONS:
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
    case STRING:
      return (!strcmp(l1->stringval(), l2->stringval())) ? C_T : C_NIL;
      break;
    case LAMBDA:
    case NLAMBDA:
      return funeq(l1, l2);
      break;
    case INTEGER:
      return (l1->intval() == l2->intval() ? C_T : C_NIL);
      break;
    default:
      break;
  }
  return C_NIL;
}

PRIMITIVE pred::nlistp(LISPT a)
{
  if(type_of(a) != CONS)
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
  if(type_of(a) != SYMBOL)
    return C_NIL;
  else if(type_of(a->symval().value) != UNBOUND)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE pred::litatom(LISPT a)
{
  if(type_of(a) == SYMBOL)
    return C_T;
  else
    return C_NIL;
}

PRIMITIVE pred::xtypeof(LISPT a)
{
  switch(type_of(a))
  {
    case NIL:
      return C_NIL;
    case SYMBOL:
      return C_SYMBOL;
    case INTEGER:
      return C_INTEGER;
    case BIGNUM:
      return C_BIGNUM;
    case FLOAT:
      return C_FLOAT;
    case INDIRECT:
      return C_INDIRECT;
    case CONS:
      return C_CONS;
    case STRING:
      return C_STRING;
    case SUBR:
      return C_SUBR;
    case FSUBR:
      return C_FSUBR;
    case LAMBDA:
      return C_LAMBDA;
    case NLAMBDA:
      return C_NLAMBDA;
    case CLOSURE:
      return C_CLOSURE;
    case UNBOUND:
      return C_UNBOUND;
    case ENVIRON:
      return C_ENVIRON;
    case TRUE:
      return C_T;
    case FREE:
      return C_FREE;
    case ENDOFFILE:
      return C_ENDOFFILE;
    case ERROR:
      return C_ERROR;
    case FILET:
      return C_FILE;
    default:
      return C_NIL;
  }
  return C_NIL;
}

pred::pred(lisp& lisp) : base(lisp)
{
  mkprim(PN_LISTP, ::lisp::listp, 1, SUBR);
  mkprim(PN_NLISTP, ::lisp::nlistp, 1, SUBR);
  mkprim(PN_NEQ, ::lisp::neq, 2, SUBR);
  mkprim(PN_NUMBERP, ::lisp::numberp, 1, SUBR);
  mkprim(PN_MEMB, ::lisp::memb, 2, SUBR);
  mkprim(PN_EQUAL, ::lisp::equal, 2, SUBR);
  mkprim(PN_BOUNDP, ::lisp::boundp, 1, SUBR);
  mkprim(PN_LITATOM, ::lisp::litatom, 1, SUBR);
  mkprim(PN_TYPEOF, ::lisp::xtypeof, 1, SUBR);
}

} // namespace lisp
