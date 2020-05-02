/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <cstring>

#include "libisp.hh"

namespace lisp
{
/* Return symbols print name as a string. */
PRIMITIVE string::symstr(LISPT sym)
{
  _lisp.check(sym, SYMBOL);
  return mkstring(_lisp, sym->symval().pname);
}

/* T if s is a string, NIL otherwise. */
PRIMITIVE string::stringp(LISPT s)
{
  if(type_of(s) == STRING)
    return C_T;
  return C_NIL;
}

/* T if both strings are equal */
PRIMITIVE string::streq(LISPT s1, LISPT s2)
{
  _lisp.check(s1, STRING);
  _lisp.check(s2, STRING);
  if(!strcmp(s1->stringval(), s2->stringval()))
    return C_T;
  return C_NIL;
}

PRIMITIVE string::strcomp(LISPT s1, LISPT s2)
{
  _lisp.check(s1, STRING);
  _lisp.check(s2, STRING);
  return mknumber(_lisp, strcmp(s1->stringval(), s2->stringval()));
}

/* Concatenate arbitrary many strings to
   to one string */
PRIMITIVE string::concat(LISPT strlist)
{
  int size = 0;
  for(auto sl = strlist; !is_NIL(sl); sl = sl->cdr())
  {
    _lisp.check(sl->car(), STRING);
    size += std::strlen(sl->car()->stringval());
  }
  auto* ns = a().realmalloc((unsigned)size + 1);
  if(ns == nullptr)
    return _lisp.error(OUT_OF_MEMORY, C_NIL);
  ns[0] = '\0';
  while(!is_NIL(strlist))
  {
    strcat(ns, strlist->car()->stringval());
    strlist = strlist->cdr();
  }
  return mkstring(_lisp, ns);
}

/* Return string length of s */
PRIMITIVE string::strlen(LISPT s)
{
  _lisp.check(s, STRING);
  return mknumber(_lisp, std::strlen(s->stringval()));
}

/* Extract a substring from start to end.
   If start or end is out of bounds, return
   NIL. If end is one less than start the
   zero length string is returned. end equal
   to zero if start is equal to one is accepted. */
PRIMITIVE string::substr(LISPT str, LISPT start, LISPT end)
{
  _lisp.check(str, STRING);
  _lisp.check(start, INTEGER);
  _lisp.check(end, INTEGER);
  auto s = start->intval();
  auto e = end->intval();
  auto size = e - s + 1;
  if(size < 0 || s > static_cast<int>(std::strlen(str->stringval()))
    || e > static_cast<int>(std::strlen(str->stringval())) || s <= 0 || e < 0)
    return C_NIL;
  auto* ns = a().realmalloc((unsigned)size + 1);
  if(ns == nullptr)
    return _lisp.error(OUT_OF_MEMORY, C_NIL);
  ns[size] = '\0';
  for(size = 0; s <= e; s++, size++)
  {
    ns[size] = *(str->stringval() + s - 1);
  }
  return mkstring(_lisp, ns);
}

string::string(lisp& lisp): base(lisp) {}

void string::init()
{
  alloc::mkprim(PN_STRINGP, ::lisp::stringp, 1, SUBR);
  alloc::mkprim(PN_STREQ, ::lisp::streq, 2, SUBR);
  alloc::mkprim(PN_CONCAT, ::lisp::concat, -1, SUBR);
  alloc::mkprim(PN_STRLEN, ::lisp::strlen, 1, SUBR);
  alloc::mkprim(PN_SUBSTR, ::lisp::substr, 3, SUBR);
  alloc::mkprim(PN_SYMSTR, ::lisp::symstr, 1, SUBR);
  alloc::mkprim(PN_STRCMP, ::lisp::strcomp, 2, SUBR);
}

} // namespace lisp
