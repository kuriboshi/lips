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
PRIMITIVE symstr(LISPT sym)
{
  check(sym, SYMBOL);
  return mkstring(sym->symval().pname);
}

/* T if s is a string, NIL otherwise. */
PRIMITIVE stringp(LISPT s)
{
  if(TYPEOF(s) == STRING)
    return C_T;
  return C_NIL;
}

/* T if both strings are equal */
PRIMITIVE streq(LISPT s1, LISPT s2)
{
  check(s1, STRING);
  check(s2, STRING);
  if(!strcmp(s1->stringval(), s2->stringval()))
    return C_T;
  return C_NIL;
}

PRIMITIVE strcomp(LISPT s1, LISPT s2)
{
  check(s1, STRING);
  check(s2, STRING);
  return mknumber(strcmp(s1->stringval(), s2->stringval()));
}

/* Concatenate arbitrary many strings to
   to one string */
PRIMITIVE concat(LISPT strlist)
{
  int size = 0;
  for(auto sl = strlist; !ISNIL(sl); sl = sl->cdr())
  {
    check(sl->car(), STRING);
    size += strlen(sl->car()->stringval());
  }
  auto* ns = realmalloc((unsigned)size + 1);
  if(ns == nullptr)
    return error(OUT_OF_MEMORY, C_NIL);
  ns[0] = '\0';
  while(!ISNIL(strlist))
  {
    strcat(ns, strlist->car()->stringval());
    strlist = strlist->cdr();
  }
  return mkstring(ns);
}

/* Return string length of s */
PRIMITIVE xstrlen(LISPT s)
{
  check(s, STRING);
  return mknumber(strlen(s->stringval()));
}

/* Extract a substring from start to end.
   If start or end is out of bounds, return
   NIL. If end is one less than start the
   zero length string is returned. end equal
   to zero if start is equal to one is accepted. */
PRIMITIVE substr(LISPT str, LISPT start, LISPT end)
{
  check(str, STRING);
  check(start, INTEGER);
  check(end, INTEGER);
  auto s = start->intval();
  auto e = end->intval();
  auto size = e - s + 1;
  if(size < 0 || s > static_cast<int>(strlen(str->stringval())) || e > static_cast<int>(strlen(str->stringval()))
    || s <= 0 || e < 0)
    return C_NIL;
  auto* ns = realmalloc((unsigned)size + 1);
  if(ns == nullptr)
    return error(OUT_OF_MEMORY, C_NIL);
  ns[size] = '\0';
  for(size = 0; s <= e; s++, size++)
  {
    ns[size] = *(str->stringval() + s - 1);
  }
  return mkstring(ns);
}

string::string()
{
  mkprim(PN_STRINGP, stringp, 1, SUBR);
  mkprim(PN_STREQ, streq, 2, SUBR);
  mkprim(PN_CONCAT, concat, -1, SUBR);
  mkprim(PN_STRLEN, xstrlen, 1, SUBR);
  mkprim(PN_SUBSTR, substr, 3, SUBR);
  mkprim(PN_SYMSTR, symstr, 1, SUBR);
  mkprim(PN_STRCMP, strcomp, 2, SUBR);
}

} // namespace lisp
