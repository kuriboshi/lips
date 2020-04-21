/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */

#include <cstring>

#include "libisp.hh"

/* Return symbols print name as a string. */
PRIMITIVE symstr(LISPT sym)
{
  CHECK(sym, SYMBOL);
  return mkstring(SYMVAL(sym).pname);
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
  CHECK(s1, STRING);
  CHECK(s2, STRING);
  if(!strcmp(STRINGVAL(s1), STRINGVAL(s2)))
    return C_T;
  return C_NIL;
}

PRIMITIVE strcomp(LISPT s1, LISPT s2)
{
  CHECK(s1, STRING);
  CHECK(s2, STRING);
  return mknumber(strcmp(STRINGVAL(s1), STRINGVAL(s2)));
}

/* Concatenate arbitrary many strings to
   to one string */
PRIMITIVE concat(LISPT strlist)
{
  LISPT sl;
  int size;
  char* ns;

  size = 0;
  for(sl = strlist; !ISNIL(sl); sl = CDR(sl))
  {
    CHECK(CAR(sl), STRING);
    size += strlen(STRINGVAL(CAR(sl)));
  }
  ns = (char*)safemalloc((unsigned)size + 1);
  if(ns == nullptr)
    return error(OUT_OF_MEMORY, C_NIL);
  ns[0] = '\0';
  while(!ISNIL(strlist))
  {
    strcat(ns, STRINGVAL(CAR(strlist)));
    strlist = CDR(strlist);
  }
  return mkstring(ns);
}

/* Return string length of s */
PRIMITIVE xstrlen(LISPT s)
{
  CHECK(s, STRING);
  return mknumber((long)strlen(STRINGVAL(s)));
}

/* Extract a substring from start to end.
   If start or end is out of bounds, return
   NIL. If end is one less than start the
   zero length string is returned. end equal
   to zero if start is equal to one is accepted. */
PRIMITIVE substr(LISPT str, LISPT start, LISPT end)
{
  int size;
  int s, e;
  char* ns;

  CHECK(str, STRING);
  CHECK(start, INTEGER);
  CHECK(end, INTEGER);
  s = INTVAL(start);
  e = INTVAL(end);
  size = e - s + 1;
  if(size < 0 || s > static_cast<int>(strlen(STRINGVAL(str))) || e > static_cast<int>(strlen(STRINGVAL(str))) || s <= 0
    || e < 0)
    return C_NIL;
  ns = (char*)safemalloc((unsigned)size + 1);
  if(ns == nullptr)
    return error(OUT_OF_MEMORY, C_NIL);
  ns[size] = '\0';
  for(size = 0; s <= e; s++, size++)
  {
    ns[size] = *(STRINGVAL(str) + s - 1);
  }
  return mkstring(ns);
}

void init_string()
{
  mkprim(PN_STRINGP, stringp, 1, SUBR);
  mkprim(PN_STREQ, streq, 2, SUBR);
  mkprim(PN_CONCAT, concat, -1, SUBR);
  mkprim(PN_STRLEN, xstrlen, 1, SUBR);
  mkprim(PN_SUBSTR, substr, 3, SUBR);
  mkprim(PN_SYMSTR, symstr, 1, SUBR);
  mkprim(PN_STRCMP, strcomp, 2, SUBR);
}
