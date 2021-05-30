/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
/* Return symbols print name as a string. */
PRIMITIVE string::symstr(LISPT sym)
{
  l.check(sym, SYMBOL);
  return mkstring(l, sym->symval().pname);
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
  l.check(s1, STRING);
  l.check(s2, STRING);
  if(s1->stringval() == s2->stringval())
    return C_T;
  return C_NIL;
}

PRIMITIVE string::strcomp(LISPT s1, LISPT s2)
{
  l.check(s1, STRING);
  l.check(s2, STRING);
  return mknumber(l, s1->stringval().compare(s2->stringval()));
}

/* Concatenate arbitrary many strings to
   to one string */
PRIMITIVE string::concat(LISPT strlist)
{
  std::string result;
  for(auto sl = strlist; !is_NIL(sl); sl = sl->cdr())
  {
    l.check(sl->car(), STRING);
    result += sl->car()->stringval();
  }
  return mkstring(l, result);
}

/* Return string length of s */
PRIMITIVE string::strlen(LISPT s)
{
  l.check(s, STRING);
  return mknumber(l, s->stringval().length());
}

/* Extract a substring from start to end.
   If start or end is out of bounds, return
   NIL. If end is one less than start the
   zero length string is returned. end equal
   to zero if start is equal to one is accepted. */
PRIMITIVE string::substr(LISPT str, LISPT start, LISPT end)
{
  l.check(str, STRING);
  l.check(start, INTEGER);
  l.check(end, INTEGER);
  auto s = start->intval();
  auto e = end->intval();
  auto size = e - s + 1;
  if(size < 0 || s > str->stringval().length()
    || e > str->stringval().length() || s <= 0 || e < 0)
    return C_NIL;
  return mkstring(l, str->stringval().substr(s, e));
}

string::string(lisp& lisp): base(lisp) {}

void string::init()
{
  // clang-format off
  mkprim(PN_STRINGP, ::lisp::stringp, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_STREQ,   ::lisp::streq,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_CONCAT,  ::lisp::concat,  subr_t::S_EVAL, subr_t::S_SPREAD);
  mkprim(PN_STRLEN,  ::lisp::strlen,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_SUBSTR,  ::lisp::substr,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_SYMSTR,  ::lisp::symstr,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_STRCMP,  ::lisp::strcomp, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format on
}

} // namespace lisp
