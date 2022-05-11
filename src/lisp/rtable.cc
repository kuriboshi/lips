//
// Lips, lisp shell.
// Copyright 1988-1989, 2022.
//
#include <cstdlib>
#include "rtable.hh"

namespace lisp::rtable
{
//
// Read macros.
//
LISPT dquote(lisp& l, LISPT stream)
{
  check(stream, type::FILET);
  std::string buffer;
  auto c = stream->file()->getch();
  while(c != '"')
  {
    if(c == '\\')
      c = stream->file()->getch();
    buffer.push_back(c);
    c = stream->file()->getch();
  }
  return mkstring(l, buffer);
}

LISPT squote(lisp& l, LISPT stream)
{
  check(stream, type::FILET);
  int c;
  if((c = stream->file()->getch()) == ')' /*|| is_sepr(l, c)*/)
  {
    stream->file()->ungetch(c);
    return C_QUOTE;
  }
  stream->file()->ungetch(c);
  return cons(l, C_QUOTE, cons(l, io::lispread(l, stream->file()), NIL));
}

LISPT getenv(lisp&, LISPT stream)
{
  check(stream, type::FILET);
  auto sym = ratom(stream->file());
  check(sym, type::SYMBOL, type::STRING);
  auto val = std::getenv(sym->getstr().c_str());
  return mkstring(val);
}

namespace pn
{
inline constexpr auto GETENV = "rmgetenv";
}

void init()
{
  mkprim(pn::GETENV, getenv, subr_t::subr::EVAL, subr_t::spread::SPREAD);
}
} // namespace lisp::rtable
