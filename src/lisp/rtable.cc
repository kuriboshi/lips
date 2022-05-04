//
// Lips, lisp shell.
// Copyright 1988-1989, 2022.
//
#include "rtable.hh"

namespace lisp::rm
{
//
// Read macros.
//
LISPT dquote(lisp& l, file_t& file, LISPT, char)
{
  std::string buffer;
  auto c = file.getch();
  while(c != '"')
  {
    if(c == '\\')
      c = file.getch();
    buffer.push_back(c);
    c = file.getch();
  }
  return mkstring(l, buffer);
}

LISPT squote(lisp& l, file_t& file, LISPT, char)
{
  int c;
  if((c = file.getch()) == ')' || is_sepr(l, c))
  {
    file.ungetch(c);
    return C_QUOTE;
  }
  file.ungetch(c);
  return cons(l, C_QUOTE, cons(l, io::lispread(l, file), NIL));
}
} // namespace lisp::rm
