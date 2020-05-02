//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#include "libisp.hh"
#include "except.hh"

static int macro(lisp::lisp& l, lisp::LISPT*)
{
  l.e().unwind();
  throw lisp::lisp_reset();
}

int main()
{
  lisp::lisp lisp;
  lisp.repl(mkstring(lisp, "> "), [](lisp::lisp& lisp, lisp::LISPT*) -> int { return macro(lisp, nullptr); });
}
