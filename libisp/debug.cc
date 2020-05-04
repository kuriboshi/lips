/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <cerrno>

#include "libisp.hh"

using namespace lisp;

namespace lisp
{
PRIMITIVE evaltrace(lisp& l, LISPT state)
{
  auto i = l.e().trace();

  if(!is_NIL(state))
  {
    l.check(state, INTEGER);
    l.e().trace(state->intval());
  }
  return mknumber(l, i);
}

debug::debug(lisp& lisp): base(lisp) {}

void debug::init()
{
  alloc::mkprim(PN_EVALTRACE, evaltrace, 1, SUBR);
}

} // namespace lisp
