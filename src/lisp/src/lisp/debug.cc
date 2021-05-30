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
  // clang-format off
  mkprim(PN_EVALTRACE, evaltrace, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format in
}

} // namespace lisp
