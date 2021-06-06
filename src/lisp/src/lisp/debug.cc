/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "libisp.hh"

namespace lisp
{
debug::debug(): base() {}
debug::debug(lisp& lisp): base(lisp) {}

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

inline constexpr auto PN_EVALTRACE = "evaltrace"; // evaltrace

void debug::init()
{
  // clang-format off
  mkprim(PN_EVALTRACE, evaltrace, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format in
}

} // namespace lisp
