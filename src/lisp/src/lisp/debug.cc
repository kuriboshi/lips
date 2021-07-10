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
    l.check(state, type::INTEGER);
    l.e().trace(state->intval());
  }
  return mknumber(l, i);
}

namespace pn
{
inline constexpr auto EVALTRACE = "evaltrace"; // evaltrace
}

void debug::init()
{
  // clang-format off
  mkprim(pn::EVALTRACE, evaltrace, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  // clang-format in
}

} // namespace lisp
