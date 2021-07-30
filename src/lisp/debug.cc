/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include "debug.hh"
#include "alloc.hh"
#include "io.hh"
#include "eval.hh"

namespace lisp::debug
{
LISPT evaltrace(lisp& l, LISPT state)
{
  auto i = l.e().trace();

  if(!is_NIL(state))
  {
    check(state, type::INTEGER);
    l.e().trace(state->intval());
  }
  return mknumber(l, i);
}

namespace pn
{
inline constexpr auto EVALTRACE = "evaltrace"; // evaltrace
}

void init()
{
  // clang-format off
  mkprim(pn::EVALTRACE, evaltrace, subr_t::subr::EVAL, subr_t::spread::SPREAD);
  // clang-format in
}

}
