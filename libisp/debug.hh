//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_EVALTRACE = "evaltrace"; // evaltrace

class debug: public base
{
public:
  debug(lisp&);
  ~debug() = default;
  static void init();
};

} // namespace lisp
