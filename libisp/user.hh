//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
class user
{
public:
  user();
  ~user() = default;
};

extern LISPT funeq(LISPT, LISPT);

} // namespace lisp
