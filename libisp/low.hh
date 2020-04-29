//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
/* variables */
extern LISPT verboseflg;

class low
{
public:
  low();
  ~low() = default;
};

extern LISPT progn(LISPT);

} // namespace lisp
