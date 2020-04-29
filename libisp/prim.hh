//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
class prim
{
public:
  prim();
  ~prim() = default;
};

extern LISPT rplaca(LISPT, LISPT);
extern LISPT rplacd(LISPT, LISPT);
extern LISPT append(LISPT);
extern LISPT tconc(LISPT, LISPT);

} // namespace lisp
