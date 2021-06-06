//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
class base
{
public:
  base(): l(lisp::current()), a(l.a()), e(l.e()) {}
  base(lisp& lisp): l(lisp), a(lisp.a()), e(lisp.e()) {}
  virtual ~base() = default;

protected:
  lisp& l;
  alloc& a;
  evaluator& e;
};

} // namespace lisp
