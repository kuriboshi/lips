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
  base(lisp& lisp): l(lisp), a(lisp.a()), e(lisp.e()) {}
  virtual ~base() = default;

protected:
  void add_mark_object(LISPT* o) { a.add_mark_object(o); }
  lisp& l;
  alloc& a;
  evaluator& e;
};

} // namespace lisp
