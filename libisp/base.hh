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
  void mkprim(const char* pname, LISPT (*fname)(lisp&), short nrpar, lisp_type type)
  {
    a.mkprim(pname, fname, nrpar, type);
  }
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT), short nrpar, lisp_type type)
  {
    a.mkprim(pname, fname, nrpar, type);
  }
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT), short nrpar, lisp_type type)
  {
    a.mkprim(pname, fname, nrpar, type);
  }
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT, LISPT), short nrpar, lisp_type type)
  {
    a.mkprim(pname, fname, nrpar, type);
  }
  lisp& l;
  alloc& a;
  evaluator& e;
};

} // namespace lisp
