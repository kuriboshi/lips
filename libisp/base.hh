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
  base(lisp& lisp): _lisp(lisp) {}
  virtual ~base() = default;

protected:
  void add_mark_object(LISPT* o) { a().add_mark_object(o); }
  void mkprim(const char* pname, LISPT (*fname)(lisp&), short nrpar, lisp_type type)
  {
    a().mkprim(pname, fname, nrpar, type);
  }
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT), short nrpar, lisp_type type)
  {
    a().mkprim(pname, fname, nrpar, type);
  }
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT), short nrpar, lisp_type type)
  {
    a().mkprim(pname, fname, nrpar, type);
  }
  void mkprim(const char* pname, LISPT (*fname)(lisp&, LISPT, LISPT, LISPT), short nrpar, lisp_type type)
  {
    a().mkprim(pname, fname, nrpar, type);
  }
  lisp& l() const { return _lisp; }
  alloc& a() const { return _lisp.a(); }
  lisp& _lisp;
};

} // namespace lisp
