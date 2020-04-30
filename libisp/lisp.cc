//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#include "libisp.hh"

namespace lisp
{

lisp::lisp() : 
  _alloc(*new alloc(*this)),
  _eval(*new evaluator(*this))
{
}

lisp::~lisp()
{}

} // namespace lisp
