//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#include "libisp.hh"

namespace lisp
{

lisp::lisp() : _eval(*new evaluator(*this)),
               _alloc(*new alloc(*this))
{}

lisp::~lisp()
{}

} // namespace lisp
