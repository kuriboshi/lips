//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//

#include "env.hh"

namespace lisp
{

environment::environment()
  : path(initcvar("path", mungepath(getenv("PATH")))),
    home(initcvar("home", mkstring(getenv("HOME")))),
    globsort(initcvar("globsort", T))
{}

}
