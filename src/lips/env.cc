//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//

#include "env.hh"

namespace lisp
{

env::env()
  : path(initcvar("path", mungepath())),
    home(initcvar("home", mkstring(get("HOME")))),
    globsort(initcvar("globsort", T))
{}

}
