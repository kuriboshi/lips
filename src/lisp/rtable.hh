//
// Lips, lisp shell.
// Copyright 1988-1989,2022 Krister Joas
//

#ifndef LISP_RTABLE_HH
#define LISP_RTABLE_HH

#include <array>
#include <vector>
#include "lisp.hh"

namespace lisp
{
class lisp_t;
using LISPT = ref_ptr<lisp_t>;
class file_t;
using ref_file_t = ref_ptr<file_t>;
class lisp;
}

namespace lisp::rtable
{
void init();

LISPT dquote(lisp&, LISPT);
LISPT squote(lisp&, LISPT);
LISPT getenv(lisp&, LISPT);
} // namespace lisp::rtable

#endif
