//
// Lips, lisp shell.
// Copyright 1988-1989,2022 Krister Joas
//

#include "lisp.hh"

namespace lisp::rm
{
LISPT dquote(lisp&, file_t&, LISPT, char);
LISPT squote(lisp&, file_t&, LISPT, char);
} // namespace lisp::rm
