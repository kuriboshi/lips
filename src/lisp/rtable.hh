//
// Lips, lisp shell.
// Copyright 1988-1989,2022 Krister Joas
//

#include "lisp.hh"

namespace lisp::rm
{
void init();

LISPT dquote(lisp&, LISPT);
LISPT squote(lisp&, LISPT);
LISPT getenv(lisp&, LISPT);
} // namespace lisp::rm

namespace lisp
{
inline LISPT getenv(lisp& l, LISPT stream) { return rm::getenv(l, stream); }
} // namespace lisp
