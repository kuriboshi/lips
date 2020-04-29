//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

namespace lisp
{
class pred
{
public:
  pred();
  ~pred() = default;
};

extern LISPT memb(LISPT, LISPT);
extern LISPT equal(LISPT, LISPT);

} // namespace lisp
