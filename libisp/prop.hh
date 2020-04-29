//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

namespace lisp
{

class prop
{
public:
  prop();
  ~prop() = default;
};

extern LISPT putprop(LISPT, LISPT, LISPT);
extern LISPT getprop(LISPT, LISPT);

} // namespace lisp
