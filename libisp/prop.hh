//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

namespace lisp {

extern LISPT setplist(LISPT, LISPT);
extern LISPT getplist(LISPT);
extern LISPT putprop(LISPT, LISPT, LISPT);
extern LISPT getprop(LISPT, LISPT);
extern LISPT remprop(LISPT, LISPT);

}
