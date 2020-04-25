//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

namespace lisp
{

extern LISPT numberp(LISPT);
extern LISPT listp(LISPT);
extern LISPT memb(LISPT, LISPT);
extern LISPT equal(LISPT, LISPT);
extern LISPT nlistp(LISPT);
extern LISPT neq(LISPT, LISPT);
extern LISPT boundp(LISPT);
extern LISPT litatom(LISPT);
extern LISPT xtypeof(LISPT);

}
