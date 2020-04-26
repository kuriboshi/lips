//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"

namespace lisp
{
struct rtinfo
{
  unsigned char chclass[128];
  LISPT (*rmacros[128])(FILE*, LISPT, char);
};

/* variables */
extern LISPT top;
extern LISPT rstack;
extern int printlevel;
extern int thisplevel;
extern bool echoline;
extern struct rtinfo currentrt;

/* functions */
enum char_class
{
  SEPR = 001,   // seperator
  BRK = 002,    // break character
  INSERT = 004, // insert read macro
  SPLICE = 010, // splice read macro
  INFIX = 014,  // infix read macro
  RMACRO = 014  // read macro mask
};

inline bool issepr(int c) { return (currentrt.chclass[c] & SEPR) == SEPR; }
inline bool isbrk(int c) { return (currentrt.chclass[c] & BRK) == BRK; }
inline bool isrm(int c) { return (currentrt.chclass[c] & RMACRO) == RMACRO; }
inline bool isinsert(int c) { return (currentrt.chclass[c] & RMACRO) == INSERT; }
inline bool issplice(int c) { return (currentrt.chclass[c] & RMACRO) == SPLICE; }
inline bool isinfix(int c) { return (currentrt.chclass[c] & RMACRO) == INFIX; }

extern LISPT ratom(FILE*);
extern LISPT lispread(FILE*, int);
extern LISPT readline(FILE*);
extern LISPT patom(LISPT, FILE*, int);
extern LISPT terpri(FILE*);
extern LISPT prinbody(LISPT, FILE*, int);
extern LISPT prin0(LISPT, FILE*, int);
extern LISPT print(LISPT, FILE*);

} // namespace lisp
