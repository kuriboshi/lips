//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "source.hh"
#include "sink.hh"

namespace lisp
{
class read
{
public:
  read(lisp& lisp) : _lisp(lisp) {}
  ~read() = default;

  LISPT top = nullptr;             /* used for threading the input structure */
  LISPT rstack = nullptr;          /* partially built structure read stack */
  int printlevel = 0;    /* maximum print level */
  int thisplevel = 0;        /* during print, print level */
  static bool echoline; /* is true if ! has been used */

  void pushr(LISPT w) { rstack = cons(_lisp, w, rstack); }
  void popr(LISPT& w)
  {
    w = rstack->car();
    rstack = rstack->cdr();
  }

  static constexpr int NUL = '\0';
  static constexpr int MAXATOMSIZE = 128; /* max length of atom read can handle */

  static char buf[MAXATOMSIZE];

  bool integerp(char*, int* res);
  bool floatp(char*);
  LISPT parsebuf(char*);
  LISPT ratom(source*);
  LISPT splice(LISPT c, LISPT, int tailp);
  LISPT lispread(source*, int line);
  static LISPT rmexcl(read&, source*, LISPT, char);
  static LISPT rmdquote(read&, source*, LISPT, char);
  static LISPT rmsquote(read&, source*, LISPT, char);
  static LISPT rmpipe(read&, source*, LISPT, char);
  static LISPT rmredir(read&, source*, LISPT, char);
  static LISPT rmbg(read&, source*, LISPT, char);
  static LISPT rmuser(read&, source*, LISPT, char);

#if 0
  static LISPT userreadmacros[128];
#endif

  LISPT readline(source*);
  LISPT patom(LISPT, sink*, int esc);
  LISPT terpri(sink*);
  LISPT prinbody(LISPT, sink*, int esc);
  LISPT prin0(LISPT, sink*, int esc);
  LISPT print(LISPT, sink*);

private:
  alloc& a() { return _lisp.a(); }
  lisp& _lisp;
};

struct rtinfo
{
  unsigned char chclass[128];
  LISPT (*rmacros[128])(read&, source*, LISPT, char);
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

inline LISPT ratom(lisp& l, source* f) { return read(l).ratom(f); }
inline LISPT lispread(lisp& l, source* f, int i) { return read(l).lispread(f, i); }
inline LISPT readline(lisp& l, source* f) { return read(l).readline(f); }
inline LISPT patom(lisp& l, LISPT a, sink* f, int i) { return read(l).patom(a, f, i); }
inline LISPT terpri(lisp& l, sink* f) { return read(l).terpri(f); }
inline LISPT prinbody(lisp& l, LISPT a, sink* f, int i) { return read(l).prinbody(a, f, i); }
inline LISPT prin0(lisp& l, LISPT a, sink* f, int i) { return read(l).prin0(a, f, i); }
inline LISPT print(lisp& l, LISPT a, sink* f) { return read(l).print(a, f); }

} // namespace lisp
