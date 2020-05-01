//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class unix : public base
{
public:
  unix(lisp&);
  ~unix() = default;
  static void init();

  LISPT uxerrno();
  LISPT uxaccess(LISPT name, LISPT mode);
  LISPT uxalarm(LISPT seconds);
  LISPT uxchdir(LISPT dirname);
  LISPT uxchmod(LISPT name, LISPT mode);
  LISPT uxclose(LISPT fildes);
  LISPT uxcreat(LISPT name, LISPT mode);
  LISPT uxdup(LISPT fildes);
  LISPT uxgetuid();
  LISPT uxgeteuid();
  LISPT uxgetgid();
  LISPT uxgetegid();
  LISPT uxgetpid();
  LISPT uxkill(LISPT pid, LISPT sig);
  LISPT uxlink(LISPT name1, LISPT name2);
  LISPT uxnice(LISPT incr);
  LISPT uxopen(LISPT name, LISPT mode);
  LISPT uxsetuid(LISPT uid);
  LISPT uxsetgid(LISPT gid);
#if 0
  LISPT uxsignal(LISPT sig, LISPT fun);
#endif
  LISPT uxunlink(LISPT name);

#if 0
private:
  static LISPT sighandler[];
  static void sighandle(int);
#endif
};

inline LISPT uxerrno(lisp& l) { return unix(l).uxerrno(); }
inline LISPT uxaccess(lisp& l, LISPT name, LISPT mode) { return unix(l).uxaccess(name, mode); }
inline LISPT uxalarm(lisp& l, LISPT seconds) { return unix(l).uxalarm(seconds); }
inline LISPT uxchdir(lisp& l, LISPT dirname) { return unix(l).uxchdir(dirname); }
inline LISPT uxchmod(lisp& l, LISPT name, LISPT mode) { return unix(l).uxchmod(name, mode); }
inline LISPT uxclose(lisp& l, LISPT fildes) { return unix(l).uxclose(fildes); }
inline LISPT uxcreat(lisp& l, LISPT name, LISPT mode) { return unix(l).uxcreat(name, mode); }
inline LISPT uxdup(lisp& l, LISPT fildes) { return unix(l).uxdup(fildes); }
inline LISPT uxgetuid(lisp& l) { return unix(l).uxgetuid(); }
inline LISPT uxgeteuid(lisp& l) { return unix(l).uxgeteuid(); }
inline LISPT uxgetgid(lisp& l) { return unix(l).uxgetgid(); }
inline LISPT uxgetegid(lisp& l) { return unix(l).uxgetegid(); }
inline LISPT uxgetpid(lisp& l) { return unix(l).uxgetpid(); }
inline LISPT uxkill(lisp& l, LISPT pid, LISPT sig) { return unix(l).uxkill(pid, sig); }
inline LISPT uxlink(lisp& l, LISPT name1, LISPT name2) { return unix(l).uxlink(name1, name2); }
inline LISPT uxnice(lisp& l, LISPT incr) { return unix(l).uxnice(incr); }
inline LISPT uxopen(lisp& l, LISPT name, LISPT mode) { return unix(l).uxopen(name, mode); }
inline LISPT uxsetuid(lisp& l, LISPT uid) { return unix(l).uxsetuid(uid); }
inline LISPT uxsetgid(lisp& l, LISPT gid) { return unix(l).uxsetgid(gid); }
#if 0
inline LISPT uxsignal(lisp& l, LISPT sig, LISPT fun) { return unix(l).uxsignal(sig, fun); }
#endif
inline LISPT uxunlink(lisp& l, LISPT name) { return unix(l).uxunlink(name); }

} // namespace lisp
