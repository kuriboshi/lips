//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class posix : public base
{
public:
  posix(lisp&);
  ~posix() = default;
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

inline LISPT uxerrno(lisp& l) { return posix(l).uxerrno(); }
inline LISPT uxaccess(lisp& l, LISPT name, LISPT mode) { return posix(l).uxaccess(name, mode); }
inline LISPT uxalarm(lisp& l, LISPT seconds) { return posix(l).uxalarm(seconds); }
inline LISPT uxchdir(lisp& l, LISPT dirname) { return posix(l).uxchdir(dirname); }
inline LISPT uxchmod(lisp& l, LISPT name, LISPT mode) { return posix(l).uxchmod(name, mode); }
inline LISPT uxclose(lisp& l, LISPT fildes) { return posix(l).uxclose(fildes); }
inline LISPT uxcreat(lisp& l, LISPT name, LISPT mode) { return posix(l).uxcreat(name, mode); }
inline LISPT uxdup(lisp& l, LISPT fildes) { return posix(l).uxdup(fildes); }
inline LISPT uxgetuid(lisp& l) { return posix(l).uxgetuid(); }
inline LISPT uxgeteuid(lisp& l) { return posix(l).uxgeteuid(); }
inline LISPT uxgetgid(lisp& l) { return posix(l).uxgetgid(); }
inline LISPT uxgetegid(lisp& l) { return posix(l).uxgetegid(); }
inline LISPT uxgetpid(lisp& l) { return posix(l).uxgetpid(); }
inline LISPT uxkill(lisp& l, LISPT pid, LISPT sig) { return posix(l).uxkill(pid, sig); }
inline LISPT uxlink(lisp& l, LISPT name1, LISPT name2) { return posix(l).uxlink(name1, name2); }
inline LISPT uxnice(lisp& l, LISPT incr) { return posix(l).uxnice(incr); }
inline LISPT uxopen(lisp& l, LISPT name, LISPT mode) { return posix(l).uxopen(name, mode); }
inline LISPT uxsetuid(lisp& l, LISPT uid) { return posix(l).uxsetuid(uid); }
inline LISPT uxsetgid(lisp& l, LISPT gid) { return posix(l).uxsetgid(gid); }
#if 0
inline LISPT uxsignal(lisp& l, LISPT sig, LISPT fun) { return posix(l).uxsignal(sig, fun); }
#endif
inline LISPT uxunlink(lisp& l, LISPT name) { return posix(l).uxunlink(name); }

} // namespace lisp
