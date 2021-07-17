//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
class posix: public base
{
public:
  posix();
  posix(lisp&);
  ~posix() = default;
  static void init();

  LISPT uxerrno();
  LISPT uxaccess(LISPT name, LISPT mode);
  LISPT uxalarm(LISPT seconds);
  LISPT uxchdir(LISPT dirname);
  LISPT uxchmod(LISPT name, LISPT mode);
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
inline LISPT uxerrno() { return posix().uxerrno(); }
inline LISPT uxaccess(lisp& l, LISPT name, LISPT mode) { return posix(l).uxaccess(name, mode); }
inline LISPT uxaccess(LISPT name, LISPT mode) { return posix().uxaccess(name, mode); }
inline LISPT uxalarm(lisp& l, LISPT seconds) { return posix(l).uxalarm(seconds); }
inline LISPT uxalarm(LISPT seconds) { return posix().uxalarm(seconds); }
inline LISPT uxchdir(lisp& l, LISPT dirname) { return posix(l).uxchdir(dirname); }
inline LISPT uxchdir(LISPT dirname) { return posix().uxchdir(dirname); }
inline LISPT uxchmod(lisp& l, LISPT name, LISPT mode) { return posix(l).uxchmod(name, mode); }
inline LISPT uxchmod(LISPT name, LISPT mode) { return posix().uxchmod(name, mode); }
inline LISPT uxcreat(lisp& l, LISPT name, LISPT mode) { return posix(l).uxcreat(name, mode); }
inline LISPT uxcreat(LISPT name, LISPT mode) { return posix().uxcreat(name, mode); }
inline LISPT uxdup(lisp& l, LISPT fildes) { return posix(l).uxdup(fildes); }
inline LISPT uxdup(LISPT fildes) { return posix().uxdup(fildes); }
inline LISPT uxgetuid(lisp& l) { return posix(l).uxgetuid(); }
inline LISPT uxgetuid() { return posix().uxgetuid(); }
inline LISPT uxgeteuid(lisp& l) { return posix(l).uxgeteuid(); }
inline LISPT uxgeteuid() { return posix().uxgeteuid(); }
inline LISPT uxgetgid(lisp& l) { return posix(l).uxgetgid(); }
inline LISPT uxgetgid() { return posix().uxgetgid(); }
inline LISPT uxgetegid(lisp& l) { return posix(l).uxgetegid(); }
inline LISPT uxgetegid() { return posix().uxgetegid(); }
inline LISPT uxgetpid(lisp& l) { return posix(l).uxgetpid(); }
inline LISPT uxgetpid() { return posix().uxgetpid(); }
inline LISPT uxkill(lisp& l, LISPT pid, LISPT sig) { return posix(l).uxkill(pid, sig); }
inline LISPT uxkill(LISPT pid, LISPT sig) { return posix().uxkill(pid, sig); }
inline LISPT uxlink(lisp& l, LISPT name1, LISPT name2) { return posix(l).uxlink(name1, name2); }
inline LISPT uxlink(LISPT name1, LISPT name2) { return posix().uxlink(name1, name2); }
inline LISPT uxnice(lisp& l, LISPT incr) { return posix(l).uxnice(incr); }
inline LISPT uxnice(LISPT incr) { return posix().uxnice(incr); }
inline LISPT uxsetuid(lisp& l, LISPT uid) { return posix(l).uxsetuid(uid); }
inline LISPT uxsetuid(LISPT uid) { return posix().uxsetuid(uid); }
inline LISPT uxsetgid(lisp& l, LISPT gid) { return posix(l).uxsetgid(gid); }
inline LISPT uxsetgid(LISPT gid) { return posix().uxsetgid(gid); }
#if 0
inline LISPT uxsignal(lisp& l, LISPT sig, LISPT fun) { return posix(l).uxsignal(sig, fun); }
inline LISPT uxsignal(LISPT sig, LISPT fun) { return posix().uxsignal(sig, fun); }
#endif
inline LISPT uxunlink(lisp& l, LISPT name) { return posix(l).uxunlink(name); }
inline LISPT uxunlink(LISPT name) { return posix().uxunlink(name); }

} // namespace lisp
