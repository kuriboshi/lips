//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#pragma once

#include "lisp.hh"
#include "base.hh"

namespace lisp
{
inline constexpr auto PN_UXACCESS = "access";   // check file access
inline constexpr auto PN_UXALARM = "alarm";     // set alarm clock
inline constexpr auto PN_UXCHDIR = "chdir";     // change directory
inline constexpr auto PN_UXCHMOD = "chmode";    // change mode of file
inline constexpr auto PN_UXCLOSE = "close";     // close file
inline constexpr auto PN_UXCREAT = "creat";     // create file
inline constexpr auto PN_UXDUP = "dup";         // duplicate fileno
inline constexpr auto PN_UXERRNO = "errno";     // return latest error
inline constexpr auto PN_UXGETUID = "getuid";   // get user id
inline constexpr auto PN_UXGETEUID = "geteuid"; // get effective user id
inline constexpr auto PN_UXGETGID = "getgid";   // set group id
inline constexpr auto PN_UXGETEGID = "getegid"; // get effective group id
inline constexpr auto PN_UXGETPID = "getpid";   // get process id
inline constexpr auto PN_UXKILL = "killproc";   // kill process
inline constexpr auto PN_UXLINK = "link";       // link file
inline constexpr auto PN_UXNICE = "setnice";    // set nice
inline constexpr auto PN_UXOPEN = "open";       // open file
inline constexpr auto PN_UXSETUID = "setuid";   // set user id
inline constexpr auto PN_UXSETGID = "setgid";   // set group id
inline constexpr auto PN_SIGNAL = "signal";     // install signal handler
inline constexpr auto PN_UXUNLINK = "unlink";   // unlink file

class posix: public base
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
inline LISPT uxerrno() { return posix(lisp::current()).uxerrno(); }
inline LISPT uxaccess(lisp& l, LISPT name, LISPT mode) { return posix(l).uxaccess(name, mode); }
inline LISPT uxaccess(LISPT name, LISPT mode) { return posix(lisp::current()).uxaccess(name, mode); }
inline LISPT uxalarm(lisp& l, LISPT seconds) { return posix(l).uxalarm(seconds); }
inline LISPT uxalarm(LISPT seconds) { return posix(lisp::current()).uxalarm(seconds); }
inline LISPT uxchdir(lisp& l, LISPT dirname) { return posix(l).uxchdir(dirname); }
inline LISPT uxchdir(LISPT dirname) { return posix(lisp::current()).uxchdir(dirname); }
inline LISPT uxchmod(lisp& l, LISPT name, LISPT mode) { return posix(l).uxchmod(name, mode); }
inline LISPT uxchmod(LISPT name, LISPT mode) { return posix(lisp::current()).uxchmod(name, mode); }
inline LISPT uxclose(lisp& l, LISPT fildes) { return posix(l).uxclose(fildes); }
inline LISPT uxclose(LISPT fildes) { return posix(lisp::current()).uxclose(fildes); }
inline LISPT uxcreat(lisp& l, LISPT name, LISPT mode) { return posix(l).uxcreat(name, mode); }
inline LISPT uxcreat(LISPT name, LISPT mode) { return posix(lisp::current()).uxcreat(name, mode); }
inline LISPT uxdup(lisp& l, LISPT fildes) { return posix(l).uxdup(fildes); }
inline LISPT uxdup(LISPT fildes) { return posix(lisp::current()).uxdup(fildes); }
inline LISPT uxgetuid(lisp& l) { return posix(l).uxgetuid(); }
inline LISPT uxgetuid() { return posix(lisp::current()).uxgetuid(); }
inline LISPT uxgeteuid(lisp& l) { return posix(l).uxgeteuid(); }
inline LISPT uxgeteuid() { return posix(lisp::current()).uxgeteuid(); }
inline LISPT uxgetgid(lisp& l) { return posix(l).uxgetgid(); }
inline LISPT uxgetgid() { return posix(lisp::current()).uxgetgid(); }
inline LISPT uxgetegid(lisp& l) { return posix(l).uxgetegid(); }
inline LISPT uxgetegid() { return posix(lisp::current()).uxgetegid(); }
inline LISPT uxgetpid(lisp& l) { return posix(l).uxgetpid(); }
inline LISPT uxgetpid() { return posix(lisp::current()).uxgetpid(); }
inline LISPT uxkill(lisp& l, LISPT pid, LISPT sig) { return posix(l).uxkill(pid, sig); }
inline LISPT uxkill(LISPT pid, LISPT sig) { return posix(lisp::current()).uxkill(pid, sig); }
inline LISPT uxlink(lisp& l, LISPT name1, LISPT name2) { return posix(l).uxlink(name1, name2); }
inline LISPT uxlink(LISPT name1, LISPT name2) { return posix(lisp::current()).uxlink(name1, name2); }
inline LISPT uxnice(lisp& l, LISPT incr) { return posix(l).uxnice(incr); }
inline LISPT uxnice(LISPT incr) { return posix(lisp::current()).uxnice(incr); }
inline LISPT uxopen(lisp& l, LISPT name, LISPT mode) { return posix(l).uxopen(name, mode); }
inline LISPT uxopen(LISPT name, LISPT mode) { return posix(lisp::current()).uxopen(name, mode); }
inline LISPT uxsetuid(lisp& l, LISPT uid) { return posix(l).uxsetuid(uid); }
inline LISPT uxsetuid(LISPT uid) { return posix(lisp::current()).uxsetuid(uid); }
inline LISPT uxsetgid(lisp& l, LISPT gid) { return posix(l).uxsetgid(gid); }
inline LISPT uxsetgid(LISPT gid) { return posix(lisp::current()).uxsetgid(gid); }
#if 0
inline LISPT uxsignal(lisp& l, LISPT sig, LISPT fun) { return posix(l).uxsignal(sig, fun); }
inline LISPT uxsignal(LISPT sig, LISPT fun) { return posix(lisp::current()).uxsignal(sig, fun); }
#endif
inline LISPT uxunlink(lisp& l, LISPT name) { return posix(l).uxunlink(name); }
inline LISPT uxunlink(LISPT name) { return posix(lisp::current()).uxunlink(name); }

} // namespace lisp
