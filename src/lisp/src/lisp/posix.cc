/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <cerrno>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>

#include "libisp.hh"

namespace lisp
{
posix::posix(): base() {}
posix::posix(lisp& lisp): base(lisp) {}

PRIMITIVE posix::uxerrno() { return mknumber(l, errno); }

PRIMITIVE posix::uxaccess(LISPT name, LISPT mode)
{
  l.check(name, lisp_type::STRING);
  l.check(mode, lisp_type::INTEGER);
  return mknumber(l, access(name->stringval().c_str(), mode->intval()));
}

PRIMITIVE posix::uxalarm(LISPT seconds)
{
  l.check(seconds, lisp_type::INTEGER);
  return mknumber(l, alarm(seconds->intval()));
}

PRIMITIVE posix::uxchdir(LISPT dirname)
{
  l.check(dirname, lisp_type::STRING);
  return mknumber(l, chdir(dirname->stringval().c_str()));
}

PRIMITIVE posix::uxchmod(LISPT name, LISPT mode)
{
  l.check(name, lisp_type::STRING);
  l.check(mode, lisp_type::INTEGER);
  return mknumber(l, chmod(name->stringval().c_str(), mode->intval()));
}

PRIMITIVE posix::uxclose(LISPT fildes)
{
  l.check(fildes, lisp_type::FILET);
  if(fildes->fileval().close())
    return C_T;
  return C_NIL;
}

PRIMITIVE posix::uxcreat(LISPT name, LISPT mode)
{
  int i;

  l.check(name, lisp_type::STRING);
  l.check(mode, lisp_type::INTEGER);
  i = creat(name->stringval().c_str(), mode->intval());
  if(i < 0)
    return C_NIL;
  else
    return mknumber(l, i);
}

PRIMITIVE posix::uxdup(LISPT fildes)
{
  l.check(fildes, lisp_type::INTEGER);
  return mknumber(l, dup(fildes->intval()));
}

PRIMITIVE posix::uxgetuid() { return mknumber(l, getuid()); }

PRIMITIVE posix::uxgeteuid() { return mknumber(l, geteuid()); }

PRIMITIVE posix::uxgetgid() { return mknumber(l, getgid()); }

PRIMITIVE posix::uxgetegid() { return mknumber(l, getegid()); }

PRIMITIVE posix::uxgetpid() { return mknumber(l, getpid()); }

PRIMITIVE posix::uxkill(LISPT pid, LISPT sig)
{
  l.check(pid, lisp_type::INTEGER);
  l.check(sig, lisp_type::INTEGER);
  return mknumber(l, kill(pid->intval(), sig->intval()));
}

PRIMITIVE posix::uxlink(LISPT name1, LISPT name2)
{
  l.check(name1, lisp_type::STRING);
  l.check(name2, lisp_type::STRING);
  return mknumber(l, link(name1->stringval().c_str(), name2->stringval().c_str()));
}

PRIMITIVE posix::uxnice(LISPT incr)
{
  l.check(incr, lisp_type::INTEGER);
  return mknumber(l, nice(incr->intval()));
}

PRIMITIVE posix::uxopen(LISPT name, LISPT mode)
{
  bool readmode = true;
  bool appendmode = false;

  l.check(name, lisp_type::STRING);
  if(!is_NIL(mode))
  {
    l.check(mode, lisp_type::SYMBOL);
    if(EQ(mode, C_READ))
      readmode = true;
    else if(EQ(mode, C_WRITE))
      readmode = false;
    else if(EQ(mode, C_APPEND))
    {
      readmode = false;
      appendmode = true;
    }
    else
      return l.error(UNKNOWN_REQUEST, mode);
  }
  auto f = readmode ? std::make_unique<file_t>(std::make_unique<file_source>(name->stringval()))
    : std::make_unique<file_t>(std::make_unique<file_sink>(name->stringval().c_str(), appendmode));
  if(!f)
    return l.error(CANT_OPEN, name);
  LISPT newfile = nullptr;
  set(newfile, lisp_type::FILET, getobject(l));
  newfile->fileval(std::move(f));
  return newfile;
}

PRIMITIVE posix::uxsetuid(LISPT uid)
{
  l.check(uid, lisp_type::INTEGER);
  return mknumber(l, setuid(uid->intval()));
}

PRIMITIVE posix::uxsetgid(LISPT gid)
{
  l.check(gid, lisp_type::INTEGER);
  return mknumber(l, setgid(gid->intval()));
}

#if 0
LISPT posix::sighandler[];
void posix::sighandle(int sig) { eval(_lisp, sighandler[sig]); }

PRIMITIVE posix::uxsignal(LISPT sig, LISPT fun)
{
  check(sig, INTEGER);

  if(sig->intval() >= NSIG || sig->intval() < 1)
    return error(ILLEGAL_SIGNAL, sig);
  if(is_NIL(fun))
  {
    signal(sig->intval(), SIG_IGN);
    sighandler[sig->intval()] = C_NIL;
  }
  else if(is_T(fun))
  {
    signal(sig->intval(), SIG_DFL);
    sighandler[sig->intval()] = C_NIL;
  }
  else
  {
    sighandler[sig->intval()] = fun;
    signal(sig->intval(), sighandle);
  }
  return C_T;
}
#endif

PRIMITIVE posix::uxunlink(LISPT name)
{
  l.check(name, lisp_type::STRING);
  return mknumber(l, unlink(name->stringval().c_str()));
}

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

void posix::init()
{
  // clang-format off
  mkprim(PN_UXACCESS,  ::lisp::uxaccess,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXALARM,   ::lisp::uxalarm,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXCHDIR,   ::lisp::uxchdir,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXCHMOD,   ::lisp::uxchmod,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXCLOSE,   ::lisp::uxclose,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXCREAT,   ::lisp::uxcreat,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXDUP,     ::lisp::uxdup,     subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXERRNO,   ::lisp::uxerrno,   subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXGETUID,  ::lisp::uxgetuid,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXGETEUID, ::lisp::uxgeteuid, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXGETGID,  ::lisp::uxgetgid,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXGETEGID, ::lisp::uxgetegid, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXGETPID,  ::lisp::uxgetpid,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXKILL,    ::lisp::uxkill,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXLINK,    ::lisp::uxlink,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXNICE,    ::lisp::uxnice,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXOPEN,    ::lisp::uxopen,    subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXSETUID,  ::lisp::uxsetuid,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_UXSETGID,  ::lisp::uxsetgid,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
#if 0
  mkprim(PN_SIGNAL,    ::lisp::uxsignal,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
#endif
  mkprim(PN_UXUNLINK,  ::lisp::uxunlink,  subr_t::S_EVAL, subr_t::S_NOSPREAD);
  // clang-format on
}

} // namespace lisp
