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
  l.check(name, type::STRING);
  l.check(mode, type::INTEGER);
  return mknumber(l, access(name->stringval().c_str(), mode->intval()));
}

PRIMITIVE posix::uxalarm(LISPT seconds)
{
  l.check(seconds, type::INTEGER);
  return mknumber(l, alarm(seconds->intval()));
}

PRIMITIVE posix::uxchdir(LISPT dirname)
{
  l.check(dirname, type::STRING);
  return mknumber(l, chdir(dirname->stringval().c_str()));
}

PRIMITIVE posix::uxchmod(LISPT name, LISPT mode)
{
  l.check(name, type::STRING);
  l.check(mode, type::INTEGER);
  return mknumber(l, chmod(name->stringval().c_str(), mode->intval()));
}

PRIMITIVE posix::uxclose(LISPT fildes)
{
  l.check(fildes, type::FILET);
  if(fildes->fileval().close())
    return C_T;
  return NIL;
}

PRIMITIVE posix::uxcreat(LISPT name, LISPT mode)
{
  int i;

  l.check(name, type::STRING);
  l.check(mode, type::INTEGER);
  i = creat(name->stringval().c_str(), mode->intval());
  if(i < 0)
    return NIL;
  else
    return mknumber(l, i);
}

PRIMITIVE posix::uxdup(LISPT fildes)
{
  l.check(fildes, type::INTEGER);
  return mknumber(l, dup(fildes->intval()));
}

PRIMITIVE posix::uxgetuid() { return mknumber(l, getuid()); }

PRIMITIVE posix::uxgeteuid() { return mknumber(l, geteuid()); }

PRIMITIVE posix::uxgetgid() { return mknumber(l, getgid()); }

PRIMITIVE posix::uxgetegid() { return mknumber(l, getegid()); }

PRIMITIVE posix::uxgetpid() { return mknumber(l, getpid()); }

PRIMITIVE posix::uxkill(LISPT pid, LISPT sig)
{
  l.check(pid, type::INTEGER);
  l.check(sig, type::INTEGER);
  return mknumber(l, kill(pid->intval(), sig->intval()));
}

PRIMITIVE posix::uxlink(LISPT name1, LISPT name2)
{
  l.check(name1, type::STRING);
  l.check(name2, type::STRING);
  return mknumber(l, link(name1->stringval().c_str(), name2->stringval().c_str()));
}

PRIMITIVE posix::uxnice(LISPT incr)
{
  l.check(incr, type::INTEGER);
  return mknumber(l, nice(incr->intval()));
}

PRIMITIVE posix::uxopen(LISPT name, LISPT mode)
{
  bool readmode = true;
  bool appendmode = false;

  l.check(name, type::STRING);
  if(!is_NIL(mode))
  {
    l.check(mode, type::SYMBOL);
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
  set(newfile, type::FILET, getobject(l));
  newfile->fileval(std::move(f));
  return newfile;
}

PRIMITIVE posix::uxsetuid(LISPT uid)
{
  l.check(uid, type::INTEGER);
  return mknumber(l, setuid(uid->intval()));
}

PRIMITIVE posix::uxsetgid(LISPT gid)
{
  l.check(gid, type::INTEGER);
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
    sighandler[sig->intval()] = NIL;
  }
  else if(is_T(fun))
  {
    signal(sig->intval(), SIG_DFL);
    sighandler[sig->intval()] = NIL;
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
  l.check(name, type::STRING);
  return mknumber(l, unlink(name->stringval().c_str()));
}

namespace pn
{
inline constexpr auto UXACCESS = "access";   // check file access
inline constexpr auto UXALARM = "alarm";     // set alarm clock
inline constexpr auto UXCHDIR = "chdir";     // change directory
inline constexpr auto UXCHMOD = "chmode";    // change mode of file
inline constexpr auto UXCLOSE = "close";     // close file
inline constexpr auto UXCREAT = "creat";     // create file
inline constexpr auto UXDUP = "dup";         // duplicate fileno
inline constexpr auto UXERRNO = "errno";     // return latest error
inline constexpr auto UXGETUID = "getuid";   // get user id
inline constexpr auto UXGETEUID = "geteuid"; // get effective user id
inline constexpr auto UXGETGID = "getgid";   // set group id
inline constexpr auto UXGETEGID = "getegid"; // get effective group id
inline constexpr auto UXGETPID = "getpid";   // get process id
inline constexpr auto UXKILL = "killproc";   // kill process
inline constexpr auto UXLINK = "link";       // link file
inline constexpr auto UXNICE = "setnice";    // set nice
inline constexpr auto UXOPEN = "open";       // open file
inline constexpr auto UXSETUID = "setuid";   // set user id
inline constexpr auto UXSETGID = "setgid";   // set group id
inline constexpr auto SIGNAL = "signal";     // install signal handler
inline constexpr auto UXUNLINK = "unlink";   // unlink file
} // namespace pn

void posix::init()
{
  // clang-format off
  mkprim(pn::UXACCESS,  ::lisp::uxaccess,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXALARM,   ::lisp::uxalarm,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXCHDIR,   ::lisp::uxchdir,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXCHMOD,   ::lisp::uxchmod,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXCLOSE,   ::lisp::uxclose,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXCREAT,   ::lisp::uxcreat,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXDUP,     ::lisp::uxdup,     subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXERRNO,   ::lisp::uxerrno,   subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXGETUID,  ::lisp::uxgetuid,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXGETEUID, ::lisp::uxgeteuid, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXGETGID,  ::lisp::uxgetgid,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXGETEGID, ::lisp::uxgetegid, subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXGETPID,  ::lisp::uxgetpid,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXKILL,    ::lisp::uxkill,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXLINK,    ::lisp::uxlink,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXNICE,    ::lisp::uxnice,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXOPEN,    ::lisp::uxopen,    subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXSETUID,  ::lisp::uxsetuid,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::UXSETGID,  ::lisp::uxsetgid,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
#if 0
  mkprim(pn::SIGNAL,    ::lisp::uxsignal,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
#endif
  mkprim(pn::UXUNLINK,  ::lisp::uxunlink,  subr_t::subr::EVAL, subr_t::spread::NOSPREAD);
  // clang-format on
}

} // namespace lisp
