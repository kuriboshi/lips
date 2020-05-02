/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>

#include "libisp.hh"

namespace lisp
{
PRIMITIVE posix::uxerrno() { return mknumber(_lisp, errno); }

PRIMITIVE posix::uxaccess(LISPT name, LISPT mode)
{
  _lisp.check(name, STRING);
  _lisp.check(mode, INTEGER);
  return mknumber(_lisp, access(name->stringval(), mode->intval()));
}

PRIMITIVE posix::uxalarm(LISPT seconds)
{
  _lisp.check(seconds, INTEGER);
  return mknumber(_lisp, alarm(seconds->intval()));
}

PRIMITIVE posix::uxchdir(LISPT dirname)
{
  _lisp.check(dirname, STRING);
  return mknumber(_lisp, chdir(dirname->stringval()));
}

PRIMITIVE posix::uxchmod(LISPT name, LISPT mode)
{
  _lisp.check(name, STRING);
  _lisp.check(mode, INTEGER);
  return mknumber(_lisp, chmod(name->stringval(), mode->intval()));
}

PRIMITIVE posix::uxclose(LISPT fildes)
{
  _lisp.check(fildes, FILET);
  if(fildes->fileval()->close())
    return C_T;
  return C_NIL;
}

PRIMITIVE posix::uxcreat(LISPT name, LISPT mode)
{
  int i;

  _lisp.check(name, STRING);
  _lisp.check(mode, INTEGER);
  i = creat(name->stringval(), mode->intval());
  if(i < 0)
    return C_NIL;
  else
    return mknumber(_lisp, i);
}

PRIMITIVE posix::uxdup(LISPT fildes)
{
  _lisp.check(fildes, INTEGER);
  return mknumber(_lisp, dup(fildes->intval()));
}

PRIMITIVE posix::uxgetuid() { return mknumber(_lisp, getuid()); }

PRIMITIVE posix::uxgeteuid() { return mknumber(_lisp, geteuid()); }

PRIMITIVE posix::uxgetgid() { return mknumber(_lisp, getgid()); }

PRIMITIVE posix::uxgetegid() { return mknumber(_lisp, getegid()); }

PRIMITIVE posix::uxgetpid() { return mknumber(_lisp, getpid()); }

PRIMITIVE posix::uxkill(LISPT pid, LISPT sig)
{
  _lisp.check(pid, INTEGER);
  _lisp.check(sig, INTEGER);
  return mknumber(_lisp, kill(pid->intval(), sig->intval()));
}

PRIMITIVE posix::uxlink(LISPT name1, LISPT name2)
{
  _lisp.check(name1, STRING);
  _lisp.check(name2, STRING);
  return mknumber(_lisp, link(name1->stringval(), name2->stringval()));
}

PRIMITIVE posix::uxnice(LISPT incr)
{
  _lisp.check(incr, INTEGER);
  return mknumber(_lisp, nice(incr->intval()));
}

PRIMITIVE posix::uxopen(LISPT name, LISPT mode)
{
  bool readmode = true;
  bool appendmode = false;

  _lisp.check(name, STRING);
  if(!is_NIL(mode))
  {
    _lisp.check(mode, SYMBOL);
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
      return _lisp.error(UNKNOWN_REQUEST, mode);
  }
  auto* f = readmode
    ? new file_t(new io::filesource(name->stringval()))
    : new file_t(new io::filesink(name->stringval(), appendmode));
  if(!f)
    return _lisp.error(CANT_OPEN, name);
  LISPT newfile = nullptr;
  set(newfile, FILET, getobject(_lisp));
  newfile->fileval(f);
  return newfile;
}

PRIMITIVE posix::uxsetuid(LISPT uid)
{
  _lisp.check(uid, INTEGER);
  return mknumber(_lisp, setuid(uid->intval()));
}

PRIMITIVE posix::uxsetgid(LISPT gid)
{
  _lisp.check(gid, INTEGER);
  return mknumber(_lisp, setgid(gid->intval()));
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
  _lisp.check(name, STRING);
  return mknumber(_lisp, unlink(name->stringval()));
}

posix::posix(lisp& lisp) : base(lisp) {}

void posix::init()
{
  alloc::mkprim(PN_UXACCESS, ::lisp::uxaccess, 2, SUBR);
  alloc::mkprim(PN_UXALARM, ::lisp::uxalarm, 1, SUBR);
  alloc::mkprim(PN_UXCHDIR, ::lisp::uxchdir, 1, SUBR);
  alloc::mkprim(PN_UXCHMOD, ::lisp::uxchmod, 2, SUBR);
  alloc::mkprim(PN_UXCLOSE, ::lisp::uxclose, 1, SUBR);
  alloc::mkprim(PN_UXCREAT, ::lisp::uxcreat, 2, SUBR);
  alloc::mkprim(PN_UXDUP, ::lisp::uxdup, 1, SUBR);
  alloc::mkprim(PN_UXERRNO, ::lisp::uxerrno, 0, SUBR);
  alloc::mkprim(PN_UXGETUID, ::lisp::uxgetuid, 0, SUBR);
  alloc::mkprim(PN_UXGETEUID, ::lisp::uxgeteuid, 0, SUBR);
  alloc::mkprim(PN_UXGETGID, ::lisp::uxgetgid, 0, SUBR);
  alloc::mkprim(PN_UXGETEGID, ::lisp::uxgetegid, 0, SUBR);
  alloc::mkprim(PN_UXGETPID, ::lisp::uxgetpid, 0, SUBR);
  alloc::mkprim(PN_UXKILL, ::lisp::uxkill, 2, SUBR);
  alloc::mkprim(PN_UXLINK, ::lisp::uxlink, 2, SUBR);
  alloc::mkprim(PN_UXNICE, ::lisp::uxnice, 1, SUBR);
  alloc::mkprim(PN_UXOPEN, ::lisp::uxopen, 2, SUBR);
  alloc::mkprim(PN_UXSETUID, ::lisp::uxsetuid, 1, SUBR);
  alloc::mkprim(PN_UXSETGID, ::lisp::uxsetgid, 1, SUBR);
#if 0
  alloc::mkprim(PN_SIGNAL, ::lisp::uxsignal, 2, SUBR);
#endif
  alloc::mkprim(PN_UXUNLINK, ::lisp::uxunlink, 1, SUBR);
}

} // namespace lisp
