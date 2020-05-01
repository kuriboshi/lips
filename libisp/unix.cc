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
PRIMITIVE unix::uxerrno() { return mknumber(_lisp, errno); }

PRIMITIVE unix::uxaccess(LISPT name, LISPT mode)
{
  check(name, STRING);
  check(mode, INTEGER);
  return mknumber(_lisp, access(name->stringval(), mode->intval()));
}

PRIMITIVE unix::uxalarm(LISPT seconds)
{
  check(seconds, INTEGER);
  return mknumber(_lisp, alarm(seconds->intval()));
}

PRIMITIVE unix::uxchdir(LISPT dirname)
{
  check(dirname, STRING);
  return mknumber(_lisp, chdir(dirname->stringval()));
}

PRIMITIVE unix::uxchmod(LISPT name, LISPT mode)
{
  check(name, STRING);
  check(mode, INTEGER);
  return mknumber(_lisp, chmod(name->stringval(), mode->intval()));
}

PRIMITIVE unix::uxclose(LISPT fildes)
{
  check(fildes, FILET);
  if(fclose(fildes->fileval()) == -1)
    return C_NIL;
  else
    return C_T;
}

PRIMITIVE unix::uxcreat(LISPT name, LISPT mode)
{
  int i;

  check(name, STRING);
  check(mode, INTEGER);
  i = creat(name->stringval(), mode->intval());
  if(i < 0)
    return C_NIL;
  else
    return mknumber(_lisp, i);
}

PRIMITIVE unix::uxdup(LISPT fildes)
{
  check(fildes, INTEGER);
  return mknumber(_lisp, dup(fildes->intval()));
}

PRIMITIVE unix::uxgetuid() { return mknumber(_lisp, getuid()); }

PRIMITIVE unix::uxgeteuid() { return mknumber(_lisp, geteuid()); }

PRIMITIVE unix::uxgetgid() { return mknumber(_lisp, getgid()); }

PRIMITIVE unix::uxgetegid() { return mknumber(_lisp, getegid()); }

PRIMITIVE unix::uxgetpid() { return mknumber(_lisp, getpid()); }

PRIMITIVE unix::uxkill(LISPT pid, LISPT sig)
{
  check(pid, INTEGER);
  check(sig, INTEGER);
  return mknumber(_lisp, kill(pid->intval(), sig->intval()));
}

PRIMITIVE unix::uxlink(LISPT name1, LISPT name2)
{
  check(name1, STRING);
  check(name2, STRING);
  return mknumber(_lisp, link(name1->stringval(), name2->stringval()));
}

PRIMITIVE unix::uxnice(LISPT incr)
{
  check(incr, INTEGER);
  return mknumber(_lisp, nice(incr->intval()));
}

PRIMITIVE unix::uxopen(LISPT name, LISPT mode)
{
  const char* openmode = nullptr;

  check(name, STRING);
  if(is_NIL(mode))
    openmode = "r";
  else
  {
    check(mode, SYMBOL);
    if(EQ(mode, C_READ))
      openmode = "r";
    else if(EQ(mode, C_WRITE))
      openmode = "w";
    else if(EQ(mode, C_APPEND))
      openmode = "a";
    else
      return error(UNKNOWN_REQUEST, mode);
  }
  auto* f = fopen(name->stringval(), openmode);
  if(!f)
    return error(CANT_OPEN, name);
  LISPT newfile = nullptr;
  set(newfile, FILET, getobject(_lisp));
  newfile->fileval(f);
  return newfile;
}

PRIMITIVE unix::uxsetuid(LISPT uid)
{
  check(uid, INTEGER);
  return mknumber(_lisp, setuid(uid->intval()));
}

PRIMITIVE unix::uxsetgid(LISPT gid)
{
  check(gid, INTEGER);
  return mknumber(_lisp, setgid(gid->intval()));
}

#if 0
LISPT unix::sighandler[];
void unix::sighandle(int sig) { eval(_lisp, sighandler[sig]); }

PRIMITIVE unix::uxsignal(LISPT sig, LISPT fun)
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

PRIMITIVE unix::uxunlink(LISPT name)
{
  check(name, STRING);
  return mknumber(_lisp, unlink(name->stringval()));
}

unix::unix(lisp& lisp) : base(lisp) {}

void unix::init()
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
