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
LISPT sighandler[NSIG - 1];

PRIMITIVE uxerrno() { return mknumber(errno); }

PRIMITIVE uxaccess(LISPT name, LISPT mode)
{
  check(name, STRING);
  check(mode, INTEGER);
  return mknumber(access(name->stringval(), mode->intval()));
}

PRIMITIVE uxalarm(LISPT seconds)
{
  check(seconds, INTEGER);
  return mknumber(alarm(seconds->intval()));
}

PRIMITIVE uxchdir(LISPT dirname)
{
  check(dirname, STRING);
  return mknumber(chdir(dirname->stringval()));
}

PRIMITIVE uxchmod(LISPT name, LISPT mode)
{
  check(name, STRING);
  check(mode, INTEGER);
  return mknumber(chmod(name->stringval(), mode->intval()));
}

PRIMITIVE uxclose(LISPT fildes)
{
  check(fildes, FILET);
  if(fclose(fildes->fileval()) == -1)
    return C_NIL;
  else
    return C_T;
}

PRIMITIVE uxcreat(LISPT name, LISPT mode)
{
  int i;

  check(name, STRING);
  check(mode, INTEGER);
  i = creat(name->stringval(), mode->intval());
  if(i < 0)
    return C_NIL;
  else
    return mknumber(i);
}

PRIMITIVE uxdup(LISPT fildes)
{
  check(fildes, INTEGER);
  return mknumber(dup(fildes->intval()));
}

PRIMITIVE uxgetuid() { return mknumber(getuid()); }

PRIMITIVE uxgeteuid() { return mknumber(geteuid()); }

PRIMITIVE uxgetgid() { return mknumber(getgid()); }

PRIMITIVE uxgetegid() { return mknumber(getegid()); }

PRIMITIVE uxgetpid() { return mknumber(getpid()); }

PRIMITIVE uxkill(LISPT pid, LISPT sig)
{
  check(pid, INTEGER);
  check(sig, INTEGER);
  return mknumber(kill(pid->intval(), sig->intval()));
}

PRIMITIVE uxlink(LISPT name1, LISPT name2)
{
  check(name1, STRING);
  check(name2, STRING);
  return mknumber(link(name1->stringval(), name2->stringval()));
}

PRIMITIVE uxnice(LISPT incr)
{
  check(incr, INTEGER);
  return mknumber(nice(incr->intval()));
}

PRIMITIVE uxopen(LISPT name, LISPT mode)
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
  set(newfile, FILET, getobject());
  newfile->fileval(f);
  return newfile;
}

PRIMITIVE uxsetuid(LISPT uid)
{
  check(uid, INTEGER);
  return mknumber(setuid(uid->intval()));
}

PRIMITIVE uxsetgid(LISPT gid)
{
  check(gid, INTEGER);
  return mknumber(setgid(gid->intval()));
}

/*ARGSUSED*/
void sighandle(int sig) { eval(sighandler[sig]); }

PRIMITIVE uxsignal(LISPT sig, LISPT fun)
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

PRIMITIVE uxunlink(LISPT name)
{
  check(name, STRING);
  return mknumber(unlink(name->stringval()));
}

unix::unix()
{
  mkprim(PN_UXACCESS, uxaccess, 2, SUBR);
  mkprim(PN_UXALARM, uxalarm, 1, SUBR);
  mkprim(PN_UXCHDIR, uxchdir, 1, SUBR);
  mkprim(PN_UXCHMOD, uxchmod, 2, SUBR);
  mkprim(PN_UXCLOSE, uxclose, 1, SUBR);
  mkprim(PN_UXCREAT, uxcreat, 2, SUBR);
  mkprim(PN_UXDUP, uxdup, 1, SUBR);
  mkprim(PN_UXERRNO, uxerrno, 0, SUBR);
  mkprim(PN_UXGETUID, uxgetuid, 0, SUBR);
  mkprim(PN_UXGETEUID, uxgeteuid, 0, SUBR);
  mkprim(PN_UXGETGID, uxgetgid, 0, SUBR);
  mkprim(PN_UXGETEGID, uxgetegid, 0, SUBR);
  mkprim(PN_UXGETPID, uxgetpid, 0, SUBR);
  mkprim(PN_UXKILL, uxkill, 2, SUBR);
  mkprim(PN_UXLINK, uxlink, 2, SUBR);
  mkprim(PN_UXNICE, uxnice, 1, SUBR);
  mkprim(PN_UXOPEN, uxopen, 2, SUBR);
  mkprim(PN_UXSETUID, uxsetuid, 1, SUBR);
  mkprim(PN_UXSETGID, uxsetgid, 1, SUBR);
  mkprim(PN_SIGNAL, uxsignal, 2, SUBR);
  mkprim(PN_UXUNLINK, uxunlink, 1, SUBR);
}

} // namespace lisp
