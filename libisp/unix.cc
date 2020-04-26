/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <signal.h>
#include <unistd.h>
#include <fcntl.h>

#include "libisp.hh"

namespace lisp {

LISPT sighandler[NSIG - 1];

PRIMITIVE uxerrno()
{
  return mknumber(errno);
}

PRIMITIVE uxaccess(LISPT name, LISPT mode)
{
  CHECK(name, STRING);
  CHECK(mode, INTEGER);
  return mknumber(access(name->stringval(), (int)mode->intval()));
}

PRIMITIVE uxalarm(LISPT seconds)
{
  CHECK(seconds, INTEGER);
  return mknumber(alarm(seconds->intval()));
}

PRIMITIVE uxchdir(LISPT dirname)
{
  CHECK(dirname, STRING);
  return mknumber(chdir(dirname->stringval()));
}

PRIMITIVE uxchmod(LISPT name, LISPT mode)
{
  CHECK(name, STRING);
  CHECK(mode, INTEGER);
  return mknumber(chmod(name->stringval(), (int)mode->intval()));
}

PRIMITIVE uxclose(LISPT fildes)
{
  CHECK(fildes, FILET);
  if(fclose(fildes->fileval()) == -1)
    return C_NIL;
  else
    return C_T;
}

PRIMITIVE uxcreat(LISPT name, LISPT mode)
{
  int i;

  CHECK(name, STRING);
  CHECK(mode, INTEGER);
  i = creat(name->stringval(), (int)mode->intval());
  if(i < 0)
    return C_NIL;
  else
    return mknumber(i);
}

PRIMITIVE uxdup(LISPT fildes)
{
  CHECK(fildes, INTEGER);
  return mknumber(dup((int)fildes->intval()));
}

PRIMITIVE uxgetuid()
{
  return mknumber(getuid());
}

PRIMITIVE uxgeteuid()
{
  return mknumber(geteuid());
}

PRIMITIVE uxgetgid()
{
  return mknumber(getgid());
}

PRIMITIVE uxgetegid()
{
  return mknumber(getegid());
}

PRIMITIVE uxgetpid()
{
  return mknumber(getpid());
}

PRIMITIVE uxkill(LISPT pid, LISPT sig)
{
  CHECK(pid, INTEGER);
  CHECK(sig, INTEGER);
  return mknumber(kill((int)pid->intval(), (int)sig->intval()));
}

PRIMITIVE uxlink(LISPT name1, LISPT name2)
{
  CHECK(name1, STRING);
  CHECK(name2, STRING);
  return mknumber(link(name1->stringval(), name2->stringval()));
}

PRIMITIVE uxnice(LISPT incr)
{
  CHECK(incr, INTEGER);
  return mknumber(nice((int)incr->intval()));
}

PRIMITIVE uxopen(LISPT name, LISPT mode)
{
  const char* openmode = nullptr;

  CHECK(name, STRING);
  if(ISNIL(mode))
    openmode = "r";
  else
  {
    CHECK(mode, SYMBOL);
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
  SET(newfile, FILET, getobject());
  newfile->fileval(f);
  return newfile;
}

PRIMITIVE uxsetuid(LISPT uid)
{
  CHECK(uid, INTEGER);
  return mknumber(setuid((int)uid->intval()));
}

PRIMITIVE uxsetgid(LISPT gid)
{
  CHECK(gid, INTEGER);
  return mknumber(setgid((int)gid->intval()));
}

/*ARGSUSED*/
void sighandle(int sig)
{
  eval(sighandler[sig]);
}

PRIMITIVE uxsignal(LISPT sig, LISPT fun)
{
  CHECK(sig, INTEGER);

  if(sig->intval() >= NSIG || sig->intval() < 1)
    return error(ILLEGAL_SIGNAL, sig);
  if(ISNIL(fun))
  {
    signal((int)sig->intval(), SIG_IGN);
    sighandler[sig->intval()] = C_NIL;
  }
  else if(IST(fun))
  {
    signal((int)sig->intval(), SIG_DFL);
    sighandler[sig->intval()] = C_NIL;
  }
  else
  {
    sighandler[sig->intval()] = fun;
    signal((int)sig->intval(), sighandle);
  }
  return C_T;
}

PRIMITIVE uxunlink(LISPT name)
{
  CHECK(name, STRING);
  return mknumber(unlink(name->stringval()));
}

void init_unix()
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

}
