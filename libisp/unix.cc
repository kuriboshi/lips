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

#define MAXFILES 8

LISPT sighandler[NSIG - 1];

PRIMITIVE uxerrno()
{
  return mknumber((long)errno);
}

PRIMITIVE uxaccess(LISPT name, LISPT mode)
{
  CHECK(name, STRING);
  CHECK(mode, INTEGER);
  return mknumber((long)access(STRINGVAL(name), (int)INTVAL(mode)));
}

PRIMITIVE uxalarm(LISPT seconds)
{
  CHECK(seconds, INTEGER);
  return mknumber((long)alarm(INTVAL(seconds)));
}

PRIMITIVE uxchdir(LISPT dirname)
{
  CHECK(dirname, STRING);
  return mknumber((long)chdir(STRINGVAL(dirname)));
}

PRIMITIVE uxchmod(LISPT name, LISPT mode)
{
  CHECK(name, STRING);
  CHECK(mode, INTEGER);
  return mknumber((long)chmod(STRINGVAL(name), (int)INTVAL(mode)));
}

PRIMITIVE uxclose(LISPT fildes)
{
  CHECK(fildes, FILET);
  if(fclose(FILEVAL(fildes)) == -1)
    return C_NIL;
  else
    return C_T;
}

PRIMITIVE uxcreat(LISPT name, LISPT mode)
{
  int i;

  CHECK(name, STRING);
  CHECK(mode, INTEGER);
  i = creat(STRINGVAL(name), (int)INTVAL(mode));
  if(i < 0)
    return C_NIL;
  else
    return mknumber((long)i);
}

PRIMITIVE uxdup(LISPT fildes)
{
  CHECK(fildes, INTEGER);
  return mknumber((long)dup((int)INTVAL(fildes)));
}

PRIMITIVE uxgetuid()
{
  return mknumber((long)getuid());
}

PRIMITIVE uxgeteuid()
{
  return mknumber((long)geteuid());
}

PRIMITIVE uxgetgid()
{
  return mknumber((long)getgid());
}

PRIMITIVE uxgetegid()
{
  return mknumber((long)getegid());
}

PRIMITIVE uxgetpid()
{
  return mknumber((long)getpid());
}

PRIMITIVE uxkill(LISPT pid, LISPT sig)
{
  CHECK(pid, INTEGER);
  CHECK(sig, INTEGER);
  return mknumber((long)kill((int)INTVAL(pid), (int)INTVAL(sig)));
}

PRIMITIVE uxlink(LISPT name1, LISPT name2)
{
  CHECK(name1, STRING);
  CHECK(name2, STRING);
  return mknumber((long)link(STRINGVAL(name1), STRINGVAL(name2)));
}

PRIMITIVE uxnice(LISPT incr)
{
  CHECK(incr, INTEGER);
  return mknumber((long)nice((int)INTVAL(incr)));
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
  auto* f = fopen(STRINGVAL(name), openmode);
  if(!f)
    return error(CANT_OPEN, name);
  LISPT newfile;
  SET(newfile, FILET, getobject());
  FILEVAL(newfile) = f;
  return newfile;
}

PRIMITIVE uxsetuid(LISPT uid)
{
  CHECK(uid, INTEGER);
  return mknumber((long)setuid((int)INTVAL(uid)));
}

PRIMITIVE uxsetgid(LISPT gid)
{
  CHECK(gid, INTEGER);
  return mknumber((long)setgid((int)INTVAL(gid)));
}

/*ARGSUSED*/
void sighandle(int sig)
{
  eval(sighandler[sig]);
}

PRIMITIVE uxsignal(LISPT sig, LISPT fun)
{
  CHECK(sig, INTEGER);

  if(INTVAL(sig) >= NSIG || INTVAL(sig) < 1)
    return error(ILLEGAL_SIGNAL, sig);
  if(ISNIL(fun))
  {
    signal((int)INTVAL(sig), SIG_IGN);
    sighandler[INTVAL(sig)] = C_NIL;
  }
  else if(IST(fun))
  {
    signal((int)INTVAL(sig), SIG_DFL);
    sighandler[INTVAL(sig)] = C_NIL;
  }
  else
  {
    sighandler[INTVAL(sig)] = fun;
    signal((int)INTVAL(sig), sighandle);
  }
  return C_T;
}

PRIMITIVE uxunlink(LISPT name)
{
  CHECK(name, STRING);
  return mknumber((long)unlink(STRINGVAL(name)));
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
