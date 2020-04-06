/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 *
 */
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <strings.h>
#include "lips.h"

#define MAXARGS         256
#define EXECHASH        1023    /* Hash table size for commands */
#define DEFAULT_SHELL   "/bin/sh"

#ifndef lint
static char rcsid[] = "$Id$";
#endif

extern int execve();
extern int putenv();
extern char *getenv(), *index();
extern char *getwd();
extern char **environ;
extern DIR *opendir();
extern struct direct *readdir();
extern void init_term();
extern void end_term();
LISPT p_setenv();

#define UNION_WAIT int

int insidefork = 0;             /* Is nonzero in the child after */
                                /* a fork */

static BITS32 exechash[EXECHASH/32];       /* One bit set for each program */
static int pgrp;                           /* Process group of current job */

#ifdef JOB_CONTROL
struct job
{
  int jobnum;                   /* Job number */
  int procid;                   /* Process id */
  UNION_WAIT status;            /* Return value */
  char *wdir;                   /* Working directory */
  LISPT exp;                    /* Job expression */
  struct job *next;             /* Pointer to next job */
  int background;               /* Nonzero means job runs in bg */
  int running;                  /* Nonzero if running */
};

static struct job *joblist = NULL;     /* List of jobs */
static struct job *cjoblist = NULL;    /* List of collected jobs */
#endif

/* 
 * preparefork - Sets the processgroup to the group currently beeing built. 
 *               Resets signals to their default value.
 */
static void
preparefork()
{
  (void) setpgid(0, pgrp);
  (void) signal(SIGHUP,  SIG_DFL);
  (void) signal(SIGINT,  SIG_DFL);
  (void) signal(SIGQUIT, SIG_DFL);
  (void) signal(SIGTSTP, SIG_DFL);
  (void) signal(SIGILL,  SIG_DFL);
  (void) signal(SIGSEGV, SIG_DFL);
  (void) signal(SIGBUS,  SIG_DFL);
  (void) signal(SIGTTIN, SIG_DFL);
  (void) signal(SIGTTOU, SIG_DFL);
}

/* 
 * strsave - saves string in argument STR in a safe place with malloc.  
 *           Returns NULL if either STR is NULL or malloc fail to allocate 
 *           more memory.
 */
char *
strsave(str)
  char *str;
{
  char *newstr;

  if (str == NULL) return NULL;
  newstr = (char *) safemalloc((unsigned) strlen(str) + 1);
  if (newstr == NULL) return NULL;
  (void) strcpy(newstr, str);
  return newstr;
}

/* 
 * Routines for managing the jobs. Jobs are saved on a linked list 
 * JOBLIST. When a job exits, storage associated with it are released 
 * unless it was run in background in which case it is saved on another 
 * list, CJOBLIST. This list is freed when a background job exits, 
 * signaling its parent with a SIGCHLD.
 */
/* 
 * printjob - print the job pointed to by JOB in the following format:
 *            [n]   Status (other info)  (command line)
 *            Status is Done if job has exited.
 */
#ifdef JOB_CONTROL
static void
printjob(job)
  struct job *job;
{
  char buffer[80];

  (void) sprintf(buffer, "[%d]  %d ", job->jobnum, job->procid);
  if (job->running)
    (void) strcat(buffer, "Running");
  else if (WIFEXITED(job->status))
    (void) strcat(buffer, "Done");
  else if (WIFSTOPPED(job->status))
    (void) strcat(buffer, sys_siglist[WSTOPSIG(job->status)]);
  else
    {
      (void) strcat(buffer, sys_siglist[WTERMSIG(job->status)]);
      if (WCOREDUMP(job->status))
        (void) strcat(buffer, " (core dumped)");
    }
  (void) strcat(buffer, "\t");
  (void) fputs(buffer, primout);
  (void) xprint(job->exp, C_NIL);
}
#endif

/* 
 * recordjob - register job with process id PID in the linked list of jobs. If 
 *             BG is non-zero, job is registered as running in background.
 *             Returns 0 if all went well, non-zero otherwise.
 */
static int
recordjob(pid, bg)
  int pid, bg;
{
#ifdef JOB_CONTROL
  char wd[MAXPATHLEN];          /* Store working directory */
  struct job *job;

  if (insidefork) return 0;     /* Skip this if in a fork. */
  job = (struct job *) safemalloc(sizeof(struct job));
  if (job == NULL) return 1;
  if (joblist) job->jobnum = (joblist->jobnum) + 1;
  else job->jobnum = 1;
  job->procid = pid;
  job->status = 0;
  job->wdir = strsave(getwd(wd)); /* Not a fatal error if NULL */
  job->next = joblist;
  job->exp = input_exp;
  job->background = bg;
  job->running = 1;
  joblist = job;
#endif
  return 0;
}

/* 
 * collectjob - updates job list with PID as process id, and STAT as exit 
 *              status.
 */
static void
collectjob(pid, stat)
  int pid;
  UNION_WAIT stat;
{
#ifdef JOB_CONTROL
  struct job *i, *j;

  i = NULL;
  for (j = joblist; j; i = j, j = j->next)
    if (j->procid == pid)
      {
        j->running = 0;
        j->status = stat;
        if (!WIFSTOPPED(j->status))
          {
            if (i) i->next = j->next;
            else joblist = j->next;
            if (j->background)          /* When running in background, */
              {                         /* save on another list to be */
                j->next = cjoblist;     /* collected when signaled with */
                cjoblist = j;           /* SIGCHLD. */
              }
            else
              {
                free(j->wdir);
                free((char *)j);
              }
            if (WIFSIGNALED(j->status)
                && WTERMSIG(j->status) != SIGINT)
              printjob(j);     /* Print if not interrupted. */
          }
        else
          printjob(j);
        break;
      }
#endif
}

/* printdone - Sweeps CJOBLIST and prints each job it frees. */
void
printdone()
{
#ifdef JOB_CONTROL
  for (; cjoblist; cjoblist = cjoblist->next)
    {
      printjob(cjoblist);
      free(cjoblist->wdir);
      free((char *)cjoblist);
    }
#endif
}

/* 
 * mfork - Forks and initializes the child. If the process hasn't 
 *         previously been forked, its pid is used as process group id. It 
 *         also grabs the tty for the new process group. Mfork returns the 
 *         pid returned by fork.
 */
static int
mfork()
{
  int pid;

  if ((pid = fork()) == 0)
    {
      if (!insidefork)
        {
          pgrp = getpid();
          (void) ioctl(0, TIOCSPGRP, (char *)&pgrp);
          insidefork = 1;
          end_term();
        }
      preparefork();
      return pid;
    }
  else if (pid < 0)
    {
      if (insidefork)
        (void) fprintf(stderr, "%s\n", sys_errlist[errno]);
      else
        (void) syserr(C_NIL);
      return pid;
    }
  (void) recordjob(pid, 0);
  return pid;
}

/* ltoa - Converts a long to its ascii representation. */
char *
ltoa(v)
  long v;
{
  static char buf[20];

  (void) sprintf(buf, "%ld", v);
  return buf;
}

/*
 * checkmeta - checks the string S if it contains any non-quoted
 *             meta characters in which case it returns true.
 *             It also strips off all quote-characters (backslash).
 */
static int
checkmeta(s)
  char *s;
{
  int i;

  for (i=0; s[i]; i++)
    if (s[i] == '\\')
      {
	strcpy(s+i, s+i+1);
	continue;
      }
    else if (index("*?[]", s[i])) return 1;
  return 0;
}

/* 
 * makeexec - Parse command lin and build argument vector suitable for 
 *            execve. Returns NULL if some error occured, like a no match 
 *            for wild cards. Returns pointers to globbed arguments.
 */
static char **
makeexec(command)
  LISPT command;
{
  LISPT files, com; 
  int i, mask, ok;
  static char *args[MAXARGS];
  char **t;

  ok = 0;
  com = command;
  mask = sigblock(sigmask(SIGINT));     /* Dangerous to interrupt here */
  for (t = args; *t != NULL; t++)
    {
      (void) free(*t);
      *t = NULL;
    }
  (void) sigsetmask(mask);
  for (i=0; TYPEOF(com) == CONS && i < (MAXARGS - 1);
       com = CDR(com))
    {
    again:
      if (TYPEOF(CAR(com)) == SYMBOL)
        {
          char *c;

	  c = strsave(extilde(GETSTR(CAR(com)), 1));
	  if (c == NULL) return NULL;
	  if (!checkmeta(c))
	    args[i++] = c;
	  else
	    {
	      if (ok == 0) ok = 1;
	      if (i == 0)
		{
		  (void) error(AMBIGUOUS, CAR(com));
		  return NULL;
		}
	      files = expandfiles(c, 0, 0, 1);
	      if (!ISNIL(files)) ok = 2;
	      while (TYPEOF(files) == CONS)
		{
		  args[i++] = strsave(GETSTR(CAR(files)));
		  files = CDR(files);
		}
	    }
	}
      else if (TYPEOF(CAR(com)) == INTEGER)
        args[i++] = strsave(ltoa(INTVAL(CAR(com))));
      else if (TYPEOF(CAR(com)) == STRING)
        {
          if ((args[i++]
               = strsave(GETSTR(CAR(com)))) == NULL)
            return NULL;
        }
      else if (TYPEOF(CAR(com)) == CONS)
	{
	  (void) rplaca(com, eval(CAR(com)));
	  goto again;
	}
      else
        {
          (void) error(ILLEGAL_ARG, CAR(com));
          return NULL;
        }
    }
  args[i] = NULL;
  if (ok == 1)
    {
      (void) error(NO_MATCH, CDR(command));
      return NULL;
    }
  return args;
}

/* 
 * waitfork - If there is a fork with pid PID, wait for it and return its 
 *            status. If PID is 0 it means to wait for the first process 
 *            to exit.
 */
static UNION_WAIT
waitfork(pid)
  int pid;
{
  int wpid;
  UNION_WAIT wstat;

  do
    {
      wpid = wait3(&wstat, WUNTRACED, (struct rusage *) NULL);
      if (wpid != -1 && !insidefork) collectjob(wpid, wstat);
    }
  while (pid && pid != wpid && wpid != -1);
  if (WIFSIGNALED(wstat))
    {
      unwind();
      longjmp(toplevel, 6);
    }
  return wstat;
}

void
checkfork()
{
  int wpid;
  UNION_WAIT wstat;

  do
    {
      wpid = wait3(&wstat, WUNTRACED | WNOHANG, (struct rusage *) NULL);
      if (wpid > 0)
        collectjob(wpid, wstat);
    }
  while (wpid > 0);
}
          

/* 
 * exec - Forks (if not already in a fork, in which case it works as 
 *        execve, overlaying the current process), and execs NAME with
 *        original command in COMMAND. It then waits for the process to
 *        return (using waitfork). Exec either returns T or ERROR depending
 *        success or failure for some reason.
 */
static LISPT
exec(name, command)
  char *name;
  LISPT command;
{
  char **args;
  int pid;
  UNION_WAIT status;

  if ((args = makeexec(command)) == NULL)
    return C_ERROR;
  if (insidefork)
    {
      (void) execve(name, args, environ);
      if (errno == ENOEXEC)
        (void) execvp(name, args);
      (void) fprintf(stderr, "%s\n", sys_errlist[errno]);
      exit(1);                          /* No return */
    }
  else if ((pid = mfork()) == 0)
    {
      execve(name, args, environ);
      if (errno == ENOEXEC)
        (void) execvp(name, args);
      (void) fprintf(stderr, "%s\n", sys_errlist[errno]);
      exit(1);
    }
  else if (pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  return mknumber((long) WEXITSTATUS(status));
}

/* 
 * ifexec - Returns non-zero if directory DIR contains a NAME that is
 *          executable.
 */
static int
ifexec(dir, name)
  char *dir, *name;
{
  static char path[MAXNAMLEN];
  struct stat buf;

  (void) strcpy(path, dir);
  (void) strcat(path, "/");
  (void) strcat(path, name);
  if (stat(path, &buf) == -1) return 0;
  if ((buf.st_mode & (S_IEXEC | S_IFREG)) == (S_IEXEC | S_IFREG))
    return 1;
  else return 0;
}

/* hashfun - Calculates the hash function used in hashtable. */
static BITS32
hashfun(str)
  register char *str;
{
  register long i;
  register int bc;

  i = 0;
  bc = 0;
  while (*str)
    {
      i ^= (*str++) << bc;
      bc = (bc + 7) % 10;
    }
  return i % (EXECHASH);
}

/* 
 * execcommand - Tries to execute the lisp expression exp as a command. 
 *               execcomand returns 0 if there is no executable file in 
 *               the path, 1 if the command was successively run and -1 if 
 *               there was some error.
 */
int
execcommand(exp, res)
  LISPT exp;
  LISPT *res;
{
  LISPT cdir;
  char *command;
  char comdir[MAXPATHLEN];
  BITS32 i, possible;
  LISPT tmp;

  *res = C_T;
  command = extilde(GETSTR(CAR(exp)), 1);
  if (command == NULL) return -1;
  if (*command == '/' || strpbrk(command, "/") != NULL)
    {
      if (EQ(exec(command, exp), C_ERROR)) return -1;
      else return 1;
    }

  i = hashfun(command);
  possible = exechash[i/32]&(1<<(i%32));

  for (cdir = path; TYPEOF(cdir) == CONS; cdir = CDR(cdir))
    {
      if (ISNIL(CAR(cdir)) || strcmp(GETSTR(CAR(cdir)), ".") == 0)
        (void) strcpy(comdir, ".");
      else if (possible)
        {
	  /* This isn't really necessary, is it? */
	  if (TYPEOF (CAR (cdir)) != STRING
	      && TYPEOF (CAR (cdir)) != SYMBOL)
	    return -1;
          (void) strcpy(comdir, GETSTR(CAR(cdir)));
        }
      else continue;
      if (ifexec(comdir, command))
        {
          (void) strcat(comdir, "/");
          (void) strcat(comdir, command);
          if (EQ(exec(comdir, exp), C_ERROR)) return -1;
          else return 1;
        }
      else
        continue;
    }
  return 0;
}

/* 
 * setenviron - Set environmet variable VAR to VAL. No sorting of the 
 *              entries is done.
 */
static void
setenviron(var, val)
  char *var, *val;
{
  int i;
  char *env;

  for (i = 0; environ[i]; i++)
    {
      if (!strncmp(var, environ[i], strlen(var)))
        {
          (void) free(environ[i]);
          environ[i] = (char *) safemalloc((unsigned) strlen(var)
					   + strlen(val) + 2);
          if (environ[i] == NULL)
	    {
	      (void) error(OUT_OF_MEMORY, C_NIL);
	      return;
	    }
          (void) strcpy(environ[i], var);
          (void) strcat(environ[i], "=");
          (void) strcat(environ[i], val);
          return;
        }
    }
#ifdef PUTENV
  env = (char *) safemalloc((unsigned) strlen(var) + strlen(val) + 2);
  (void) strcpy(env, var);
  (void) strcat(env, "=");
  (void) strcat(env, val);
  (void) putenv(env);
#else
  (void) setenv(var, val, 1);
#endif
}

/* Primitives */

PRIMITIVE to(cmd, file, filed)
  LISPT cmd, file, filed;
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if (ISNIL(cmd)) return C_NIL;
  CHECK2(file, STRING, SYMBOL);
  if (ISNIL(filed)) oldfd = 1;
  else
    {
      CHECK(filed, INTEGER);
      oldfd = INTVAL(filed);
    }
  if ((fd = creat(GETSTR(file), 0644)) == -1)
    return syserr(file);
  if ((pid = mfork()) == 0)
    {
      if (dup2(fd, oldfd) < 0)
        {
          (void) fprintf(stderr, "%s\n", sys_errlist[errno]);
          exit(1);
        }
      (void) eval(cmd);
      (void) exit(0);
    }
  else if (pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  (void) close(fd);
  return mknumber((long) WEXITSTATUS(status));
}

PRIMITIVE toto(cmd, file, filed)
  LISPT cmd, file, filed;
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if (ISNIL(cmd)) return C_NIL;
  CHECK2(file, STRING, SYMBOL);
  if (ISNIL(filed)) oldfd = 1;
  else
    {
      CHECK(filed, INTEGER);
      oldfd = INTVAL(filed);
    }
  if ((fd = open(GETSTR(file), O_WRONLY|O_CREAT|O_APPEND, 0644)) == -1)
    return syserr(file);
  if ((pid = mfork()) == 0)
    {
      if (dup2(fd, oldfd) < 0)
        {
          (void) fprintf(stderr, "%s\n", sys_errlist[errno]);
          exit(1);
        }
      (void) eval(cmd);
      (void) exit(0);
    }
  else if (pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  (void) close(fd);
  return mknumber((long) WEXITSTATUS(status));
}

PRIMITIVE from(cmd, file, filed)
  LISPT cmd, file, filed;
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if (ISNIL(cmd)) return C_NIL; 
  CHECK2(file, STRING, SYMBOL);
  if (ISNIL(filed)) oldfd = 0;
  else
    {
      CHECK(filed, INTEGER);
      oldfd = INTVAL(filed);
    }
  if ((fd = open(GETSTR(file), O_RDONLY)) == -1)
    return syserr(file);
  if ((pid = mfork()) == 0)
    {
      if (dup2(fd, oldfd) < 0)
        {
          (void) fprintf(stderr, "%s\n", sys_errlist[errno]);
          exit(1);
        }
      (void) eval(cmd);
      (void) exit(0);
    }
  else if (pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  (void) close(fd);
  return mknumber((long) WEXITSTATUS(status));
}

PRIMITIVE pipecmd(cmds)
  LISPT cmds;
{
  int pd[2];
  int pid;
  UNION_WAIT status;

  if (ISNIL(cmds))
    return C_NIL;
  if (ISNIL(CDR(cmds)))
    return eval(CAR(cmds));
  if ((pid = mfork()) == 0)
    {
      (void) pipe(pd);
      if ((pid = mfork()) == 0)
        {
          (void) close(pd[0]);
          if (dup2(pd[1], 1) < 0)
            {
              (void) fprintf(stderr, "%s\n", sys_errlist[errno]);
              exit(1);
            }
          (void) eval(CAR(cmds));
          exit(0);
        }
      else if (pid < 0)
        exit(1);
      cmds = CDR(cmds);
      (void) close(pd[1]);
      if (dup2(pd[0], 0) < 0)
        {
          (void) fprintf(stderr, "%s\n", sys_errlist[errno]);
          exit(1);
        }
      (void) eval(CAR(cmds));
      status = waitfork(pid);
      exit(0);
    }
  else if (pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  return mknumber((long) WEXITSTATUS(status));
}

PRIMITIVE back(l)
  LISPT l;
{
  int pid;

  if ((pid = fork()) == 0)
    {
      pgrp = getpid();
      insidefork = 1;
      preparefork();
      (void) eval(l);
      exit(0);
    }
  else if (pid < 0)
    return C_ERROR;
  (void) recordjob(pid, 1);
  (void) printf("[%d] %d\n", joblist->jobnum, pid);
  return mknumber((long) pid);
}

PRIMITIVE stop()
{
  end_term();
  (void) kill(0, SIGSTOP);
  init_term();
  return C_T;
}

PRIMITIVE rehash()
{
  DIR *odir;
  struct direct *rdir;
  char *sdir;
  BITS32 i;
  LISPT p;

  for (i = 0; i < EXECHASH / 32; i++)
    exechash[i] = 0;
  for (p = path; TYPEOF(p) == CONS; p = CDR(p))
    {
      if (ISNIL(CAR(p)))
        continue;
      else
        {
          CHECK2(CAR(p), STRING, SYMBOL);
          sdir = GETSTR(CAR(p));
        }
      if ((odir = opendir(sdir)) == NULL)
        continue;
      while ((rdir = readdir(odir)) != NULL)
        {
          i = hashfun(rdir->d_name);
          exechash[i / 32] |= 1 << (i % 32);
        }
      closedir(odir);
    }
  return C_NIL;
}

PRIMITIVE jobs()
{
#ifdef JOB_CONTROL
  struct job *j;

  for (j = joblist; j; j = j->next)
    printjob(j);
#endif
  return C_NIL;
}

PRIMITIVE fg(job)
  LISPT job;
{
#ifdef JOB_CONTROL
  struct job *j;
  int pgrp;
  UNION_WAIT status;

  if (ISNIL(job))
    {
      for (j = joblist; j; j = j->next)
        if (WIFSTOPPED(j->status)) break;
    }
  else
    {
      CHECK(job, INTEGER);
      for (j = joblist; j; j = j->next)
        if (j->jobnum == INTVAL(job)) break;
    }
  if (j)
    {
      pgrp = getpgid(j->procid);
      j->running = 1;
      printjob(j);
      end_term();
      (void) ioctl(0, TIOCSPGRP, (char *)&pgrp);
      if (WIFSTOPPED(j->status))
        if (killpg(pgrp, SIGCONT) < 0)
          return syserr(mknumber((long)pgrp));
      j->status = 0;
      j->background = 0;
      status = waitfork(j->procid);
      return mknumber((long) _WSTATUS(status));
    }
  return error(NO_SUCH_JOB, job);
#endif
}

PRIMITIVE bg(job)
  LISPT job;
{
#ifdef JOB_CONTROL
  struct job *j;
  int pgrp;

  if (ISNIL(job))
    {
      for (j = joblist; j; j = j->next)
        if (!j->background) break;
    }
  else
    {
      CHECK(job, INTEGER);
      for (j = joblist; j; j = j->next)
        if (j->jobnum == INTVAL(job)) break;
    }
  if (j)
    {
      pgrp = getpgid(j->procid);
      j->status = 0;
      j->running = 1;
      printjob(j);
      (void) ioctl(0, TIOCSPGRP, (char *)&pgrp);
      if (!j->background)
        if (killpg(pgrp, SIGCONT) < 0)
          return syserr(mknumber((long)pgrp));
      j->background = 1;
      return C_T;
    }
  return error(NO_SUCH_JOB, job);
#endif
}

PRIMITIVE p_setenv(var, val)
  LISPT var, val;
{
  CHECK2(var, STRING, SYMBOL);
  CHECK2(val, STRING, SYMBOL);
  setenviron(GETSTR(var), GETSTR(val));
  return var;
}  

PRIMITIVE getenviron(var)
  LISPT var;
{
  char *s;

  CHECK2(var, STRING, SYMBOL);
  s = getenv(GETSTR(var));
  if (s == NULL)
    return C_NIL;
  else
    return mkstring(s);
}

PRIMITIVE cd(dir, emess)
  LISPT dir, emess;
{
  LISPT ndir;
  char wd[1024];

  if (ISNIL(dir)) ndir = home;
  else
    {
      ndir = glob(dir);
      if (TYPEOF(ndir) == CONS)
        ndir = CAR(ndir);
    }
  if (ISNIL(ndir))
    {
      if (ISNIL(emess)) return error(NO_MATCH, dir);
      else return C_NIL;
    }
  if (chdir(GETSTR(ndir)) == -1)
    {
      if (ISNIL(emess)) return syserr(dir);
      else return C_NIL;
    }
  else
    {
      (void) getwd(wd);
      setenviron("PWD", wd);
      return C_T;
    }
}

PRIMITIVE doexec(cmd)
  LISPT cmd;
{
  LISPT res;

  insidefork = 1;                       /* Prevent exec from forking */
  end_term();
  switch (execcommand(cmd, &res))
    {
    case -1:
      init_term();
      return C_ERROR;
      break;
    default:
      break;                            /* Never reached */
    }
  init_term();
  return C_NIL;
}

void
init_exec()
{
  mkprim(PN_EXPAND,    expand,     3, SUBR);
  mkprim(PN_TO,        to,         3, FSUBR);
  mkprim(PN_FROM,      from,       3, FSUBR);
  mkprim(PN_TOTO,      toto,       3, FSUBR);
  mkprim(PN_PIPECMD,   pipecmd,   -1, FSUBR);
  mkprim(PN_BACK,      back,      -1, FSUBR);
  mkprim(PN_STOP,      stop,       0, FSUBR);
  mkprim(PN_CD,        cd,         1, FSUBR);
  mkprim(PN_REHASH,    rehash,     0, FSUBR);
  mkprim(PN_JOBS,      jobs,       0, FSUBR);
  mkprim(PN_FG,        fg,         1, FSUBR);
  mkprim(PN_BG,        bg,         1, FSUBR);
  mkprim(PN_SETENV,    p_setenv,   2, FSUBR);
  mkprim(PN_GETENV,    getenviron, 1, FSUBR);
  mkprim(PN_EXEC,      doexec,    -1, FSUBR);
  (void) rehash();
  undefhook = execcommand;
}
