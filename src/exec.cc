/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 * $Id$
 *
 */
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <signal.h>
#include <string.h>

#include <libisp.hh>
#include "glob.hh"
#include "main.hh"
#include "top.hh"

using namespace lisp;

#define MAXARGS 256
#define EXECHASH 1023 /* Hash table size for commands */
#define DEFAULT_SHELL "/bin/sh"

extern char** environ;
LISPT p_setenv(LISPT, LISPT);

#define UNION_WAIT int

int insidefork = 0; /* Is nonzero in the child after */
                    /* a fork */

static BITS32 exechash[EXECHASH / 32]; /* One bit set for each program */
static int pgrp;                       /* Process group of current job */

#ifdef JOB_CONTROL
struct job
{
  int jobnum;        /* Job number */
  int procid;        /* Process id */
  UNION_WAIT status; /* Return value */
  char* wdir;        /* Working directory */
  LISPT exp;         /* Job expression */
  struct job* next;  /* Pointer to next job */
  int background;    /* Nonzero means job runs in bg */
  int running;       /* Nonzero if running */
};

static struct job* joblist = NULL;  /* List of jobs */
static struct job* cjoblist = NULL; /* List of collected jobs */
#endif

/* 
 * preparefork - Sets the processgroup to the group currently beeing built. 
 *               Resets signals to their default value.
 */
static void preparefork()
{
  signal(SIGHUP, SIG_DFL);
  signal(SIGINT, SIG_DFL);
  signal(SIGQUIT, SIG_DFL);
  signal(SIGTSTP, SIG_DFL);
  signal(SIGILL, SIG_DFL);
  signal(SIGSEGV, SIG_DFL);
  signal(SIGBUS, SIG_DFL);
  signal(SIGTTIN, SIG_DFL);
  signal(SIGTTOU, SIG_DFL);
}

/* 
 * strsave - saves string in argument STR in a safe place with malloc.  
 *           Returns NULL if either STR is NULL or malloc fail to allocate 
 *           more memory.
 */
char* strsave(const char* str)
{
  char* newstr;

  if(str == NULL)
    return NULL;
  newstr = (char*)realmalloc((unsigned)strlen(str) + 1);
  if(newstr == NULL)
    return NULL;
  strcpy(newstr, str);
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
static void printjob(struct job* job)
{
  char buffer[80];

  sprintf(buffer, "[%d]  %d ", job->jobnum, job->procid);
  if(job->running)
    strcat(buffer, "Running");
  else if(WIFEXITED(job->status))
    strcat(buffer, "Done");
  else if(WIFSTOPPED(job->status))
    strcat(buffer, sys_siglist[WSTOPSIG(job->status)]);
  else
  {
    strcat(buffer, sys_siglist[WTERMSIG(job->status)]);
    if(WCOREDUMP(job->status))
      strcat(buffer, " (core dumped)");
  }
  strcat(buffer, "\t");
  fputs(buffer, primout);
  xprint(job->exp, C_NIL);
}
#endif

/* 
 * recordjob - register job with process id PID in the linked list of jobs. If 
 *             BG is non-zero, job is registered as running in background.
 *             Returns 0 if all went well, non-zero otherwise.
 */
static int recordjob(int pid, int bg)
{
#ifdef JOB_CONTROL
  struct job* job;

  if(insidefork)
    return 0; /* Skip this if in a fork. */
  job = (struct job*)realmalloc(sizeof(struct job));
  if(job == NULL)
    return 1;
  if(joblist)
    job->jobnum = (joblist->jobnum) + 1;
  else
    job->jobnum = 1;
  job->procid = pid;
  job->status = 0;
  job->wdir = getcwd(NULL, 0); /* Not a fatal error if NULL */
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
static void collectjob(int pid, UNION_WAIT stat)
{
#ifdef JOB_CONTROL
  struct job *i, *j;

  i = NULL;
  for(j = joblist; j; i = j, j = j->next)
    if(j->procid == pid)
    {
      j->running = 0;
      j->status = stat;
      if(!WIFSTOPPED(j->status))
      {
        if(i)
          i->next = j->next;
        else
          joblist = j->next;
        if(WIFSIGNALED(j->status) && WTERMSIG(j->status) != SIGINT)
          printjob(j);        /* Print if not interrupted. */
        if(j->background)     /* When running in background, */
        {                     /* save on another list to be */
          j->next = cjoblist; /* collected when signaled with */
          cjoblist = j;       /* SIGCHLD. */
        }
        else
        {
          free(j->wdir);
          free((char*)j);
        }
      }
      else
        printjob(j);
      break;
    }
#endif
}

/* printdone - Sweeps CJOBLIST and prints each job it frees. */
void printdone()
{
#ifdef JOB_CONTROL
  for(; cjoblist; cjoblist = cjoblist->next)
  {
    printjob(cjoblist);
    free(cjoblist->wdir);
    free((char*)cjoblist);
  }
#endif
}

/* 
 * mfork - Forks and initializes the child. If the process hasn't 
 *         previously been forked, its pid is used as process group id. It 
 *         also grabs the tty for the new process group. Mfork returns the 
 *         pid returned by fork.
 */
static int mfork()
{
  int pid;

  if((pid = fork()) == 0)
  {
    pgrp = getpid();
    if(!insidefork)
    {
      setpgid(1, pgrp);
      tcsetpgrp(1, pgrp);
      insidefork = 1;
    }
    preparefork();
    return pid;
  }
  else if(pid < 0)
  {
    if(insidefork)
      fprintf(stderr, "%s\n", strerror(errno));
    else
      syserr(C_NIL);
    return pid;
  }
  recordjob(pid, 0);
  return pid;
}

/* ltoa - Converts a long to its ascii representation. */
char* ltoa(long v)
{
  static char buf[20];

  sprintf(buf, "%ld", v);
  return buf;
}

/*
 * checkmeta - checks the string S if it contains any non-quoted
 *             meta characters in which case it returns true.
 *             It also strips off all quote-characters (backslash).
 */
static int checkmeta(char* s)
{
  int i;

  for(i = 0; s[i]; i++)
    if(s[i] == '\\')
    {
      strcpy(s + i, s + i + 1);
      continue;
    }
    else if(index("*?[]", s[i]))
      return 1;
  return 0;
}

/* 
 * makeexec - Parse command lin and build argument vector suitable for 
 *            execve. Returns NULL if some error occured, like a no match 
 *            for wild cards. Returns pointers to globbed arguments.
 */
static char** makeexec(LISPT command)
{
  LISPT files, com;
  int i, ok;
  sigset_t new_mask;
  sigset_t old_mask;
  static char* args[MAXARGS];
  char** t;

  ok = 0;
  com = command;
  sigemptyset(&new_mask);
  sigaddset(&new_mask, SIGINT);
  sigprocmask(SIG_BLOCK, &new_mask, &old_mask); /* Dangerous to interrupt here */
  for(t = args; *t != NULL; t++)
  {
    free(*t);
    *t = NULL;
  }
  sigprocmask(SIG_SETMASK, &old_mask, NULL);
  for(i = 0; TYPEOF(com) == CONS && i < (MAXARGS - 1); com = CDR(com))
  {
  again:
    if(TYPEOF(CAR(com)) == SYMBOL)
    {
      char* c = strsave(extilde(GETSTR(CAR(com)), 1));
      if(c == NULL)
        return NULL;
      if(!checkmeta(c))
        args[i++] = c;
      else
      {
        if(ok == 0)
          ok = 1;
        if(i == 0)
        {
          error(AMBIGUOUS, CAR(com));
          return NULL;
        }
        files = expandfiles(c, 0, 0, 1);
        if(!ISNIL(files))
          ok = 2;
        while(TYPEOF(files) == CONS)
        {
          args[i++] = strsave(GETSTR(CAR(files)));
          files = CDR(files);
        }
      }
    }
    else if(TYPEOF(CAR(com)) == INTEGER)
      args[i++] = strsave(ltoa(INTVAL(CAR(com))));
    else if(TYPEOF(CAR(com)) == STRING)
    {
      if((args[i++] = strsave(GETSTR(CAR(com)))) == NULL)
        return NULL;
    }
    else if(TYPEOF(CAR(com)) == CONS)
    {
      rplaca(com, eval(CAR(com)));
      goto again;
    }
    else
    {
      error(ILLEGAL_ARG, CAR(com));
      return NULL;
    }
  }
  args[i] = NULL;
  if(ok == 1)
  {
    error(NO_MATCH, CDR(command));
    return NULL;
  }
  return args;
}

/* 
 * waitfork - If there is a fork with pid PID, wait for it and return its 
 *            status. If PID is 0 it means to wait for the first process 
 *            to exit.
 */
static UNION_WAIT waitfork(int pid)
{
  int wpid;
  UNION_WAIT wstat;

  do
  {
    wpid = wait3(&wstat, WUNTRACED, (struct rusage*)NULL);
    if(wpid != -1 && !insidefork)
      collectjob(wpid, wstat);
  } while(pid && pid != wpid && wpid != -1);
  if(WIFSIGNALED(wstat))
  {
    unwind();
    throw lips_error("waitfork");
  }
  return wstat;
}

void checkfork()
{
  int wpid;
  UNION_WAIT wstat;

  do
  {
    wpid = wait3(&wstat, WUNTRACED | WNOHANG, (struct rusage*)NULL);
    if(wpid > 0)
      collectjob(wpid, wstat);
  } while(wpid > 0);
}

/* 
 * exec - Forks (if not already in a fork, in which case it works as 
 *        execve, overlaying the current process), and execs NAME with
 *        original command in COMMAND. It then waits for the process to
 *        return (using waitfork). Exec either returns T or ERROR depending
 *        success or failure for some reason.
 */
static LISPT exec(const char* name, LISPT command)
{
  char** args;
  int pid;
  UNION_WAIT status;

  if((args = makeexec(command)) == NULL)
    return C_ERROR;
  if(insidefork)
  {
    execve(name, args, environ);
    if(errno == ENOEXEC)
      execvp(name, args);
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1); /* No return */
  }
  else if((pid = mfork()) == 0)
  {
    execve(name, args, environ);
    if(errno == ENOEXEC)
      execvp(name, args);
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  return mknumber((long)WEXITSTATUS(status));
}

/* 
 * ifexec - Returns non-zero if directory DIR contains a NAME that is
 *          executable.
 */
static int ifexec(const char* dir, const char* name)
{
  static char path[MAXNAMLEN];
  struct stat buf;

  strcpy(path, dir);
  strcat(path, "/");
  strcat(path, name);
  if(stat(path, &buf) == -1)
    return 0;
  if((buf.st_mode & (S_IEXEC | S_IFREG)) == (S_IEXEC | S_IFREG))
    return 1;
  else
    return 0;
}

/* hashfun - Calculates the hash function used in hashtable. */
static BITS32 hashfun(const char* str)
{
  long i;
  int bc;

  i = 0;
  bc = 0;
  while(*str)
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
int execcommand(LISPT exp, LISPT* res)
{
  LISPT cdir;
  const char* command;
  char comdir[MAXPATHLEN];
  BITS32 i, possible;

  *res = C_T;
  command = extilde(GETSTR(CAR(exp)), 1);
  if(command == NULL)
    return -1;
  if(*command == '/' || strpbrk(command, "/") != NULL)
  {
    if(EQ(exec(command, exp), C_ERROR))
      return -1;
    else
      return 1;
  }

  i = hashfun(command);
  possible = exechash[i / 32] & (1 << (i % 32));

  for(cdir = path; TYPEOF(cdir) == CONS; cdir = CDR(cdir))
  {
    if(ISNIL(CAR(cdir)) || strcmp(GETSTR(CAR(cdir)), ".") == 0)
      strcpy(comdir, ".");
    else if(possible)
    {
      /* This isn't really necessary, is it? */
      if(TYPEOF(CAR(cdir)) != STRING && TYPEOF(CAR(cdir)) != SYMBOL)
        return -1;
      strcpy(comdir, GETSTR(CAR(cdir)));
    }
    else
      continue;
    if(ifexec(comdir, command))
    {
      strcat(comdir, "/");
      strcat(comdir, command);
      if(EQ(exec(comdir, exp), C_ERROR))
        return -1;
      else
        return 1;
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
static void setenviron(const char* var, const char* val)
{
#ifdef PUTENV
  char* env = (char*)safemalloc((unsigned)strlen(var) + strlen(val) + 2);
  strcpy(env, var);
  strcat(env, "=");
  strcat(env, val);
  putenv(env);
#else
  char* var_ = (char*)realmalloc((unsigned)strlen(var) + 1);
  char* val_ = (char*)realmalloc((unsigned)strlen(val) + 1);
  setenv(strcpy(var_, var), strcpy(val_, val), 1);
#endif
}

/* Primitives */

PRIMITIVE to(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(ISNIL(cmd))
    return C_NIL;
  CHECK2(file, STRING, SYMBOL);
  if(ISNIL(filed))
    oldfd = 1;
  else
  {
    CHECK(filed, INTEGER);
    oldfd = INTVAL(filed);
  }
  if((fd = creat(GETSTR(file), 0644)) == -1)
    return syserr(file);
  if((pid = mfork()) == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      fprintf(stderr, "%s\n", strerror(errno));
      exit(1);
    }
    eval(cmd);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  close(fd);
  return mknumber((long)WEXITSTATUS(status));
}

PRIMITIVE toto(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(ISNIL(cmd))
    return C_NIL;
  CHECK2(file, STRING, SYMBOL);
  if(ISNIL(filed))
    oldfd = 1;
  else
  {
    CHECK(filed, INTEGER);
    oldfd = INTVAL(filed);
  }
  if((fd = open(GETSTR(file), O_WRONLY | O_CREAT | O_APPEND, 0644)) == -1)
    return syserr(file);
  if((pid = mfork()) == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      fprintf(stderr, "%s\n", strerror(errno));
      exit(1);
    }
    eval(cmd);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  close(fd);
  return mknumber((long)WEXITSTATUS(status));
}

PRIMITIVE from(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(ISNIL(cmd))
    return C_NIL;
  CHECK2(file, STRING, SYMBOL);
  if(ISNIL(filed))
    oldfd = 0;
  else
  {
    CHECK(filed, INTEGER);
    oldfd = INTVAL(filed);
  }
  if((fd = open(GETSTR(file), O_RDONLY)) == -1)
    return syserr(file);
  if((pid = mfork()) == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      fprintf(stderr, "%s\n", strerror(errno));
      exit(1);
    }
    eval(cmd);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  close(fd);
  return mknumber((long)WEXITSTATUS(status));
}

PRIMITIVE pipecmd(LISPT cmds)
{
  int pd[2];
  int pid;
  UNION_WAIT status;

  if(ISNIL(cmds))
    return C_NIL;
  if(ISNIL(CDR(cmds)))
    return eval(CAR(cmds));
  if((pid = mfork()) == 0)
  {
    pipe(pd);
    if((pid = mfork()) == 0)
    {
      close(pd[0]);
      if(dup2(pd[1], 1) < 0)
      {
        fprintf(stderr, "%s\n", strerror(errno));
        exit(1);
      }
      eval(CAR(cmds));
      exit(0);
    }
    else if(pid < 0)
      exit(1);
    cmds = CDR(cmds);
    close(pd[1]);
    if(dup2(pd[0], 0) < 0)
    {
      fprintf(stderr, "%s\n", strerror(errno));
      exit(1);
    }
    eval(CAR(cmds));
    status = waitfork(pid);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  return mknumber((long)WEXITSTATUS(status));
}

PRIMITIVE back(LISPT l)
{
  int pid;

  if((pid = fork()) == 0)
  {
    pgrp = getpid();
    insidefork = 1;
    preparefork();
    eval(l);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  recordjob(pid, 1);
  printf("[%d] %d\n", joblist->jobnum, pid);
  return mknumber((long)pid);
}

PRIMITIVE stop()
{
  kill(0, SIGSTOP);
  return C_T;
}

PRIMITIVE rehash()
{
  DIR* odir;
  struct dirent* rdir;
  const char* sdir;
  BITS32 i;
  LISPT p;

  for(i = 0; i < EXECHASH / 32; i++) exechash[i] = 0;
  for(p = path; TYPEOF(p) == CONS; p = CDR(p))
  {
    if(ISNIL(CAR(p)))
      continue;
    else
    {
      CHECK2(CAR(p), STRING, SYMBOL);
      sdir = GETSTR(CAR(p));
    }
    if((odir = opendir(sdir)) == NULL)
      continue;
    while((rdir = readdir(odir)) != NULL)
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
  struct job* j;

  for(j = joblist; j; j = j->next) printjob(j);
#endif
  return C_NIL;
}

PRIMITIVE fg(LISPT job)
{
#ifdef JOB_CONTROL
  struct job* j;
  int pgrp;
  UNION_WAIT status;

  if(ISNIL(job))
  {
    for(j = joblist; j; j = j->next)
      if(WIFSTOPPED(j->status))
        break;
  }
  else
  {
    CHECK(job, INTEGER);
    for(j = joblist; j; j = j->next)
      if(j->jobnum == INTVAL(job))
        break;
  }
  if(j)
  {
    pgrp = getpgid(j->procid);
    j->running = 1;
    printjob(j);
    tcsetpgrp(1, pgrp);
    if(WIFSTOPPED(j->status))
      if(killpg(pgrp, SIGCONT) < 0)
        return syserr(mknumber((long)pgrp));
    j->status = 0;
    j->background = 0;
    status = waitfork(j->procid);
    return mknumber((long)WEXITSTATUS(status));
  }
  return error(NO_SUCH_JOB, job);
#endif
}

PRIMITIVE bg(LISPT job)
{
#ifdef JOB_CONTROL
  struct job* j;
  int pgrp;

  if(ISNIL(job))
  {
    for(j = joblist; j; j = j->next)
      if(!j->background)
        break;
  }
  else
  {
    CHECK(job, INTEGER);
    for(j = joblist; j; j = j->next)
      if(j->jobnum == INTVAL(job))
        break;
  }
  if(j)
  {
    pgrp = getpgid(j->procid);
    j->status = 0;
    j->running = 1;
    printjob(j);
    tcsetpgrp(1, pgrp);
    if(!j->background)
      if(killpg(pgrp, SIGCONT) < 0)
        return syserr(mknumber((long)pgrp));
    j->background = 1;
    return C_T;
  }
  return error(NO_SUCH_JOB, job);
#endif
}

PRIMITIVE p_setenv(LISPT var, LISPT val)
{
  CHECK2(var, STRING, SYMBOL);
  CHECK2(val, STRING, SYMBOL);
  setenviron(GETSTR(var), GETSTR(val));
  return var;
}

PRIMITIVE getenviron(LISPT var)
{
  char* s;

  CHECK2(var, STRING, SYMBOL);
  s = getenv(GETSTR(var));
  if(s == NULL)
    return C_NIL;
  else
    return mkstring(s);
}

PRIMITIVE cd(LISPT dir, LISPT emess)
{
  LISPT ndir;

  if(ISNIL(dir))
    ndir = home;
  else
  {
    ndir = glob(dir);
    if(TYPEOF(ndir) == CONS)
      ndir = CAR(ndir);
  }
  if(ISNIL(ndir))
  {
    if(ISNIL(emess))
      return error(NO_MATCH, dir);
    else
      return C_NIL;
  }
  if(chdir(GETSTR(ndir)) == -1)
  {
    if(ISNIL(emess))
      return syserr(dir);
    else
      return C_NIL;
  }
  else
  {
    char* wd = getcwd(NULL, 0);
    setenviron("PWD", wd);
    free(wd);
    return C_T;
  }
}

PRIMITIVE doexec(LISPT cmd)
{
  LISPT res;

  insidefork = 1; /* Prevent exec from forking */
  switch(execcommand(cmd, &res))
  {
    case -1:
      return C_ERROR;
      break;
    default:
      break; /* Never reached */
  }
  return C_NIL;
}

void init_exec()
{
  mkprim(PN_EXPAND, expand, 3, SUBR);
  mkprim(PN_TO, to, 3, FSUBR);
  mkprim(PN_FROM, from, 3, FSUBR);
  mkprim(PN_TOTO, toto, 3, FSUBR);
  mkprim(PN_PIPECMD, pipecmd, -1, FSUBR);
  mkprim(PN_BACK, back, -1, FSUBR);
  mkprim(PN_STOP, stop, 0, FSUBR);
  mkprim(PN_CD, cd, 2, FSUBR);
  mkprim(PN_REHASH, rehash, 0, FSUBR);
  mkprim(PN_JOBS, jobs, 0, FSUBR);
  mkprim(PN_FG, fg, 1, FSUBR);
  mkprim(PN_BG, bg, 1, FSUBR);
  mkprim(PN_SETENV, p_setenv, 2, FSUBR);
  mkprim(PN_GETENV, getenviron, 1, FSUBR);
  mkprim(PN_EXEC, doexec, -1, FSUBR);
  rehash();
  undefhook = execcommand;
}
