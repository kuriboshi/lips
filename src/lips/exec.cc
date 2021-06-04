/*
 * Lips, lisp shell.
 * Copyright 1988, 2020 Krister Joas
 *
 */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>

#include <doctest/doctest.h>
#include <string>
#include <iostream>
#include <cstdlib>
#include <cerrno>
#include <csignal>
#include <cstring>

#include <lisp/libisp.hh>
#include <lisp/except.hh>
#include "glob.hh"
#include "main.hh"
#include "top.hh"
#include "exec.hh"

extern lisp::lisp* L;

using namespace lisp;
using namespace std::literals;

inline constexpr int EXECHASH = 1023; /* Hash table size for commands */

extern char** environ;
LISPT p_setenv(LISPT, LISPT);

using UNION_WAIT = int;

bool insidefork = false; // Is nonzero in the child after a fork

static int exechash[EXECHASH / 32]; /* One bit set for each program */
static int pgrp;                    /* Process group of current job */

#ifdef JOB_CONTROL
struct job_t
{
  int jobnum;        /* Job number */
  int procid;        /* Process id */
  UNION_WAIT status; /* Return value */
  char* wdir;        /* Working directory */
  LISPT exp;         /* Job expression */
  job_t* next;       /* Pointer to next job */
  int background;    /* Nonzero means job runs in bg */
  int running;       /* Nonzero if running */
};

static job_t* joblist = nullptr;  /* List of jobs */
static job_t* cjoblist = nullptr; /* List of collected jobs */
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
static void printjob(job_t* job)
{
  char buffer[80];

  sprintf(buffer, "[%d]  %d ", job->jobnum, job->procid);
  if(job->running)
    strcat(buffer, "Running");
  else if(WIFEXITED(job->status))
    strcat(buffer, "Done");
  else if(WIFSTOPPED(job->status))
    strcat(buffer, strsignal(WSTOPSIG(job->status)));
  else
  {
    strcat(buffer, strsignal(WTERMSIG(job->status)));
    if(WCOREDUMP(job->status))
      strcat(buffer, " (core dumped)");
  }
  strcat(buffer, "\t");
  L->primout().puts(buffer);
  print(*L, job->exp, C_NIL);
}
#endif

/* 
 * recordjob - register job with process id PID in the linked list of jobs. If 
 *             BG is non-zero, job is registered as running in background.
 *             Returns true if all went well, false otherwise.
 */
static bool recordjob(int pid, int bg)
{
#ifdef JOB_CONTROL
  if(insidefork)
    return true; /* Skip this if in a fork. */
  auto* job = new job_t;
  if(job == nullptr)
    return false;
  if(joblist)
    job->jobnum = (joblist->jobnum) + 1;
  else
    job->jobnum = 1;
  job->procid = pid;
  job->status = 0;
  job->wdir = getcwd(nullptr, 0); /* Not a fatal error if nullptr */
  job->next = joblist;
  job->exp = input_exp;
  job->background = bg;
  job->running = 1;
  joblist = job;
#endif
  return true;
}

/* 
 * collectjob - updates job list with PID as process id, and STAT as exit 
 *              status.
 */
static void collectjob(int pid, UNION_WAIT stat)
{
#ifdef JOB_CONTROL
  job_t* i = nullptr;
  for(auto* j = joblist; j; i = j, j = j->next)
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
      syserr(*L, C_NIL);
    return pid;
  }
  recordjob(pid, 0);
  return pid;
}

/*
 * check_meta - checks the string S if it contains any non-quoted meta
 *              characters in which case it returns true.  It also strips off
 *              all quote-characters (backslash).
 */
static std::pair<bool, std::string> check_meta(const std::string& s)
{
  std::string result;
  bool meta = false;
  bool quote = false;
  for(auto c: s)
  {
    if(c == '\\')
    {
      quote = true;
      continue;
    }
    else if(quote)
      quote = false;
    else if("*?[]"s.find_first_of(c) != std::string::npos)
    {
      meta = true;
    }
    result.push_back(c);
  }
  return std::pair(meta, result);
}

TEST_CASE("exec.cc: check_meta")
{
  SUBCASE("test 1")
  {
    auto b = check_meta("hello");
    CHECK(!b.first);
  }
  SUBCASE("test 2")
  {
    auto b = check_meta("hello*");
    CHECK(b.first);
  }
  SUBCASE("test 3")
  {
    auto b = check_meta("hello\\*");
    CHECK(!b.first);
    CHECK(b.second == "hello*"s);
  }
  SUBCASE("test 4")
  {
    auto b = check_meta("hello\\*\\[\\]");
    CHECK(!b.first);
    CHECK(b.second == "hello*[]"s);
  }
  SUBCASE("test 5")
  {
    auto b = check_meta("hello\\*[a]\\*");
    CHECK(b.first);
    CHECK(b.second == "hello*[a]*"s);
  }
}

static std::optional<std::vector<std::string>> process_one(LISPT arg)
{
  std::vector<std::string> args;
  if(type_of(arg) == SYMBOL)
  {
    auto c = extilde2(arg->getstr(), true);
    if(!c)
      return {};
    auto [meta, str] = check_meta(*c);
    if(!meta)
      args.push_back(str);
    else
    {
      auto files = expandfiles(c->c_str(), 0, 0, 1);
      if(type_of(files) == CONS)
      {
        for(auto f: files)
          args.push_back(f->getstr());
      }
      else if(is_NIL(files))
      {
        error(NO_MATCH, arg);
        return {};
      }
    }
  }
  else if(type_of(arg) == INTEGER)
    args.push_back(std::to_string(arg->intval()));
  else if(type_of(arg) == STRING)
    args.push_back(arg->getstr());
  else if(type_of(arg) == CONS)
  {
    auto result = process_one(eval(arg));
    if(!result)
      return {};
    for(auto s: *result)
      args.push_back(s);
  }
  else
  {
    error(ILLEGAL_ARG, arg);
    return {};
  }
  return args;
}

/* 
 * make_exec - Parse command line and build argument vector suitable for
 *             execve. Returns nullptr if some error occured, like a no match
 *             for wild cards. Returns pointers to globbed arguments.
 */
static std::optional<std::vector<std::string>> make_exec(LISPT command)
{
  std::vector<std::string> args;

  for(auto i: command)
  {
    auto p = process_one(i);
    if(p)
      for(auto j: *p)
        args.push_back(j);
  }
  return args;
}

TEST_CASE("exec.cc: make_exec")
{
  SUBCASE("(make_exec (a b c)) -> a b c")
  {
    auto result = make_exec(cons(mkstring("a"), cons(mkstring("b"), cons(mkstring("c"), C_NIL))));
    REQUIRE(result);
    CHECK(result->size() == 3);
    auto i = result->begin();
    CHECK(*i++ == "a");
    CHECK(*i++ == "b");
    CHECK(*i++ == "c");
  }
  SUBCASE("(make_exec (100)) -> 100")
  {
    auto result = make_exec(cons(mknumber(100), C_NIL));
    REQUIRE(result);
    CHECK(result->at(0) == "100"s);
  }
  SUBCASE("(make_exec (+ 1 2)) -> 3")
  {
    // auto expr = cons(cons(intern("+"), cons(mknumber(1), cons(mknumber(2), C_NIL))), C_NIL);
    auto is = file_t("((+ 1 2))");
    auto expr = lispread(is);
    auto result = make_exec(expr);
    REQUIRE(result);
    CHECK(result->at(0) == "3"s);
  }
  SUBCASE("(make_exec (/b*)) -> /bin")
  {
    file_t is("(/b*)");
    auto expr = lispread(is);
    auto result = make_exec(expr);
    REQUIRE(result);
    REQUIRE(!result->empty());
    CHECK(result->at(0) == "/bin"s);
  }
  SUBCASE("(make_exec (/a*)) -> <empty>")
  {
    file_t is("(/a*)");
    auto expr = lispread(is);
    auto result = make_exec(expr);
    REQUIRE(result);
    REQUIRE(result->empty());
  }
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
    UNION_WAIT stat;
    wpid = wait3(&stat, WUNTRACED, nullptr);
    if(wpid != -1 && !insidefork)
      collectjob(wpid, stat);
    if(wpid == pid)
      wstat = stat;
  } while(errno == EINTR || (pid != 0 && pid != wpid && wpid != -1));
  if(WIFSIGNALED(wstat))
  {
    L->e().unwind();
    throw lisp_error("waitfork");
  }
  return wstat;
}

void checkfork()
{
  int wpid;
  UNION_WAIT wstat;

  do
  {
    wpid = wait3(&wstat, WUNTRACED | WNOHANG, nullptr);
    if(wpid > 0)
      collectjob(wpid, wstat);
  } while(wpid > 0);
}

/* 
 * execute - Forks (if not already in a fork, in which case it works as execve,
 *           overlaying the current process), and execs NAME with original
 *           command in COMMAND. It then waits for the process to return (using
 *           waitfork). Exec either returns T or ERROR depending success or
 *           failure for some reason.
 */
static LISPT execute(const std::string& name, LISPT command)
{
  auto args = make_exec(command);
  if(!args)
    return C_ERROR;
  std::vector<char*> argv;
  for(auto a: *args)
    argv.push_back(a.data());
  argv.push_back(nullptr);
  if(insidefork)
  {
    execve(name.c_str(), &argv[0], environ);
    if(errno == ENOEXEC)
      execvp(name.c_str(), &argv[0]);
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1);
    /* No return */
  }
  auto pid = mfork();
  if(pid == 0)
  {
    execve(name.c_str(), &argv[0], environ);
    if(errno == ENOEXEC)
      execvp(name.c_str(), &argv[0]);
    fprintf(stderr, "%s\n", strerror(errno));
    exit(1);
  }
  else if(pid < 0)
    return C_ERROR;
  auto status = waitfork(pid);
  return mknumber(*L, WEXITSTATUS(status));
}

TEST_CASE("execute")
{
  auto result = execute("/bin/ls", cons(mkstring("ls"), C_NIL));
  CHECK(result->intval() == 0);
}

/* 
 * ifexec - Returns true if directory DIR contains a NAME that is executable.
 */
static bool ifexec(const char* dir, const char* name)
{
  static char path[MAXNAMLEN];
  struct stat buf;

  strcpy(path, dir);
  strcat(path, "/");
  strcat(path, name);
  if(stat(path, &buf) == -1)
    return false;
  if((buf.st_mode & (S_IEXEC | S_IFREG)) == (S_IEXEC | S_IFREG))
    return true;
  return false;
}

/* hashfun - Calculates the hash function used in hashtable. */
static int hashfun(const char* str)
{
  int i;
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
  int i, possible;

  *res = C_T;
  command = extilde(exp->car()->getstr().c_str(), 1);
  if(command == nullptr)
    return -1;
  if(*command == '/' || strpbrk(command, "/") != nullptr)
  {
    if(EQ(execute(command, exp), C_ERROR))
      return -1;
    else
      return 1;
  }

  i = hashfun(command);
  possible = exechash[i / 32] & (1 << (i % 32));

  for(cdir = path; type_of(cdir) == CONS; cdir = cdir->cdr())
  {
    if(is_NIL(cdir->car()) || strcmp(cdir->car()->getstr().c_str(), ".") == 0)
      strcpy(comdir, ".");
    else if(possible)
    {
      /* This isn't really necessary, is it? */
      if(type_of(cdir->car()) != STRING && type_of(cdir->car()) != SYMBOL)
        return -1;
      strcpy(comdir, cdir->car()->getstr().c_str());
    }
    else
      continue;
    if(ifexec(comdir, command))
    {
      strcat(comdir, "/");
      strcat(comdir, command);
      if(EQ(execute(comdir, exp), C_ERROR))
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
  auto* env = L->a().realmalloc((unsigned)strlen(var) + strlen(val) + 2);
  strcpy(env, var);
  strcat(env, "=");
  strcat(env, val);
  putenv(env);
#else
  auto* var_ = L->a().realmalloc((unsigned)strlen(var) + 1);
  auto* val_ = L->a().realmalloc((unsigned)strlen(val) + 1);
  setenv(strcpy(var_, var), strcpy(val_, val), 1);
#endif
}

/* Primitives */

PRIMITIVE exec::to(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return C_NIL;
  l.check(file, STRING, SYMBOL);
  if(is_NIL(filed))
    oldfd = 1;
  else
  {
    l.check(filed, INTEGER);
    oldfd = filed->intval();
  }
  if((fd = creat(file->getstr().c_str(), 0644)) == -1)
    return syserr(l, file);
  if((pid = mfork()) == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      l.stderr().format("{}\n", strerror(errno));
      exit(1);
    }
    eval(l, cmd);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  close(fd);
  return mknumber(l, WEXITSTATUS(status));
}

PRIMITIVE exec::toto(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return C_NIL;
  l.check(file, STRING, SYMBOL);
  if(is_NIL(filed))
    oldfd = 1;
  else
  {
    l.check(filed, INTEGER);
    oldfd = filed->intval();
  }
  if((fd = open(file->getstr().c_str(), O_WRONLY | O_CREAT | O_APPEND, 0644)) == -1)
    return syserr(*L, file);
  if((pid = mfork()) == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      L->stderr().format("{}\n", strerror(errno));
      exit(1);
    }
    eval(*L, cmd);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  close(fd);
  return mknumber(*L, WEXITSTATUS(status));
}

PRIMITIVE exec::from(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return C_NIL;
  l.check(file, STRING, SYMBOL);
  if(is_NIL(filed))
    oldfd = 0;
  else
  {
    l.check(filed, INTEGER);
    oldfd = filed->intval();
  }
  if((fd = open(file->getstr().c_str(), O_RDONLY)) == -1)
    return syserr(l, file);
  if((pid = mfork()) == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      l.stderr().format("{}\n", strerror(errno));
      exit(1);
    }
    eval(l, cmd);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  close(fd);
  return mknumber(l, WEXITSTATUS(status));
}

PRIMITIVE exec::pipecmd(LISPT cmds)
{
  int pd[2];
  int pid;
  UNION_WAIT status;

  if(is_NIL(cmds))
    return C_NIL;
  if(is_NIL(cmds->cdr()))
    return eval(l, cmds->car());
  if((pid = mfork()) == 0)
  {
    pipe(pd);
    if((pid = mfork()) == 0)
    {
      close(pd[0]);
      if(dup2(pd[1], 1) < 0)
      {
        l.stderr().format("{}\n", strerror(errno));
        exit(1);
      }
      eval(l, cmds->car());
      exit(0);
    }
    else if(pid < 0)
      exit(1);
    cmds = cmds->cdr();
    close(pd[1]);
    if(dup2(pd[0], 0) < 0)
    {
      l.stderr().format("{}\n", strerror(errno));
      exit(1);
    }
    eval(l, cmds->car());
    status = waitfork(pid);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  return mknumber(l, WEXITSTATUS(status));
}

PRIMITIVE exec::back(LISPT x)
{
  int pid;

  if((pid = fork()) == 0)
  {
    pgrp = getpid();
    insidefork = 1;
    preparefork();
    eval(l, x);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  recordjob(pid, 1);
  printf("[%d] %d\n", joblist->jobnum, pid);
  return mknumber(l, pid);
}

PRIMITIVE exec::stop()
{
  kill(0, SIGSTOP);
  return C_T;
}

PRIMITIVE exec::rehash()
{
  DIR* odir;
  struct dirent* rdir;
  const char* sdir;

  for(int i = 0; i < EXECHASH / 32; i++)
    exechash[i] = 0;

  for(auto p = path; type_of(p) == CONS; p = p->cdr())
  {
    if(is_NIL(p->car()))
      continue;
    else
    {
      l.check(p->car(), STRING, SYMBOL);
      sdir = p->car()->getstr().c_str();
    }
    if((odir = opendir(sdir)) == nullptr)
      continue;
    while((rdir = readdir(odir)) != nullptr)
    {
      auto i = hashfun(rdir->d_name);
      exechash[i / 32] |= 1 << (i % 32);
    }
    closedir(odir);
  }
  return C_NIL;
}

PRIMITIVE exec::jobs()
{
#ifdef JOB_CONTROL
  for(auto* j = joblist; j; j = j->next) printjob(j);
#endif
  return C_NIL;
}

PRIMITIVE exec::fg(LISPT job)
{
#ifdef JOB_CONTROL
  job_t* j = nullptr;

  if(is_NIL(job))
  {
    for(j = joblist; j; j = j->next)
      if(WIFSTOPPED(j->status))
        break;
  }
  else
  {
    l.check(job, INTEGER);
    for(j = joblist; j; j = j->next)
      if(j->jobnum == job->intval())
        break;
  }
  if(j)
  {
    auto pgrp = getpgid(j->procid);
    j->running = 1;
    printjob(j);
    tcsetpgrp(1, pgrp);
    if(WIFSTOPPED(j->status))
      if(killpg(pgrp, SIGCONT) < 0)
        return syserr(l, mknumber(l, pgrp));
    j->status = 0;
    j->background = 0;
    auto status = waitfork(j->procid);
    return mknumber(l, WEXITSTATUS(status));
  }
  return l.error(NO_SUCH_JOB, job);
#endif
}

PRIMITIVE exec::bg(LISPT job)
{
#ifdef JOB_CONTROL
  job_t* j = nullptr;

  if(is_NIL(job))
  {
    for(j = joblist; j; j = j->next)
      if(!j->background)
        break;
  }
  else
  {
    l.check(job, INTEGER);
    for(j = joblist; j; j = j->next)
      if(j->jobnum == job->intval())
        break;
  }
  if(j)
  {
    auto pgrp = getpgid(j->procid);
    j->status = 0;
    j->running = 1;
    printjob(j);
    tcsetpgrp(1, pgrp);
    if(!j->background)
      if(killpg(pgrp, SIGCONT) < 0)
        return syserr(l, mknumber(l, pgrp));
    j->background = 1;
    return C_T;
  }
  return l.error(NO_SUCH_JOB, job);
#endif
}

PRIMITIVE exec::p_setenv(LISPT var, LISPT val)
{
  l.check(var, STRING, SYMBOL);
  l.check(val, STRING, SYMBOL);
  setenviron(var->getstr().c_str(), val->getstr().c_str());
  return var;
}

PRIMITIVE exec::getenviron(LISPT var)
{
  l.check(var, STRING, SYMBOL);
  char* s = getenv(var->getstr().c_str());
  if(s == nullptr)
    return C_NIL;
  return mkstring(l, s);
}

PRIMITIVE exec::cd(LISPT dir, LISPT emess)
{
  LISPT ndir;

  if(is_NIL(dir))
    ndir = home;
  else
  {
    ndir = glob(dir);
    if(type_of(ndir) == CONS)
      ndir = ndir->car();
  }
  if(is_NIL(ndir))
  {
    if(is_NIL(emess))
      return l.error(NO_MATCH, dir);
    return C_NIL;
  }
  if(chdir(ndir->getstr().c_str()) == -1)
  {
    if(is_NIL(emess))
      return syserr(l, dir);
    return C_NIL;
  }
  else
  {
    char* wd = getcwd(nullptr, 0);
    setenviron("PWD", wd);
    free(wd);
    return C_T;
  }
  return ndir; // TODO: Is this correct?
}

PRIMITIVE exec::doexec(LISPT cmd)
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

exec::exec(lisp& lisp): base(lisp) {}

void exec::init()
{
  // clang-format off
  // mkprim(PN_EXPAND, expand, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_TO,      ::lisp::to,         subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_FROM,    ::lisp::from,       subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_TOTO,    ::lisp::toto,       subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_PIPECMD, ::lisp::pipecmd,    subr_t::S_NOEVAL, subr_t::S_SPREAD);
  mkprim(PN_BACK,    ::lisp::back,       subr_t::S_NOEVAL, subr_t::S_SPREAD);
  mkprim(PN_STOP,    ::lisp::stop,       subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_CD,      ::lisp::cd,         subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_REHASH,  ::lisp::rehash,     subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_JOBS,    ::lisp::jobs,       subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_FG,      ::lisp::fg,         subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_BG,      ::lisp::bg,         subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_SETENV,  ::lisp::p_setenv,   subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_GETENV,  ::lisp::getenviron, subr_t::S_NOEVAL, subr_t::S_NOSPREAD);
  mkprim(PN_EXEC,    ::lisp::doexec,     subr_t::S_NOEVAL, subr_t::S_SPREAD);
  // clang-format on
  ::lisp::rehash(*L);
  L->e().undefhook = execcommand;
}
