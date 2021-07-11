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

#include <fmt/format.h>
#include <doctest/doctest.h>
#include <filesystem>
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

using namespace lisp;
using namespace std::literals;

std::unordered_map<std::string, std::string> exec::exechash;
extern char** environ;
LISPT p_setenv(LISPT, LISPT);

using UNION_WAIT = int;

bool insidefork = false;        // Is nonzero in the child after a fork

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
  std::string buffer = fmt::format("[{}]  {} ", job->jobnum, job->procid);
  if(job->running)
    buffer += "Running";
  else if(WIFEXITED(job->status))
    buffer += "Done";
  else if(WIFSTOPPED(job->status))
    buffer += strsignal(WSTOPSIG(job->status));
  else
  {
    buffer += strsignal(WTERMSIG(job->status));
    if(WCOREDUMP(job->status))
      buffer += " (core dumped)";
  }
  buffer += "\t";
  primout().format(buffer);
  print(job->exp, NIL);
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
  job->exp = top::input_exp;
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
      std::cerr << fmt::format("{}\n", strerror(errno));
    else
      syserr(NIL);
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
  if(type_of(arg) == type::SYMBOL)
  {
    auto c = glob::extilde(arg->getstr(), true);
    if(!c)
      return {};
    auto [meta, str] = check_meta(*c);
    if(!meta)
      args.push_back(str);
    else
    {
      auto files = glob::expandfiles(*c, false, false, true);
      if(type_of(files) == type::CONS)
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
  else if(type_of(arg) == type::INTEGER)
    args.push_back(std::to_string(arg->intval()));
  else if(type_of(arg) == type::STRING)
    args.push_back(arg->getstr());
  else if(type_of(arg) == type::CONS)
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
    auto result = make_exec(cons(mkstring("a"), cons(mkstring("b"), cons(mkstring("c"), NIL))));
    REQUIRE(result);
    CHECK(result->size() == 3);
    auto i = result->begin();
    CHECK(*i++ == "a");
    CHECK(*i++ == "b");
    CHECK(*i++ == "c");
  }
  SUBCASE("(make_exec (100)) -> 100")
  {
    auto result = make_exec(cons(mknumber(100), NIL));
    REQUIRE(result);
    CHECK(result->at(0) == "100"s);
  }
  SUBCASE("(make_exec (+ 1 2)) -> 3")
  {
    auto expr = lispread("((+ 1 2))");
    auto result = make_exec(expr);
    REQUIRE(result);
    CHECK(result->at(0) == "3"s);
  }
  SUBCASE("(make_exec (/b*)) -> /bin")
  {
    auto expr = lispread("(/b*)");
    auto result = make_exec(expr);
    REQUIRE(result);
    REQUIRE(!result->empty());
    CHECK(result->at(0) == "/bin"s);
  }
  SUBCASE("(make_exec (/a*)) -> <empty>")
  {
    auto expr = lispread("(/a*)");
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
    unwind();
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
  for(auto& a: *args)
    argv.push_back(a.data());
  argv.push_back(nullptr);
  if(insidefork)
  {
    execve(name.c_str(), &argv[0], environ);
    if(errno == ENOEXEC)
      execvp(name.c_str(), &argv[0]);
    std::cerr << strerror(errno) << '\n';
    exit(1);
    /* No return */
  }
  auto pid = mfork();
  if(pid == 0)
  {
    execve(name.c_str(), &argv[0], environ);
    if(errno == ENOEXEC)
      execvp(name.c_str(), &argv[0]);
    std::cerr << strerror(errno) << '\n';
    exit(1);
  }
  else if(pid < 0)
    return C_ERROR;
  auto status = waitfork(pid);
  return mknumber(WEXITSTATUS(status));
}

TEST_CASE("execute")
{
  auto result = execute("/bin/ls", cons(mkstring("ls"), NIL));
  CHECK(result->intval() == 0);
}

/* 
 * ifexec - Returns true if directory DIR contains a NAME that is executable.
 */
static bool ifexec(const std::filesystem::path& dir, const std::filesystem::path& name)
{
  auto path = dir / name;
  std::error_code ec;
  auto status = std::filesystem::status(path, ec);
  if(ec)
    return false;
  if(status.type() == std::filesystem::file_type::regular
    && ((status.permissions() & std::filesystem::perms::others_exec) != std::filesystem::perms::none
      || (status.permissions() & std::filesystem::perms::group_exec) != std::filesystem::perms::none
      || (status.permissions() & std::filesystem::perms::owner_exec) != std::filesystem::perms::none))
    return true;
  return false;
}

/* 
 * execcommand - Tries to execute the lisp expression exp as a command. 
 *               execcomand returns 0 if there is no executable file in 
 *               the path, 1 if the command was successively run and -1 if 
 *               there was some error.
 */
int exec::execcommand(LISPT exp, LISPT* res)
{
  *res = T;
  auto command = glob::extilde(exp->car()->getstr(), true);
  if(!command || command->empty())
    return -1;
  if(command->at(0) == '/' || strpbrk(command->c_str(), "/") != nullptr)
  {
    if(EQ(execute(*command, exp), C_ERROR))
      return -1;
    else
      return 1;
  }

  auto cmd = exechash.find(*command);

  std::string comdir;
  for(auto cdir: path)
  {
    if(is_NIL(cdir) || cdir->getstr() == ".")
      comdir = ".";
    else if(cmd != exechash.end() && *command == cmd->first)
      comdir = cdir->getstr();
    else
      continue;
    if(ifexec(comdir, *command))
    {
      comdir += "/";
      comdir += *command;
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

/* Primitives */

PRIMITIVE exec::redir_to(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return NIL;
  l.check(file, type::STRING, type::SYMBOL);
  if(is_NIL(filed))
    oldfd = 1;
  else
  {
    l.check(filed, type::INTEGER);
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

PRIMITIVE exec::redir_append(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return NIL;
  l.check(file, type::STRING, type::SYMBOL);
  if(is_NIL(filed))
    oldfd = 1;
  else
  {
    l.check(filed, type::INTEGER);
    oldfd = filed->intval();
  }
  if((fd = open(file->getstr().c_str(), O_WRONLY | O_CREAT | O_APPEND, 0644)) == -1)
    return syserr(file);
  if((pid = mfork()) == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      l.stderr().format("{}\n", strerror(errno));
      exit(1);
    }
    eval(cmd);
    exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  status = waitfork(pid);
  close(fd);
  return mknumber(WEXITSTATUS(status));
}

PRIMITIVE exec::redir_from(LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return NIL;
  l.check(file, type::STRING, type::SYMBOL);
  if(is_NIL(filed))
    oldfd = 0;
  else
  {
    l.check(filed, type::INTEGER);
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
    return NIL;
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
  std::cout << fmt::format("[{}] {}\n", joblist->jobnum, pid);
  return mknumber(l, pid);
}

PRIMITIVE exec::stop()
{
  kill(0, SIGSTOP);
  return T;
}

PRIMITIVE exec::rehash()
{
  exechash.clear();

  for(auto p: path)
  {
    if(is_NIL(p))
      continue;
    check(p, type::STRING, type::SYMBOL);
    for(auto& odir: std::filesystem::directory_iterator(p->getstr()))
      exechash.try_emplace(odir.path().filename().string(), odir.path().parent_path().string());
  }
  return NIL;
}

PRIMITIVE exec::jobs()
{
#ifdef JOB_CONTROL
  for(auto* j = joblist; j; j = j->next) printjob(j);
#endif
  return NIL;
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
    l.check(job, type::INTEGER);
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
    l.check(job, type::INTEGER);
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
    return T;
  }
  return l.error(NO_SUCH_JOB, job);
#endif
}

PRIMITIVE exec::p_setenv(LISPT var, LISPT val)
{
  l.check(var, type::STRING, type::SYMBOL);
  l.check(val, type::STRING, type::SYMBOL);
  setenv(var->getstr().c_str(), val->getstr().c_str(), 1);
  return var;
}

PRIMITIVE exec::getenviron(LISPT var)
{
  l.check(var, type::STRING, type::SYMBOL);
  char* s = getenv(var->getstr().c_str());
  if(s == nullptr)
    return NIL;
  return mkstring(l, s);
}

PRIMITIVE exec::cd(LISPT dir, LISPT emess)
{
  LISPT ndir;

  if(is_NIL(dir))
    ndir = home;
  else
  {
    ndir = expand(dir);
    if(type_of(ndir) == type::CONS)
      ndir = ndir->car();
  }
  if(is_NIL(ndir))
  {
    if(is_NIL(emess))
      return l.error(NO_MATCH, dir);
    return NIL;
  }
  if(chdir(ndir->getstr().c_str()) == -1)
  {
    if(is_NIL(emess))
      return syserr(l, dir);
    return NIL;
  }
  else
  {
    auto wd = std::filesystem::current_path();
    setenv("PWD", wd.c_str(), 1);
    return T;
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
  return NIL;
}

exec::exec(lisp& lisp): base(lisp) {}

void exec::init()
{
  // clang-format off
  // mkprim(PN_EXPAND, expand, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(PN_REDIR_TO,     ::lisp::redir_to,     subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_REDIR_FROM,   ::lisp::redir_from,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_REDIR_APPEND, ::lisp::redir_append, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_PIPECMD,      ::lisp::pipecmd,      subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(PN_BACK,         ::lisp::back,         subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(PN_STOP,         ::lisp::stop,         subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_CD,           ::lisp::cd,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_REHASH,       ::lisp::rehash,       subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_JOBS,         ::lisp::jobs,         subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_FG,           ::lisp::fg,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_BG,           ::lisp::bg,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_SETENV,       ::lisp::p_setenv,     subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_GETENV,       ::lisp::getenviron,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(PN_EXEC,         ::lisp::doexec,       subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  // clang-format on
  rehash();
  undefhook(execcommand);
}
