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
#include <lisp/iter.hh>
#include "glob.hh"
#include "main.hh"
#include "top.hh"
#include "exec.hh"
#include "env.hh"

using namespace lisp;
using namespace std::literals;

std::unordered_map<std::string, std::string> exec::exechash;
extern char** environ;
LISPT p_setenv(LISPT, LISPT);

using UNION_WAIT = int;

bool insidefork = false;        // Is nonzero in the child after a fork

static int pgrp;                    /* Process group of current job */

struct job_t
{
  int jobnum;        // Job number
  int procid;        // Process id
  UNION_WAIT status; // Return value
  std::string wdir;  // Working directory
  LISPT exp;         // Job expression
  bool background;   // Nonzero means job runs in bg
  bool running;      // Nonzero if running
};

static std::list<job_t> joblist;  // List of jobs
static std::list<job_t> cjoblist; // List of collected jobs

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
static void printjob(const job_t& job)
{
  std::string buffer = fmt::format("[{}]  {} ", job.jobnum, job.procid);
  if(job.running)
    buffer += "Running";
  else if(WIFEXITED(job.status))
    buffer += "Done";
  else if(WIFSTOPPED(job.status))
    buffer += strsignal(WSTOPSIG(job.status));
  else
  {
    buffer += strsignal(WTERMSIG(job.status));
    if(WCOREDUMP(job.status))
      buffer += " (core dumped)";
  }
  buffer += "\t";
  primout().format(buffer);
  print(job.exp, false);
}

/* 
 * recordjob - register job with process id PID in the linked list of jobs. If 
 *             BG is non-zero, job is registered as running in background.
 *             Returns true if all went well, false otherwise.
 */
static bool recordjob(int pid, bool bg)
{
  if(insidefork)
    return true; /* Skip this if in a fork. */
  job_t job;
  if(!joblist.empty())
    job.jobnum = joblist.front().jobnum + 1;
  else
    job.jobnum = 1;
  job.procid = pid;
  job.status = 0;
  job.wdir = std::filesystem::current_path().string();
  job.exp = top::input_exp;
  job.background = bg;
  job.running = true;
  joblist.push_front(job);
  return true;
}

/* 
 * collectjob - updates job list with PID as process id, and STAT as exit 
 *              status.
 */
static void collectjob(int pid, UNION_WAIT stat)
{
  for(auto job = joblist.begin(); job != joblist.end(); ++job)
  {
    if(job->procid == pid)
    {
      job->running = false;
      job->status = stat;
      if(!WIFSTOPPED(job->status))
      {
        if(!job->background && WIFSIGNALED(job->status) && WTERMSIG(job->status) != SIGINT)
          printjob(*job);       // Print if not interrupted
        if(job->background) // When running in background, save on another list to be
        {                     
          // Collected when signaled with SIGCHLD
          cjoblist.push_front(*job);
        }
        job = joblist.erase(job);
      }
      else
        printjob(*job);
      break;
    }
  }
}

/* printdone - Sweeps CJOBLIST and prints each job it frees. */
void printdone()
{
  for(const auto& job: cjoblist)
    printjob(job);
  cjoblist.clear();
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
  for(auto cdir: env->path)
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

LISPT exec::redir_to(lisp& l, LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return NIL;
  check(file, type::STRING, type::SYMBOL);
  if(is_NIL(filed))
    oldfd = 1;
  else
  {
    check(filed, type::INTEGER);
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
  ::close(fd);
  return mknumber(l, WEXITSTATUS(status));
}

LISPT exec::redir_append(lisp& l, LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return NIL;
  check(file, type::STRING, type::SYMBOL);
  if(is_NIL(filed))
    oldfd = 1;
  else
  {
    check(filed, type::INTEGER);
    oldfd = filed->intval();
  }
  if((fd = ::open(file->getstr().c_str(), O_WRONLY | O_CREAT | O_APPEND, 0644)) == -1)
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
  ::close(fd);
  return mknumber(WEXITSTATUS(status));
}

LISPT exec::redir_from(lisp& l, LISPT cmd, LISPT file, LISPT filed)
{
  int fd, pid, oldfd;
  UNION_WAIT status;

  if(is_NIL(cmd))
    return NIL;
  check(file, type::STRING, type::SYMBOL);
  if(is_NIL(filed))
    oldfd = 0;
  else
  {
    check(filed, type::INTEGER);
    oldfd = filed->intval();
  }
  if((fd = ::open(file->getstr().c_str(), O_RDONLY)) == -1)
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
  ::close(fd);
  return mknumber(l, WEXITSTATUS(status));
}

LISPT exec::pipecmd(lisp& l, LISPT cmds)
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
    if(pipe(pd) == -1)
    {
      l.stderr().format("{}\n", strerror(errno));
      exit(1);
    }
    if((pid = mfork()) == 0)
    {
      ::close(pd[0]);
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
    ::close(pd[1]);
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

LISPT exec::back(lisp& l, LISPT x)
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
  std::cout << fmt::format("[{}] {}\n", joblist.front().jobnum, pid);
  return mknumber(l, pid);
}

LISPT exec::stop(lisp& l)
{
  kill(0, SIGSTOP);
  return T;
}

LISPT exec::rehash(lisp&)
{
  do_rehash();
  return NIL;
}

void exec::do_rehash()
{
  exechash.clear();

  for(auto p: env->path)
  {
    if(is_NIL(p))
      continue;
    check(p, type::STRING, type::SYMBOL);
    for(auto& odir: std::filesystem::directory_iterator(p->getstr()))
      exechash.try_emplace(odir.path().filename().string(), odir.path().parent_path().string());
  }
}

LISPT exec::jobs(lisp& l)
{
  for(const auto& job: joblist)
    printjob(job);
  return NIL;
}

LISPT exec::fg(lisp& l, LISPT job)
{
  job_t* current = nullptr;

  if(is_NIL(job))
  {
    for(auto& j: joblist)
    {
      if(WIFSTOPPED(j.status))
      {
        current = &j;
        break;
      }
    }
  }
  else
  {
    check(job, type::INTEGER);
    for(auto& j: joblist)
    {
      if(j.jobnum == job->intval())
      {
        current = &j;
        break;
      }
    }
  }
  if(current)
  {
    auto pgrp = getpgid(current->procid);
    current->running = true;
    printjob(*current);
    tcsetpgrp(1, pgrp);
    if(WIFSTOPPED(current->status))
      if(killpg(pgrp, SIGCONT) < 0)
        return syserr(l, mknumber(l, pgrp));
    current->status = 0;
    current->background = false;
    auto status = waitfork(current->procid);
    return mknumber(l, WEXITSTATUS(status));
  }
  return l.error(NO_SUCH_JOB, job);
}

LISPT exec::bg(lisp& l, LISPT job)
{
  job_t* current = nullptr;

  if(is_NIL(job))
  {
    for(auto& j: joblist)
    {
      if(!j.background)
      {
        current = &j;
        break;
      }
    }
  }
  else
  {
    check(job, type::INTEGER);
    for(auto& j: joblist)
    {
      if(j.jobnum == job->intval())
      {
        current = &j;
        break;
      }
    }
  }
  if(current)
  {
    auto pgrp = getpgid(current->procid);
    current->status = 0;
    current->running = true;
    printjob(*current);
    tcsetpgrp(1, pgrp);
    if(!current->background)
      if(killpg(pgrp, SIGCONT) < 0)
        return syserr(l, mknumber(l, pgrp));
    current->background = true;
    return T;
  }
  return l.error(NO_SUCH_JOB, job);
}

LISPT exec::p_setenv(lisp& l, LISPT var, LISPT val)
{
  check(var, type::STRING, type::SYMBOL);
  check(val, type::STRING, type::SYMBOL);
  setenv(var->getstr().c_str(), val->getstr().c_str(), 1);
  return var;
}

LISPT exec::getenviron(lisp& l, LISPT var)
{
  check(var, type::STRING, type::SYMBOL);
  char* s = getenv(var->getstr().c_str());
  if(s == nullptr)
    return NIL;
  return mkstring(l, s);
}

LISPT exec::cd(lisp& l, LISPT dir, LISPT emess)
{
  LISPT ndir;

  if(is_NIL(dir))
    ndir = env->home;
  else
  {
    ndir = expand(l, dir, NIL, NIL);
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

LISPT exec::doexec(lisp& l, LISPT cmd)
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

void exec::init()
{
  // clang-format off
  // mkprim(pn::EXPAND, expand, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(pn::REDIR_TO,     redir_to,     subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::REDIR_FROM,   redir_from,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::REDIR_APPEND, redir_append, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::PIPECMD,      pipecmd,      subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::BACK,         back,         subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::STOP,         stop,         subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::CD,           cd,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::REHASH,       rehash,       subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::JOBS,         jobs,         subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::FG,           fg,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::BG,           bg,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::SETENV,       p_setenv,     subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::GETENV,       getenviron,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::EXEC,         doexec,       subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  // clang-format on
  do_rehash();
  undefhook(execcommand);
}
