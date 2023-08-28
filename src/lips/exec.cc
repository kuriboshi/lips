//
// Lips, lisp shell.
// Copyright 1988, 2020-2023 Krister Joas
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>

#include <fmt/format.h>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <iostream>
#include <array>
#include <system_error>
#include <cstdlib>
#include <csignal>

#include <lisp/lisp.hh>
#include "job.hh"
#include "glob.hh"
#include "main.hh"
#include "top.hh"
#include "exec.hh"
#include "env.hh"
#include "lips_error.hh"

using namespace lisp;
using namespace std::literals;

#if defined(__APPLE__) || defined(__FreeBSD__)
extern char** environ; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
#endif

// Is nonzero in the child after a fork
bool insidefork = false; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

static std::unordered_map<std::string, std::string> exechash; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

namespace
{
///
/// @brief preparefork Prepares a provess after fork.
///
/// @details Sets the processgroup to the group currently beeing built.  Resets
/// signals to their default value.
///
void preparefork()
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

///
/// @brief mfork Forks and initializes the child.
///
/// @details Forks and initializes the child. If the process hasn't previously
/// been forked, its pid is used as process group id. It also grabs the tty for
/// the new process group.
///
/// @return The pid of the new process.
///
int mfork()
{
  int pid = 0;

  if(pid = fork(); pid == 0)
  {
    auto pgrp = getpid();
    if(!insidefork)
    {
      setpgid(1, pgrp);
      tcsetpgrp(1, pgrp);
      insidefork = true;
    }
    preparefork();
    return pid;
  }
  if(pid < 0)
  {
    if(insidefork)
      std::cerr << fmt::format("{}\n", std::error_code(errno, std::system_category()).message());
    else
      vm::primerr()->format("{}\n", std::error_code(errno, std::system_category()).message());
    return pid;
  }
  if(!insidefork)
    job::recordjob(job::createjob(pid), false);
  return pid;
}

///
/// @brief check_meta Checks for meta characters.
///
/// @details Checks the string S if it contains any non-quoted meta characters
/// in which case it returns true. It also strips off all quote-characters
/// (backslash).
///
/// @param s String to check.
///
/// @return Pair of bool and string. The bool is true if string contains any
/// meta characters otherwise false. The string is the input string stripped of
/// any quote characters.
///
std::pair<bool, std::string> check_meta(const std::string& s)
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
    if(quote)
      quote = false;
    else if("*?[]"s.find_first_of(c) != std::string::npos)
    {
      meta = true;
    }
    result.push_back(c);
  }
  return {meta, result};
}

std::optional<std::vector<std::string>> process_one(lisp_t arg)
{
  std::vector<std::string> args;
  if(type_of(arg) == object::type::Symbol)
  {
    auto c = glob::extilde(arg->getstr());
    if(!c)
      return {};
    auto [meta, str] = check_meta(*c);
    if(!meta)
      args.push_back(str);
    else
    {
      auto files = glob::expandfiles(*c, true);
      if(type_of(files) == object::type::Cons)
      {
        for(auto f: files)
          args.push_back(f->getstr());
      }
      else if(is_nil(files))
      {
        error(lips_errc::no_match, arg);
        return {};
      }
    }
  }
  else if(type_of(arg) == object::type::Integer)
    args.push_back(std::to_string(arg->intval()));
  else if(type_of(arg) == object::type::String)
    args.push_back(arg->getstr());
  else if(type_of(arg) == object::type::Cons)
  {
    auto result = process_one(eval(arg));
    if(!result)
      return {};
    for(auto s: *result)
      args.push_back(s);
  }
  else
  {
    error(lips_errc::illegal_arg, arg);
    return {};
  }
  return args;
}

///
/// @brief make_exec Builds a command line for execve.
///
/// @details Parses command line expression and builds argument vector suitable
/// for execve.
///
/// @return Vector with command and arguments.
///
std::vector<std::string> make_exec(lisp_t command)
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

///
/// @brief waitfork Wait for a process.
///
/// @details Wait for specific process or the first one if PID is zero.
///
/// @param pid The process id to wait for. Zero mean wait for the first process
/// to change its status.
///
/// @return The status.
///
job::stat_t waitfork(pid_t pid)
{
  job::stat_t stat{0};

  while(true)
  {
    auto wpid = waitpid(pid, &stat.stat, WUNTRACED);
    if(wpid == -1 && errno == EINTR)
      continue;
    if(wpid == pid)
    {
      if(WIFSIGNALED(stat.stat))
      {
        unwind();
        throw lisp_error(lips_errc::error, "waitfor");
      }
      job::collectjob(wpid, stat);
      break;
    }
    if(wpid != -1 && !insidefork)
      job::collectjob(wpid, stat);
  }
  return stat;
}

///
/// @brief execute Execute a process.
///
/// @details Fork a new process, if not already in a fork, and calls execve.
/// Wait for the process to return (using waitfork).
///
/// @param name Name of the program.
/// @param command List of command arguments.
///
/// @return C_ERROR if there is an error or the exit status of the process.
///
lisp_t execute(const std::string& name, lisp_t command)
{
  auto args = make_exec(command);
  std::vector<char*> argv;
  for(auto& a: args)
    argv.push_back(a.data());
  argv.push_back(nullptr);
  if(insidefork)
  {
    execve(name.c_str(), argv.data(), environ);
    if(errno == ENOEXEC)
      execvp(name.c_str(), argv.data());
    std::cerr << std::error_code(errno, std::system_category()).message() << '\n';
    ::exit(1);
    // No return
  }
  auto pid = mfork();
  if(pid == 0)
  {
    execve(name.c_str(), argv.data(), environ);
    if(errno == ENOEXEC)
      execvp(name.c_str(), argv.data());
    std::cerr << std::error_code(errno, std::system_category()).message() << '\n';
    ::exit(1);
  }
  else if(pid < 0)
    return C_ERROR;
  auto status = waitfork(pid);
  return mknumber(WEXITSTATUS(status.stat));
}

///
/// @brief ifexec Check if file is executable.
///
/// @param dir Directory to check.
/// @param name Name of file to check.
///
/// @return True if directory DIR contains a NAME that is executable.
///
bool ifexec(const std::filesystem::path& dir, const std::filesystem::path& name)
{
  auto path = dir / name;
  std::error_code ec;
  auto status = std::filesystem::status(path, ec);
  if(ec)
    return false;
  return (status.type() == std::filesystem::file_type::regular
    && ((status.permissions() & std::filesystem::perms::others_exec) != std::filesystem::perms::none
      || (status.permissions() & std::filesystem::perms::group_exec) != std::filesystem::perms::none
      || (status.permissions() & std::filesystem::perms::owner_exec) != std::filesystem::perms::none));
}

} // namespace

void checkfork()
{
  while(true)
  {
    job::stat_t wstat{0};
    auto wpid = waitpid(-1, &wstat.stat, WUNTRACED | WNOHANG);
    if(wpid > 0)
      job::collectjob(wpid, wstat);
    else
      break;
  }
}

namespace lisp::exec
{
///
/// @brief execcommand - Tries to execute the lisp expression exp as a command.
///
/// @details Execcomand returns 0 if there is no executable file in the path, 1
/// if the command was successively run and -1 if there was some error.
///
int execcommand(lisp_t exp, lisp_t* res)
{
  *res = T;
  auto command = glob::extilde(exp->car()->getstr());
  if(!command || command->empty())
    return -1;
  if(command->at(0) == '/' || strpbrk(command->c_str(), "/") != nullptr)
  {
    if(execute(*command, exp) == C_ERROR)
      return -1;
    return 1;
  }

  auto cmd = exechash.find(*command);

  std::string comdir;
  for(auto cdir: environment->path())
  {
    if(is_nil(cdir) || cdir->getstr() == ".")
      comdir = "."s;
    else if(cmd != exechash.end() && *command == cmd->first)
      comdir = cdir->getstr();
    else
      continue;
    if(ifexec(comdir, *command))
    {
      comdir += "/";
      comdir += *command;
      if(execute(comdir, exp) == C_ERROR)
        return -1;
      return 1;
    }
  }
  return 0;
}

// Primitives

lisp_t redir_to(lisp_t cmd, lisp_t file, lisp_t filed)
{
  int fd = 0;
  int pid = 0;
  int oldfd = 0;

  if(is_nil(cmd))
    return nil;
  check(file, object::type::String, object::type::Symbol);
  if(is_nil(filed))
    oldfd = 1;
  else
  {
    check(filed, object::type::Integer);
    oldfd = static_cast<int>(filed->intval());
  }
  if(fd = ::open(file->getstr().c_str(), O_WRONLY | O_CREAT | O_TRUNC, 0644); fd == -1)
    return error(std::error_code(errno, std::system_category()), file);
  if(pid = mfork(); pid == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      vm::stderr()->format("{}\n", std::error_code(errno, std::system_category()).message());
      ::exit(1);
    }
    eval(cmd);
    ::exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  auto status = waitfork(pid);
  ::close(fd);
  return mknumber(WEXITSTATUS(status.stat));
}

lisp_t redir_append(lisp_t cmd, lisp_t file, lisp_t filed)
{
  int fd = 0;
  int pid = 0;
  int oldfd = 0;

  if(is_nil(cmd))
    return nil;
  check(file, object::type::String, object::type::Symbol);
  if(is_nil(filed))
    oldfd = 1;
  else
  {
    check(filed, object::type::Integer);
    oldfd = static_cast<int>(filed->intval());
  }
  if(fd = ::open(file->getstr().c_str(), O_WRONLY | O_CREAT | O_APPEND, 0644); fd == -1) // NOLINT
    return error(std::error_code(errno, std::system_category()), file);
  if(pid = mfork(); pid == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      vm::stderr()->format("{}\n", std::error_code(errno, std::system_category()).message());
      ::exit(1);
    }
    eval(cmd);
    ::exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  auto status = waitfork(pid);
  ::close(fd);
  return mknumber(WEXITSTATUS(status.stat));
}

lisp_t redir_from(lisp_t cmd, lisp_t file, lisp_t filed)
{
  int fd = 0;
  int pid = 0;
  int oldfd = 0;

  if(is_nil(cmd))
    return nil;
  check(file, object::type::String, object::type::Symbol);
  if(is_nil(filed))
    oldfd = 0;
  else
  {
    check(filed, object::type::Integer);
    oldfd = static_cast<int>(filed->intval());
  }
  if(fd = ::open(file->getstr().c_str(), O_RDONLY); fd == -1) // NOLINT
    return error(std::error_code(errno, std::system_category()), file);
  if(pid = mfork(); pid == 0)
  {
    if(dup2(fd, oldfd) < 0)
    {
      vm::stderr()->format("{}\n", std::error_code(errno, std::system_category()).message());
      ::exit(1);
    }
    eval(cmd);
    ::exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  auto status = waitfork(pid);
  ::close(fd);
  return mknumber(WEXITSTATUS(status.stat));
}

lisp_t pipecmd(lisp_t cmds)
{
  print(cmds);

  if(is_nil(cmds))
    return nil;
  if(is_nil(cmds->cdr()))
    return eval(cmds->car());

  int pid = 0;
  if(pid = mfork(); pid == 0)
  {
    std::array<int, 2> pd{};
    if(pipe(pd.data()) == -1) // NOLINT
    {
      vm::stderr()->format("{}\n", std::error_code(errno, std::system_category()).message());
      ::exit(1);
    }
    if(pid = mfork(); pid == 0)
    {
      ::close(pd[0]);
      if(dup2(pd[1], 1) < 0)
      {
        vm::stderr()->format("{}\n", std::error_code(errno, std::system_category()).message());
        ::exit(1);
      }
      eval(cmds->car());
      ::exit(0);
    }
    else if(pid < 0)
      ::exit(1);
    cmds = cmds->cdr();
    ::close(pd[1]);
    if(dup2(pd[0], 0) < 0)
    {
      vm::stderr()->format("{}\n", std::error_code(errno, std::system_category()).message());
      ::exit(1);
    }
    eval(cmds->car());
    auto status = waitfork(pid);
    ::exit(WEXITSTATUS(status.stat));
  }
  else if(pid < 0)
    return C_ERROR;
  auto status = waitfork(pid);
  return mknumber(WEXITSTATUS(status.stat));
}

lisp_t back(lisp_t x)
{
  int pid = 0;

  if(pid = fork(); pid == 0)
  {
    insidefork = true;
    preparefork();
    eval(x);
    ::exit(0);
  }
  else if(pid < 0)
    return C_ERROR;
  if(!insidefork)
    job::recordjob(job::createjob(pid), true);
  if(auto* currentjob = job::findjob([](const auto&) { return true; }); currentjob != nullptr)
    std::cout << fmt::format("[{}] {}\n", currentjob->jobnum, currentjob->procid);
  return mknumber(pid);
}

lisp_t stop()
{
  kill(0, SIGSTOP);
  return T;
}

lisp_t rehash()
{
  do_rehash();
  return nil;
}

void do_rehash()
{
  exechash.clear();

  for(auto p: environment->path())
  {
    if(is_nil(p))
      continue;
    check(p, object::type::String, object::type::Symbol);
    std::error_code ec;
    for(const auto& odir:
      std::filesystem::directory_iterator(p->getstr(), std::filesystem::directory_options::skip_permission_denied, ec))
      exechash.try_emplace(odir.path().filename().string(), odir.path().parent_path().string());
  }
}

lisp_t jobs()
{
  job::printjobs();
  return nil;
}

lisp_t fg(lisp_t job)
{
  job::job_t* current = nullptr;

  if(is_nil(job))
    current = job::findjob([](const auto& j) { return WIFSTOPPED(j.status); });
  else
  {
    check(job, object::type::Integer);
    current = job::findjob([&job](const auto& j) { return j.jobnum == job->intval(); });
  }
  if(current != nullptr)
  {
    auto pgrp = getpgid(current->procid);
    current->running = true;
    job::printjob(*current);
    tcsetpgrp(1, pgrp);
    if(WIFSTOPPED(current->status))
      if(killpg(pgrp, SIGCONT) < 0)
        return error(std::error_code(errno, std::system_category()), mknumber(pgrp));
    current->status = 0;
    current->background = false;
    auto status = waitfork(current->procid);
    return mknumber(WEXITSTATUS(status.stat));
  }
  return error(lips_errc::no_such_job, job);
}

lisp_t bg(lisp_t job)
{
  job::job_t* current = nullptr;

  if(is_nil(job))
    current = job::findjob([](const auto& j) { return !j.background; });
  else
  {
    check(job, object::type::Integer);
    current = job::findjob([&job](const auto& j) { return j.jobnum == job->intval(); });
  }
  if(current != nullptr)
  {
    auto pgrp = getpgid(current->procid);
    current->status = 0;
    current->running = true;
    job::printjob(*current);
    tcsetpgrp(1, pgrp);
    if(!current->background)
      if(killpg(pgrp, SIGCONT) < 0)
        return error(std::error_code(errno, std::system_category()), mknumber(pgrp));
    current->background = true;
    return T;
  }
  return error(lips_errc::no_such_job, job);
}

lisp_t setenv(lisp_t var, lisp_t val)
{
  check(var, object::type::String, object::type::Symbol);
  check(val, object::type::String, object::type::Symbol);
  ::setenv(var->getstr().c_str(), val->getstr().c_str(), 1);
  return var;
}

lisp_t getenviron(lisp_t var)
{
  check(var, object::type::String, object::type::Symbol);
  char* s = getenv(var->getstr().c_str());
  if(s == nullptr)
    return nil;
  return mkstring(s);
}

lisp_t cd(lisp_t dir, lisp_t emess)
{
  lisp_t ndir;

  if(is_nil(dir))
    ndir = environment->home();
  else
  {
    ndir = expand(dir);
    if(type_of(ndir) == object::type::Cons)
      ndir = ndir->car();
  }
  if(is_nil(ndir))
  {
    if(is_nil(emess))
      return error(lips_errc::no_match, dir);
    return nil;
  }
  if(chdir(ndir->getstr().c_str()) == -1)
  {
    if(is_nil(emess))
      return error(std::error_code(errno, std::system_category()), dir);
    return nil;
  }
  auto wd = std::filesystem::current_path();
  ::setenv("PWD", wd.c_str(), 1);
  return T;
}

lisp_t doexec(lisp_t cmd)
{
  lisp_t res;

  insidefork = true; // Prevent exec from forking
  switch(execcommand(cmd, &res))
  {
    case -1:
      return C_ERROR;
      break;
    default:
      break; // Never reached
  }
  return nil;
}

void init()
{
  // clang-format off
  // mkprim(pn::EXPAND, expand, subr_t::S_EVAL, subr_t::S_NOSPREAD);
  mkprim(pn::REDIR_TO,     redir_to,     subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::REDIR_FROM,   redir_from,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::REDIR_APPEND, redir_append, subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::PIPECMD,      pipecmd,      subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::BACK,         back,         subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  mkprim(pn::STOP,         stop,         subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::CD,           cd,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::REHASH,       rehash,       subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::JOBS,         jobs,         subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::FG,           fg,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::BG,           bg,           subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::SETENV,       setenv,       subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::GETENV,       getenviron,   subr_t::subr::NOEVAL, subr_t::spread::NOSPREAD);
  mkprim(pn::EXEC,         doexec,       subr_t::subr::NOEVAL, subr_t::spread::SPREAD);
  // clang-format on
  do_rehash();
  undefhook(execcommand);
}
} // namespace lisp::exec

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("exec.cc: check_meta")
{
  SECTION("test 1")
  {
    auto b = check_meta("hello");
    CHECK(!b.first);
  }
  SECTION("test 2")
  {
    auto b = check_meta("hello*");
    CHECK(b.first);
  }
  SECTION("test 3")
  {
    auto b = check_meta("hello\\*");
    CHECK(!b.first);
    CHECK(b.second == "hello*"s);
  }
  SECTION("test 4")
  {
    auto b = check_meta(R"(hello\*\[\])");
    CHECK(!b.first);
    CHECK(b.second == "hello*[]"s);
  }
  SECTION("test 5")
  {
    auto b = check_meta("hello\\*[a]\\*");
    CHECK(b.first);
    CHECK(b.second == "hello*[a]*"s);
  }
}

// NOLINTNEXTLINE(readability-function-cognitive-complexity)
TEST_CASE("exec.cc: make_exec")
{
  SECTION("(make_exec (a b c)) -> a b c")
  {
    auto result = make_exec(cons(mkstring("a"), cons(mkstring("b"), cons(mkstring("c"), nil))));
    CHECK(result.size() == 3);
    auto i = result.begin();
    CHECK(*i++ == "a");
    CHECK(*i++ == "b");
    CHECK(*i++ == "c");
  }
  SECTION("(make_exec (100)) -> 100")
  {
    auto result = make_exec(cons(mknumber(100), nil)); // NOLINT: Test code
    CHECK(result.at(0) == "100"s);
  }
  SECTION("(make_exec (plus 1 2)) -> 3")
  {
    auto expr = lispread("((plus 1 2))");
    auto result = make_exec(expr);
    CHECK(result.at(0) == "3"s);
  }
  SECTION("(make_exec (/b*)) -> /bin")
  {
    auto expr = lispread("(/b*)");
    auto result = make_exec(expr);
    REQUIRE(!result.empty());
    CHECK(result.at(0) == "/bin"s);
  }
  SECTION("(make_exec (/a*)) -> <empty>")
  {
    auto expr = lispread("(/a*)");
    auto result = make_exec(expr);
    REQUIRE(result.empty());
  }
}

TEST_CASE("execute")
{
  auto result = execute("/bin/ls", cons(mkstring("ls"), nil));
  CHECK(result->intval() == 0);
}
