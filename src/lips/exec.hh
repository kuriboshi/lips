//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

#ifndef LIPS_EXEC_HH
#define LIPS_EXEC_HH

/// @file exec.hh
///
/// # Shell Functions
///
/// This section describes functions which implement shell features like output
/// redirection, changing the working directory, job control, etc.
///
/// All functions that in some way redirects its input or output are executed
/// in a fork.  This means that redirecting I/O of a lisp function doesn't make
/// permanent changes to the `lips` environment.  No global variables are
/// changed.

#include <unordered_map>
#include <filesystem>
#include <lisp/lisp.hh>

#include "job.hh"

namespace lisp::exec
{
class process
{
public:
  /// @brief Prepares a process after fork.
  ///
  /// Sets the processgroup to the group currently beeing built.  Resets
  /// signals to their default value.
  void prepare();
  /// @brief Forks and initializes the child.
  ///
  /// Forks and initializes the child. If the process hasn't previously been
  /// forked, its pid is used as process group id. It also grabs the tty for the
  /// new process group.
  ///
  /// @returns The pid of the new process.
  int fork();
  /// @brief Checks for meta characters.
  ///
  /// Checks the string S if it contains any non-quoted meta characters in which
  /// case it returns true. It also strips off all quote-characters (backslash).
  ///
  /// @param s String to check.
  ///
  /// @returns Pair of bool and string. The bool is true if string contains any
  /// meta characters otherwise false. The string is the input string stripped of
  /// any quote characters.
  std::pair<bool, std::string> check_meta(const std::string& s);
  std::optional<std::vector<std::string>> process_one(const lisp_t& arg);
  /// @brief Builds a command line for execve.
  ///
  /// Parses command line expression and builds argument vector suitable for
  /// execve.
  ///
  /// @returns Vector with command and arguments.
  std::vector<std::string> make_exec(const lisp_t& command);
  /// @brief Wait for a process.
  ///
  /// Wait for specific process or the first one if PID is zero.
  ///
  /// @param pid The process id to wait for. Zero mean wait for the first process
  /// to change its status.
  ///
  /// @returns The status.
  job::stat_t waitfork(pid_t pid);
  /// @brief Execute a process.
  ///
  /// Fork a new process, if not already in a fork, and calls execve.  Wait for
  /// the process to return (using waitfork).
  ///
  /// @param name Name of the program.
  /// @param command List of command arguments.
  ///
  /// @returns ERROR if there is an error or the exit status of the process.
  lisp_t execute(const std::string& name, const lisp_t& command);
  /// @brief Check if file is executable.
  ///
  /// @param dir Directory to check.
  /// @param name Name of file to check.
  ///
  /// @returns True if directory DIR contains a NAME that is executable.
  bool is_executable(const std::filesystem::path& dir, const std::filesystem::path& name);
};

/// @brief Tries to execute the lisp expression exp as a command.
///
/// Execcomand returns 0 if there is no executable file in the path, 1 if the
/// command was successively run and -1 if there was some error.
int execcommand(lisp_t exp, lisp_t* res);
void init();

/// @brief Redirects output of command to a file.
/// @lisp{(redir-to cmd file fd),NLambda Function}
///
/// The command in _cmd_ is executed with the file descriptor in _filed_
/// redirected to a file named _file_.
///
/// @returns The exit status of the command once it exits. If something went
/// wrong and the symbol `error` is returned. If _cmd_ is `nil` then `nil` is
/// returned.
lisp_t redir_to(lisp_t cmd, lisp_t file, lisp_t filed);
/// @brief Redirects output, appending to a file.
/// @lisp{(redir-append cmd file filed),NLambda Function}
///
/// The file descriptor in _filed_ is redirected and the output from the
/// command _cmd_ is redirected to a file named _file_.
///
/// @returns The exit status of the command once it exits. If something went
/// wrong and the symbol `error` is returned. If _cmd_ is `nil` then `nil` is
/// returned.
lisp_t redir_append(lisp_t cmd, lisp_t file, lisp_t filed);
/// @brief Redirect input to a command from a file.
///
/// The file descriptor in _filed_ is redirected to read from a file named
/// _file_ when _cmd_ is executed.
///
/// @returns The exit status of the command once it exits. If something went
/// wrong and the symbol `error` is returned. If _cmd_ is `nil` then `nil` is
/// returned.
lisp_t redir_from(lisp_t cmd, lisp_t file, lisp_t filed);
/// @brief Connect output from one command to the input of the next.
/// @lisp{(pipe-cmd cmds),NLambda Function}
///
/// The argument _cmds_ is a list of commands which are in themselves lists. For example:
///
/// ```lisp
/// (pipe-cmd (find . -name foo) (xargs grep bar))
/// ```
///
/// If the list _cmds_ only contains one command no piping of the output is done.
///
/// @param cmds A list of command lists.
///
/// @returns The exit status of the last command once it exits. If something
/// went wrong and the symbol `error` is returned. If _cmd_ is `nil` then `nil`
/// is returned.
lisp_t pipecmd(lisp_t cmds);
/// @brief Runs a command in the background.
/// @lisp{(back cmd),NLambda Function}
///
/// Runs the command _cmd_ in the background.
///
/// @returns The process ID of the process running in the background. The
/// symbol `error` if the fork fails.
lisp_t back(lisp_t cmd);
/// @brief Stop the lips shell and return to parent process.
/// @lisp{(stop-lips),NLambda Function}
///
/// @returns `t`.
lisp_t stop();
/// @brief Lists active background jobs.
/// @lisp{(jobs),NLambda Function}
///
/// Prints a list of all active background jobs and their status.
///
/// @returns `nil`.
lisp_t jobs();
/// @brief Brings a background job to the foreground.
/// @lisp((fg job),NLambda Function}
///
/// Brings the specified _job_ to the foreground, or the current job if `nil`.
///
/// @returns The exit status when the job finishes.
lisp_t fg(lisp_t job);
/// @brief Runs a stopped job in the background.
/// @lisp{(bg job),NLambda Function}
///
/// Runs the _job_, which is normally in a stopped state, in the background.
///
/// @returns `t`.
lisp_t bg(lisp_t job);
/// @brief Sets an environment variable.
/// @lisp{(setenv var val),NLambda Function}
///
/// Sets the environment variable `var` to the value `val`. Each parameter must
/// be either a symbol or a string.
///
/// @returns The variable name.
lisp_t setenv(lisp_t var, lisp_t val);
/// @brief Gets the value of an environment variable.
/// @lisp{(getenv var),NLambda Function}
///
/// @returns The value of the variable or `nil` if not set.
lisp_t getenv(lisp_t var);
/// @brief Change current working directory.
/// @lisp{(cd dir emess),NLambda Function}
///
/// Changes the current working directory to _dir_. If _emess_ is `nil` then an
/// error is signaled if the error does not exist. If non-`nil` the error is
/// ignored. The value of the environment variable PWD is updated to the
/// current working directory.
///
/// If _dir_ is `nil` then the current working directory is changed to the
/// user's home directory. Glob patterns are expanded.
///
/// @returns `t` if the directory changed successfully, `nil' otherwise.
lisp_t cd(lisp_t dir, lisp_t emess);
lisp_t doexec(lisp_t cmd);

/// @brief Rehashes the command hash.
/// @lisp{(rehash)}
///
/// The `lips` shell maintains a hash map of command to path. The `rehash`
/// function updates the hash map with any changes.
///
/// @returns `nil`.
lisp_t rehash();
/// @brief Replace the lips shell with a command.
/// @lisp{(exec cmd),NLambda Function}
///
/// @returns If successful `exec` does not return, `nil` if not successful.
void do_rehash();
} // namespace lisp::exec

namespace lisp::pn
{
inline const std::string CD = "cd";                  // change directory
inline const std::string EXPAND = "expand";          // expand wildcards
inline const std::string REDIR_TO = "redir-to";      // redirect to file
inline const std::string REDIR_FROM = "redir-from";  // redirect from file
inline const std::string REDIR_APPEND = "append-to"; // redirect appending to file
inline const std::string PIPECMD = "pipe-cmd";       // pipe commands
inline const std::string BACK = "back";              // run command in background
inline const std::string STOP = "stop-lips";         // stop lips, return to superior
inline const std::string REHASH = "rehash";          // recalculate hash table
inline const std::string JOBS = "jobs";              // list jobs
inline const std::string FG = "fg";                  // run job in foreground
inline const std::string BG = "bg";                  // run job in background
inline const std::string SETENV = "setenv";          // set environment variable
inline const std::string GETENV = "getenv";          // get value of variable
inline const std::string EXEC = "exec";              // overlay lips with command
} // namespace lisp::pn

extern bool insidefork; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

extern void printdone();
extern void checkfork();

namespace lisp::atoms
{
inline const lisp_t BACK = intern(pn::BACK);
inline const lisp_t EXEC = intern(pn::EXEC);
inline const lisp_t OLDVAL = intern("oldval");
inline const lisp_t PIPE = intern(pn::PIPECMD);
inline const lisp_t PROGN = intern("progn");
inline const lisp_t REDIR_APPEND = intern(pn::REDIR_APPEND);
inline const lisp_t REDIR_FROM = intern(pn::REDIR_FROM);
inline const lisp_t REDIR_TO = intern(pn::REDIR_TO);
} // namespace lisp::atoms

#endif
