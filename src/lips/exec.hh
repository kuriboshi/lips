//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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

namespace lisp::exec
{
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
/// (pipe-cmd '((find . -name foo) (xargs grep bar)))
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
lisp_t getenviron(lisp_t var);
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
inline constexpr auto CD = "cd";                  // change directory
inline constexpr auto EXPAND = "expand";          // expand wildcards
inline constexpr auto REDIR_TO = "redir-to";      // redirect to file
inline constexpr auto REDIR_FROM = "redir-from";  // redirect from file
inline constexpr auto REDIR_APPEND = "append-to"; // redirect appending to file
inline constexpr auto PIPECMD = "pipe-cmd";       // pipe commands
inline constexpr auto BACK = "back";              // run command in background
inline constexpr auto STOP = "stop-lips";         // stop lips, return to superior
inline constexpr auto REHASH = "rehash";          // recalculate hash table
inline constexpr auto JOBS = "jobs";              // list jobs
inline constexpr auto FG = "fg";                  // run job in foreground
inline constexpr auto BG = "bg";                  // run job in background
inline constexpr auto SETENV = "setenv";          // set environment variable
inline constexpr auto GETENV = "getenv";          // get value of variable
inline constexpr auto EXEC = "exec";              // overlay lips with command
} // namespace lisp::pn

extern bool insidefork; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

extern void printdone();
extern void checkfork();

#endif
