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

#include <unordered_map>
#include <lisp/lisp.hh>

namespace lisp::exec
{
void init();

lisp_t redir_to(lisp_t cmd, lisp_t file, lisp_t filed);
lisp_t redir_append(lisp_t cmd, lisp_t file, lisp_t filed);
lisp_t redir_from(lisp_t cmd, lisp_t file, lisp_t filed);
lisp_t pipecmd(lisp_t cmds);
lisp_t back(lisp_t x);
lisp_t stop();
lisp_t jobs();
lisp_t fg(lisp_t job);
lisp_t bg(lisp_t job);
lisp_t setenv(lisp_t var, lisp_t val);
lisp_t getenviron(lisp_t var);
lisp_t cd(lisp_t dir, lisp_t emess);
lisp_t doexec(lisp_t cmd);

lisp_t rehash();
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

extern bool insidefork;

extern void printdone();
extern void checkfork();

#endif
