//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
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

lisp_t redir_to(context&, lisp_t cmd, lisp_t file, lisp_t filed);
lisp_t redir_append(context&, lisp_t cmd, lisp_t file, lisp_t filed);
lisp_t redir_from(context&, lisp_t cmd, lisp_t file, lisp_t filed);
lisp_t pipecmd(context&, lisp_t cmds);
lisp_t back(context&, lisp_t x);
lisp_t stop(context&);
lisp_t jobs(context&);
lisp_t fg(context&, lisp_t job);
lisp_t bg(context&, lisp_t job);
lisp_t setenv(context&, lisp_t var, lisp_t val);
lisp_t getenviron(context&, lisp_t var);
lisp_t cd(context&, lisp_t dir, lisp_t emess);
lisp_t doexec(context&, lisp_t cmd);

lisp_t rehash(context&);
void do_rehash();
} // namespace lisp::exec

namespace lisp
{
inline lisp_t redir_to(context& ctx, lisp_t cmd, lisp_t file, lisp_t filed)
{
  return exec::redir_to(ctx, cmd, file, filed);
}
inline lisp_t redir_append(context& ctx, lisp_t cmd, lisp_t file, lisp_t filed)
{
  return exec::redir_append(ctx, cmd, file, filed);
}
inline lisp_t redir_from(context& ctx, lisp_t cmd, lisp_t file, lisp_t filed)
{
  return exec::redir_from(ctx, cmd, file, filed);
}
inline lisp_t pipecmd(context& ctx, lisp_t cmds) { return exec::pipecmd(ctx, cmds); }
inline lisp_t back(context& ctx, lisp_t x) { return exec::back(ctx, x); }
inline lisp_t stop(context& ctx) { return exec::stop(ctx); }
inline lisp_t rehash(context&)
{
  exec::do_rehash();
  return NIL;
}
inline lisp_t rehash()
{
  exec::do_rehash();
  return NIL;
}
inline lisp_t jobs(context& ctx) { return exec::jobs(ctx); }
inline lisp_t fg(context& ctx, lisp_t job) { return exec::fg(ctx, job); }
inline lisp_t bg(context& ctx, lisp_t job) { return exec::bg(ctx, job); }
inline lisp_t setenv(context& ctx, lisp_t var, lisp_t val) { return exec::setenv(ctx, var, val); }
inline lisp_t getenviron(context& ctx, lisp_t var) { return exec::getenviron(ctx, var); }
inline lisp_t cd(context& ctx, lisp_t dir, lisp_t emess) { return exec::cd(ctx, dir, emess); }
inline lisp_t doexec(context& ctx, lisp_t cmd) { return exec::doexec(ctx, cmd); }

namespace pn
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
} // namespace pn

} // namespace lisp

extern bool insidefork;

extern void printdone();
extern void checkfork();

#endif
