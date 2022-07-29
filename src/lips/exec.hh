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

LISPT redir_to(context&, LISPT cmd, LISPT file, LISPT filed);
LISPT redir_append(context&, LISPT cmd, LISPT file, LISPT filed);
LISPT redir_from(context&, LISPT cmd, LISPT file, LISPT filed);
LISPT pipecmd(context&, LISPT cmds);
LISPT back(context&, LISPT x);
LISPT stop(context&);
LISPT jobs(context&);
LISPT fg(context&, LISPT job);
LISPT bg(context&, LISPT job);
LISPT setenv(context&, LISPT var, LISPT val);
LISPT getenviron(context&, LISPT var);
LISPT cd(context&, LISPT dir, LISPT emess);
LISPT doexec(context&, LISPT cmd);

LISPT rehash(context&);
void do_rehash();
} // namespace lisp::exec

namespace lisp
{
inline LISPT redir_to(context& ctx, LISPT cmd, LISPT file, LISPT filed)
{
  return exec::redir_to(ctx, cmd, file, filed);
}
inline LISPT redir_append(context& ctx, LISPT cmd, LISPT file, LISPT filed)
{
  return exec::redir_append(ctx, cmd, file, filed);
}
inline LISPT redir_from(context& ctx, LISPT cmd, LISPT file, LISPT filed)
{
  return exec::redir_from(ctx, cmd, file, filed);
}
inline LISPT pipecmd(context& ctx, LISPT cmds) { return exec::pipecmd(ctx, cmds); }
inline LISPT back(context& ctx, LISPT x) { return exec::back(ctx, x); }
inline LISPT stop(context& ctx) { return exec::stop(ctx); }
inline LISPT rehash(context&)
{
  exec::do_rehash();
  return NIL;
}
inline LISPT rehash()
{
  exec::do_rehash();
  return NIL;
}
inline LISPT jobs(context& ctx) { return exec::jobs(ctx); }
inline LISPT fg(context& ctx, LISPT job) { return exec::fg(ctx, job); }
inline LISPT bg(context& ctx, LISPT job) { return exec::bg(ctx, job); }
inline LISPT setenv(context& ctx, LISPT var, LISPT val) { return exec::setenv(ctx, var, val); }
inline LISPT getenviron(context& ctx, LISPT var) { return exec::getenviron(ctx, var); }
inline LISPT cd(context& ctx, LISPT dir, LISPT emess) { return exec::cd(ctx, dir, emess); }
inline LISPT doexec(context& ctx, LISPT cmd) { return exec::doexec(ctx, cmd); }

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
