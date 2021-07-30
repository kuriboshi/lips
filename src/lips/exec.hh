/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <unordered_map>
#include <lisp/lisp.hh>

namespace lisp
{
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
}

class exec
{
public:
  static void init();

  static LISPT redir_to(lisp&, LISPT cmd, LISPT file, LISPT filed);
  static LISPT redir_append(lisp&, LISPT cmd, LISPT file, LISPT filed);
  static LISPT redir_from(lisp&, LISPT cmd, LISPT file, LISPT filed);
  static LISPT pipecmd(lisp&, LISPT cmds);
  static LISPT back(lisp&, LISPT x);
  static LISPT stop(lisp&);
  static LISPT jobs(lisp&);
  static LISPT fg(lisp&, LISPT job);
  static LISPT bg(lisp&, LISPT job);
  static LISPT p_setenv(lisp&, LISPT var, LISPT val);
  static LISPT getenviron(lisp&, LISPT var);
  static LISPT cd(lisp&, LISPT dir, LISPT emess);
  static LISPT doexec(lisp&, LISPT cmd);

  static LISPT rehash(lisp&);
  static void do_rehash();

private:
  static int execcommand(LISPT, LISPT*);
  static std::unordered_map<std::string, std::string> exechash;
};

inline LISPT redir_to(lisp& l, LISPT cmd, LISPT file, LISPT filed) { return exec::redir_to(l, cmd, file, filed); }
inline LISPT redir_append(lisp& l, LISPT cmd, LISPT file, LISPT filed) { return exec::redir_append(l, cmd, file, filed); }
inline LISPT redir_from(lisp& l, LISPT cmd, LISPT file, LISPT filed) { return exec::redir_from(l, cmd, file, filed); }
inline LISPT pipecmd(lisp& l, LISPT cmds) { return exec::pipecmd(l, cmds); }
inline LISPT back(lisp& l, LISPT x) { return exec::back(l, x); }
inline LISPT stop(lisp& l) { return exec::stop(l); }
inline LISPT rehash(lisp&) { exec::do_rehash(); return NIL; }
inline LISPT rehash() { exec::do_rehash(); return NIL; }
inline LISPT jobs(lisp& l) { return exec::jobs(l); }
inline LISPT fg(lisp& l, LISPT job) { return exec::fg(l, job); }
inline LISPT bg(lisp& l, LISPT job) { return exec::bg(l, job); }
inline LISPT p_setenv(lisp& l, LISPT var, LISPT val) { return exec::p_setenv(l, var, val); }
inline LISPT getenviron(lisp& l, LISPT var) { return exec::getenviron(l, var); }
inline LISPT cd(lisp& l, LISPT dir, LISPT emess) { return exec::cd(l, dir, emess); }
inline LISPT doexec(lisp& l, LISPT cmd) { return exec::doexec(l, cmd); }

} // namespace lisp

extern bool insidefork;

extern void printdone();
extern void checkfork();
