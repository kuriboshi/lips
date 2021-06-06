/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <unordered_map>
#include <lisp/lisp.hh>
#include <lisp/base.hh>

namespace lisp
{
inline constexpr auto PN_CD = "cd";            // change directory
inline constexpr auto PN_EXPAND = "expand";    // expand wildcards
inline constexpr auto PN_TO = "redir-to";      // redirect to file
inline constexpr auto PN_FROM = "redir-from";  // redirect from file
inline constexpr auto PN_TOTO = "append-to";   // redirect appending to file
inline constexpr auto PN_PIPECMD = "pipe-cmd"; // pipe commands
inline constexpr auto PN_BACK = "back";        // run command in background
inline constexpr auto PN_STOP = "stop-lips";   // stop lips, return to superior
inline constexpr auto PN_REHASH = "rehash";    // recalculate hash table
inline constexpr auto PN_JOBS = "jobs";        // list jobs
inline constexpr auto PN_FG = "fg";            // run job in foreground
inline constexpr auto PN_BG = "bg";            // run job in background
inline constexpr auto PN_SETENV = "setenv";    // set environment variable
inline constexpr auto PN_GETENV = "getenv";    // get value of variable
inline constexpr auto PN_EXEC = "exec";        // overlay lips with command

class exec: public base
{
public:
  exec(lisp&);
  ~exec() = default;
  static void init();

  LISPT to(LISPT cmd, LISPT file, LISPT filed);
  LISPT toto(LISPT cmd, LISPT file, LISPT filed);
  LISPT from(LISPT cmd, LISPT file, LISPT filed);
  LISPT pipecmd(LISPT cmds);
  LISPT back(LISPT x);
  LISPT stop();
  static LISPT rehash();
  LISPT jobs();
  LISPT fg(LISPT job);
  LISPT bg(LISPT job);
  LISPT p_setenv(LISPT var, LISPT val);
  LISPT getenviron(LISPT var);
  LISPT cd(LISPT dir, LISPT emess);
  LISPT doexec(LISPT cmd);

private:
  static int execcommand(LISPT, LISPT*);
  static std::unordered_map<std::string, std::string> exechash;
};

inline LISPT to(lisp& l, LISPT cmd, LISPT file, LISPT filed) { return exec(l).to(cmd, file, filed); }
inline LISPT toto(lisp& l, LISPT cmd, LISPT file, LISPT filed) { return exec(l).toto(cmd, file, filed); }
inline LISPT from(lisp& l, LISPT cmd, LISPT file, LISPT filed) { return exec(l).from(cmd, file, filed); }
inline LISPT pipecmd(lisp& l, LISPT cmds) { return exec(l).pipecmd(cmds); }
inline LISPT back(lisp& l, LISPT x) { return exec(l).back(x); }
inline LISPT stop(lisp& l) { return exec(l).stop(); }
inline LISPT rehash(lisp& l) { return exec(l).rehash(); }
inline LISPT rehash() { return rehash(lisp::current()); }
inline LISPT jobs(lisp& l) { return exec(l).jobs(); }
inline LISPT fg(lisp& l, LISPT job) { return exec(l).fg(job); }
inline LISPT bg(lisp& l, LISPT job) { return exec(l).bg(job); }
inline LISPT p_setenv(lisp& l, LISPT var, LISPT val) { return exec(l).p_setenv(var, val); }
inline LISPT getenviron(lisp& l, LISPT var) { return exec(l).getenviron(var); }
inline LISPT cd(lisp& l, LISPT dir, LISPT emess) { return exec(l).cd(dir, emess); }
inline LISPT doexec(lisp& l, LISPT cmd) { return exec(l).doexec(cmd); }

} // namespace lisp

extern bool insidefork;

extern void printdone();
extern void checkfork();
