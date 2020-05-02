/*
 * Lips, lisp shell.
 * Copyright 1989, 2020 Krister Joas
 *
 */

#pragma once

namespace lisp
{
inline constexpr auto PN_CD = "cd";                 // change directory
inline constexpr auto PN_EXPAND = "expand";         // expand wildcards
inline constexpr auto PN_TO = "redir-to";           // redirect to file
inline constexpr auto PN_FROM = "redir-from";       // redirect from file
inline constexpr auto PN_TOTO = "append-to";        // redirect appending to file
inline constexpr auto PN_PIPECMD = "pipe-cmd";      // pipe commands
inline constexpr auto PN_BACK = "back";             // run command in background
inline constexpr auto PN_STOP = "stop-lips";        // stop lips, return to superior
inline constexpr auto PN_REHASH = "rehash";         // recalculate hash table
inline constexpr auto PN_JOBS = "jobs";             // list jobs
inline constexpr auto PN_FG = "fg";                 // run job in foreground
inline constexpr auto PN_BG = "bg";                 // run job in background
inline constexpr auto PN_SETENV = "setenv";         // set environment variable
inline constexpr auto PN_GETENV = "getenv";         // get value of variable
inline constexpr auto PN_EXEC = "exec";             // overlay lips with command
inline constexpr auto PN_PRINTHIST = "??";          // print history

} // namespace lisp
