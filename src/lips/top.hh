/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <functional>
#include <lisp/lisp.hh>
#include <lisp/base.hh>

namespace lisp
{
inline constexpr auto PN_PRINTHIST = "??"; // print history

class top: public base
{
public:
  top(lisp& lisp, const options_t& options) : base(lisp), options(options) {}
  ~top() = default;

  static void init();

  bool toploop(LISPT* tprompt, int (*macrofun)(LISPT*), file_t&);

  static LISPT transform(LISPT list);
  static LISPT findalias(LISPT exp);
  static void promptprint(LISPT prompt);

  // History functions
  static LISPT printhist();
  static LISPT histget(int num, LISPT hlist);
  static void phist();
  static void addhist(LISPT);
  static void remhist();
  static void trimhist();
  static LISPT history; // Holds the history list.
  static LISPT histnum; // Current event number.
  static LISPT histmax; // Maximum number of events to save.

  // Read table functions
  static LISPT rmexcl(lisp&, file_t&, LISPT, char);

  static std::function<LISPT(LISPT)> transform_hook; // Applied on input if non-nullptr.
  static std::function<void()> prompt_hook;          // Called before the prompt is printed.

private:
  static LISPT alias_expanded; //For checking alias loops.
  static LISPT promptform;     // Evaluated before printing the prompt.

  const options_t& options;
};

} // namespace lisp

extern std::string current_prompt;
extern lisp::LISPT input_exp;
