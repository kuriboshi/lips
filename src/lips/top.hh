/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <lisp/lisp.hh>
#include <lisp/base.hh>

namespace lisp
{
inline constexpr auto PN_PRINTHIST = "??"; // print history

class top: public base
{
public:
  top(lisp&);
  ~top() = default;

  static void init();

  static bool toploop(LISPT* tprompt, int (*macrofun)(LISPT*), file_t&);

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

  static LISPT (*transformhook)(LISPT); // Applied on input if non-nullptr.
  static void (*beforeprompt)();        // Called before the prompt is printed.

private:
  static LISPT alias_expanded; //For checking alias loops.
  static LISPT promptform;     // Evaluated before printing the prompt.
};

} // namespace lisp

extern char current_prompt[];
extern lisp::LISPT input_exp;
extern lisp::LISPT (*transformhook)(lisp::LISPT);
extern void (*beforeprompt)();
