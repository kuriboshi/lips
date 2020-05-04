/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <lisp.hh>
#include <base.hh>

namespace lisp
{
inline constexpr auto PN_PRINTHIST = "??";          // print history

class top: public base
{
public:
  top(lisp&);
  ~top() = default;

  static void init();
  static LISPT printhist();

  static void phist();
  static void addhist(LISPT);
  static void remhist();
  static void trimhist();
  static LISPT history;         // Holds the history list.
  static LISPT histnum;         // Current event number.
  static LISPT histmax;         // Maximum number of events to save.
};

}

extern char current_prompt[];
extern lisp::LISPT input_exp;
extern lisp::LISPT topexp;
extern lisp::LISPT alias_expanded;
extern lisp::LISPT (*transformhook)(lisp::LISPT);
extern void (*beforeprompt)();

extern lisp::LISPT histget(int, lisp::LISPT);
extern lisp::LISPT findalias(lisp::LISPT);
extern bool toploop(lisp::LISPT*, int (*)(lisp::LISPT*));
