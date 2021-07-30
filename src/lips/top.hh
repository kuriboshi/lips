/*
 * Lips, lisp shell.
 * Copyright 2020 Krister Joas
 *
 */

#pragma once

#include <functional>
#include <lisp/lisp.hh>

namespace lisp
{
inline constexpr auto PN_PRINTHIST = "??"; // print history

class top
{
public:
  top(lisp& lisp, const options_t& options, file_t& file) : l(lisp), options(options), file(file) {}
  ~top() = default;

  static void init(alloc&);

  LISPT operator()(LISPT);

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

  // Read table functions
  static LISPT rmexcl(lisp&, file_t&, LISPT, char);

  static LISPT input_exp;
  static std::function<LISPT(LISPT)> transform_hook; // Applied on input if non-nullptr.
  static std::function<void()> prompt_hook;          // Called before the prompt is printed.

private:
  static LISPT alias_expanded; //For checking alias loops.

  class cvariables
  {
  public:
    cvariables(alloc& a)
      : history(initcvar("history", NIL)),
        histnum(initcvar("histnum", a.mknumber(1L))),
        histmax(initcvar("histmax", a.mknumber(100L))),
        topprompt(initcvar("prompt", a.mkstring("!_"))),
        brkprompt(initcvar("brkprompt", a.mkstring("!:"))),
        promptform(initcvar("promptform", NIL))
    {}

    cvariable& history; // Holds the history list.
    cvariable& histnum; // Current event number.
    cvariable& histmax; // Maximum number of events to save.
    cvariable& topprompt;
    cvariable& brkprompt;
    cvariable& promptform;     // Evaluated before printing the prompt.
  };
  static std::unique_ptr<cvariables> variables;

  lisp& l;
  const options_t& options;
  file_t& file;
  int _level = 0;
};

} // namespace lisp

extern std::string current_prompt;
