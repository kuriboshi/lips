//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#ifndef LIPS_TOP_HH
#define LIPS_TOP_HH

#include <functional>
#include <lisp/lisp.hh>

namespace lisp
{
inline constexpr auto PN_PRINTHIST = "??"; // print history

class top
{
public:
  top(lisp& lisp, const options_t& options, ref_file_t file) : l(lisp), _options(options), _file(file) {}
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
  static LISPT rmexcl(lisp&, ref_file_t, LISPT, char);

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

    cvariable_t& history;       // Holds the history list.
    cvariable_t& histnum;       // Current event number.
    cvariable_t& histmax;       // Maximum number of events to save.
    cvariable_t& topprompt;
    cvariable_t& brkprompt;
    cvariable_t& promptform;    // Evaluated before printing the prompt.
  };
  static std::unique_ptr<cvariables> variables;

  lisp& l;
  const options_t& _options;
  ref_file_t _file;
  int _level = 0;
};

} // namespace lisp

extern std::string current_prompt;

#endif
