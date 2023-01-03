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

#ifndef LIPS_TOP_HH
#define LIPS_TOP_HH

#include <functional>
#include <lisp/lisp.hh>

namespace lisp
{
class top
{
public:
  top(const options_t& options, ref_file_t file)
    : _options(options),
      _file(file)
  {}
  ~top() = default;

  static void init();

  lisp_t operator()(lisp_t);

  lisp_t transform(lisp_t list);
  lisp_t findalias(lisp_t exp);
  static void promptprint(lisp_t prompt);

  // History functions
  static lisp_t printhist();
  static lisp_t histget(int num, lisp_t hlist);
  static void phist();
  static void addhist(lisp_t);
  static void remhist();
  static void trimhist();

  // Read table functions
  static lisp_t rmexcl(lisp_t);

  static lisp_t input_exp;
  static std::function<lisp_t(lisp_t)> transform_hook;           // Applied on input if non-nullptr.
  static std::function<void()> prompt_hook;                      // Called before the prompt is printed.

private:
  static lisp_t alias_expanded; //For checking alias loops.

  class cvariables
  {
  public:
    cvariables()
      : history(initcvar("history", nil)),
        histnum(initcvar("histnum", mknumber(1L))),
        histmax(initcvar("histmax", mknumber(100L))),
        topprompt(initcvar("prompt", mkstring("!_"))),
        brkprompt(initcvar("brkprompt", mkstring("!:"))),
        promptform(initcvar("promptform", nil))
    {}

    cvariable_t& history; // Holds the history list.
    cvariable_t& histnum; // Current event number.
    cvariable_t& histmax; // Maximum number of events to save.
    cvariable_t& topprompt;
    cvariable_t& brkprompt;
    cvariable_t& promptform; // Evaluated before printing the prompt.
  };
  static std::unique_ptr<cvariables> variables;

  static bool _echoline;
  const options_t& _options;
  ref_file_t _file;
  int _level = 0;
};

} // namespace lisp

extern std::string current_prompt;

#endif
