//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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

#include "main.hh"

namespace lisp
{
class top
{
public:
  top(options_t options, ref_file_t file)
    : _options(options),
      _file(file)
  {}
  ~top() = default;

  static void init();

  lisp_t operator()(lisp_t);

  static lisp_t transform(lisp_t list);
  static lisp_t findalias(lisp_t exp);
  static void promptprint(lisp_t prompt);

  // History functions
  static lisp_t printhist();
  static lisp_t histget(std::int64_t num, lisp_t hlist);
  static void phist();
  static void addhist(lisp_t);
  static void remhist();
  static void trimhist();

  // Read table functions
  static lisp_t rmexcl(lisp_t);

  static lisp_t input_exp;
  static std::function<lisp_t(lisp_t)> transform_hook; // Applied on input if non-nullptr.
  static std::function<void()> prompt_hook;            // Called before the prompt is printed.

private:
  static lisp_t alias_expanded; //For checking alias loops.

  class cvariables
  {
  public:
    cvariables()
      : _history(makecvar("history", nil)),
        _histnum(makecvar("histnum", mknumber(1L))),
        _histmax(makecvar("histmax", mknumber(100L))),
        _topprompt(makecvar("prompt", mkstring("!_"))),
        _brkprompt(makecvar("brkprompt", mkstring("!:"))),
        _promptform(makecvar("promptform", nil))
    {}

    cvariable_t& history() const { return _history->cvariable(); }
    cvariable_t& histnum() const { return _histnum->cvariable(); }
    const cvariable_t& histmax() const { return _histmax->cvariable(); }
    const cvariable_t& topprompt() const { return _topprompt->cvariable(); }
    const cvariable_t& brkprompt() const { return _brkprompt->cvariable(); }
    cvariable_t& promptform() const { return _promptform->cvariable(); }

  private:
    lisp_t _history;            // Holds the history list.
    lisp_t _histnum;            // Current event number.
    lisp_t _histmax;            // Maximum number of events to save.
    lisp_t _topprompt;
    lisp_t _brkprompt;
    lisp_t _promptform;         // Evaluated before printing the prompt.
  };
  static std::unique_ptr<cvariables> variables;

  static bool _echoline;
  options_t _options;
  ref_file_t _file;
  int _level = 0;
};

} // namespace lisp

extern std::string current_prompt;

#endif
