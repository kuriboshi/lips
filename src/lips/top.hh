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
#include "term.hh"

namespace lisp
{
class top
{
public:
  top(options_t options, std::unique_ptr<term_source> terminal)
    : _options(std::move(options)),
      _terminal(std::move(terminal))
  {}
  ~top() = default;

  top(const top&) = delete;
  top(top&&) = delete;
  top& operator=(const top&) = delete;
  top& operator=(top&&) = delete;

  static void init();

  lisp_t operator()(lisp_t);

  static lisp_t transform(lisp_t list);
  /// @brief Expands aliases in expression EXP.
  ///
  /// If car of _exp_ is a literal atom `findalias` checks for an alias
  /// substitution on property `alias`. If that is non-`nil` another expansion
  /// is tried until the alias property is `nil`. Alias looping is detected by
  /// saving each expanded atom on the list `alias_expanded`. One indirection
  /// is allowed in order to permit 'alias ls ls -F'.
  static lisp_t findalias(lisp_t exp);
  void set_prompt(lisp_t prompt);

  // History functions
  static lisp_t printhist();
  /// @brief Print the history list.
  static void print_history();
  /// @brief Add event to the history list.
  static void add_history(lisp_t);
  /// @brief Remove last event from the history list.
  static void remove_history();
  /// @brief Trim the history list to keep it shorter than histmax.
  static void trim_history();
  /// @brief Return the _num_ entry from the history list _hlist_, or `nil` if
  /// there is no entry.
  static lisp_t get_history(integer_t::value_type num, lisp_t hlist);

  /// @brief Redo read macro.
  ///
  /// The following commands are recognized.
  ///
  /// | character | effect                    |
  /// |-----------|---------------------------|
  /// | !!        | last command              |
  /// | !-n       | the n'th previous command |
  /// | !n        | command n                 |
  /// | !s        | command with prefix s     |
  /// | !$        | last argument             |
  /// | !*        | all arguments             |
  ///
  /// Others could be added easily.
  static lisp_t rmexcl(lisp_t);

  // NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
  static lisp_t input_exp;
  static std::function<lisp_t(lisp_t)> transform_hook; // Applied on input if non-nullptr.
  static std::function<void()> prompt_hook;            // Called before the prompt is printed.
  // NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)

private:
  // @brief For checking alias loops.
  static lisp_t alias_expanded; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

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
    lisp_t _history;    // Holds the history list.
    lisp_t _histnum;    // Current event number.
    lisp_t _histmax;    // Maximum number of events to save.
    lisp_t _topprompt;  // The top level prompt.
    lisp_t _brkprompt;  // The break loop prompt.
    lisp_t _promptform; // Evaluated before printing the prompt.
  };
  static std::unique_ptr<cvariables> variables; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

  static bool _echoline; // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
  options_t _options;
  std::unique_ptr<term_source> _terminal;
  int _level = 0;
  std::string _current_prompt;
};

} // namespace lisp

#endif
