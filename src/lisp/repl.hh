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

#ifndef LISP_REPL_HH
#define LISP_REPL_HH

#include "lisp.hh"

namespace lisp
{

class repl
{
public:
  repl(lisp&);
  ~repl() = default;

  class level
  {
  public:
    level(repl& repl)
      : _repl(repl)
    {
      ++_repl._level;
    }
    ~level() { --_repl._level; }

  private:
    repl& _repl;
  };

  LISPT operator()(LISPT);

private:
  lisp& l;
  int _level = 0;
  LISPT _prompt;
  LISPT _break_prompt;
};

} // namespace lisp

#endif
