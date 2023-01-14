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

#ifndef LISP_REPL_HH
#define LISP_REPL_HH

#include "types.hh"

namespace lisp
{

class vm;

class repl
{
public:
  repl(vm&);
  ~repl() = default;

  lisp_t operator()(lisp_t);

private:
  void main_loop();

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

  class vm& _vm;
  int _level = -1;
};

} // namespace lisp

#endif
