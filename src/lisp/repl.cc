//
// Lips, lisp shell.
// Copyright 2020-2024 Krister Joas
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

#include "alloc.hh"
#include "atoms.hh"
#include "file.hh"
#include "repl.hh"
#include "vm.hh"

namespace lisp
{

repl::repl(class vm& vm)
  : _vm(vm)
{
  _vm.interactive(true);
}

void repl::main_loop()
{
  while(true)
  {
    vm::primout()->format("> ");
    auto expr = lispread(vm::primin());
    if(expr == atoms::ENDOFFILE)
      break;
    auto result = eval(expr);
    print(result);
  }
}

lisp_t repl::operator()(const lisp_t& exp)
{
  const level begin(*this);
  if(_level == 0)
  {
    main_loop();
    return nil;
  }
  while(true)
  {
    vm::primout()->format("{}: ", _level);
    auto com = lispread(vm::primin());
    if(com == atoms::ENDOFFILE)
      break;
    /* OK, EVAL, ^, ... */
    if(type_of(com) == object::type::Symbol && com->as_symbol()->pname == "help")
    {
      prin0("(go) continue"_s);
      terpri();
      prin0("(reset) back to top loop"_s);
      terpri();
      prin0("(bt) print backtrace"_s);
      terpri();
      prin0("(return exp) return expression"_s);
      terpri();
      continue;
    }
    if(type_of(com) == object::type::Cons)
    {
      if(com->car() == atoms::GO)
        return eval(exp);
      if(com->car() == atoms::RESET)
        throw lisp_reset();
      if(com->car() == atoms::BT)
      {
        _vm.bt();
        continue;
      }
      if(com->car() == atoms::RETURN)
        return is_nil(com->cdr()) ? nil : com->cdr()->car();
    }
    auto result = eval(com);
    print(result);
  }
  return nil;
}

} // namespace lisp
