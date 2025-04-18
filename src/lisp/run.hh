//
// Lips, lisp shell.
// Copyright 2021-2025 Krister Joas
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

#pragma once

#include <iostream>

#include "repl.hh"
#include "types.hh"
#include "vm.hh"

namespace lisp
{

inline int run(vm& vm, std::ostream& out = std::cout)
{
  repl repl(vm);
  vm.repl = [&repl](const lisp_t& exp) -> lisp_t { return repl(exp); };
  while(true)
  {
    try
    {
      vm.repl(nil);
      vm.repl = nullptr;
      // If we return normally from repl we exit the program
      return 0;
    }
    catch(const lisp_reset&)
    {
      vm.unwind();
    }
    catch(const lisp_error& ex)
    {
      out << "lisp_error: " << ex.what() << '\n';
      vm.unwind();
    }
    catch(const lisp_finish& ex)
    {
      return ex.exit_code;
    }
    catch(const std::exception& ex)
    {
      // Any standard exception resets the vm.
      out << "exception: " << ex.what() << '\n';
      vm.unwind();
    }
  }
}

} // namespace lisp
