//
// Lips, lisp shell.
// Copyright 2021-2022 Krister Joas
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

#ifndef LIPS_RUN_HH
#define LIPS_RUN_HH

#include <iostream>
#include "lisp/lisp.hh"
#include "lisp/repl.hh"

namespace lisp
{

inline int run(context& ctx, std::ostream& out = std::cout)
{
  repl repl(ctx);
  ctx.repl = [&repl](LISPT) -> LISPT { return repl(NIL); };
  while(true)
  {
    try
    {
      ctx.repl(NIL);
      // If we return normally from repl we exit the program
      return 0;
    }
    catch(const lisp_reset& ex)
    {
      ctx.e().reset();
    }
    catch(const lisp_error& ex)
    {
      out << ex.what() << std::endl;
    }
    catch(const lisp_finish& ex)
    {
      return ex.exit_code;
    }
    catch(const std::exception& ex)
    {
      out << ex.what() << std::endl;
    }
  }
  return 1;
}

} // namespace lisp

#endif
