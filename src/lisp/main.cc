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

#include <vector>
#include <string>

#include "alloc.hh"
#include "file.hh"
#include "run.hh"

int main(int argc, const char** argv)
{
  lisp::context ctx;
  lisp::vm vm(ctx);
  std::vector<std::string> args{argv + 1, argv + argc};
  for(auto f: args)
  {
    try
    {
      lisp::load(lisp::mkstring(f));
    }
    catch(const lisp::lisp_finish& ex)
    {
      return ex.exit_code;
    }
    catch(const std::exception& ex)
    {
      std::cout << f << ": " << ex.what() << std::endl;
      return 1;
    }
  }
  return lisp::run(vm);
}
