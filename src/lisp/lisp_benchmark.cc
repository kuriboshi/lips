//
// Lips, lisp shell.
// Copyright 2022-2023 Krister Joas
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
#include <iostream>

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_session.hpp>

#include "lisp.hh"

int main(int argc, const char** argv)
{
  try
  {
    Catch::Session session;
    std::vector<std::string> load;
    std::vector<std::string> loadpath;
    session.applyCommandLine(argc, argv);
    auto ctx = std::make_unique<lisp::context_t>();
    lisp::vm_t vm(std::move(ctx));
    return session.run();
  }
  catch(const lisp::lisp_finish& ex)
  {
    return ex.exit_code;
  }
  catch(const std::exception& ex)
  {
    std::cerr << ex.what() << std::endl;
    return 1;
  }
}
