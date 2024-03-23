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

#include <vector>
#include <string>
#include <iostream>
#include <stdexcept>

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_session.hpp>

#include "alloc.hh"
#include "context.hh"
#include "except.hh"
#include "file.hh"
#include "io.hh"
#include "types.hh"
#include "vm.hh"

namespace
{
template<typename I>
lisp::lisp_t buildpath(I i, I end)
{
  if(i == end)
    return lisp::nil;
  auto s = lisp::mkstring(*i);
  return lisp::cons(s, buildpath(++i, end));
}
} // namespace

int main(int argc, const char** argv)
try
{
  Catch::Session session;
  std::vector<std::string> load;
  std::vector<std::string> loadpath;
  using namespace Catch::Clara;
  auto cli = session.cli() | Opt(load, "load")["--load"]("Load a LISP file")
    | Opt(loadpath, "loadpath")["--loadpath"]("Set load loadpath");
  session.cli(cli);
  session.applyCommandLine(argc, argv);
  try
  {
    lisp::vm::get();
    // This exception is never thrown
    throw std::runtime_error("lisp::vm::get didn't throw an exception");
  }
  catch(const std::runtime_error& ex)
  {
    std::string s0{ex.what()};
    std::string s1{"lisp::vm has not been created"};
    if(s0 != s1)
      throw;
  }
  auto context = std::make_unique<lisp::context_t>();
  lisp::vm_t vm(std::move(context));
  std::ostringstream os;
  auto quiet = lisp::ref_file_t::create(os);
  lisp::vm::primerr(quiet);
  if(!loadpath.empty())
  {
    auto path = buildpath(begin(loadpath), end(loadpath));
    lisp::vm::loadpath(path);
  }
  for(auto i: load)
    lisp::loadfile(i);
  auto result = session.run();
  return result;
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
