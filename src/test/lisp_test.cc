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

#define CATCH_CONFIG_RUNNER
#include <vector>
#include <string>
#include <catch2/catch.hpp>
#include <lisp/lisp.hh>

namespace
{
template<typename I>
lisp::LISPT buildpath(I i, I end)
{
  if(i == end)
    return lisp::NIL;
  auto s = lisp::mkstring(*i);
  return lisp::cons(s, buildpath(++i, end));
}
}

int main(int argc, const char** argv)
{
  try
  {
    Catch::Session session;
    std::vector<std::string> load;
    std::vector<std::string> loadpath;
    using namespace Catch::clara;
    auto cli = session.cli()
      | Opt(load, "load")["--load"]("Load a LISP file")
      | Opt(loadpath, "loadpath")["--loadpath"]("Set load loadpath");
    session.cli(cli);
    session.applyCommandLine(argc, argv);
    lisp::lisp lisp;
    if(!loadpath.empty())
    {
      auto path = buildpath(loadpath.begin(), loadpath.end());
      lisp.loadpath(path);
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
}
