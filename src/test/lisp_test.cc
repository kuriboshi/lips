//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

#include <vector>
#include <string>
#include <lisp/libisp.hh>

namespace
{
template<typename I>
lisp::LISPT buildpath(I i, I end)
{
  if(i == end)
    return lisp::NIL;
  return lisp::cons(lisp::mkstring(*i), buildpath(++i, end));
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
