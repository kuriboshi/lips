//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

#include <vector>
#include <string>
#include "libisp.hh"
#include "except.hh"
#include "repl.hh"
#include "main.hh"

int main(int argc, const char** argv)
{
  bool test = false;
  Catch::Session session;
  using namespace Catch::clara;
  auto cli = session.cli() | Opt(test) ["--test"]("Turn on test");
  session.cli( cli ); 
  session.applyCommandLine(argc, argv);

  lisp::lisp lisp;
  if(test)
  {
    auto result = session.run();
    return result;
  }
  std::vector<std::string> args{argv + 1, argv + argc};
  for(auto f: args)
  {
    try
    {
      lisp::load(lisp::mkstring(f));
    }
    catch(const std::exception& ex)
    {
      std::cout << f << ": " << ex.what() << std::endl;
      return 1;
    }
  }
  return lisp::run(lisp);
}
