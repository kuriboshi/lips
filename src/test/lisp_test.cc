//
// Lips, lisp shell.
// Copyright 2020-2022 Krister Joas
//

#define CATCH_CONFIG_RUNNER
#include <catch2/catch.hpp>

#include <lisp/libisp.hh>

int main(int argc, const char** argv)
{
  Catch::Session session;
  session.applyCommandLine(argc, argv);
  lisp::lisp lisp;
  auto result = session.run();
  return result;
}
