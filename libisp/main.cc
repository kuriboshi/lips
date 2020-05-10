//
// Lips, lisp shell.
// Copyright 2020 Krister Joas
//

#include <iostream>
#include "libisp.hh"
#include "except.hh"

static int macro(lisp::lisp& l, lisp::LISPT*)
{
  l.e().unwind();
  throw lisp::lisp_reset();
}

int main()
{
  lisp::lisp lisp;
  auto prompt = mkstring(lisp, "> ");
  lisp.a().add_mark_object(&prompt);
  while(true)
  {
    try
    {
      lisp.repl(prompt, [](lisp::lisp& lisp, lisp::LISPT*) -> int { return macro(lisp, nullptr); });
      // If we return normally from repl we exit the program
      return 0;
    }
    catch(const lisp::lisp_error& ex)
    {
      std::cout << ex.what() << std::endl;
    }
    catch(const std::exception& ex)
    {
      std::cout << ex.what() << std::endl;
    }
  }
}
