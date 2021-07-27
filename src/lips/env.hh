//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//

#ifndef LIPS_ENV_HH
#define LIPS_ENV_HH

#include <string>
#include <lisp/libisp.hh>

namespace lisp
{

class environment
{
public:
  environment();

  cvariable& path;     // Search path for executables.
  cvariable& home;     // Home directory.
  cvariable& globsort;

private:
  /*
   * Processes the environment variable PATH and returns a list
   * of all directories in PATH.
   */
  LISPT mungepath(const std::string& pstr)
  {
    LISPT result = NIL;
    auto pos = pstr.size();
    for(;;)
    {
      auto next = pstr.rfind(':', pos);
      if(next == std::string::npos)
      {
        result = cons(mkstring(pstr.substr(0, pos - next)), result);
        break;
      }
      result = cons(mkstring(pstr.substr(next + 1, pos - next)), result);
      pos = next;
      if(pos == 0)
      {
        result = cons(mkstring(""), result);
        break;
      }
      --pos;
    }
    return result;
  }
};

}

#endif
