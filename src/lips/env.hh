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

#ifndef LIPS_ENV_HH
#define LIPS_ENV_HH

#include <string>
#include <lisp/lisp.hh>

namespace lisp
{

class env
{
public:
  env();

  cvariable_t& path;            // Search path for executables.
  cvariable_t& home;            // Home directory.
  cvariable_t& globsort;

  static std::string get(const std::string& pstr)
  {
    auto e = getenv(pstr.c_str());
    std::string result;
    if(e != nullptr)
      result = e;
    return result;
  }

private:
  /*
   * Processes the environment variable PATH and returns a list
   * of all directories in PATH.
   */
  static LISPT mungepath()
  {
    auto pstr = get("PATH");
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
