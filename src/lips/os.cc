//
// Lips, lisp shell.
// Copyright 1989, 2020-2022 Krister Joas
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

#include <cstdio>
#include <unistd.h>

namespace lisp
{
/*
 * Read a characters from a terminal.  Returns 0 if
 * no character was read.  The character is returned in
 * the single character buffer cp.
 */
bool readchar(FILE* file, char* cp)
{
  auto i = read(fileno(file), cp, 1);
  return i == 1;
}

} // namespace lisp
