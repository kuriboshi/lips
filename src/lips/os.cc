//
// Lips, lisp shell.
// Copyright 1989, 2020-2022, 2024 Krister Joas
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

#include "os.hh"

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

namespace lisp
{
bool readchar(char* cp)
{
  auto i = read(0, cp, 1);
  return i == 1;
}

} // namespace lisp
