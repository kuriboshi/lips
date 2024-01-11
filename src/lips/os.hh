//
// Lips, lisp shell.
// Copyright 2020-2022, 2024 Krister Joas
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

#ifndef LIPS_OS_HH
#define LIPS_OS_HH

namespace lisp
{
/// @internal Read a character from stdin.
///
/// The character read is put in the single character buffer _cp_.
///
/// Returns true if a character was read. If no character was read then this
/// will be interpreted as end of file and false is returned. In this case the
/// contents pointed to by _cp_ is left unchanged.
extern bool readchar(char* c);
} // namespace lisp

#endif
