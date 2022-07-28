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

#ifndef LISP_DETAILS_STRING_HH
#define LISP_DETAILS_STRING_HH

#include <lisp/lisp.hh>

namespace lisp::details::string
{

void init();

LISPT symstr(context&, LISPT);
LISPT stringp(context&, LISPT);
LISPT strequal(context&, LISPT, LISPT);
LISPT strcmp(context&, LISPT, LISPT);
LISPT concat(context&, LISPT);
LISPT strlen(context&, LISPT);
LISPT substring(context&, LISPT, LISPT, LISPT);

} // namespace lisp::details::string

#endif
