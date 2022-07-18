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

#ifndef LISP_DETAILS_FILE_HH
#define LISP_DETAILS_FILE_HH

#include <lisp/lisp.hh>

namespace lisp::details::file
{

void init();

LISPT open(lisp&, LISPT, LISPT);
LISPT close(lisp&, LISPT);
LISPT ratom(lisp&, LISPT);
LISPT readc(lisp&, LISPT);
LISPT read(lisp&, LISPT);
LISPT print(lisp&, LISPT, LISPT);
LISPT load(lisp&, LISPT);
LISPT terpri(lisp&, LISPT);
LISPT prin1(lisp&, LISPT, LISPT);
LISPT prin2(lisp&, LISPT, LISPT);
LISPT printlevel(lisp&, LISPT);
LISPT spaces(lisp&, LISPT, LISPT);
LISPT readline(lisp&, LISPT);

bool loadfile(lisp&, const std::string&);

} // namespace lisp::details::file

#endif
