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

#include <lisp/types.hh>

namespace lisp::details::file
{

void init();

LISPT open(context&, LISPT, LISPT);
LISPT close(context&, LISPT);
LISPT ratom(context&, LISPT);
LISPT readc(context&, LISPT);
LISPT read(context&, LISPT);
LISPT print(context&, LISPT, LISPT);
LISPT load(context&, LISPT);
LISPT terpri(context&, LISPT);
LISPT prin1(context&, LISPT, LISPT);
LISPT prin2(context&, LISPT, LISPT);
LISPT printlevel(context&, LISPT);
LISPT spaces(context&, LISPT, LISPT);
LISPT readline(context&, LISPT);

bool loadfile(context&, const std::string&);

} // namespace lisp::details::file

#endif
