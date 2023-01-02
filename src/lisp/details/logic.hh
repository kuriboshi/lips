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

#ifndef LISP_DETAILS_LOGIC_HH
#define LISP_DETAILS_LOGIC_HH

#include <lisp/types.hh>

namespace lisp::details::logic
{

void init();

lisp_t p_and(lisp_t);
lisp_t p_or(lisp_t);
lisp_t p_not(lisp_t);

} // namespace lisp::details::logic

#endif
