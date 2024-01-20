//
// Lips, lisp shell.
// Copyright 2020-2023 Krister Joas
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

#ifndef LISP_DETAILS_PROP_HH
#define LISP_DETAILS_PROP_HH

#include <lisp/types.hh>

namespace lisp::details::property
{

void init();

lisp_t getplist(const lisp_t&);
lisp_t getprop(const lisp_t&, const lisp_t&);
lisp_t putprop(const lisp_t&, const lisp_t&, const lisp_t&);
lisp_t remprop(const lisp_t&, const lisp_t&);
lisp_t setplist(const lisp_t&, const lisp_t&);

} // namespace lisp::details::property

#endif
