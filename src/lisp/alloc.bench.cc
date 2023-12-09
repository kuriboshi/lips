//
// Lips, lisp shell
// Copyright 2022 Krister Joas
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

#include <string>

#include <catch2/catch_test_macros.hpp>
#include <catch2/benchmark/catch_benchmark.hpp>

#include "alloc.hh"
#include "types.hh"

namespace lisp
{

template<int N>
class cons_test
{
public:
  cons_test()
  {
    for(auto i = N; i > 0; --i)
      l = cons(nil, l);
  }
  lisp_t l;
};

TEST_CASE("alloc: cons")
{
  BENCHMARK("alloc: cons(500)") { return cons_test<500>(); };
}

} // namespace lisp
