//
// Lips, lisp shell.
// Copyright 2021-2023 Krister Joas
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

#ifndef LISP_SYMBOL_HH
#define LISP_SYMBOL_HH

#include <cstdint>
#include <string>
#include <string_view>
#include <unordered_map>
#include <utility>
#include <vector>

#include "ref_ptr.hh"
#include "types.hh"

namespace lisp
{
class object;
using lisp_t = ref_ptr<object>;
extern lisp_t C_UNBOUND;

namespace symbol
{
struct sv_equal
{
  using is_transparent = std::true_type;

  bool operator()(std::string_view l, std::string_view r) const noexcept { return l == r; }
};

struct sv_hash
{
  using is_transparent = std::true_type;

  auto operator()(std::string_view str) const noexcept { return std::hash<std::string_view>()(str); }
};

class symbol_t final: public ref_count<symbol_t>
{
public:
  symbol_t() = default;
  ~symbol_t() = default;

  std::string pname;
  lisp_t self{};         // The lisp_t object for this symbol
  lisp_t value{};        // Value
  lisp_t plist{};        // The property list
  lisp_t topval{};       // Holds top value (not used yet)
  bool constant = false; // If true this is a constant which can't be set

  /// @brief The new and delete operators uses the global pool to create objects.
  static void* operator new(std::size_t) { return _pool.allocate(); }
  static void operator delete(void* x) { _pool.deallocate(x); }
  static void operator delete(symbol_t* x, std::destroying_delete_t) { _pool.deallocate(x); }

  static std::size_t freecount() { return _pool.size(); }

  static symbol_t* intern(std::string_view pname)
  {
    auto p = store().find(pname);
    if(p != store().end())
      return p->second;
    auto* sym = new symbol_t;
    sym->pname = pname;
    sym->value = C_UNBOUND;
    auto i = store().insert(std::make_pair(pname, sym));
    return i.first->second;
  }

  static void unintern(std::string_view pname)
  {
    auto p = store().find(pname);
    delete p->second;
    store().erase(p);
  }

  static bool exists(std::string_view pname) { return store().find(pname) != store().end(); }

  using store_t = std::unordered_map<std::string, symbol_t*, sv_hash, sv_equal>;
  static store_t& store()
  {
    static store_t _store;
    return _store;
  }

private:
  symbol_t(pool_test_t) { throw std::runtime_error("symbol_t"); }
  template<class T>
  friend void pool_test();

  using pool_t = pool<symbol_t, 256>;
  static pool_t _pool;
};

using ref_symbol_t = ref_ptr<symbol_t>;

} // namespace symbol
} // namespace lisp

#endif
