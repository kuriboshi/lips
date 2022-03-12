//
// Lips, lisp shell.
// Copyright 2021 Krister Joas
//

#ifndef LISP_SYMBOL_HH
#define LISP_SYMBOL_HH

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

#include "ref_ptr.hh"
#include "lisp.hh"

namespace lisp
{
class lisp_t;
using LISPT = ref_ptr<lisp_t>;
extern LISPT C_UNBOUND;

namespace symbol
{
struct symbol_t;
using store_t = std::vector<symbol_t>;
using symbol_index_t = store_t::size_type;
using symbol_collection_id = std::uint32_t;

struct symbol_index
{
  symbol_index_t index;
};

// The print_name class contains the symbol store identifier and the name of
// the symbol.
struct print_name
{
  symbol_collection_id ident = 0;
  symbol_index_t index = 0;
  std::string name;
};

struct symbol_t
{
  print_name pname;      // The printname of the atom
  LISPT self{};          // The LISPT object for this symbol
  LISPT value{};         // Value
  LISPT plist{};         // The property list
  LISPT topval{};        // Holds top value (not used yet)
  bool constant = false; // If true this is a constant which can't be set
};

class symbol_store_t
{
public:
  symbol_store_t(symbol_collection_id id): _id(id) {}
  symbol_store_t(symbol_store_t&& other) noexcept
    : _id(other._id), _map(std::move(other._map)), _store(std::move(other._store))
  {}
  ~symbol_store_t() {}

  symbol_store_t& operator=(const symbol_store_t&) = delete;
  symbol_store_t& operator=(symbol_store_t&& other) noexcept
  {
    std::swap(other._map, _map);
    std::swap(other._store, _store);
    _id = other._id;
    return *this;
  }
  bool exists(const std::string& name) { return _map.find(name) != _map.end(); }
  symbol_t& get(const std::string& name)
  {
    auto p = _map.find(name);
    if(p != _map.end())
      return _store[p->second];
    symbol_t symbol;
    symbol.pname = {_id, _store.size(), name};
    symbol.value = C_UNBOUND;
    _store.push_back(symbol);
    _map.emplace(name, symbol.pname.index);
    return _store[symbol.pname.index];
  }
  symbol_t& get(symbol_index_t index) { return _store.at(index); }
  store_t::iterator begin() { return _store.begin(); }
  store_t::iterator end() { return _store.end(); }

private:
  symbol_collection_id _id;
  using map_type = std::unordered_map<std::string, symbol_index_t>;
  map_type _map;
  store_t _store;
};

class symbol_collection
{
public:
  static const constexpr symbol_collection_id global_id = 0;

  symbol_collection()
  {
    // Create the global symbol store
    create();
  }
  ~symbol_collection() {}

  std::unordered_map<symbol_collection_id, symbol_store_t> collection;

  symbol_store_t& create()
  {
    auto [p, inserted] = collection.try_emplace(_free, _free);
    ++_free;
    return p->second;
  }

  symbol_store_t& symbol_store(symbol_collection_id id)
  {
    auto p = collection.find(id);
    if(p == collection.end())
      throw std::runtime_error("no such symbol store");
    return p->second;
  }

  bool exists(symbol_collection_id id, const std::string& name)
  {
    auto p = collection.find(id);
    if(p == collection.end())
      throw std::runtime_error("no such symbol store");
    return p->second.exists(name);
  }

  symbol_t& get(const print_name& pname)
  {
    auto p = collection.find(pname.ident);
    if(p == collection.end())
      throw std::runtime_error("no such symbol store");
    return p->second.get(pname.name);
  }

  symbol_t& get(symbol_collection_id id, const std::string& name)
  {
    auto p = collection.find(id);
    if(p == collection.end())
      throw std::runtime_error("no such symbol store");
    return p->second.get(name);
  }

  symbol_t& get(symbol_collection_id id, symbol_index_t index)
  {
    auto p = collection.find(id);
    if(p == collection.end())
      throw std::runtime_error("no such symbol store");
    return p->second.get(index);
  }

private:
  symbol_collection_id _free = 0;
};

} // namespace symbol
} // namespace lisp

#endif
