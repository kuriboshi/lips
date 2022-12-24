//
// Lips, lisp shell.
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

#ifndef LISP_POOL_HH
#define LISP_POOL_HH

#include <new>
#include <list>

namespace lisp
{
/// @brief Memory pool for objects of type T.
///
/// @tparam T The type of objects tracked by this pool.
/// @tparam N The number of object of type T per block of memory.
template<typename T, std::size_t N = 64>
class pool final
{
public:
  /// @brief Stores a block of data to hold the objects of type T.
  struct data final
  {
    data() { _block = std::malloc(N * sizeof(T)); }
    /// @brief Disallow copying.
    data(const data&) = delete;
    /// @brief Disallow assignment.
    data& operator=(const data&) = delete;
    /// @brief Move constuctor.
    data(data&& d)
    {
      _block = d._block;
      _free_items = d._free_items;
      d._block = nullptr;
      d._free_items = 0;
    }
    /// @brief Disallow move assignment.
    data& operator=(data&&) = delete;
    /// @brief Destructor.
    ~data() { std::free(_block); }
    void* _block;
    std::size_t _free_items = N;
  };
  /// @brief Returns the number of free items.
  std::size_t size() const
  {
    if(!_storage.empty())
      return _free_count + _storage.front()._free_items;
    return _free_count;
  }

  /// @brief Allow objects using the pool to allocate and deallocate objects.
  friend T;

private:
  /// @brief Allocate memory for an object of type T.
  T* allocate()
  {
    // If we have any free objects already available we return that.
    if(_free != nullptr)
    {
      auto* tmp = _free->_next;
      T* object = std::construct_at(reinterpret_cast<T*>(_free));
      _free = tmp;
      --_free_count;
      return object;
    }
    // If we have no blocks of memory already allocated we allocate one block.
    if(_storage.empty())
    {
      data d;
      _storage.push_front(std::move(d));
      _current = _storage.front()._block;
    }
    // If the block we have has no more data we need to allocate a new one and
    // link it up to a double linked list.
    if(_storage.front()._free_items == 0)
    {
      _storage.push_front(data());
      _current = _storage.front()._block;
    }
    // Placement new of the new object from our storage. The _current member
    // points to the first available memory in the data block.
    T* object = ::new(_current) T;
    --_storage.front()._free_items;
    _current = static_cast<char*>(_current) + sizeof(T);
    return object;
  }
  /// @brief The item struct is used to link objects returned to the pool by
  /// deallocate in a linked list.
  struct item
  {
    item* _next;
  };
  /// @brief Return an object to the pool.
  void deallocate(void* obj)
  {
    // Need to call the destructor before pooling the object.
    static_cast<T*>(obj)->~T();
    item* i = static_cast<item*>(obj);
    i->_next = _free;
    _free = i;
    ++_free_count;
  }

  /// @brief Pointer to the head of a list of object previously returned to the
  /// pool by deallocate.
  item* _free = nullptr;
  /// @brief Counter of objects returned to the pool.
  unsigned _free_count = 0;
  /// @brief Pointer to the storage block from where new objects are allocated
  /// if there are no objects available in the free list.
  std::list<data> _storage;
  /// @brief Pointer to the next available memory in the data block.
  void* _current = nullptr;
};

struct pool_test_t {};

} // namespace lisp

#endif
