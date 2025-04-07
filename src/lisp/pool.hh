//
// Lips, lisp shell.
// Copyright 2022-2023, 2025 Krister Joas
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

#pragma once

#include <memory>
#include <new>
#include <list>
#include <cstdint>

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
  /// @brief Returns the number of free items.
  std::size_t size() const
  {
    if(!_storage.empty())
      return _free_count + _storage.front()->free_items;
    return _free_count;
  }

  /// @brief Allow objects using the pool to allocate and deallocate objects.
  friend T;

private:
  /// @brief Stores a block of data to hold the objects of type T.
  struct data final
  {
    /// @brief Default constructor.
    data() = default;
    /// @brief Disallow copying.
    data(const data&) = delete;
    /// @brief Disallow assignment.
    data& operator=(const data&) = delete;
    /// @brief Disallow move constuctor.
    data(data&& d) = delete;
    /// @brief Disallow move assignment.
    data& operator=(data&&) = delete;
    /// @brief Default destructor.
    ~data() = default;

    /// @brief A block of data.
    alignas(alignof(T)) std::byte block[N * sizeof(T)];
    /// @brief Counts the number of free items in the block.
    std::size_t free_items{N};
  };

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
      _storage.push_front(std::make_unique<data>());
      _current = _storage.front()->block;
    }
    // If the block we have has no more data we need to allocate a new one and
    // link it up to a double linked list.
    if(_storage.front()->free_items == 0)
    {
      _storage.push_front(std::make_unique<data>());
      _current = _storage.front()->block;
    }
    // Construct an object of type T at the first available memory location in
    // the data block.
    T* object = std::construct_at(reinterpret_cast<T*>(_current));
    --_storage.front()->free_items;
    if(_storage.front()->free_items > 0)
    {
      // Adjust the _current pointer to point to the next available memory
      // slot. Cast to std::uintptr_t to do the pointer arithmetic.
      auto address = reinterpret_cast<std::uintptr_t>(_current);
      // Ensure alignment of T objects.
      auto offset = address % alignof(T);
      if(offset != 0)
        address += alignof(T) - offset;
      _current = reinterpret_cast<std::byte*>(address + sizeof(T));
    }
    return object;
  }

  /// @brief The item struct is used to link objects returned to the pool by
  /// deallocate in a linked list.
  struct item
  {
    item* _next{nullptr};
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
  item* _free{nullptr};
  /// @brief Counter of objects returned to the pool.
  unsigned _free_count{0};
  /// @brief Pointer to the storage block from where new objects are allocated
  /// if there are no objects available in the free list.
  std::list<std::unique_ptr<data>> _storage;
  /// @brief Pointer to the next available memory in the data block.
  std::byte* _current{nullptr};
};

struct pool_test_t
{};

} // namespace lisp
