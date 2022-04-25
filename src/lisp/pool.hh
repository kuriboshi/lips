#ifndef LISP_POOL_HH
#define LISP_POOL_HH

#include <cstdlib>
#include <new>
#include <deque>

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
    data()
    {
      _block = std::malloc(N * sizeof(T));
    }
    ~data()
    {
      std::free(_block);
    }
    void* _block;
    data* _next = nullptr;
    data* _prev = nullptr;
    std::size_t _free_items = N;
  };
  std::size_t size() const
  {
    if(_data != nullptr)
      return _free_count + _data->_free_items;
    return _free_count;
  }
  /// @brief Allocate memory for an object of type T.
  T* allocate()
  {
    // If we have any free objects already available we return that.
    if(_free != nullptr)
    {
      T* object = reinterpret_cast<T*>(_free);
      _free = _free->_next;
      --_free_count;
      return object;
    }
    // If we have no blocks of memory already allocated we allocate one block.
    if(_data == nullptr)
    {
      _data = new data;
      _current = _data->_block;
    }
    // If the block we have has no more data we need to allocate a new one and
    // link it up to a double linked list.
    if(_data->_free_items == 0)
    {
      _data->_prev = new data;
      _data->_prev->_next = _data;
      _data = _data->_prev;
      _current = _data->_block;
    }
    // Placement new of the new object from our storage. The _current member
    // points to the first available memory in the data block.
    T* object = ::new (_current) T;
    --_data->_free_items;
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
private:
  /// @brief Pointer to the head of a list of object previously returned to the
  /// pool by deallocate.
  item* _free = nullptr;
  /// @brief Counter of objects returned to the pool.
  unsigned _free_count = 0;
  /// @brief Pointer to the storage block from where new objects are allocated
  /// if there are no objects available in the free list.
  data* _data = nullptr;
  /// @brief Pointer to the next available memory in the data block.
  void* _current = nullptr;
};
}

#endif
