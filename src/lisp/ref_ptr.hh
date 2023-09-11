//
// Lips, lisp shell.
// Copyright 2022-2023 Krister Joas
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

#ifndef LISP_REF_PTR_HH
#define LISP_REF_PTR_HH

#include <atomic>
#include <utility>

namespace lisp
{
// Very simple reference smart pointer with the reference counter built into
// the object pointed to.

/// @brief Use this type as the counter type for non-threaded applications.
using unsafe_counter = unsigned;
/// @brief Use this type for multithreaded applications.
using safe_counter = std::atomic<unsigned>;

///
/// @brief Default deleter simply deletes the object.
///
/// @details Override for a specific type get a different behaviour.
///
template<typename T>
inline void ref_deleter(T* p)
{
  delete p;
}

///
/// @brief Derive from this in order to use the ref_ptr class.
///
/// @details This reference counter can be used with or without a mutex
///   depending on the Counter template argument. If Counter is the
///   unsafe_counter type then it's not thread safe. If, on the other hand, the
///   type is safe_counter then the reference counter is thread safe.
///
/// @tparam T This is the type to be reference counted. T should derive from
///   ref_count<T>.
/// @tparam Counter The counter type used for reference counting. It can either
///   be one of the predefined unsafe_counter or safe_counter types or any type
///   for which the pre-increment and pre-decrement operators are defined.
///
template<typename T, typename Counter = safe_counter>
class ref_count
{
public:
  /// @brief Increase the reference counter.
  void retain() { ++_counter; }
  /// @brief Decrease the reference counter.
  ///
  /// @details Once the counter reaches zero the object is automatically
  /// deleted.
  void release()
  {
    if(--_counter == 0)
      ref_deleter(static_cast<T*>(this));
  }

protected:
  /// @brief Default constructor protected to not allow creating on the stack.
  ref_count() = default;

private:
  /// @brief The reference counter.
  mutable Counter _counter{0};
  /// @brief Only the ref_ptr class is allowed to create objects of type
  /// ref_count.
  template<typename U>
  friend class ref_ptr;
};

///
/// @brief Reference pointer which controls the life cycle of T.
///
/// @details T needs to provide the member functions @c retain and
/// @c release by, for example, deriving from @c ref_count.
///
template<typename T>
class ref_ptr final
{
public:
  /// @brief Default constructor initializes to nullptr.
  ref_ptr() = default;
  /// @brief Create an empty ref_ptr.
  ref_ptr(const std::nullptr_t)
    : _ptr(nullptr)
  {}
  /// @brief
  ref_ptr(T* p)
    : _ptr(p)
  {
    if(_ptr != nullptr)
      _ptr->retain();
  }
  /// @brief Copy constructor will increase the reference count.
  ref_ptr(const ref_ptr& x)
    : _ptr(x._ptr)
  {
    if(_ptr != nullptr)
      _ptr->retain();
  }
  /// @brief Move constructor
  ref_ptr(ref_ptr&& x) noexcept
    : _ptr(x._ptr)
  {
    x._ptr = nullptr;
  }
  /// @brief Create a ref_ptr<T> with a pointer to an object of type T.
  /// @brief Creates an object of type T and takes ownership of it.
  template<typename... Ts>
  static ref_ptr<T> create(Ts&&... ts)
  {
    return ref_ptr<T>(new T(std::forward<Ts>(ts)...));
  }
  /// @brief Assignment operator.
  ///
  /// @details The reference counter of the assigned object is incremented. If
  /// the assignee is not null then the reference counter is decremented before
  /// it's set.
  ref_ptr& operator=(const ref_ptr& x)
  {
    if(this != &x)
    {
      if(x._ptr != nullptr)
        x._ptr->retain();
      if(_ptr != nullptr)
        _ptr->release();
      _ptr = x._ptr;
    }
    return *this;
  }
  ref_ptr& operator=(ref_ptr&& x) noexcept
  {
    if(this != &x)
    {
      if(_ptr != nullptr)
        _ptr->release();
      _ptr = std::move(x._ptr);
      x._ptr = nullptr;
    }
    return *this;
  }
  ref_ptr& operator=(std::nullptr_t)
  {
    if(_ptr != nullptr)
      _ptr->release();
    _ptr = nullptr;
    return *this;
  }
  ref_ptr& operator=(T* p)
  {
    ref_ptr(p).swap(*this);
    return *this;
  }
  void swap(ref_ptr& s) noexcept
  {
    auto* tmp = _ptr;
    _ptr = s._ptr;
    s._ptr = tmp;
  }
  /// @brief Decrease the reference counter.
  ~ref_ptr()
  {
    if(_ptr != nullptr)
      _ptr->release();
  }
  /// @brief Smart pointer access operator.
  T& operator*() const { return *_ptr; }
  /// @brief Smart pointer access operator.
  T* operator->() const { return _ptr; }
  /// @brief Smart pointer comparor for sorting purposes.
  bool operator<(const ref_ptr<T>& x) const { return *_ptr < *x._ptr; }
  explicit operator bool() const noexcept { return _ptr != nullptr; }

private:
  /// @brief The pointer to the object being controlled.
  T* _ptr{nullptr};
  /// @brief Smart pointer comparor for sorting purposes.
  friend bool operator<(const ref_ptr<T>& x, const ref_ptr<T>& y) { return x._ptr < y._ptr; }
  /// @brief Equality operator.
  friend bool operator==(const ref_ptr<T>& l, const ref_ptr<T>& r) { return l._ptr == r._ptr; }
  /// @brief Equality operator.
  friend bool operator!=(const ref_ptr<T>& l, const ref_ptr<T>& r) { return l._ptr != r._ptr; }
};
} // namespace lisp

#endif
