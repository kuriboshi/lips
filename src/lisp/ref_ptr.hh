//
// Lips, lisp shell.
// Copyright 2022 Krister Joas
//

#ifndef LISP_REF_PTR_HH
#define LISP_REF_PTR_HH

namespace lisp
{
// Very simple reference smart pointer with the reference counter built into
// the object pointed to.

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
/// @details This is the simplest type of reference counter. There is no mutex
/// locking access to the counter so it is not suitable for a multi threaded
/// environment.
///
template<typename T>
class ref_count
{
public:
  ref_count() = default;
  /// @brief Increase the reference counter.
  void retain()
  {
    ++_counter;
  }
  /// @brief Decrease the reference counter.
  ///
  /// @details Once the counter reaches zero the object is automatically
  /// deleted.
  void release()
  {
    if(!--_counter)
      ref_deleter(static_cast<T*>(this));
  }

protected:
  ~ref_count() = default;

private:
  /// @brief The reference counter.
  int _counter = 0;
};

///
/// @brief Reference pointer which controls the life cycle of T.
///
/// @details T needs to provide the member functions @c retain and
/// @c release by, for example, deriving from @c ref_count.
///
template<typename T>
class ref_ptr
{
public:
  ref_ptr() = default;
  ref_ptr(T* p): _ptr(p)
  {
    if(_ptr)
      _ptr->retain();
  }
  /// @brief Copy constructor will increase the reference count.
  ref_ptr(const ref_ptr& x): _ptr(x._ptr)
  {
    if(_ptr)
      _ptr->retain();
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
      if(x._ptr)
        x._ptr->retain();
      if(_ptr)
        _ptr->release();
      _ptr = x._ptr;
    }
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
    if(_ptr)
      _ptr->release();
  }
  /// @brief Smart pointer access operator.
  T& operator*() const { return *_ptr; }
  /// @brief Smart pointer access operator.
  T* operator->() const { return _ptr; }
  explicit operator bool() const noexcept
  {
    return _ptr != nullptr;
  }
private:
  /// @brief The pointer to the object being controlled.
  T* _ptr = nullptr;
  /// @brief Smart pointer comparor for sorting purposes.
  friend bool operator<(const ref_ptr<T>& x, const ref_ptr<T>& y) { return x._ptr < y._ptr; }
  /// @brief Equality operator.
  friend bool operator==(const ref_ptr<T>& l, const ref_ptr<T>& r)
  {
    return l._ptr == r._ptr;
  }
  /// @brief Equality operator.
  friend bool operator!=(const ref_ptr<T>& l, const ref_ptr<T>& r)
  {
    return l._ptr != r._ptr;
  }
};
}

#endif
