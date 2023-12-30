# Data Types

> ```cpp
> inline constexpr auto nil = nullptr;
> ```

The nil object is the same as the null pointer.

> ```cpp
> class cons_t final: public ref_count<cons_t>
> ```

The cons cell.

A cons cell contains two pieces of data: The head (traditionally called
car) and the tail (traditionally called cdr).

> ```cpp
> class destblock_t final
> ```

Destination block is used to collect the parameters to a function.

The destblock_t is used to store variables and their values.  Each block of
variable/value pairs is proceeded by a control block which contains the
following pieces of information: The size of the block, the index of the
variable/value pair currently being set, and a link to another destblock_t
in a chain of blocks.

> ```cpp
> template<typename... Result, std::size_t... Sequence>
> auto make_tuple(const std::vector<lisp_t>& values, std::index_sequence<Sequence...>)
> ```

The `make_tuple` overloads creates a tuple from a vector.

- _Result_ Parameter pack used for the tuple.
- _Sequence_ Index sequence of result types.
- _values_ A vector with zero or more `lisp_t` values.

**Returns**: A tuple with the values from the vector _values_.

> ```cpp
> template<typename... Result>
> std::tuple<Result...> make_tuple(const std::vector<lisp_t>& values)
> ```

Makes a tuple with the values in a vector.

- _Result_ The pack of tuple types.
- _values_ A vector with zero or more lisp_t values.

**Returns**: A tuple with the values from the vector _values_.

> ```cpp
> class func_base
> ```

Base class for type erasure of the lisp function.

> ```cpp
> template<typename... Args>
> class func_t: public func_base
> ```

This class can store a function object with a variable number of
arguments.

The stored function can be called either via a variadic operator() or an
operator() taking a vector of arguments.

> ```cpp
> template<typename... Args>
> std::unique_ptr<func_base> make_fun(lisp_t (*fun)(Args...))
> ```

The `make_fun` set of overloads creates an object of type
func_t<Args...>.

The object created can then be stored in the `subr_t` object and later
called with the right number of parameters. There are three overloads
taking care of different cases of functions.

The first one handles simple function pointers.

- _Args_ The zero or more function argument types.
- _fun_ The function pointer.

**Returns**: A `unique_ptr` to _func_base_ which can then be stored in a
`subr_t`.

> ```cpp
> template<typename Lambda, typename... Args>
> std::unique_ptr<func_base> make_fun(Lambda fun, lisp_t (Lambda::*)(Args...) const)
> ```

Helper function to create a func_base pointer for lambdas.

- _Lambda_ The lambda function type.
- _Args_ The zero or more function argument types.
- _fun_ The lambda function.

**Returns**: A `unique_ptr` to _func_base_ which can then be stored in a
`subr_t`.

> ```cpp
> template<typename Lambda>
> std::unique_ptr<func_base> make_fun(Lambda&& fun)
> ```

Create a func_base pointer for lambdas.

- _Lambda_ The lambda function type.
- _fun_ The lambda function.

**Returns**: A `unique_ptr` to _func_base_ which can then be stored in a
`subr_t`.

> ```cpp
> class subr_t final
> ```

Structure describing a built-in function.

Built-in function can have zero, one, two, or three parameters.  They can
either evaluate their parameters or not (special forms).  Function can be
either spread (fixed number of arguments) or nospread (variable number of
arguments).

> ```cpp
> class lambda_t final: public ref_count<lambda_t>
> ```

Lambda representation.

> ```cpp
> class closure_t final: public ref_count<closure_t>
> ```

A closure (static binding).

> ```cpp
> class integer_t final
> ```

Wraps an integer value.

> ```cpp
> class double_t final
> ```

Wraps a floating point value.

> ```cpp
> class string_t final: public ref_count<string_t>
> ```

Wraps a string value.

> ```cpp
> class cvariable_t final
> ```

A representation of a C++ variable linked to a lisp variable.

Wraps a lisp_t value in such a way that the value can be changed from
either the C++ context or the lisp context and have the value be reflected
in both.

> ```cpp
> struct indirect_t
> ```

Wraps an indirect value type.

> ```cpp
> class file_t final: public ref_count<file_t>
> ```

Wraps a file source or sink.

> ```cpp
> class object final: public ref_count<object>
> ```

A class able to hold a value of any lisp type

The lisp objects are stored in a variant with accessor methods to set or
get the values. There is no checking of the correct type for the accessor
functions so calling them for the wrong type throws an exception.

> ```cpp
> inline object::type type_of(const lisp_t& a)
> inline object::type type_of(const object& a)
> inline object::type type_of(const cvariable_t& a)
> ```

Return the type of the object or the object inside a `lisp_t`
object.

Since lisp_t may be nullptr, which represents the nil value, it's not safe
to call lisp_t->gettype() directly.

> ```cpp
> inline bool is_T(const lisp_t& x)
> ```

Checks if the parameter is equal to the symbol `t`.

> ```cpp
> inline bool is_nil(const lisp_t& x)
> ```

Checks if the lisp_t value is equal to `nil`.

> ```cpp
> inline bool is_nil(const object& x)
> ```

Checks if the object value is equal to `nil`.

> ```cpp
> inline bool is_nil(const cvariable_t& x)
> ```

Checks if the cvariable_t value is equal to `nil`.
