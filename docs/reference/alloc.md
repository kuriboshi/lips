# Memory Allocation

Functions which are responsible for allocating objects and managing object
life cycle.

> ```cpp
> template<typename T>
> lisp_t getobject(T value)
> ```

Allocates an object from the list of free objects.

The _getobject_ functions allocates an _object_ and initializes it with the
value passed as an argument to _getobject_.

- _T_ The type to be stored.
- _value_ A value of a type which can be stored in an _object_.

**Returns**: A lisp_t object initialized with an object of type T.

> ```cpp
> template<typename T>
> requires std::convertible_to<T, integer_t::value_type>
> lisp_t getobject(T value)
> ```

Partial specialization when T is convertible to an integer.

- _T_ The integer type.
- _value_ An integer type value convertible to a 64 bit integer.

**Returns**: A lisp_t object initialised to hold an integer value.

> ```cpp
> template<>
> inline lisp_t getobject(double_t::value_type value)
> ```

Specialization for a double which then becomes a floating point
object.

- _value_ A floating point value (double).

**Returns**: A lisp_t object initialized to hold a floating point value.

> ```cpp
> inline lisp_t mkstring(const std::string& str)
> ```

Create a string.

- _str_ The string.

**Returns**: A lisp_t object of type string.

> ```cpp
> inline lisp_t mknumber(integer_t::value_type value)
> ```

Create an integer number.

- _value_ The integer number.

**Returns**: An integer number as a lisp_t object.

> ```cpp
> inline lisp_t mkfloat(double_t::value_type value)
> ```

Create a floating point number.

- _value_ The floating point number.

**Returns**: A floating point number as a LISP object.

> `(cons a b)` (_Function_)

```cpp
inline lisp_t cons(lisp_t a, lisp_t b) { return getobject(new cons_t
```

Builds a cons cell out of the arguments.

The most basic of lisp functions.  Allocate a cons cell and fill in the
cell's car and cdr parts.

- _a_ The value to put in the head (car) of the cons cell.
- _b_ The value to put in the tail (cdr) of the cons cell.

**Returns**: The cons cell.

> `(obarray)` (_Function_)

```cpp
inline lisp_t obarray()
```

Builds a list of symbols in the local symbol table.

**Returns**: A list of local symbols in no particular order.

> `(freecount)` (_Function_)

```cpp
inline lisp_t freecount()
```

Number of free cell in the free cell list.

**Returns**: The number of free cells.

> ```cpp
> inline lisp_t intern(std::string_view pname)
> ```

Make interned symbol in obarray.

Create an interned symbol in the global symbol table.

- _pname_ The print name of the symbol.

**Returns**: The symbol as a LISP object.

> ```cpp
> inline lisp_t mkatom(std::string_view s)
> ```

Create a literal atom.

Currently there is no difference between `intern` and `mkatom` as they both
create a symbol in the global symbol table.

**Returns**: The literal atom.

> ```cpp
> template<typename Fun>
> void mkprim(std::string_view pname, Fun&& fun, enum subr_t::subr subr, enum subr_t::spread spread)
> ```

Templated function which registers a primary function.

The function registered can have one of the following signatures.

@verbatim
  lisp_t fun()
  lisp_t fun(lisp_t)
  lisp_t fun(lisp_t, lisp_t)
  lisp_t fun(lisp_t, lisp_t, lisp_t)
@endverbatim

When called from C++ the arguments are always evaluated since it's
following the C++ conventions.

- _pname_ Print name.
- _fun_ The function to register.
- _subr_ Specifies if the function should evaluate (SUBR) or not (FSUBR)
its arguments.
- _spread_ Specifies if the function should take a specific number of
arguments (SPREAD) or not (NOSPREAD).

> ```cpp
> inline cvariable_t& initcvar(std::string_view name, lisp_t val)
> ```

Initializes a lisp symbol for use in the C++ program.

This function links a variable in lisp with a variable in C++ so that
changing the value in one domain will be reflected in the other.  The lisp
variable will have the print name _name_.  In C++ the type cvariable_t
will work in many contexts which expects a value of type lisp_t.  If
assigned to in C++ the lisp value will change, if the value is set with
setq in lisp the C++ value will change.

- _name_ The lisp print name.
- _val_ The initial value.

**Returns**: A reference of type cvariable_t which wraps the lisp_t value.

> ```cpp
> inline lisp_t makecvar(std::string_view name, lisp_t val)
> ```

Creates a lisp_t object containing a cvariable_t.

Similar to initcvar but returns a lisp_t object instead.

- _name_ The lisp print name.
- _val_ The initial value.

**Returns**: A lisp_t object containing a cvariable_t value.

> ```cpp
> template<typename... Ts>
> requires(std::same_as<Ts, lisp_t> && ...)
> lisp_t mklist(lisp_t first, Ts... rest)
> ```

Convenience function which creates a list from a variadic list of
items.

This function takes one or more objects of type lisp_t and creates a list.

- _Ts_ List of types, all of which have to be of type lisp_t.
- _first_ First object in the list.
- _rest_ Rest of the list of objects.

**Returns**: The list.

> ```cpp
> inline lisp::lisp_t operator"" _s(const char* s, std::size_t)
> ```

Creates a lisp string.

> ```cpp
> inline lisp::lisp_t operator"" _a(const char* s, std::size_t)
> ```

Creates an atom.

> ```cpp
> inline lisp::lisp_t operator"" _l(unsigned long long i)
> ```

Creates a number.

> ```cpp
> inline lisp::lisp_t operator"" _l(long double d)
> ```

Creates a floating point value.

> ```cpp
> inline lisp::lisp_t operator"" _e(const char* s, std::size_t)
> ```

Evaluates a lisp expression in a string.
