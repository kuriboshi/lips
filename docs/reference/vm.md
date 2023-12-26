# Function Evaluation

Functions in this section relate to the evaluation of lisp expressions.

> ```cpp
> template<Context Context>
> class vm_t final: public vm
> ```

Templated derived class which implements the virtual functions of
vm.

The vm_t template provides access to some global symbols like the primary
input and output file, the stdin and stdout, and some other global
symbols. It also provides access to the implementation of error functions.

The Context needs to satisfy the _Context_ concept. The type
`lisp::context_t` provides a default implementation.

- _Context_ The _context_ holds the actual implementation.

> `(eval fn)` (_Function_)

```cpp
inline lisp_t eval(lisp_t expr)
```

Evaluate a lisp expression.

- _expr_ The lisp expression to evaluate.

**Returns**: Returns the result of the evaluation.

> ```cpp
> inline lisp_t eval(const std::string& expr)
> ```

Evaluate a lisp expression read from a string.

> `(apply fn lis)` (_Function_)

```cpp
inline lisp_t apply(lisp_t fn, lisp_t list)
```

Apply a function to a list of arguments.

Applies the function _fn_ to the arguments in the list _list_ as if _fn_ is
called with the list as its arguments.

```lisp
(apply car '((a b c)))
  => a
```

> `(apply* fn args...)` (_NoSpread Function_)

A nospread version of `apply`.

```lisp
(apply* car '(a b c))
  => a
```

> `(backtrace)` (_Function_)

```cpp
inline lisp_t backtrace()
```

Print a backtrace of the control stack.

Subject to change between any version of the interpeter.

**Returns**: `nil`

> `(topofstack)` (_Function_)

```cpp
inline lisp_t topofstack()
```

Retrieves the most recent environment.

Subject to change between any version of the interpeter.

**Returns**: An object of type environment.

> `(destblock)` (_Function_)

```cpp
inline lisp_t destblock(lisp_t a)
```

Converts an object of type environment to a list.

Subject to change between any version of the interpeter.

**Returns**: A list representing a destblock.
