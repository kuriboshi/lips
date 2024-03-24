# Arithmetic Functions

`lips` supports both integer (currently only 64 bit values are supported)
and floating point numbers (double). There are functions specific for
either integers or floating points as well as generic functions which can
take either type.

> `(abs n)` (_Function_)

```cpp
inline lisp_t abs(const lisp_t& n)
```

Returns the absolute value of _n_.

> `(add1 n)` (_Function_)

```cpp
inline lisp_t add1(const lisp_t& n)
```

Returns the argument _n_ plus 1.

Same as `(plus n 1)`.

> `(difference x y)` (_Function_)

```cpp
inline lisp_t difference(const lisp_t& x, const lisp_t& y)
```

Calculates the difference between _x_ and _y_.

```lisp
(difference 8 3)
  => 5
```

Returns the difference.

> `(divide x y)` (_Function_)

```cpp
inline lisp_t divide(const lisp_t& x, const lisp_t& y)
```

Divides _x_ by _y_.

The result may be an integer or a floating point depending on the types of
_x_ and _y_. If both are integers the result will be an integer and if
either _x_ or _y_, ot both, is a floating point number the result will be a
floating point number.

Returns the result of the division.

> `(eqp x y)` (_Function_)

```cpp
inline lisp_t eqp(const lisp_t& x, const lisp_t& y)
```

Compares two integer or floating point values for equality.

(Note: This differs from Interlisp which returns `t` if _x_ and _y_ are
`eq` or if they are numbers and are equal in value.)

> `(fdifference x y)` (_Function_)

```cpp
inline lisp_t fdifference(const lisp_t& x, const lisp_t& y)
```

Returns the floating point difference between _x_ and _y_.

> `(fdivide a b)` (_Function_)

```cpp
inline lisp_t fdivide(const lisp_t& x, const lisp_t& y)
```

Floating point division of _x_ by _y_.

Returns the result of the division.

> `(fplus args...)` (_NoSpread Function_)

```cpp
inline lisp_t fplus(const lisp_t& args)
```

Floating point addition of _args_.

Returns the sum of all the arguments.

> `(ftimes args)` (_NoSpread Function_)

```cpp
inline lisp_t ftimes(const lisp_t& args)
```

Floating point multiplication.

Returns the floating point multiplication of _args_.

> `(geq x y)` (_Function_)

```cpp
inline lisp_t geq(const lisp_t& x, const lisp_t& y)
```

Compares if one numeric values are greater or equal to another.

Returns 't' if $x \ge y$, `nil` otherwise.

> `(greaterp x y)` (_Function_)

```cpp
inline lisp_t greaterp(const lisp_t& x, const lisp_t& y)
```

Compares of one numeric value is greater than another.

Returns `t` if $x > y$, `nil` otherwise.

> `(idifference a b)` (_Function_)

```cpp
inline lisp_t idifference(const lisp_t& a, const lisp_t& b)
```

Integer subtraction.

> `(iminus a)` (_Function_)

```cpp
inline lisp_t iminus(const lisp_t& a)
```

Returns the _a_ with the opposite sign.

Same as `(idifference 0 a)`.

> `(iplus args...)` (_NoSpread Function_)

```cpp
inline lisp_t iplus(const lisp_t& args)
```

Returns the sum of the arguments.

> `(iquotient a b)` (_Function_)

```cpp
inline lisp_t iquotient(const lisp_t& a, const lisp_t& b)
```

Returns the truncated result of $a / b$.

```lisp
(iquotient 3 2) => 1
(iquotient -3 2) => -1
```

> `(iremainder a b)` (_Function_)

```cpp
inline lisp_t iremainder(const lisp_t& a, const lisp_t& b)
```

Returns the remainder of $a / b$.

```lisp
(iremainder 3 2) => 1
```

> `(itimes args...)` (_NoSpread Function_)

```cpp
inline lisp_t itimes(const lisp_t& args)
```

Returns the result of multiplying the arguments.

```lisp
(itimes 1 2 3 4) => 24
```

> `(itof i)` (_Function_)

```cpp
inline lisp_t itof(const lisp_t& i)
```

Returns the integer _i_ converted to a floating point value.

> `(leq x y)` (_Function_)

```cpp
inline lisp_t leq(const lisp_t& x, const lisp_t& y)
```

Returns `t` if $a \le b$, `nil` otherwise.

> `(lessp x y)` (_Function_)

```cpp
inline lisp_t lessp(const lisp_t& x, const lisp_t& y)
```

Returns `t` if $a < b$, `nil` otherwise.

> `(times args...)` (_NoSpread Function_)

```cpp
inline lisp_t times(const lisp_t& args)
```

Returns the result of multiplying the arguments.

> `(minus a)` (_Function_)

```cpp
inline lisp_t minus(const lisp_t& a)
```

Returns _a_ with the opposite sign.

> `(minusp x)` (_Function_)

```cpp
inline lisp_t minusp(const lisp_t& x)
```

Returns `t` if _x_ is a negative number, `nil` otherwise.

> `(neqp x y)` (_Function_)

```cpp
inline lisp_t neqp(const lisp_t& x, const lisp_t& y)
```

Returns `t` if $x \neq y$, `nil` otherwise.

> `(plus args...)` (_NoSpread Function_)

```cpp
inline lisp_t plus(const lisp_t& args)
```

Returns the sum of all _args_.

> `(sub1 a)` (_Function_)

```cpp
inline lisp_t sub1(const lisp_t& a)
```

Returns $a - 1$.

Same as `(difference a 1)`.

> `(zerop x)` (_Function_)

```cpp
inline lisp_t zerop(const lisp_t& x)
```

Returns `t` if $x = 0$, `nil` otherwise.
