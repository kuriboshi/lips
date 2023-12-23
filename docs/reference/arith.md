# Arithmetic Functions

`lips` supports both integer and floating point numbers. There are
functions specific for either integers or floating points as well as
generic functions which can take either type.

> `(abs n)` (_Function_)

```cpp
inline lisp_t abs(lisp_t n)
```

Returns the absolute value of _n_.

> `(add1 n)` (_Function_)

```cpp
inline lisp_t add1(lisp_t n)
```

Returns the argument _n_ plus 1.

Same as `(plus n 1)`.

> `(difference x y)` (_Function_)

```cpp
inline lisp_t difference(lisp_t x, lisp_t y)
```

Calculates the difference between _x_ and _y_.

```lisp
(difference 8 3)
  => 5
```

Returns the difference.

> `(divide x y)` (_Function_)

```cpp
inline lisp_t divide(lisp_t x, lisp_t y)
```

Divides _x_ by _y_.

The result may be an integer or a floating point depending on the types of
_x_ and _y_. If both are integers the result will be an integer and if
either _x_ or _y_, ot both, is a floating point number the result will be a
floating point number.

Returns the result of the division.

> `(eqp x y)` (_Function_)

```cpp
inline lisp_t eqp(lisp_t x, lisp_t y)
```

Compares two integer or floating point values for equality.

> `(fdifference x y)` (_Function_)

```cpp
inline lisp_t fdifference(lisp_t x, lisp_t y)
```

Returns the floating point difference between _x_ and _y_.

> `(fdivide a b)` (_Function_)

```cpp
inline lisp_t fdivide(lisp_t x, lisp_t y)
```

Floating point division of _x_ by _y_.

Returns the result of the division.

> `(fplus args...)` (_NoSpread Function_)

```cpp
inline lisp_t fplus(lisp_t args)
```

Floating point addition of _args_.

Returns the sum of all the arguments.

> `(ftimes args)` (_NoSpread Function_)

```cpp
inline lisp_t ftimes(lisp_t args)
```

Floating point multiplication.

Returns the floating point multiplication of _args_.

> `(geq x y)` (_Function_)

```cpp
inline lisp_t geq(lisp_t x, lisp_t y)
```

Compares if one numeric values are greater or equal to another.

Returns 't' if $x \ge y$, `nil` otherwise.

> `(greaterp x y)` (_Function_)

```cpp
inline lisp_t greaterp(lisp_t x, lisp_t y)
```

Compares of one numeric value is greater than another.

Returns `t` if $x > y$, `nil` otherwise.

> `(idifference a b)` (_Function_)

```cpp
inline lisp_t idifference(lisp_t a, lisp_t b)
```

Integer subtraction.

> `(iminus a)` (_Function_)

```cpp
inline lisp_t iminus(lisp_t a)
```

Returns the _a_ with the opposite sign.

Same as `(idifference 0 a)`.

> `(iplus args...)` (_NoSpread Function_)

```cpp
inline lisp_t iplus(lisp_t args)
```

Returns the sum of the arguments.

> `(iquotient a b)` (_Function_)

```cpp
inline lisp_t iquotient(lisp_t a, lisp_t b)
```

Returns the truncated result of $a / b$.

```lisp
(iquotient 3 2) => 1
(iquotient -3 2) => -1
```

> `(iremainder a b)` (_Function_)

```cpp
inline lisp_t iremainder(lisp_t a, lisp_t b)
```

Returns the remainder of $a / b$.

```lisp
(iremainder 3 2) => 1
```

> `(itimes args...)` (_NoSpread Function_)

```cpp
inline lisp_t itimes(lisp_t args)
```

Returns the result of multiplying the arguments.

```lisp
(itimes 1 2 3 4) => 24
```

> `(itof i)` (_Function_)

```cpp
inline lisp_t itof(lisp_t i)
```

Returns the integer _i_ converted to a floating point value.

> `(leq x y)` (_Function_)

```cpp
inline lisp_t leq(lisp_t x, lisp_t y)
```

Returns `t` if $a \le b$, `nil` otherwise.

> `(lessp x y)` (_Function_)

```cpp
inline lisp_t lessp(lisp_t x, lisp_t y)
```

Returns `t` if $a < b$, `nil` otherwise.

> `(times args...)` (_NoSpread Function_)

```cpp
inline lisp_t times(lisp_t args)
```

Returns the result of multiplying the arguments.

> `(minus a)` (_Function_)

```cpp
inline lisp_t minus(lisp_t a)
```

Returns _a_ with the opposite sign.

> `(minusp x)` (_Function_)

```cpp
inline lisp_t minusp(lisp_t x)
```

Returns `t` if _x_ is a negative number, `nil` otherwise.

> `(neqp x y)` (_Function_)

```cpp
inline lisp_t neqp(lisp_t x, lisp_t y)
```

Returns `t` if $x \neq y$, `nil` otherwise.

> `(plus args...)` (_NoSpread Function_)

```cpp
inline lisp_t plus(lisp_t args)
```

Returns the sum of all _args_.

> `(sub1 a)` (_Function_)

```cpp
inline lisp_t sub1(lisp_t a)
```

Returns $a - 1$.

Same as `(difference a 1)`.

> `(zerop x)` (_Function_)

```cpp
inline lisp_t zerop(lisp_t x)
```

Returns `t` if $x = 0$, `nil` otherwise.
