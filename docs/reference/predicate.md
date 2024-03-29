# Predicates

> `(eq a b)` (_Function_)

```cpp
inline lisp_t eq(const lisp_t& a, const lisp_t& b)
```

`t` if the objects _a_ and _b_ are the same object. Integers are
considered 'eq' if their values are the same.

> `(atom a)` (_Function_)

```cpp
inline lisp_t atom(const lisp_t& a)
```

`t` if _a_ is `nil`, `t`, a symbol, an integer, or a floating point
object.

> `(numberp n)` (_Function_)

```cpp
inline lisp_t numberp(const lisp_t& n)
```

Returns T if argument is a number (either an integer or a floating
point value).

> `(listp l)` (_Function_)

```cpp
inline lisp_t listp(const lisp_t& l)
```

Returns T if the argument is a list, i.e. a cons cell.

> `(memb atom list)` (_Function_)

```cpp
inline lisp_t memb(const lisp_t& atom, const lisp_t& list)
```

Compares each CAR of the _list_ and if _eq_ returns the list at that
point.

Looks for an element _atom_ in _list_ using `eq`, returning the tail with
that element at the head. Returns `nil` if not found.

```lisp
(memb 'a '(a b c))
  => (a b c)
(memb 'b '(a b c))
  => (b c)
(memb 'd '(a b c))
  => nil
```

> `(equal l1 lf2)` (_Function_)

```cpp
inline lisp_t equal(const lisp_t& x, const lisp_t& y)
```

Returns T if the two lisp expressions are equal.

Returns `t` if _x_ and _y_ are `eq`, or if _x_ and _y_ are `eqp`, or if _x_
and _y_ are `strequal`, or if _x_ and _y_ are lists `(and (equal (car x)
(car x)) (equal (cdr x) (cdr y)))`.

> `(nlistp a)` (_Function_)

```cpp
inline lisp_t nlistp(const lisp_t& a)
```

Returns T if the expression is not a cons cell.

`t` if _x_ is not a list, otherwise `nil`. Same as `(not (listp x))`.

> `(neq a b)` (_Function_)

```cpp
inline lisp_t neq(const lisp_t& a, const lisp_t& b)
```

Returns T of a and b are not the same object.

> `(boundp a)` (_Function_)

```cpp
inline lisp_t boundp(const lisp_t& a)
```

Returns T if the symbol is unbound.

> `(litatom a)` (_Function_)
> `(symbolp a)` (_Function_)

```cpp
inline lisp_t litatom(const lisp_t& a)
```

Returns T if the value is a symbol or nil.

The function `symbolp` is an alias for `litatom`.

> `(typeof a)` (_Function_)

```cpp
inline lisp_t xtypeof(const lisp_t& a)
```

Returns the type of an expression as a symbol.

Returns a symbol depending on the type of the argument.

| Symbol    | Type                     |
|-----------|--------------------------|
| symbol    | Symbol                   |
| integer   | Integer                  |
| float     | Floating point           |
| indirect  | Indirect value           |
| cons      | Cons cell / list         |
| string    | String                   |
| subr      | Compiled function        |
| fsubr     | Compiled noeval function |
| lambda    | Lambda function          |
| nlambda   | NLambda function         |
| closure   | Closure                  |
| environ   | Environment              |
| file      | File                     |
| cvariable | C/++ Variable            |
