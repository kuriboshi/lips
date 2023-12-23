# Predicates

> ```cpp
> inline lisp_t numberp(lisp_t a)
> ```

Returns T if argument is a number (either an integer or a floating
point value).

> ```cpp
> inline lisp_t listp(lisp_t a)
> ```

Returns T if the argument is a list, i.e. a cons cell.

> ```cpp
> inline lisp_t memb(lisp_t atom, lisp_t list)
> ```

Compares each CAR of the @a list and if @a eq returns the list at
that point.

(memb 'b '(a b c)) => (b c)

> ```cpp
> inline lisp_t equal(lisp_t l1, lisp_t l2)
> ```

Returns T if the two lisp expressions are equal.

> ```cpp
> inline lisp_t nlistp(lisp_t a)
> ```

Returns T if the expression is not a cons cell.

> ```cpp
> inline lisp_t neq(lisp_t a, lisp_t b)
> ```

Returns T of a and b are not the same object.

> ```cpp
> inline lisp_t boundp(lisp_t a)
> ```

Returns T if the symbol is unbound.

> ```cpp
> inline lisp_t litatom(lisp_t a)
> ```

Returns T if the value is a symbol or nil.

> ```cpp
> inline lisp_t xtypeof(lisp_t a)
> ```

Returns the type of an expression as a symbol.
