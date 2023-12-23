# Low Level Functions

> `(cond ...)` (_NoSpread NLambda_)

```cpp
inline lisp_t cond(lisp_t clauses)
```

The cond special form.

The generalized conditional special form. The function takes zero or more
clauses. Each clause has one test followed by zero or more expressions
called consequents. The function evaluates each test in sequence until one
of them is evaluated to true (not `nil`). It then evaluates each consequent
in order and returns the value of the last consequent. If there are no
consequents the result is the value of the test expression. The degenerate
`cond` expression with no clauses at all evaluates to `nil`.

In the following example the return value is the value of the expression
`r0` if `e0` evaluates to non-`nil`, `r2` if `e1` is evaluated to
non-`nil`, `e2` if `e2` evaluates to non-`nil`. Finally, if none of the
expressions `e0`, `e1`, or `e2` is non-`nil` the final `t` provides a
default value. If none of the test expressions evaluate to non-`nil` then
the result of the entire `cond` expression is `nil`.

Note that the expressions after the conditional expressions are evaluated
in an implicit `progn` which is why the result of `e1` being non-nil is the
value of `r2`.

```lisp
(cond (e0 r0)
      (e1 r1 r2)
      (e2)
      (t r3))
```

> `(prog1 args...)` (_NoSpread NLambda_)

```cpp
inline lisp_t prog1(lisp_t a, lisp_t b)
```

Evaluates all arguments and returns the result of the first
expression.

> `(progn args...)` (_NoSpread NLambda_)

```cpp
inline lisp_t progn(lisp_t a)
```

Evaluates all arguments and retuns the result of the last
expression.

> `(set var expr)` (_Function_)

```cpp
inline lisp_t set(lisp_t var, lisp_t expr)
```

Sets the value of the symbol to the value.

Both `var` and `expr` are evaluated. Returns `val`.

- _var_ An expression which evaluates to a symbol.
- _expr_ An expression.

**Returns**: The result of evaluating the expression `expr`.

> `(setq var expr)` (_NLambda_)

```cpp
inline lisp_t setq(lisp_t var, lisp_t expr)
```

Same as 'set' but the first argument is not evaluated.

- _var_ A literal symbol.
- _expr_ An expression.

**Returns**: The result of evaluating the expression `expr`.

> `(setqq var val)` (_NLambda_)

```cpp
inline lisp_t setqq(lisp_t var, lisp_t val)
```

Same as 'set' but no argument is evaluated.

- _var_ A literal symbol.
- _val_ A constant expression which is not evaluated.

**Returns**: The unevaluated `val` expression.

> `(while first args...)` (_NoSpread NLambda_)

```cpp
inline lisp_t xwhile(lisp_t first, lisp_t second)
```

Evaluate arguments in until _first_ is false.

While the first argument is true evaluate the rest of the arguments in an
implicit `progn`. Returns `nil`.
