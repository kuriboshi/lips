# Logical Functions

> `(and args...)` (_NoSpread NLambda_)

```cpp
inline lisp_t p_and(lisp_t args)
```

If any expression evaluates to nil return `nil` otherwise return the
result of the last expression.

> `(or args...)` (_NoSpread NLambda_)

```cpp
inline lisp_t p_or(lisp_t args)
```

Returns the first expression evaluating to non-`nil`, otherwise
return `nil`.

> `(not expr)` (_Function_)

```cpp
inline lisp_t p_not(lisp_t expr)
```

Returns `t` if argument is `nil`, `nil` otherwise.

> `(if p t . f)` (_NLambda_)

Evaluates and returns _t_ if _p_ is non-`nil`, evaluates and returns
_f_ otherwise.

If the predicate _p_ evaluates to a non-`nil`\ value the expression
_t_ is evaluated and returned from the function. If _p_ evaluates to
`nil` then the value of the expression _f_ is returned.
