# Property List Functions

`lips` supports property lists on literal atoms. A property list is a list
of values stored in the _property cell_ of a literal atom. A property list
is list of alternating properties and values. For example the property list
`(a 1 b 2)` has two properties `a` and `b` with the values `1` and `2`
respectively. `eq` is used to compare properties when manipulating the
property list with the below functions.

> `(getplist a)` (_Function_)

```cpp
inline lisp_t getplist(const lisp_t& a)
```

Returns the entire property list stored in the property cell of _a_.

> `(getprop a p)` (_Function_)

```cpp
inline lisp_t getprop(const lisp_t& a, const lisp_t& p)
```

Returns the value of property _p_ stored in the property cell of the
literal atom _a_.

> `(putprop a p v)` (_Function_)

```cpp
inline lisp_t putprop(const lisp_t& a, const lisp_t& p, const lisp_t& v)
```

Puts the value _v_ in the property _p_ of _a_.

> `(remprop a p)` (_Function_)

```cpp
inline lisp_t remprop(const lisp_t& a, const lisp_t& p)
```

Removes the property _p_ from the literal atom _a_.

> `(setplist a pl)` (_Function_)

```cpp
inline lisp_t setplist(const lisp_t& a, const lisp_t& pl)
```

Sets the property list of _a_ to _pl_.
