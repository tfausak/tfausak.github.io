---
title: Clojure's threading macros
---

I recently learned Clojure. It was my first experience with a Lisp, and I
enjoyed it. It felt like a mix of Ruby, which I use in my day job, and Haskell,
which I use in my side projects.

One thing in particular stood out to me: threading macros. Some of the ones in
the standard library are [`->`][1], [`->>`][2], and [`as->`][3]. They elegantly
solve writing imperative-style code in a functional language.

They stood out to me because they're like a much better version of [Flow][4],
one of my Haskell libraries. Since functions with a variable number of
arguments are hard in Haskell, Flow only works with functions of a single
argument. Clojure's threading macros can work with any number of arguments in
any position.

If you aren't familiar with them, here is a quick overview of the threading
macros. `->` is "thread first". It is easiest to explain with examples.

``` clj
(-> x
    f)
;; (f x)
(-> x
    f
    g)  
;; (g (f x))
(-> x
    (f y))
;; (f x y)
```

It fills in the first argument and carries it through all the forms. If you
want to fill in the last argument, use `->>`, the "thread last" macro.

``` clj
(->> x
     f)
;; (f x)
(->> x
     f
     g)  
;; (g (f x))
(->> x
     (f y))
;; (f y x)
```

If you need more control than that, you can use `as->`, the "thread as" macro.
It allows you to fill in an arbitrary argument.

``` clj
(as-> x $
      (f $))
;; (f x)
(as-> x $
      (f $)
      (g $))
;; (g (f x))
(as-> x $
      (f $ y))
;; (f x y)
(as-> x $
      (f y $))
;; (f y x)
(as -> x $
      (f $ y)
      (g z $))
;; (g z (f x y))
```

These macros are a powerful way to write expressive functional pipelines. I was
surprised to find that they were idiomatic Clojure, considering [the
backlash][5] Flow got from the Haskell community. That makes me think that
Clojurists are more pragmatic than Haskellers.

Regardless, this is a neat feature of Clojure. I wish Haskell had something
similar. Unfortunately I think it would require Template Haskell, but it could
be fun to write functions like this:

``` hs
$(as-> x _
       (f _ y)
       (g z _))
-- (g z (f x y))
```

[1]: https://clojuredocs.org/clojure.core/-%3E
[2]: https://clojuredocs.org/clojure.core/-%3E%3E
[3]: http://clojuredocs.org/clojure.core/as-%3E
[4]: {% post_url 2015-04-09-write-more-understandable-haskell-with-flow %}
[5]: {% post_url 2015-04-16-on-the-reaction-to-flow %}
