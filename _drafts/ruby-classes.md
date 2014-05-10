---
layout: post
title: Ruby Classes
---

## Class

``` rb
klass = Class.new
object = klass.new
```

Is `object` an instance of `klass`?

1.  `.===`
2.  `#is_a?`
3.  `#kind_of?`
4.  `#instance_of?`
5.  `.==`
6.  `.eql?`
7.  `.equal?`
8.  `.!=`
9.  `.<=`
10. `.>=`
11. `.<=>`
12. `.object_id`
13. `.__id__`
14. `.ancestors`
15. `.name`
16. `.to_s`
17. `.inspect`
18. `#class`

## Subclass

``` rb
other = Class.new
klass = Class.new(other)
```

Is `klass` a subclass of some other class?

1.  `.<`
2.  `.superclass`

## Superclass

``` rb
klass = Class.new
other = Class.new(klass)
```

Is `klass` a superclass of some other class?

1.  `.>`

## Module

``` rb
other = Module.new
klass = Class.new { include other }
```

Does `klass` include some module?

1.  `.include?`
2.  `.included_modules`
