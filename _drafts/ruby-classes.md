---
layout: post
title: Ruby Classes
---

<https://github.com/orgsync/active_interaction/issues/179>

## Class

``` rb
klass = Class.new
object = klass.new
```

Is `object` an instance of `klass`?

1.  Typically you would use a `case` statement.
    That desugars into an `if` statement using the case equality operator.
    We're only interested in the result of the conditional.

    ``` rb
    klass === object
    # => true
    klass.class_exec { def self.===(*) false end }
    klass === object
    # => false
    ```

2.  We can swap the receiver and the argument.
    Instead of asking the class about some object,
    we can ask the object about some class.

    ``` rb
    object.is_a?(klass)
    # => true
    klass.class_exec { def is_a?(*) false end }
    object.is_a?(klass)
    # => false
    ```

3.  There are two ways to do this.
    They aren't aliased.

    ``` rb
    object.kind_of?(klass)
    # => true
    klass.class_exec { def kind_of?(*) false end }
    object.kind_of?(klass)
    # => false
    ```

4.  You can ask a more specific question about the object too.

    ``` rb
    object.instance_of?(klass)
    # => true
    klass.class_exec { def instance_of?(*) false end }
    object.instance_of?(klass)
    # => false
    ```

5.  This is more or less equivalent to the last one.
    Compare the classes themselves for equality.

    ``` rb
    klass == object.class
    # => true
    klass.class_exec { def self.==(*) false end }
    klass == object.class
    # => false
    ```

6.  Or the other kind of equality.

    ``` rb
    klass.eql?(object.class)
    # => true
    klass.class_exec { def self.eql?(*) false end }
    klass.eql?(object.class)
    # => false
    ```

7.  Or the *other* kind of equality.

    ``` rb
    klass.equal?(object.class)
    # => true
    klass.class_exec { def self.equal?(*) false end }
    klass.equal?(object.class)
    # => false
    ```

8.  Checking if things are not equal is totally different than checking if they're equal.

    ``` rb
    klass != object.class
    # => false
    klass.class_exec { def self.!=(*) true end }
    klass != object.class
    # => true
    ```

9.  Inequalities can come in handy here.

    ``` rb
    klass <= object.class
    # => true
    klass.class_exec { def self.<=(*) false end }
    klass <= object.class
    # => false
    ```

10. The other way too.

    ``` rb
    klass >= object.class
    # => true
    klass.class_exec { def self.>=(*) false end }
    klass >= object.class
    # => false
    ```

11. Annoyingly, `Class` doesn't implement `Comparable`.
    That doesn't stop it from responding to `.<=>`, though.

    ``` rb
    klass <=> object.class
    # => 0
    klass.class_exec { def self.<=>(*) nil end }
    klass <=> object.class
    # => nil
    ```

12. You can manually check for object equality.

    ``` rb
    klass.object_id == object.class.object_id
    # => true
    klass.class_exec { def self.object_id; rand(26) end }
    klass.object_id == object.class.object_id
    # => false
    ```

13. Of course, there's another way to do that too.

    ``` rb
    klass.__id__ == object.class.__id__
    # => true
    klass.class_exec { def self.__id__; rand(26) end }
    klass.__id__ == object.class.__id__
    # => false
    ```

14. You can also look through the family tree.

    ``` rb
    klass.ancestors.include?(instance.class)
    # => true
    klass.class_exec { def self.ancestors; [] end }
    klass.ancestors.include?(instance.class)
    # => false
    ```

15. How about some string comparisons?

    ``` rb
    klass.name == instance.class.name
    # => true
    klass.class_exec { def self.name; ('A'..'Z').to_a.sample end }
    klass.name == instance.class.name
    # => false
    ```

16. It would just be silly if these were aliased.

    ``` rb
    klass.to_s == instance.class.to_s
    # => true
    klass.class_exec { def self.to_s; ('A'..'Z').to_a.sample end }
    klass.to_s == instance.class.to_s
    # => false
    ```

17. Yet another way to do the same thing.

    ``` rb
    klass.inspect == instance.class.inspect
    # => true
    klass.class_exec { def self.inspect; ('A'..'Z').to_a.sample end }
    klass.inspect == instance.class.inspect
    # => false
    ```

18. Finally, you can pre-empt most of these by returning a new class every time.

    ``` rb
    instance.class == instance.class
    # => true
    klass.class_exec { def self.class; Class.new end }
    instance.class == instance.class
    # => false
    ```

## Subclass

``` rb
other = Class.new
klass = Class.new(other)
```

Is `klass` a subclass of `other`?

1.  Surely the most straightforward way.

    ```rb
    klass < other
    # => true
    klass.class_exec { def self.<(*) false end }
    klass < other
    # => false
    ```

2.  Only works for direct subclasses.

    ```rb
    klass.superclass == other
    # => true
    klass.class_exec { def self.superclass; Class.new end }
    klass.superclass == other
    # => false
    ```

## Superclass

``` rb
klass = Class.new
other = Class.new(klass)
```

Is `klass` a superclass of `other`?

1.  The easiest way.

    ``` rb
    klass > other
    # => true
    klass.class_exec { def self.>(*) false end }
    klass > other
    # => false
    ```

## Module

``` rb
other = Module.new
klass = Class.new { include other }
```

Does `klass` include `other`?

1.  So simple it reads like English.

    ``` rb
    klass.include?(other)
    # => true
    klass.class_exec { def self.include?(*) false end }
    klass.include?(other)
    # => false
    ```

2.  Take a more roundabout approach.

    ``` rb
    klass.included_modules.include?(other)
    # => true
    klass.class_exec { def self.included_modules; [] end }
    klass.included_modules.include?(other)
    # => false
    ```
