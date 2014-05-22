---
layout: post
title: 18 Ways to Compare Classes in Ruby
---

Last week, I encountered [an interesting issue][1]. It boils down to this: How
do you know if an object is an instance of a class?

I had to ask this question because I help maintain [ActiveInteraction][2], a
[command pattern][3] library. It provides a way to ensure that an object is a
certain class. For instance, if an interaction needs a `User`, it could say:

``` rb
model :someone,
  class: User
```

During execution, it's guaranteed that `someone` is in fact a `User`. Behind
the scenes, ActiveInteraction validates that using a `case` statement. In this
instance, it would look like this:

``` rb
case someone
when User
  # It's valid.
else
  # It's invalid.
end
```

Turns out that's not enough for determining if an object is an instance of a
class. In particular, test mocks pretend to be something they're not by
overriding the `#is_a?` method. Desugaring the `case` statement reveals why it
fails.

``` rb
if User === someone
  # It's valid.
else
  # It's invalid.
end
```

The `.===` method asks the class if an object is the right class. `#is_a?` does
the same thing in the other direction by asking the object if it's the right
class. Since the test mock doesn't monkey patch the class it's mocking, the
only way around this is to ask both questions.

``` rb
if User === someone
  # The class says it's valid.
elsif someone.is_a?(User)
  # The object says it's valid.
else
  # It's invalid.
end
```

While developing [a fix][4] for ActiveInteraction, I wondered if there were
other ways to do this. I did some research and discovered that there are at
least 18 different ways to see if an object is an instance of a class. And a
single line of code can break each of them.

I've compiled a list of all the different methods, along with how to break
them. A word of warning though: If you're using anything other than `.===` and
`#is_a?`, you're doing it wrong.

Alright, let's say you have a class and an instance of that class. In
particular, you have this code:

``` rb
klass = Class.new
object = klass.new
```

You have one simple question to answer: Is `object` an instance of `klass`?

1.  If you're like me, you'll reach for a `case` statement first. We already
    desugared it, so let's focus on the conditional expression.

    ``` rb
    klass === object
    # => true
    ```

    How can we break this? We need to override the `.===` method to return
    something falsey. You would usually do that like this:

    ``` rb
    class Example
      class << self
        def ===(other)
          false
        end
      end
    end
    ```

    Unfortunately we can't do that because `klass` is anonymous. So let's use
    `.class_exec` instead.

    ``` rb
    klass.class_exec { def self.===(*) false end }
    ```

    Reevaluating the conditional shows that it returns `false` after this
    change.

    ``` rb
    klass === object
    # => false
    ```

    Alright! We broke the most idiomatic way to check if an object is an
    instance of a class.

2.  Without a `case` statement, the next best thing is `#is_a?`.

    ``` rb
    object.is_a?(klass)
    # => true
    ```

    To break this, we need to override the `#is_a?` method to return something
    falsey. Under normal circumstances, you could do this:

    ``` rb
    class Example
      def is_a?(klass)
        false
      end
    end
    ```

    Since `klass` is anonymous, we'll have to use `.class_exec` again.

    ``` rb
    klass.class_exec { def is_a?(*) false end }
    ```

    Now let's check the return value of the conditional.

    ``` rb
    object.is_a?(klass)
    # => false
    ```

    Great! It returns `false`. With that, we've essentially broken class
    comparison. It would be reasonable for a program to conclude that `object`
    was not an instance of `klass`.

3.  But let's be unreasonable. Let's not stop until it's impossible.

    You might be surprised to learn that `#is_a?` and `#kind_of?` aren't
    aliases. So even though we've broken the former, the latter still works.
    Let's fix that by breaking `#kind_of?` too.

    ``` rb
    object.kind_of?(klass)
    # => true
    klass.class_exec { def kind_of?(*) false end }
    object.kind_of?(klass)
    # => false
    ```

4.  We can make a similar check by looking through the family tree.

    ``` rb
    klass.ancestors.include?(object.class)
    # => true
    klass.class_exec { def self.ancestors; [] end }
    klass.ancestors.include?(object.class)
    # => false
    ```

5.  All the methods we've tried so far would work for subclasses and modules. A
    more specific method, `#instance_of?`, only works for exact instances.

    ``` rb
    object.instance_of?(klass)
    # => true
    klass.class_exec { def instance_of?(*) false end }
    object.instance_of?(klass)
    # => false
    ```

6.  We can also manually check the instance's class for equality using `.==`.

    ``` rb
    klass == object.class
    # => true
    klass.class_exec { def self.==(*) false end }
    klass == object.class
    # => false
    ```

7.  Or we can use `.eql?` for stricter equality.

    ``` rb
    klass.eql?(object.class)
    # => true
    klass.class_exec { def self.eql?(*) false end }
    klass.eql?(object.class)
    # => false
    ```

8.  Or we can use `.equal?` for even stricter equality.

    ``` rb
    klass.equal?(object.class)
    # => true
    klass.class_exec { def self.equal?(*) false end }
    klass.equal?(object.class)
    # => false
    ```

9.  Even though we broke the helper methods, we can manually check object
    equality though `.object_id`.

    ``` rb
    klass.object_id == object.class.object_id
    # => true
    klass.class_exec { def self.object_id; rand(26) end }
    klass.object_id == object.class.object_id
    # => false
    ```

10. Ruby provides another way to get at the object ID: `.__id__`. It isn't an
    alias for `.object_id`.

    ``` rb
    klass.__id__ == object.class.__id__
    # => true
    klass.class_exec { def self.__id__; rand(26) end }
    klass.__id__ == object.class.__id__
    # => false
    ```

11. We can't see if the classes are equal, but we can see if they aren't equal
    using `.!=`.

    ``` rb
    klass != object.class
    # => false
    klass.class_exec { def self.!=(*) true end }
    klass != object.class
    # => true
    ```

12. Since we've completely broken equality, let's move on to inequality.

    ``` rb
    klass <=> object.class
    # => 0
    klass.class_exec { def self.<=>(*) nil end }
    klass <=> object.class
    # => nil
    ```

13. `Class` doesn't implement `Comparable`, so `.<=` still works even though we
    broke `.<=>`.

    ``` rb
    klass <= object.class
    # => true
    klass.class_exec { def self.<=(*) false end }
    klass <= object.class
    # => false
    ```

14. The same is true of `.>=`.

    ``` rb
    klass >= object.class
    # => true
    klass.class_exec { def self.>=(*) false end }
    klass >= object.class
    # => false
    ```

15. Despite all our efforts, it's still possible to successfully compare the
    classes using their names. Let's break that too.

    ``` rb
    klass.name == object.class.name
    # => true
    klass.class_exec { def self.name; ('A'..'Z').to_a.sample end }
    klass.name == object.class.name
    # => false
    ```

16. The default implementation of `.to_s` is the same as `.name`, but they
    aren't aliased.

    ``` rb
    klass.to_s == object.class.to_s
    # => true
    klass.class_exec { def self.to_s; ('A'..'Z').to_a.sample end }
    klass.to_s == object.class.to_s
    # => false
    ```

17. Similarly, `.inspect` is the same as the other two by default.

    ``` rb
    klass.inspect == object.class.inspect
    # => true
    klass.class_exec { def self.inspect; ('A'..'Z').to_a.sample end }
    klass.inspect == object.class.inspect
    # => false
    ```

18. Instead of breaking all these class methods, we could just make `#class`
    return a new anonymous class.

    ``` rb
    object.class == object.class
    # => true
    klass.class_exec { def class; Class.new end }
    object.class == object.class
    # => false
    ```

As you can see, it's hard to confidently say if an object is an instance of a
class. There may be even more methods than the ones I've listed here. But if
I've learned anything, it's that you don't want to do this. If you have to,
keep it simple: Use `.===` and `#is_a?`.

[1]: https://github.com/orgsync/active_interaction/issues/179
[2]: https://github.com/orgsync/active_interaction
[3]: http://en.wikipedia.org/wiki/Command_pattern
[4]: https://github.com/orgsync/active_interaction/pull/180
