---
title: Class comparison in Ruby
---

I recently encountered [an interesting issue][1]. It boils down to this: How do
you know if an object is an instance of a class?

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
least 18 different ways to make this comparison.

It would be unreasonable to make all those checks. In fact, if you're using
anything other than `.===` and `#is_a?`, you're doing it wrong. However, I was
interested in creating a class that is indistinguishable from another class. In
other words, the perfect mock.

## Creating the Perfect Mock

Before we create the mock, we need to create the class we'll be mocking.

``` rb
Cheese = Class.new
gouda = Cheese.new
```

Next up let's create the mock. It shouldn't have anything in common with the
class it's mocking.

``` rb
FakeCheese = Class.new
american = FakeCheese.new
```

With those defined, we can move on to faking the comparisons.

- [`.===`](#section)
- [`#is_a?`](#isa)
- [`#kind_of?`](#kindof)
- [`#instance_of?`](#instanceof)
- [`#class`](#class)
- [`.==`](#section-1)
- [`.eql?`](#eql)
- [`.equal?`](#equal)
- [`.object_id`](#objectid)
- [`.__id__`](#id)
- [`.<=>`](#section-2)
- [`.<`](#section-3)
- [`.>`](#section-4)
- [`.<=`](#section-5)
- [`.>=`](#section-6)
- [`.ancestors`](#ancestors)
- [`.to_s`](#tos)
- [`.inspect`](#inspect)
- [`.name`](#name)
- [`#to_s`](#tos-1)
- [`#inspect`](#inspect-1)

### `.===`

We hit a problem right out of the gate:

``` rb
Cheese === american
# => false
```

This is an issue because it means instances of `FakeCheese` won't be able to
pass as `Cheese` in `case` statements. Unfortunately there's nothing we can do
about it without monkey patching `Cheese`. Let's stay focused on the
`FakeCheese` class.

``` rb
FakeCheese === gouda
# => false
```

We can do something about this one. Let's make `FakeCheese` behave like
`Cheese` by delegating to it.

``` rb
class FakeCheese
  def self.===(other)
    Cheese === other
  end
end
```

After making that change, we can see that the conditional returns `true` now.

``` rb
FakeCheese === gouda
# => true
```

Note that we broke the default behavior:

``` rb
FakeCheese === american
# => false
```

Instances of `FakeCheese` aren't able to pass as `FakeCheese` in `case`
statements anymore. We could fix that by throwing a call to `super` somewhere
in `.===`, but remember that we're trying to build the perfect mock. If
`Cheese === american` is `false`, `FakeCheese === american` should be too.
(We'll see later that falling back to `super` doesn't always make sense.)

### `#is_a?`

Since we can't make `case` statements work without monkey patching, let's move
on to something we can fix.

``` rb
american.is_a?(Cheese)
# => false
```

We want to delegate to `Cheese` again, but this is an instance method. We don't
have an instance of `Cheese` in `FakeCheese`. We could make one and delegate to
it, but initializing `Cheese` could be complicated or expensive. Let's turn to
[`#is_a?`'s documentation][5] for inspiration.

> Returns `true` if *class* is the class of *obj*, or if *class* is one of the
> superclasses of *obj* or modules included in *obj*.

We need a class method that does the same thing. Looking at [the documentation
for `.>=`][6], it seems to fit the bill.

> Returns true if *mod* is an ancestor of *other*, or the two modules are the
> same.

Using `.>=` we can essentially delegate `#is_a?` to `Cheese` without having an
instance handy.

``` rb
class FakeCheese
  def is_a?(klass)
    Cheese >= klass
  end
end
```

Let's reevaluate our conditional to make sure it worked.

``` rb
american.is_a?(Cheese)
# => true
```

Great! That was a little tricky, but ultimately not too bad.

### `#kind_of?`

Even though `#kind_of?` and `#is_a?` do the same thing, they aren't aliases.

``` rb
american.kind_of?(Cheese)
# => false
class FakeCheese
  def kind_of?(klass)
    Cheese >= klass
  end
end
american.kind_of?(Cheese)
# => true
```

### `#instance_of?`

Unlike `#is_a?` and `#kind_of?`, `#instance_of?` checks for an exact match.

``` rb
american.instance_of?(Cheese)
# => false
class FakeCheese
  def instance_of?(klass)
    Cheese == klass
  end
end
american.instance_of?(Cheese)
# => true
```

### `#class`

Instead of using a predicate method, we can look directly at the object's
class.

``` rb
american.class
# => FakeCheese
class FakeCheese
  def class
    Cheese
  end
end
american.class
# => Cheese
```

This is the first method where falling back to `super` doesn't make sense.

### `.==`

We can check the classes themselves for equality.

``` rb
FakeCheese == Cheese
# => false
class FakeCheese
  def self.==(other)
    Cheese == other
  end
end
FakeCheese == Cheese
# => true
```

### `.eql?`

Or slightly stricter equality.

``` rb
FakeCheese.eql?(Cheese)
# => false
class FakeCheese
  def self.eql?(other)
    Cheese.eql?(other)
  end
end
FakeCheese.eql?(Cheese)
# => true
```

### `.equal?`

Or the strictest equality.

``` rb
FakeCheese.equal?(Cheese)
# => false
class FakeCheese
  def self.equal?(other)
    Cheese.equal?(other)
  end
end
FakeCheese.equal?(Cheese)
# => true
```

### `.object_id`

We can also use the object IDs to compare object equality by hand.

``` rb
FakeCheese.object_id
# => 70241271125600
class FakeCheese
  def self.object_id
    Cheese.object_id
  end
end
FakeCheese.object_id
# => 70241271152880
```

### `.__id__`

Ruby provides another way to get at the object IDs.

``` rb
FakeCheese.__id__
# => 70241271125600
class FakeCheese
  def self.__id__
    Cheese.__id__
  end
end
FakeCheese.__id__
# => 70241271152880
```

### `.<=>`

Now that we've faked all of the ways to check equality, let's move on to
inequality. The obvious place to start is with the spaceship operator.

``` rb
FakeCheese <=> Cheese
# => nil
class FakeCheese
  def self.<=>(other)
    Cheese <=> other
  end
end
FakeCheese <=> Cheese
# => 0
```

Even though `Class` implements `.<=>`, it doesn't include `Comparable`. So we
have to manually override all of the associated methods.

### `.<`

``` rb
FakeCheese < Cheese
# => nil
class FakeCheese
  def self.<(other)
    Cheese < other
  end
end
FakeCheese < Cheese
# => false
```

### `.>`

``` rb
FakeCheese > Cheese
# => nil
class FakeCheese
  def self.>(other)
    Cheese > other
  end
end
FakeCheese > Cheese
# => false
```

### `.<=`

``` rb
FakeCheese <= Cheese
# => nil
class FakeCheese
  def self.<=(other)
    Cheese <= other
  end
end
FakeCheese <= Cheese
# => true
```

### `.>=`

``` rb
FakeCheese >= Cheese
# => nil
class FakeCheese
  def self.>=(other)
    Cheese >= other
  end
end
FakeCheese >= Cheese
# => true
```

### `.ancestors`

Another way to see if two classes are the same is to see if they have the same
ancestors. Let's make `FakeCheese` pretend like it has the same family tree as
`Cheese`.

``` rb
FakeCheese.ancestors
# => [FakeCheese, Object, PP::ObjectMixin, Kernel, BasicObject]
class FakeCheese
  def self.ancestors
    Cheese.ancestors
  end
end
FakeCheese.ancestors
# => [Cheese, Object, PP::ObjectMixin, Kernel, BasicObject]
```

### `.to_s`

Having exhausted all of the somewhat reasonable ways to compare classes, let's
move on to comparing their string representations.

``` rb
FakeCheese.to_s
# => "FakeCheese"
class FakeCheese
  def self.to_s
    Cheese.to_s
  end
end
FakeCheese.to_s
# => "Cheese"
```

### `.inspect`

By default, `.to_s` and `.inspect` do the same thing, but they aren't aliased.

``` rb
FakeCheese.inspect
# => "FakeCheese"
class FakeCheese
  def self.inspect
    Cheese.inspect
  end
end
FakeCheese.inspect
# => "Cheese"
```

### `.name`

`.name` is just like `.to_s` and `.inspect`. It's not aliased either.

``` rb
FakeCheese.name
# => "FakeCheese"
class FakeCheese
  def self.name
    Cheese.name
  end
end
FakeCheese.name
# => "Cheese"
```

### `#to_s`

Instances of classes in Ruby don't use their class's string representation in
their own string representation.

``` rb
american.to_s
# => "#<FakeCheese:0x007fa3e09ccd00>"
```

Even though we overrode `.to_s`, `.inspect`, and `.name`, the instance somehow
uses the class's real name. So we have to provide a custom `#to_s`
implementation that mimics the default behavior. Other than [shifting the
object ID][7], this is pretty easy.

``` rb
class FakeCheese
  def to_s
    "#<#{Cheese}:0x#{'%x' % (object_id << 1)}>"
  end
end
american.to_s
# => "#<Cheese:0x7fa3e09ccd00>"
```

### `#inspect`

Unsurprisingly, this is not an alias.

``` rb
american.inspect
# => "#<FakeCheese:0x007fa3e09ccd00>"
class FakeCheese
  def inspect
    "#<#{Cheese}:0x#{'%x' % (object_id << 1)}>"
  end
end
american.inspect
# => "#<Cheese:0x7fa3e09ccd00>"
```

## Shorter & More Generic

We created the perfect mock, but it took a lot of code and we repeated
ourselves quite a bit. We can make it a lot simpler. Let's write a function
that takes a class and returns a perfect mock of that class.

``` rb
require 'forwardable'

def fake(klass)
  Class.new(BasicObject) do
    eigenclass = class << self; self end
    eigenclass.extend Forwardable
    eigenclass.def_delegators klass, *%i[
      <
      <=
      <=>
      ==
      ===
      >
      >=
      __id__
      ancestors
      eql?
      equal?
      inspect
      name
      object_id
      to_s
    ]

    define_method :class do
      klass
    end

    define_method :inspect do
      "#<#{klass.name}:0x#{'%x' % (__id__ << 1)}>"
    end
    alias_method :to_s, :inspect

    define_method :instance_of? do |other|
      klass == other
    end

    define_method :is_a? do |other|
      klass >= other
    end
    alias_method :kind_of?, :is_a?
  end
end
```

We can replace all our work above with just one function call.

``` rb
FakeCheese = fake(Cheese)
# => Cheese
american = FakeCheese.new
# => #<Cheese:0x7fd8e1e5ba48>
```

And it passes all the checks!

``` rb
[
  american.is_a?(Cheese),
  american.kind_of?(Cheese),
  american.instance_of?(Cheese),
  american.class == Cheese,
  FakeCheese == Cheese,
  FakeCheese.eql?(Cheese),
  FakeCheese.equal?(Cheese),
  FakeCheese.object_id == Cheese.object_id,
  FakeCheese.__id__ == Cheese.__id__,
  (FakeCheese <=> Cheese) == 0,
  FakeCheese <= Cheese && FakeCheese >= Cheese,
  FakeCheese.ancestors == Cheese.ancestors,
  FakeCheese.to_s == Cheese.to_s,
  FakeCheese.inspect == Cheese.inspect,
  FakeCheese.name == Cheese.name,
  american.to_s =~ /#{Cheese}/,
  american.inspect =~ /#{Cheese}/
].all?
# => true
```

[1]: https://github.com/orgsync/active_interaction/issues/179
[2]: http://devblog.orgsync.com/active_interaction/
[3]: http://en.wikipedia.org/wiki/Command_pattern
[4]: https://github.com/orgsync/active_interaction/pull/180
[5]: http://ruby-doc.org/core-2.1.2/Object.html#method-i-is_a-3F
[6]: http://www.ruby-doc.org/core-2.1.2/Module.html#method-i-3E-3D
[7]: http://stackoverflow.com/questions/2818602/in-ruby-why-does-inspect-print-out-some-kind-of-object-id-which-is-different
