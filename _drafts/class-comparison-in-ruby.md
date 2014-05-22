---
layout: post
title: Class Comparison in Ruby
---

I recently encountered [an interesting issue][1]. It boils down to this: How
do you know if an object is an instance of a class?

I had to ask this question because I help maintain [ActiveInteraction][2], a
[command pattern][3] library. It provides a way to ensure that an object is a
certain class. For instance, if an interaction needs a `User`, it could say:

{% highlight rb %}
model :someone,
  class: User
{% endhighlight %}

During execution, it's guaranteed that `someone` is in fact a `User`. Behind
the scenes, ActiveInteraction validates that using a `case` statement. In this
instance, it would look like this:

{% highlight rb %}
case someone
when User
  # It's valid.
else
  # It's invalid.
end
{% endhighlight %}

Turns out that's not enough for determining if an object is an instance of a
class. In particular, test mocks pretend to be something they're not by
overriding the `#is_a?` method. Desugaring the `case` statement reveals why it
fails.

{% highlight rb %}
if User === someone
  # It's valid.
else
  # It's invalid.
end
{% endhighlight %}

The `.===` method asks the class if an object is the right class. `#is_a?` does
the same thing in the other direction by asking the object if it's the right
class. Since the test mock doesn't monkey patch the class it's mocking, the
only way around this is to ask both questions.

{% highlight rb %}
if User === someone
  # The class says it's valid.
elsif someone.is_a?(User)
  # The object says it's valid.
else
  # It's invalid.
end
{% endhighlight %}

While developing [a fix][4] for ActiveInteraction,
I wondered if there were other ways to do this.
I did some research and discovered that
there are at least 18 different ways to make this comparison.

It would be unreasonable to make all those checks.
In fact, if you're using anything other than `.===` and `#is_a?`,
you're doing it wrong.
However, I was interested in creating a class that is indistinguishable from another class.
In other words, a perfect mock.

## Creating the Perfect Mock

Before we create the mock,
we need to create the class we'll be mocking.
To make things interesting,
let's have it subclass another class and include a module.

{% highlight rb %}
Food = Class.new
Milk = Module.new
class Cheese < Food
  include Milk
end
gouda = Cheese.new
{% endhighlight %}

Next up let's create the mock.
It shouldn't have anything in common with the class it's mocking.

{% highlight rb %}
FakeCheese = Class.new
american = FakeCheese.new
{% endhighlight %}

With those defined,
we can move on to faking the comparisons.

- [`.===`](#section)
- [`#is_a?`](#isa)
- [`#kind_of?`](#kindof)
- [`#instance_of?`](#instanceof)
- [`#class`](#class)
- [`.<=>`](#section-1)
- [`.<`](#section-2)
- [`.>`](#section-3)
- [`.<=`](#section-4)
- [`.>=`](#section-5)
- [`.==`](#section-6)
- [`.eql?`](#eql)
- [`.equal?`](#equal)
- [`.object_id`](#objectid)
- [`.__id__`](#id)
- [`.ancestors`](#ancestors)
- [`.to_s`](#tos)
- [`.name`](#name)
- [`.inspect`](#inspect)
- [`#to_s`](#tos-1)
- [`#inspect`](#inspect-1)
- [`.include?`](#include)
- [`.included_modules`](#includedmodules)
- [`.superclass`](#superclass)

### `.===`

{% highlight rb %}
FakeCheese === gouda
# => false
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.===(other)
    Cheese === other
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese === gouda
# => true
{% endhighlight %}

### `#is_a?`

{% highlight rb %}
american.is_a?(Cheese)
# => false
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def is_a?(klass)
    Cheese >= klass
  end
end
{% endhighlight %}

{% highlight rb %}
american.is_a?(Cheese)
# => true
{% endhighlight %}

### `#kind_of?`

{% highlight rb %}
american.kind_of?(Cheese)
# => false
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def kind_of?(klass)
    Cheese >= klass
  end
end
{% endhighlight %}

{% highlight rb %}
american.kind_of?(Cheese)
# => true
{% endhighlight %}

### `#instance_of?`

{% highlight rb %}
american.instance_of?(Cheese)
# => false
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def instance_of?(klass)
    Cheese == klass
  end
end
{% endhighlight %}

{% highlight rb %}
american.instance_of?(Cheese)
# => true
{% endhighlight %}

### `#class`

{% highlight rb %}
american.class
# => FakeCheese
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def class
    Cheese
  end
end
{% endhighlight %}

{% highlight rb %}
american.class
# => Cheese
{% endhighlight %}

### `.<=>`

{% highlight rb %}
FakeCheese <=> Cheese
# => nil
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.<=>(other)
    Cheese <=> other
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese <=> Cheese
# => 0
{% endhighlight %}

### `.<`

{% highlight rb %}
FakeCheese < Cheese
# => nil
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.<(other)
    Cheese < other
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese < Cheese
# => false
{% endhighlight %}

### `.>`

{% highlight rb %}
FakeCheese > Cheese
# => nil
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.>(other)
    Cheese > other
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese > Cheese
# => false
{% endhighlight %}

### `.<=`

{% highlight rb %}
FakeCheese <= Cheese
# => nil
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.<=(other)
    Cheese <= other
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese <= Cheese
# => true
{% endhighlight %}

### `.>=`

{% highlight rb %}
FakeCheese >= Cheese
# => nil
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.>=(other)
    Cheese >= other
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese >= Cheese
# => true
{% endhighlight %}

### `.==`

{% highlight rb %}
FakeCheese == Cheese
# => false
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.==(other)
    Cheese == other
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese == Cheese
# => true
{% endhighlight %}

### `.eql?`

{% highlight rb %}
FakeCheese.eql?(Cheese)
# => false
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.eql?(other)
    Cheese.eql?(other)
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.eql?(Cheese)
# => true
{% endhighlight %}

### `.equal?`

{% highlight rb %}
FakeCheese.equal?(Cheese)
# => false
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.equal?(other)
    Cheese.equal?(other)
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.equal?(Cheese)
# => true
{% endhighlight %}

### `.object_id`

{% highlight rb %}
FakeCheese.object_id
# => 70241271125600
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.object_id
    Cheese.object_id
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.object_id
# => 70241271152880
{% endhighlight %}

### `.__id__`

{% highlight rb %}
FakeCheese.__id__
# => 70241271125600
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.__id__
    Cheese.__id__
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.__id__
# => 70241271152880
{% endhighlight %}

### `.ancestors`

{% highlight rb %}
FakeCheese.ancestors
# => [FakeCheese, Object, PP::ObjectMixin, Kernel, BasicObject]
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.ancestors
    Cheese.ancestors
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.ancestors
# => [Cheese, Milk, Food, Object, PP::ObjectMixin, Kernel, BasicObject]
{% endhighlight %}

### `.to_s`

{% highlight rb %}
FakeCheese.to_s
# => "FakeCheese"
{% endhighlight %}

{% highlight rb %}
class Cheese
  def self.to_s
    Cheese.to_s
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.to_s
# => "Cheese"
{% endhighlight %}

### `.inspect`

{% highlight rb %}
FakeCheese.inspect
# => "FakeCheese"
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.inspect
    Cheese.inspect
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.inspect
# => "Cheese"
{% endhighlight %}

### `.name`

{% highlight rb %}
FakeCheese.name
# => "FakeCheese"
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.name
    Cheese.name
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.name
# => "Cheese"
{% endhighlight %}

### `#to_s`

{% highlight rb %}
american.to_s
# => "#<FakeCheese:0x007fa3e09ccd00>"
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def to_s
    "#<#{Cheese}:0x#{'%x' % (object_id << 1)}>"
  end
end
{% endhighlight %}

{% highlight rb %}
american.to_s
# => "#<Cheese:0x007fa3e09ccd00>"
{% endhighlight %}

### `#inspect`

{% highlight rb %}
american.inspect
# => "#<FakeCheese:0x007fa3e09ccd00>"
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def inspect
    "#<#{Cheese}:0x#{'%x' % (object_id << 1)}>"
  end
end
{% endhighlight %}

{% highlight rb %}
american.inspect
# => "#<Cheese:0x007fa3e09ccd00>"
{% endhighlight %}

### `.include?`

{% highlight rb %}
FakeCheese.include?(Milk)
# => false
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.include?(mojule)
    Cheese.include?(mojule)
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.include?(Milk)
# => true
{% endhighlight %}

### `.included_modules`

{% highlight rb %}
FakeCheese.included_modules
# => [PP::ObjectMixin, Kernel]
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.included_modules
    Cheese.included_modules
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.included_modules
# => [Milk, PP::ObjectMixin, Kernel]
{% endhighlight %}

### `.superclass`

{% highlight rb %}
FakeCheese.superclass
# => Object
{% endhighlight %}

{% highlight rb %}
class FakeCheese
  def self.superclass
    Cheese.superclass
  end
end
{% endhighlight %}

{% highlight rb %}
FakeCheese.superclass
# => Food
{% endhighlight %}

## TLDR

{% highlight rb %}
def fake(klass)
  Class.new(BasicObject) do
    eigenclass = class << self; self end
    eigenclass.extend Forwardable
    eigenclass.def_delegators klass, *%i(
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
      include?
      included_modules
      inspect
      name
      object_id
      superclass
      to_s
    )

    define_method(:instance_of?) { |x| klass == x }

    define_method(:is_a?) { |x| klass >= x }
    alias_method(:kind_of?, :is_a?)

    define_method(:inspect) { "#<#{klass.name}:0x#{'%x' % (__id__ << 1)}>" }
    alias_method(:to_s, :inspect)
  end
end
{% endhighlight %}

[1]: https://github.com/orgsync/active_interaction/issues/179
[2]: https://github.com/orgsync/active_interaction
[3]: http://en.wikipedia.org/wiki/Command_pattern
[4]: https://github.com/orgsync/active_interaction/pull/180
