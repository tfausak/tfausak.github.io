---
layout: post
title: Class Comparison in Ruby
---

Last week, I encountered [an interesting issue][1]. It boils down to this: How
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

[1]: https://github.com/orgsync/active_interaction/issues/179
[2]: https://github.com/orgsync/active_interaction
[3]: http://en.wikipedia.org/wiki/Command_pattern
