---
title: Announcing Stoplight, a Ruby circuit breaker
---

![][1]

I am proud to announce the first stable release of [Stoplight][]!
It's like traffic control for your code.
Stoplight implements the [circuit breaker design pattern][] in Ruby.
Use it to gracefully handle things that occasionally fail.

To get started, install [the `stoplight` gem][].

{% highlight sh %}
$ gem install stoplight --version '~> 1.0'
{% endhighlight %}

Once you've done that, use [the `Stoplight` method][] to create stoplights.
Each stoplight needs a name and a block to run.
Here's a simple example that calculates a rough approximation of pi.

{% highlight rb %}
require 'stoplight'

stoplight = Stoplight('roughly-pi') { 22.0 / 7.0 }
stoplight.run
# => 3.142857142857143
stoplight.color
# => "green"
{% endhighlight %}

Stoplights start off green.
If they fail enough, they switch to red.
When they're red, they short circuit and raise an error.
Here's an example that will never succeed.

{% highlight rb %}
stoplight = Stoplight('no-conversion') { 'oh'[:no] }
stoplight.run
# TypeError: no implicit conversion of Symbol into Integer
stoplight.run
# TypeError: no implicit conversion of Symbol into Integer
stoplight.run
# Switching no-conversion from green to red because TypeError no implicit conversion of Symbol into Integer
# TypeError: no implicit conversion of Symbol into Integer
stoplight.color
# => "red"
stoplight.run
# Stoplight::Error::RedLight: no-conversion
{% endhighlight %}

By default, stoplights pass errors through them.
When they're red, they'll raise a red light error.
Sometimes it makes sense to return a default value instead.
You can do this by using a fallback.
If the stoplight is green and raises an error, it'll run the fallback instead.
If the stoplight is red, it'll run the fallback instead of raising an error.
Here's an example that uses a fallback.

{% highlight rb %}
stoplight = Stoplight('zero') { 1 / 0 }.with_fallback { 0 }
stoplight.run
# => 0
stoplight.run
# => 0
stoplight.run
# Switching zero from green to red because ZeroDivisionError divided by 0
# => 0
stoplight.color
# => "red"
stoplight.run
# => 0
{% endhighlight %}

These examples scratch the surface of what stoplights can do.
They are highly configurable.
Check out [the readme][] for more examples and settings.

## Motivation

Six months ago, [OrgSync experienced a few hours of downtime][].
It started with a few external services taking longer than expected to respond.
Those delays created a negative feedback loop that eventually brought the site down.

We decided to wrap those external services in circuit breakers to protect ourselves from these types of failures.
There are a few other circuit breaker gems.
We evaluated all of them, but ultimately none of them were right for us.
So [Cameron Desautels][] and I started developing Stoplight.

Those other gems did inspire us, though.
I'll briefly cover them here.
You might find one of them useful if Stoplight isn't for you.

### [Breaker][]

Author: [Adam Hawkins][]

Pros:

- Supports external data stores, although you'd have to write your own.
- Has a great readme.
- Has a test suite.

Cons:

- Has typos in the readme.
- No interface for the data store.

Example:

    require 'breaker'

    Breaker.circuit('example').run { p true }
    # true
    # => true

This gem has the most in common with Stoplight. It's simple, has a great readme,
and supports external data stores. Sort of. You can implement your own data
store (or "repository" as they call it) and plug it in relatively easily.
However there's no protocol to implement. In spite of being simple, it
implements timeouts, which I think are orthogonal to circuit breakers.

In short, this gem is promising but has some rough edges. Had I found it when
looking for existing circuit breaker gems, I probably would have forked it
instead of starting from scratch.

### [CircuitB][]

Author: [Aleksey Gureiev][]

Pros:

- Supports Redis data store.
- Has a comprehensive test suite.
- Has a good readme.

Cons:

- Requires configuring all circuits ahead of time.
- Supports orthogonal features like timeouts.
- Hasn't really been updated in five years.
- Requires Redis gem even when using in-memory data store.
- Doesn't return result of block.

Example:

    require 'circuit_b'

    CircuitB.configure do |c|
      c.state_storage = CircuitB::Storage::Memory.new
      c.fuse 'example'
    end

    CircuitB('example') { p true }
    # true
    # => 0

This gem supports Redis data stores out of the box. It also has a comprehensive
test suite and a good readme. So it's got a lot going for it. But it's main
drawback is that it requires configuring circuits ahead of time. Since we were
planning on wrapping many things with circuit breakers, that wasn't an option
for us.

Sure, I could define a function that configures and runs the circuit all in one,
but I would be fighting the library at that point. Plus it has two other
problems: (1) it hasn't been updated in nearly five years, and (2) it doesn't
return the result of the block. That means it's only useful for side effects.

### [CircuitBreaker][]

Author: [Will Sargent][]
Company: [Typesafe][]

Pros:

- Actually implements a state machine.
- Has a test suite.
- Has a decent readme.

Cons:

- Doesn't support external data stores.
- Intended as a mixin.
- Depends on AASM.

Example:

    require 'circuit_breaker'

    class C
      include CircuitBreaker
      def example
        p true
      end
      circuit_method :example
    end

    C.new.example
    # true
    # => true

Holy cow. This one actually implements a state machine. And it just gives me the
impression that someone thought about it a lot. Which would be great, but it
doesn't support external data stores. Since we run many front ends that need to
be kept in sync, that's not acceptable.

I suspect that adding the ability to use external data stores would mess with
the purity of actually using a state machine. It doesn't seem like a reasonable
change to make to this gem.

### [Circuitbox][]

Author: [Yann Armand][]
Company: [Yammer][]

Pros:

- Has a test suite.
- Has a readme.
- Supports a percentage-based heuristic.
- Supports external data stores via ActiveSupport.

Cons:

- Feels heavyweight.
- Depends on ActiveSupport.
- First example results in a syntax error.
- Supports orthogonal features like logging.

Example:

    require 'circuitbox'

    Circuitbox.circuit(:example).run { p true }
    # D, [2015-02-03T09:05:04.307606 #1128] DEBUG -- : [CIRCUIT] closed: querying example
    # true
    # D, [2015-02-03T09:05:04.307920 #1128] DEBUG -- : [CIRCUIT] closed: example querie success
    # => true

If circuit_breaker was made by a computer scientist, circuitbox was made by a
software engineer. It feels enterprise-y. That's not necessarily a problem, but
in this case it's a little weird. Do circuits really need logging and metrics
baked into the library?

This gem also uses ActiveSupport caches for storage. That kind of makes sense,
based on its dependency on ActiveSupport, but it feels weird storing critical
application data in a cache.

### [SimpleCircuitBreaker][]

Authors: [Julius Volz][] and [Tobias Schmidt][]
Company: [SoundCloud][]

Pros:

- Has a test suite.
- Has a decent readme.
- Implemented in less than 60 lines of code.

Cons:

- Doesn't support external data stores.
- Hasn't been updated in a year.

Example:

    require 'simple_circuit_breaker'

    SimpleCircuitBreaker.new(3, 10).handle { p true }
    # true
    # => true

This one definitely lives up to its name. It's a single file, less than 60
lines. I get the feeling this is where every other gem started out. This is what
you'd do if you had to implement a circuit breaker during an interview.

That being said, it's not stupid, just simple. By not having any features (like
external data stores), it can stay focused on the simplest thing that works.
Unfortunately that doesn't cut it for me.

### [(Ya)CircuitBreaker][]

Authors: [Patrick Huesler][]
Company: [Wooga][]

Pros:

- Has a readme.
- Has a test suite.
- Allows manually managing state with #trip! and #reset!.

Cons:

- Doesn't support external data stores.
- Gem name differs from require name.
- Doesn't return result of block.
- Implements orthogonal features like timeouts.

Example:

    require 'circuit_breaker'

    CircuitBreaker::Basic.new.execute { p true }
    # true
    # => nil

I honestly don't know why this one exists. It acknowledges circuit_breaker and
simple_circuit_breaker but doesn't say why you might want to use it instead.

[1]: /static/images/2015/02/19/stoplight.svg
[stoplight]: https://github.com/orgsync/stoplight
[circuit breaker design pattern]: http://en.wikipedia.org/wiki/Circuit_breaker_design_pattern
[the stoplight gem]: https://rubygems.org/gems/stoplight
[the stoplight method]: http://www.rubydoc.info/github/orgsync/stoplight/toplevel:Stoplight
[the readme]: https://github.com/orgsync/stoplight/blob/v1.0.0/README.md#readme
[orgsync experienced some downtime]: http://status.orgsync.com/incidents/1j6zkfj2dbdy
[cameron desautels]: http://camdez.com
[breaker]: https://github.com/ahawkins/breaker
[adam hawkins]: https://github.com/ahawkins
[circuitb]: https://github.com/alg/circuit_b
[aleksey gureiev]: https://github.com/alg
[circuitbreaker]: https://github.com/wsargent/circuit_breaker
[will sargent]: https://github.com/wsargent
[typesafe]: https://typesafe.com
[circuitbox]: https://github.com/yammer/circuitbox
[yann armand]: https://github.com/yarmand
[yammer]: https://www.yammer.com
[simplecircuitbreaker]: https://github.com/soundcloud/simple_circuit_breaker
[julius volz]: https://github.com/juliusv
[tobias schmidt]: https://github.com/grobie
[soundcloud]: https://soundcloud.com
[(ya)circuitbreaker]: https://github.com/wooga/circuit_breaker
[patrick huesler]: https://github.com/phuesler
[wooga]: https://www.wooga.com
