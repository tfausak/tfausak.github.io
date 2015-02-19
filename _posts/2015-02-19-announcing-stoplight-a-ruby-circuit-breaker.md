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

Breaker was created by [Adam Hawkins][].
It has a lot in common with Stoplight.
It's simple, has great documentation, and supports external data stores.
Unfortunately there's no interface for the data stores,
so you have no choice but to read the reference implementation.

{% highlight rb %}
require 'breaker'

Breaker.circuit('example').run { p true }
# true
# => true
{% endhighlight %}

### [CircuitB][]

[Aleksey Gureev][] created CircuitB.
It also supports external data stores.
Unfortunately it requires configuring circuits ahead of time.
It also doesn't return the result of the block (although that will be fixed in version 1.2).
Those were deal breakers for us, so we couldn't use it.

{% highlight rb %}
require 'circuit_b'

CircuitB.configure do |c|
  c.state_storage = CircuitB::Storage::Memory.new
  c.fuse 'example'
end

CircuitB('example') { p true }
# true
# => 0
{% endhighlight %}

### [CircuitBreaker][]

[Will Sargent][] at [Typesafe][] created CircuitBreaker.
It actually implements a state machine behind the scenes.
That makes it easy to debug, but hard to use external data stores.
It's also intended to be used as a mixin, which isn't what we had in mind.

{% highlight rb %}
require 'circuit_breaker'

state = CircuitBreaker::CircuitState.new
handler = CircuitBreaker::CircuitHandler.new
handler.handle(state, -> { p true })
# true
# => true
{% endhighlight %}

### [Circuitbox][]

[Yann Armand][] at [Yammer][] created Circuitbox.
Of all the circuit breaker gems, it feels the most heavyweight.
It depends on ActiveSupport, which provides external data stores and logging.
It also supports advanced percentage-based heuristics.

{% highlight rb %}
require 'circuitbox'

Circuitbox.circuit(:example).run { p true }
# D, [2015-02-03T09:05:04.307606 #1128] DEBUG -- : [CIRCUIT] closed: querying example
# true
# D, [2015-02-03T09:05:04.307920 #1128] DEBUG -- : [CIRCUIT] closed: example querie success
# => true
{% endhighlight %}

### [SimpleCircuitBreaker][]

[Julius Volz][] and [Tobias Schmidt][] at [SoundCloud][] created SimpleCircuitBreaker.
It definitely lives up to its name.
At less than 60 lines of code, it's the simplest circuit breaker gem available.
This is probably how every other gem started out.
Because of its simplicity, it doesn't support external data stores.

{% highlight rb %}
require 'simple_circuit_breaker'

SimpleCircuitBreaker.new(3, 10).handle { p true }
# true
# => true
{% endhighlight %}

### [YaCircuitBreaker][]

[Patrick Huesler][] at [Wooga][] created YaCircuitBreaker.
It's one of the few that allows you to manually manage the state with `#trip!` and `#reset!`.
Unfortunately it has a few problems.
It doesn't support external data stores,
the gem name (`ya_circuit_breaker`) isn't what you require (`circuit_breaker`),
and it doesn't return the result of the block.

{% highlight rb %}
require 'circuit_breaker'

CircuitBreaker::Basic.new.execute { p true }
# true
# => nil
{% endhighlight %}

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
[aleksey gureev]: https://github.com/alg
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
[yacircuitbreaker]: https://github.com/wooga/circuit_breaker
[patrick huesler]: https://github.com/phuesler
[wooga]: https://www.wooga.com
