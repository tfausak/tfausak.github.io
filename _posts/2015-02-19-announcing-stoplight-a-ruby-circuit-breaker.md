---
title: Announcing Stoplight, a Ruby circuit breaker
---

![][1]

I am proud to announce the first stable release of [Stoplight][2]!
It's like traffic control for your code. Stoplight implements the
[circuit breaker design pattern][3] in Ruby. Use it to gracefully
handle things that occasionally fail.

To get started, install [the `stoplight` gem][4].

``` sh
$ gem install stoplight --version '~> 1.0'
```

Once you've done that, use [the `Stoplight` method][5] to create
stoplights. Each stoplight needs a name and a block to run. Here's
a simple example that calculates a rough approximation of pi.

``` rb
require 'stoplight'

stoplight = Stoplight('roughly-pi') { 22.0 / 7.0 }
stoplight.run
# => 3.142857142857143
stoplight.color
# => "green"
```

Stoplights start off green. If they fail enough, they switch to
red. When they're red, they short circuit and raise an error. Here's
an example that will never succeed.

``` rb
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
```

By default, stoplights pass errors through them. When they're red,
they'll raise a red light error. Sometimes it makes sense to return
a default value instead. You can do this by using a fallback. If
the stoplight is green and raises an error, it'll run the fallback.
If the stoplight is red, it'll run the fallback instead of raising
an error. Here's an example that uses a fallback.

``` rb
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
```

These examples scratch the surface of what stoplights can do. They
are highly configurable. Check out [the readme][6] for more examples
and settings.

## Motivation

Six months ago, [OrgSync experienced a few hours of downtime][7].
It started with a few external services taking longer than expected
to respond. Those delays created a negative feedback loop that
eventually brought the site down.

We decided to wrap those external services in circuit breakers to
protect ourselves from these types of failures. We knew of a few
circuit breaker gems and looked for more. We had a few requirements
to meet:

- State had to be persisted to an external data store like Redis.
  We run many Rails front ends that need to be synchronized.

- The circuits had to return the result of running their block. We
  weren't using these solely for their side effects.

- Any other features, like logging or metrics, were actually
  antifeatures to us. We can handle those things our own way.

We evaluated all of the gems we could find, but ultimately none of
them were right for us. So [Cameron Desautels][8] and I started
developing Stoplight. Those other gems did inspire us, though. I'll
briefly cover them here. You might find one of them useful if
Stoplight isn't right for you.

## Inspiration

### [Breaker][9]

Breaker was created by [Adam Hawkins][10]. It has a lot in common
with Stoplight. It's simple, has great documentation, and supports
external data stores. Unfortunately there's no interface for the
data stores, so you have no choice but to read the reference
implementation.

``` rb
require 'breaker'

Breaker.circuit('example').run { p true }
# true
# => true
```

### [CircuitB][11]

[Aleksey Gureev][12] created CircuitB. It also supports external
data stores. Unfortunately it requires configuring circuits ahead
of time. It also doesn't return the result of the block (although
that will be fixed in version 1.2). Those were deal breakers for
us, so we couldn't use it.

``` rb
require 'circuit_b'

CircuitB.configure do |c|
  c.state_storage = CircuitB::Storage::Memory.new
  c.fuse 'example'
end

CircuitB('example') { p true }
# true
# => 0
```

### [CircuitBreaker][13]

[Will Sargent][14] at [Typesafe][15] created CircuitBreaker. It
actually implements a state machine behind the scenes. That makes
it easy to debug, but hard to use with external data stores. It's
also intended to be used as a mixin, which isn't what we had in
mind.

``` rb
require 'circuit_breaker'

state = CircuitBreaker::CircuitState.new
handler = CircuitBreaker::CircuitHandler.new
handler.handle(state, -> { p true })
# true
# => true
```

### [Circuitbox][16]

[Yann Armand][17] at [Yammer][18] created Circuitbox. Of all the
circuit breaker gems, it feels the most heavyweight. It depends on
ActiveSupport, which provides external data stores and logging. It
also supports advanced percentage-based heuristics.

``` rb
require 'circuitbox'

Circuitbox.circuit(:example).run { p true }
# D, [2015-02-03T09:05:04.307606 #1128] DEBUG -- : [CIRCUIT] closed: querying example
# true
# D, [2015-02-03T09:05:04.307920 #1128] DEBUG -- : [CIRCUIT] closed: example querie success
# => true
```

### [SimpleCircuitBreaker][19]

[Julius Volz][20] and [Tobias Schmidt][21] at [SoundCloud][22]
created SimpleCircuitBreaker. It definitely lives up to its name.
At less than 60 lines of code, it's the simplest circuit breaker
gem available. This is probably how every other gem started out.
Because of its simplicity, it doesn't support external data stores.

``` rb
require 'simple_circuit_breaker'

SimpleCircuitBreaker.new(3, 10).handle { p true }
# true
# => true
```

### [YaCircuitBreaker][23]

[Patrick Huesler][24] at [Wooga][25] created YaCircuitBreaker. It's
one of the few that allows you to manually manage the state with
`#trip!` and `#reset!`. Unfortunately it has a few problems. It
doesn't support external data stores, the gem name (`ya_circuit_breaker`)
isn't what you require (`circuit_breaker`), and it doesn't return
the result of the block.

``` rb
require 'circuit_breaker'

CircuitBreaker::Basic.new.execute { p true }
# true
# => nil
```

[1]: /static/images/2015/02/19/stoplight.svg
[2]: https://github.com/orgsync/stoplight
[3]: http://en.wikipedia.org/wiki/Circuit_breaker_design_pattern
[4]: https://rubygems.org/gems/stoplight
[5]: http://www.rubydoc.info/github/orgsync/stoplight
[6]: https://github.com/orgsync/stoplight/blob/v1.0.0/README.md#readme
[7]: http://status.orgsync.com/incidents/1j6zkfj2dbdy
[8]: http://camdez.com
[9]: https://github.com/ahawkins/breaker
[10]: https://github.com/ahawkins
[11]: https://github.com/alg/circuit_b
[12]: https://github.com/alg
[13]: https://github.com/wsargent/circuit_breaker
[14]: https://github.com/wsargent
[15]: https://typesafe.com
[16]: https://github.com/yammer/circuitbox
[17]: https://github.com/yarmand
[18]: https://www.yammer.com
[19]: https://github.com/soundcloud/simple_circuit_breaker
[20]: https://github.com/juliusv
[21]: https://github.com/grobie
[22]: https://soundcloud.com
[23]: https://github.com/wooga/circuit_breaker
[24]: https://github.com/phuesler
[25]: https://www.wooga.com
