---
title: Announcing Stoplight, a Ruby circuit breaker
---

![][1]

I am proud to announce the release of [Stoplight][] version 1.0.0! Stoplight is
like traffic control for your code. It's an implementation of the [circuit
breaker design pattern][] as a Ruby gem. Use it to gracefully handle code that
can fail every now and then.

To start using Stoplight, install [the `stoplight` gem][].

{% highlight sh %}
$ gem install stoplight --version '~> 1.0'
{% endhighlight %}

Once you've done that, use [the `Stoplight` method][] to create stoplights. Each
stoplight needs a name and some code to run. Here's a simple example that
calculates a rough approximation of pi.

{% highlight rb %}
require 'stoplight'

stoplight = Stoplight('pi') { 22.0 / 7.0 }
stoplight.run
# => 3.142857142857143
stoplight.color
# => "green"
{% endhighlight %}

Stoplights start green. If their code fails enough times, they switch to red.
When they're red, they won't run their code anymore. Instead they'll short
circuit by raising a red light error.

{% highlight rb %}
stoplight = Stoplight('problematic') { 'oh'[:no] }
stoplight.run
# TypeError: no implicit conversion of Symbol into Integer
stoplight.run
# TypeError: no implicit conversion of Symbol into Integer
stoplight.run
# Switching problematic from green to red because TypeError no implicit conversion of Symbol into Integer
# TypeError: no implicit conversion of Symbol into Integer
stoplight.color
# => "red"
stoplight.run
# Stoplight::Error::RedLight: problematic
{% endhighlight %}

Sometimes it makes sense to return a default value instead of raising an error.
You can accomplish this by using a fallback. If the stoplight is green and
raises an error, it'll run the fallback instead. If the stoplight is red, it'll
just run the fallback instead of raising a red light error.

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

Stoplights are highly configurable. Check out [the readme][] for more examples
and settings.

## Motivation

[Cameron Desautels][] and I developed Stoplight for [OrgSync][] over the past
six months. We saw the need for a circuit breaker after some service failures
cascaded into [downtime for our entire site][].

## Inspiration

We didn't jump right into creating our own gem, though. We surveyed the existing
circuit breaker gems. Ultimately none of them were right for us, which is why we
created Stoplight. But they did inspire us, and you might find them useful if
Stoplight isn't right for you.

### [Breaker][]

Author: [Adam Hawkins][]

### [CircuitB][]

Author: [Aleksey Gureiev][]

### [CircuitBreaker][]

Author: [Will Sargent][]
Company: [Typesafe][]

### [Circuitbox][]

Author: [Yann Armand][]
Company: [Yammer][]

### [SimpleCircuitBreaker][]

Authors: [Julius Volz][] and [Tobias Schmidt][]
Company: [SoundCloud][]

### [(Ya)CircuitBreaker][]

Authors: [Patrick Huesler][]
Company: [Wooga][]

[1]: /static/images/2015/02/19/stoplight.svg
[stoplight]: https://github.com/orgsync/stoplight
[circuit breaker design pattern]: http://en.wikipedia.org/wiki/Circuit_breaker_design_pattern
[the stoplight gem]: https://rubygems.org/gems/stoplight
[the stoplight method]: http://www.rubydoc.info/github/orgsync/stoplight/toplevel:Stoplight
[the readme]: https://github.com/orgsync/stoplight/blob/v1.0.0/README.md#readme
[cameron desautels]: http://camdez.com
[orgsync]: https://www.orgsync.com
[downtime for our entire site]: http://status.orgsync.com/incidents/1j6zkfj2dbdy
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
