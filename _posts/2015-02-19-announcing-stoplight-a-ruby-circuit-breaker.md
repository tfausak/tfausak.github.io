---
title: Announcing Stoplight, a Ruby circuit breaker
---

![][1]

I am proud to announce the release of [Stoplight][] version 1.0.0! Stoplight is
like traffic control for your code. It's an implementation of the [circuit
breaker design pattern][] as a Ruby gem. Use it to gracefully handle code that
can fail every now and then.

installation

{% highlight sh %}
$ gem install stoplight --version '~> 1.0'
{% endhighlight %}

simple example

{% highlight rb %}
require 'stoplight'

stoplight = Stoplight('pi') { 22.0 / 7.0 }
stoplight.run
# => 3.142857142857143
stoplight.color
# => "green"
{% endhighlight %}

failing example

{% highlight rb %}
stoplight = Stoplight('error') { [][] }
stoplight.run
# ArgumentError: wrong number of arguments (0 for 1..2)
stoplight.run
# ArgumentError: wrong number of arguments (0 for 1..2)
stoplight.run
# Switching error from green to red because ArgumentError wrong number of arguments (0 for 1..2)
# ArgumentError: wrong number of arguments (0 for 1..2)
stoplight.color
# => "red"
{% endhighlight %}

failing example with fallback

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
{% endhighlight %}

check out the readme for more examples

---

[Cameron Desautels][] and I developed Stoplight for [OrgSync][] over the past six months. We saw the need for a circuit breaker after some service failures cascaded into [downtime for our entire site][].

---

prior art:

- `breaker` by Adam Hawkins
- `circuit_b` by Aleksey Gureiev
- `circuit_breaker` by Will Sargent at Typesafe
- `circuitbox` by Yann Armand at Yammer
- `simple_circuit_breaker` by Julius Volz and Tobias Schmidt at SoundCloud
- `ya_circuit_breaker` by Patrick Huesler and Nadia Zryanina at Wooga

[1]: /static/images/2015/02/19/stoplight.svg
[stoplight]: https://github.com/orgsync/stoplight
[circuit breaker design pattern]: http://en.wikipedia.org/wiki/Circuit_breaker_design_pattern
[cameron desautels]: http://camdez.com
[orgsync]: https://www.orgsync.com
[some downtime]: http://status.orgsync.com/incidents/1j6zkfj2dbdy
