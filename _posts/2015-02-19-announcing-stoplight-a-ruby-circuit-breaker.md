---
title: Announcing Stoplight, a Ruby circuit breaker
---

![][1]

I am proud to announce the release of [Stoplight][] version 1.0.0! Stoplight is
like traffic control for your code. It's an implementation of the [circuit
breaker design pattern][] as a Ruby gem. Use it to gracefully handle code that
can fail every now and then.

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
