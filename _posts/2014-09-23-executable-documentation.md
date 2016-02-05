---
title: Executable documentation
---

![Erudite logo][1]

Back in 1999, Tim Peters released [doctest for Python][2]. His motivation was
simple:

> - Examples are priceless.
> - Examples that don't work are worse than worthless.
> - Examples that work eventually turn into examples that don't.

Recently, I've been working on [many small side projects][3]. I found myself
with the same motivation as Mr. Peters. Since most of my projects were written
in Ruby, I looked for [a doctest gem][4]. Unfortunately I didn't like what I
found.

From what I can tell, [rubydoctest][5] is the de facto standard. It doesn't
appear to be actively maintained, and it has a couple glaring omissions, like
[ellipsis wildcards][6] and [expected exceptions][7]. I considered forking it to
fix these issues, but ultimately decided to create my own gem instead.

This made sense to me because my aims are slightly different. I want a tool that
can be used to write doctests, but it should also support a kind of literate
programming. In particular, I should be able to feed it a project's `README.md`
and it should run that as a test. This is inspired by Simon Hengel's tool
[markdown-unlit][8].

My own tool, [Erudite][9], isn't as robust as either rubydoctest or
markdown-unlit yet. Even so, it's be useful for testing. I can fire up the
executable and paste in code from [a `README.md`][10]. It'll do all the work to
determine if that code is valid or not.

``` sh
$ bundle exec erudite
>> require 'stoplight'
=> true
>> Stoplight.data_store
=> #<Stoplight::DataStore::Memory:0x... @data={}>
- PASS
- PASS
```

I'm looking forward to rounding out Erudite's feature set, and I hope you find
it useful. Check out [its project page][9] or the source [on GitHub][11]

[1]: /static/images/2014/09/23/erudite.png
[2]: https://groups.google.com/forum/#!msg/comp.lang.python/DfzH5Nrt05E/Yyd3s7fPVxwJ
[3]: /about/#projects
[4]: https://rubygems.org/search?query=doctest
[5]: https://github.com/tablatom/rubydoctest
[6]: https://github.com/tablatom/rubydoctest/issues/9
[7]: https://github.com/tablatom/rubydoctest/issues/10
[8]: https://github.com/sol/markdown-unlit
[9]: /erudite/
[10]: https://github.com/orgsync/stoplight/blob/v0.4.0/README.md#setup
[11]: https://github.com/tfausak/erudite
