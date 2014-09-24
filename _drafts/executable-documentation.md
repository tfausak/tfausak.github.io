---
layout: post
title: Executable documentation
---

![Erudite logo](/static/images/erudite.png)

Back in 1999, Tim Peters released [doctest for Python](https://groups.google.com/forum/#!msg/comp.lang.python/DfzH5Nrt05E/Yyd3s7fPVxwJ).
His motivation was simple:

> - Examples are priceless.
> - Examples that don't work are worse than worthless.
> - Examples that work eventually turn into examples that don't.

Recently, I've been working on [many small side projects](http://taylor.fausak.me/about/#projects).
I found myself with the same motivation as Mr. Peters.
Since most of my projects were written in Ruby,
I looked for [a doctest gem](https://rubygems.org/search?query=doctest).
Unfortunately I didn't like what I found.

From what I can tell, [rubydoctest](https://github.com/tablatom/rubydoctest) is the de facto standard.
It doesn't appear to be actively maintained.
And it has a couple glaring omissions, like [ellipsis wildcards](https://github.com/tablatom/rubydoctest/issues/9) and [expected exceptions](https://github.com/tablatom/rubydoctest/issues/10).
I considered forking it to fix these issues,
but ultimately decided to create my own gem instead.

This made sense to me because my aims are slightly different.
I want a tool that can be used to write doctests,
but it should also support a kind of literate programming.
In particular, I should be able to feed it a project's `README.md` and it should run that as a test.
This is inspired by Simon Hengel's tool [markdown-unlit](https://github.com/sol/markdown-unlit).

My own tool, [Erudite](http://taylor.fausak.me/erudite/), isn't as robust as either rubydoctest or markdown-unlit yet.
Even so, it's be useful for testing.
I can fire up the executable and paste in code from [a readme](https://github.com/orgsync/stoplight/blob/v0.4.0/README.md#setup).
It'll do all the work to determine if that code is valid or not.

{% highlight sh %}
$ bundle exec erudite
>> require 'stoplight'
=> true
>> Stoplight.data_store
=> #<Stoplight::DataStore::Memory:...>
- PASS
- PASS
{% endhighlight %}

I'm looking forward to rounding out Erudite's feature set,
and I hope you find it useful.
