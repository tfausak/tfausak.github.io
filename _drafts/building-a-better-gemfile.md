---
layout: post
title: Building a Better Gemfile
---

If you've been hacking on a Rails project for a while, chances are your Gemfile has spiraled out of control.
For instance, the main OrgSync Gemfile contains 127 gems.
That's a ton of dependencies, and they slow us down.
Running `bundle exec rake environment` takes at least 15 seconds.
That might not sound like much, but it adds up.
Think of it as a 15 second tax every time you do anything.

Many of the gems are severely under-specified, too.
This makes running `bundle update` downright dangerous, as you could upgrade to a new major version of a gem.
Our worst offenders were for assets, like CoffeeScript and Sass.
There's a comment right above them that says:

> Make sure you peform a full asset precompile on deploy whenever you update any of the gems in the following group.

But none of them have any versions specified.
You could argue that that's what `Gemfile.lock` is for.
I don't buy that, though.
There's a world of difference between `gem 'sass'` and `gem 'sass', '~> 3.2.10'`.

So what can be done?
With one simple helper function, you can fix both problems.
Drop this into your Gemfile before any gems:

{% highlight ruby %}
def gem(name, version, options = {})
  if options.has_key?(:require)
    if options[:require].equal?(true)
      options.delete(:require)
    end
  else
    options.merge!(require: false)
  end

  super(name, version, options)
end
{% endhighlight %}

## Happier Developers with Versions

By overwriting the `gem` function and requiring a `version` parameter, you can be sure you'll never end up with a gem with an unspecified version.
If you try to, `bundle` will blow up at you:

    .../Gemfile:3:in `gem': wrong number of arguments

This has the benefit of making `bundle update` much less risky to run.
Also, it's a lot easier to tell at a glance which versions of gems your project needs.

## Faster Startup with `require: false`

Instead of eagerly requiring all of your gems, the modified `gem` function doesn't require them unless you set `require: true`.
As a result, most of your gems won't be available any more.
You'll have to explicitly require them at the top of the file.

As a Pythonista, this doesn't bug me one bit.
If you still need convincing, consider this:
It got the OrgSync project's startup time down to 10 seconds (a 30% improvement)!
And that's not even as fast as it could be --- 43 gems are still eagerly required.
