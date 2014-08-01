---
title: Escaping Liquid tags
---

Every so often, I post code samples that contain things that look like
[Liquid][1] tags, so I have to escape them. This turned out to be harder than I
thought. I went through four tries, each better than the last.

## [The Ugly][2]

The first method works by using Liquid to output a string, and then using that
to break up the thing that needs to be escaped so that Liquid won't parse it.
Outputting a string with Liquid is easy:

    {% raw %}{{ "thing" }}{% endraw %}
    # => thing

Then you can use that to separate the opening tag from the closing tag:

    {% raw %}{{ "{% thing " }}%}{% endraw %}
    # => {% raw %}{% thing %}{% endraw %}
    {% raw %}{{ "{{ thing " }}}}{% endraw %}
    # => {% raw %}{{ thing }}{% endraw %}

I find this method to be ugly and non-obvious, so I don't recommend it. It is,
however, pretty clever. And it works with all versions of Liquid.

## [The Bad][3]

If you're outputting something that looks like Liquid output, you can escape it
by assigning it to a variable, then outputting the variable.

    {% raw %}{% assign thing = "{{ thing }}" %}{% endraw %}
    # => {% raw %}{{ thing }}{% endraw %}

This does *not* work if you want to output a Liquid tag like
`{% raw %}{% thing %}{% endraw %}` because the closing tag trips up Liquid's
parser. And of course this method gets annoying if you need to escape a lot of
things. For those reasons, I can't recommend it either.

## [The Good][4]

Finally we arrive at a reasonable solution. Liquid 2.2.2 added support for the
`{% raw %}{% literal %}{% endraw %}` tag, making escaping anything a piece of
cake. It behaves exactly like you'd expect it to:

    {% raw %}{% literal %}{% thing %}{% endliteral %}{% endraw %}
    # => {% raw %}{% thing %}{% endraw %}
    {% raw %}{% literal %}{{ thing }}{% endliteral %}{% endraw %}
    # => {% raw %}{{ thing }}{% endraw %}

This would be perfect, but Liquid removed support for it somewhere along the
line. For some reason, GitHub Pages still supports this syntax. If you never
test your site locally and only deploy to GitHub Pages, this should work for
you. That being said, I can't recommend it because the next method is just as
good and it works everywhere.

## *[Il Buono][5]*

The final and best method is the `{% raw %}{% raw %}{% endraw %}` tag. It's
just like the `{% raw %}{% literal %}{% endraw %}` tag, except it works with
Liquid 2.4.1, which is what GitHub Pages uses.

    {% raw %}{% raw %}{% thing %}{% endraw %}{{ "{% endraw " }}%}
    # => {% raw %}{% thing %}{% endraw %}
    {% raw %}{% raw %}{{ thing }}{% endraw %}{{ "{% endraw " }}%}
    # => {% raw %}{{ thing }}{% endraw %}

This is the recommended way to escape Liquid tags. It's straightforward and
supported everywhere. The only time you *can't* use this method is when you
need to escape a literal `{{ "{% endraw " }}%}`, since that will trip up the
Liquid parser. If you need to do that, I suggest the following:

    {% raw %}{{ "{% endraw " }}%}{% endraw %}
    # => {{ "{% endraw " }}%}

What's really going to bake your noodle later on is, how did I write this blog
post? Check out [the source][6] for that.

[1]: http://liquidmarkup.org
[2]: http://stackoverflow.com/questions/3426182/how-to-escape-liquid-template-tags
[3]: http://stackoverflow.com/questions/3330979/outputting-literal-curly-braces-in-liquid-templates
[4]: http://stackoverflow.com/questions/11676027/jekyll-page-failing-on-github-but-works-successfully-locally-with-safe-flag
[5]: http://wiki.shopify.com/UsingLiquid#No_Liquid_Zone:_the_raw_tag
[6]: https://raw.github.com/tfausak/tfausak.github.io/master/_posts/2013-02-03-escaping-liquid-tags.md
