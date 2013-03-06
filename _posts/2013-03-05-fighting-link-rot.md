---
layout: post
title: Fighting Link Rot
---

![Grimy chain][]

I hate [link rot][]. Going to an old website and clinking link after
link only to have them 404 frustrates me to no end. It's not an old
problem, either. Back in 1998, [Jakob Nielson reported][] that "6%
of the links on the Web are broken". I can't imagine that number's
gotten any better in the intervening years.

Fortunately, as the webmaster of this particular blog, I'm in a
unique position to ensure that my links don't rot. A couple tools
exist to help make checking all 341 links a bit easier. The [W3C
Link Checker][] works wonders, but it has some limitations. It only
works in the browser, it isn't highly configurable, and it's hard
to script or automate.

Behind the scenes, the [W3C-LinkChecker][] Perl script does all the
heavy lifting. It's much more flexible than the browser-based
solution, but its output is pretty chatty and leaves a little to
be desired. In particular, it complained about [my social widgets][]
on every single page it scanned.

I got tired of combing through that output for pertinent problems
and decided to write [my own link checker][]. It exploits a couple
characteristics of my data: all of my posts are in Markdown, all
of my links are in footnotes, and all of my footnotes are numbered.
Those facts make finding links as easy as `grep '^\[[0-9]*\]:'
_posts/*.md`. The script outputs:

    ./_posts/2011-10-23-hello-world.md
    ✖ http://pages.github.com/404!
    ✓ http://mercurial.selenic.com

The script found 127 bad links, which I fixed in [one fell swoop][].
Most of them were easy fixes, like adding or removing a trailing
slash. Others were bona fide 301s or 404s. I fixed the redirections
by following them until they stopped, which was easy enough. Some
of the missing links could be found again, like Amazon's Python S3
library, which is now known as boto. For the ones that couldn't be
found, I either deleted them outright if they weren't very important
or looked for a suitable replacement.

[grimy chain]: /static/images/2013-03-05-grimy-chain.jpg
[link rot]: http://en.wikipedia.org/wiki/Link_rot
[jakob nielson reported]: http://www.nngroup.com/articles/fighting-linkrot/
[w3c link checker]: http://validator.w3.org/checklink
[w3c-linkchecker]: http://search.cpan.org/dist/W3C-LinkChecker/
[my social widgets]: /2012/05/31/better-social-widget-lazy-loading/
[my own link checker]: https://gist.github.com/tfausak/5096655
[one fell swoop]: https://github.com/tfausak/tfausak.github.com/commit/4f5bfc7b1c677b88cfa160aa626e3ffcd6e0e58d
