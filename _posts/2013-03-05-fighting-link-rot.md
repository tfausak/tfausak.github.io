---
layout: post
title: Fighting Link Rot
---

![Grimy chain][1]

I hate [link rot][2]. Going to an old website and clinking link after
link only to have them 404 frustrates me to no end. It's not an old
problem, either. Back in 1998, [Jakob Nielson reported][3] that "6%
of the links on the Web are broken". I can't imagine that number's
gotten any better in the intervening years.

Fortunately, as the webmaster of this particular blog, I'm in a
unique position to ensure that my links don't rot. A couple tools
exist to help make checking all 341 links a bit easier. The [W3C
Link Checker][4] works wonders, but it has some limitations. It only
works in the browser, it isn't highly configurable, and it's hard
to script or automate.

Behind the scenes, the [W3C-LinkChecker][5] Perl script does all the
heavy lifting. It's much more flexible than the browser-based
solution, but its output is pretty chatty and leaves a little to
be desired. In particular, it complained about [my social widgets][6]
on every single page it scanned.

I got tired of combing through that output for pertinent problems
and decided to write [my own link checker][7]. It exploits a couple
characteristics of my data: all of my posts are in Markdown, all
of my links are in footnotes, and all of my footnotes are numbered.
Those facts make finding links as easy as `grep '^\[[0-9]*\]:'
_posts/*.md`. The script outputs:

<div class="highlight">
    <pre><code>./_posts/2011-10-23-hello-world.md
<span class="gr">✖ http://pages.github.com/404!</span>
<span class="gi">✓ http://mercurial.selenic.com</span></code></pre>
</div>

The script found 127 bad links, which I fixed in [one fell swoop][8].
Most of them were easy fixes, like adding or removing a trailing
slash. Others were bona fide 301s or 404s. I fixed the redirections
by following them until they stopped, which was easy enough. Some
of the missing links could be found again, like Amazon's Python S3
library, which is now known as boto. For the ones that couldn't be
found, I either deleted them outright if they weren't very important
or looked for a suitable replacement.

[1]: /static/images/2013-03-05-grimy-chain.jpg
[2]: http://en.wikipedia.org/wiki/Link_rot
[3]: http://www.nngroup.com/articles/fighting-linkrot/
[4]: http://validator.w3.org/checklink
[5]: http://search.cpan.org/dist/W3C-LinkChecker/
[6]: /2012/05/31/better-social-widget-lazy-loading/
[7]: https://gist.github.com/tfausak/5096655
[8]: https://github.com/tfausak/tfausak.github.com/commit/4f5bfc7
