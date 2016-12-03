---
title: Announcing Blunt, a pointless Haskell tool
---

I am happy to announce the release of Blunt. Blunt converts Haskell
expressions between the pointfree and pointful styles. I implemented
it as a web service and hosted it on Heroku. Check it out at
[blunt.herokuapp.com][1].

I created this service because I don't always have the [`pointfree`][2]
and [`pointful`][3] commands at the ready. Since they take about five
minutes to install, querying a web service is much faster. Some people
rely on Lambdabot in the Haskell channel on IRC. I don't usually idle
in IRC, so that isn't too useful to me. Plus I can easily use Blunt on
someone else's computer or even my phone.

Blunt is a simple single-page app, but it uses some cool technology behind
the scenes. In particular, it uses WebSockets to avoid the overhead
of making an HTTP request for every change to the input. And of course
the whole thing is itself written in Haskell. The excellent [Haskell on
Heroku][4] buildpack makes deploying to Heroku a piece of cake.

I learned some useful packages while developing Blunt. [Warp][5]
powers the whole thing. I have used it before and it continues to
impress. Combining it with [WAI WebSockets][6] could not have been
easier. I went from a simple HTTP server to a combination HTTP and
WebSockets server in only a couple minutes and a few lines of code.

For rendering markup, I went with [Lucid][7]. I've never used it before
and found it extremely nice to work with. If you want to build HTML in
Haskell, use Lucid. Seriously, it's that nice.

For stylesheets, I used [Clay][8]. It was also new to me. I didn't
like it quite as much as Lucid because of some little oddities like
`alignSide` and `sym2`. However I'm really just picking nits here. Clay
is an excellent way to generate CSS in Haskell.

And finally I used [JMacro][9] for scripts. Instead of being a DSL,
it's a quasi-quoter for Template Haskell. That avoids the problem of
embedding all of JavaScript inside Haskell. It adds some sugar on top
of JavaScript, but I loved it because it caught syntax errors as part
of my build process.

If you're interested in more about how I built Blunt, go look at
[the source on GitHub][10]. You'll find all the nitty-gritty details
over there. If you're only interested in using it, be sure to bookmark
[blunt.herokuapp.com][1].

[1]: https://blunt.herokuapp.com
[2]: http://hackage.haskell.org/package/pointfree
[3]: http://hackage.haskell.org/package/pointful
[4]: https://haskellonheroku.com
[5]: http://hackage.haskell.org/package/warp
[6]: http://hackage.haskell.org/package/wai-websockets
[7]: http://hackage.haskell.org/package/lucid
[8]: http://hackage.haskell.org/package/clay
[9]: http://hackage.haskell.org/package/jmacro
[10]: https://github.com/tfausak/blunt
