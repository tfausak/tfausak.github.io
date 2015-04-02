---
title: Announcing Blunt, a pointless Haskell tool
---

I am happy to announce the release of Blunt.
Blunt converts Haskell expressions between the pointfree and pointful styles.
I implemented it as a web service and hosted it on Heroku.
Check it out at [blunt.herokuapp.com][].

I created this service because I don't always have the [`pointfree`][] and [`pointful`][] commands on hand.
Since they take about five minutes to install,
querying a web service beats them handily.
Some people rely on Lambdabot in the Haskell channel on IRC.
I don't usually idle in IRC,
so that isn't too useful to me.
Plus I can easily use Blunt on someone else's computer or even my phone.

Blunt is a simple single-page app,
but it uses some neat technology behind the scenes.
In particular it uses WebSockets to avoid the overhead of making an HTTP request for every change to the input.
And of course the whole thing is written in Haskell itself.
The excellent [Haskell on Heroku][] makes deploying to Heroku a piece of cake.

I learned some useful packages while developing Blunt.
[Warp][] powers the whole thing.
I have used it before and in continues to impress.
Combining it with [WAI WebSockets][] could not have been easier.
I went from a simple HTTP server to a combination HTTP+WebSockets server in only a couple minutes and a few lines of code.

For rendering markup, I went with [Lucid][].
I never used it before and found it extremely nice to work with.
If you want to build HTML in Haskell, use Lucid.
Seriously, it's that nice.

For stylesheets, I used [Clay][].
It was new to me too.
I didn't like it quite as much as Lucid because of some little oddities like `alignSide` `sym2`.
However I'm really just picking nits here.
Clay is an excellent way to generate CSS in Haskell.

And finally I used [JMacro][] for scripts.
Instead of being a DSL, it's a quasi-quoter for Template Haskell.
That avoids the problem of embedding all of JavaScript inside Haskell.
It adds some sugar on top of JavaScript,
but I loved it because it caught syntax errors as part of my build process.

[blunt.herokuapp.com]: https://blunt.herokuapp.com
[`pointfree`]: http://hackage.haskell.org/package/pointfree
[`pointful`]: http://hackage.haskell.org/package/pointful
[haskell on heroku]: https://haskellonheroku.com/
[warp]: http://hackage.haskell.org/package/warp
[wai websockets]: http://hackage.haskell.org/package/wai-websockets
[lucid]: http://hackage.haskell.org/package/lucid
[clay]: http://hackage.haskell.org/package/clay
[jmacro]: http://hackage.haskell.org/package/jmacro
