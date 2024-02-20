---
title: Announcing Purple Yolk, a dumb Haskell extension for VS Code
---

I'm happy (although maybe not that proud) to announce [Purple Yolk][], a dumb Haskell extension for VS Code.
It basically just runs GHCi in the background for you in order to get squiggly underlines.
Also it can run HLint to get code quality hints.
And it can format Haskell and Cabal files with the formatter of your choice.
That may not sound like much, but I've found it to be enough for an effective Haskell IDE!

So why bother with this?
After all, we already have the [Haskell Language Server][] (HLS), which has a corresponding VS Code extension.
In my experience, HLS is great when it works, but often it doesn't work for mysterious reasons.
I wanted a tool that did less but did it a lot more reliably.
I figured that interacting with GHCi is about as reliable as you can get.
And using normal command line tools for everything else (like HLint, Ormolu & Gild) keeps things simple but just powerful enough.

I've been using Purple Yolk as my primary Haskell development tool for the better part of four years now.
I've used it across a wide range of projects, both at work and as a hobby.
It works well for projects of all sizes, from small scripts to giant projects with tens of thousands of lines of code and all kinds of language extensions and pre-processors.

So if you use VS Code, please take a minute to check out [Purple Yolk][] and let me know what you think.
Just don't expect much more than what I've described here!

[Purple Yolk]: https://github.com/tfausak/purple-yolk
[Haskell Language Server]: https://github.com/haskell/haskell-language-server
