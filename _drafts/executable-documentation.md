---
title: Executable documentation
---

- recently working on many small projects
- http://taylor.fausak.me/about/#projects
- noticed a common trend with their tests and documentation

- started with a bunch of unit tests
- add stubs as code gets more complicated
- felt brittle and counter to the point of having tests
- the point is prove your code works and make it easy to refactor
- so move toward integration tests
- then we struggled with documentation
- our tests had lots of examples
- but they weren't documentation
- and the documentation wasn't tested
- so it could easily fall out of date
- basically duplicating a lot of work
- we are working on combining the two into the readme
- https://github.com/orgsync/active_interaction/pull/183

- doctests are good
- but they're not approachable
- you have to know where to look to find them
- and they're closer to unit tests
- but they're like this idea on a micro scale
- used these a lot in Python
- handled some tests for hs2048 this way
- https://github.com/tfausak/hs2048/blob/0.1.0/library/Hs2048/Tile.hs#L41-L46

- since your readme says how to use your code, it's important to make it correct
- the only way to be sure is to make it executable
- literate programming is great at this
- this is more or less how I wrote my strava library
- write how you want it to look
- then implement that
- https://github.com/tfausak/strive/tree/v0.6.1#readme
