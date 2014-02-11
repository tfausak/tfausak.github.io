---
layout: post
title: Hop, a Haskell package
---

<https://github.com/tfausak/hop>

-   wanted to create a project template of sorts
-   i'm a relative haskell newbie
-   interested in teaching myself more about it

## Tools

-   use recent versions of everything
-   no sense in building on obsolete stuff
-   ghc 7.6 (7.8 soon!)
-   cabal 1.18 (sandboxes!)
-   development environment specifically
-   i use vagrant usually
-   not necessary thanks to cabal sandbox
-   new feature, but nicer than hsenv, capri, etc.

## Unit Tests

-   not as popular in haskell
-   rely more on the type system
-   still wanted them
-   hunit is the bog standard
-   quickcheck is good for properties
-   HTF ties them together
-   syntax is still ugly
-   and manually setting them up is annoying
-   hspec solves both problems
-   plus it's familiar to me coming from rspec

## Documentation

-   again, not as popular
-   again, due to types
-   regardless, most packages have it anyway
-   haddock is the bog standard
-   and it's pretty good
-   other than a slightly weird syntax
-   i would prefer markdown
-   top-level package documentation is in cabal file
-   syntax is a bastard child of yaml and haddock
-   suuucks
-   unlike ruby, no way to pull in readme

## Benchmarks

-   obviously not necessary for all code
-   but i wanted to learn how
-   criterion is amazing
-   requires basically no setup
-   automatically does all the annoying stuff
-   much nicer than ruby's benchmark module
-   still a little weird thanks to whnf
-   benchmarking only tells you that your code is slow
-   not why
-   profiling may be necessary
-   ghc provides it

## Code Quality

-   matters a lot less
-   being typed and compiled helps a lot
-   still, i like style guides
-   especially ones that can be enforced programmatically
-   hlint does a wonderful job
-   also helps you learn some haskell tricks along the way
-   also use some other tools to help
-   stylish-haskell automatically formats code
-   isn't as strict as i'd like
-   something like `go fmt` would be better
-   pointfree can't be beat for making functions point free
-   in particular, the verbose option teaches you
-   to do the reverse, pointful rocks

## Continuous Integration

-   as much as possible should be automated
-   tests and hlint are a cakewalk
-   thanks to travis, only need a one-line configuration
-   missing tools like code climate, coveralls, and gemnasium
-   hlint and friends fill in for code climate
-   hpc fills in for coveralls
-   no replacement for gemnasium
-   heck, no replacement for "bundle outdated"
-   gotta manually do "cabal list <package>" for each dependency

-   unfortunately hpc is currently broken
-   will be fixed in the next release (0.7) included in ghc 7.8
-   <https://github.com/haskell/cabal/issues/1049>

## Library

-   wanted to provide a library that can be imported
-   should be compatible with hackage
-   although not necessarily on it
-   since it doesn't do anything
-   can be a single module
-   don't intend to show how to lay out a large haskell package

## Executable

-   wanted to provide an executable that could be run
-   should be implemented in terms of library
-   something like `main = getArgs >>= library`

## Links

-   project layout and recommended tools: <http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program>
-   also: <http://www.haskell.org/haskellwiki/Structure_of_a_Haskell_project>
-   spiritual successor: <http://hackage.haskell.org/package/hnop>
-   code coverage: <http://www.haskell.org/haskellwiki/Haskell_program_coverage>
-   package development: <http://www.haskell.org/cabal/users-guide/developing-packages.html>
-   example hspec: <https://github.com/sol/hspec-example>
-   cabal sandbox: <http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html>
-   stack overflow question in the same vein as this post: <http://stackoverflow.com/questions/9662806/is-there-any-haskell-land-equivalent-to-the-ruby-lands-bundler-et-al-and-if-n>
