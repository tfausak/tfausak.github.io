---
layout: post
title: 'Haskeleton: A Haskell Project Skeleton'
---

I'm new to Haskell.
I've learned enough to feel comfortable writing programs in it.
I can solve code katas like [exercism.io][], [H-99][], and [Project Euler][].
Yet I don't feel comfortable developing software with it.
Writing idiomatic, maintainable, well-tested Haskell code remains a mystery to me.

Unfortunately, Cabal provides little guidance.
For instance, `cabal init` asks 11 questions and outputs two files totalling 26 lines.
In an effort to both improve on that and teach myself,
I built [Haskeleton][], a Haskell project skeleton.

Eventually I hope it will replace `cabal init`.
But for the time being, it's just an example project you can copy.
This post will walk you through setting up a project like Haskeleton
and explain the decisions I made along the way.

-   [Setup][]
-   [Library][]
-   [Executable][]
-   [Documentation][]
-   [Testing](#testing)
-   [Benchmarks](#benchmarks)
-   [Code Quality](#code-quality)
    -   [Test Documentation](#test-documentation)
    -   [Check Documentation Coverage](#check-documentation-coverage)
    -   [Check Code Coverage](#check-code-coverage)
    -   [Lint Code](#lint-code)
-   [Continuous Integration](#continuous-integration)
-   [Conclusion](#conclusion)

## Setup

There's no reason to make new software with old technology.
To get started, make sure you have GHC 7.6.3 and Cabal 1.18.0.2.
You can get GHC through [The Haskell Platform][]
and the latest version of Cabal with `cabal install cabal-install`.

Think of a name for your project.
I went with "husk", so replace it with your name throughout.
Make a directory for your project to get started: `mkdir husk && cd $_`.

The only necessary file is the package description.
Colloquially, this is "the Cabal file".
It starts off very simple.

{% highlight hs %}
-- husk.cabal
name:          husk
version:       0.0.0
build-type:    Simple
cabal-version: >= 1.18

library
    build-depends:    base
    default-language: Haskell2010
{% endhighlight %}

Here's a rundown of the [package properties][]:

-   `name`: The unique name.
-   `version`: The version number.
    It's a good idea to use [semantic versioning][].
-   `build-type`: The type of build.
    Setting this to `Simple` tells Cabal to use the default setup script.
    Annoyingly, the default is `Custom`.
-   `cabal-version`: The version of the Cabal specification.
    This should be the major and minor parts of your version of Cabal.

And the [build information][] for the library:

-   `build-depends`: A list of needed packages.
    Every project will depend on [`base`][], which provides the Prelude.
-   `default-language`: The version of the Haskell language report.
    The current state of the art is [`Haskell2010`][].

Now that all the boilerplate is out of the way,
let's build the package.
Start by creating a sandbox,
which sets up a private environment.

{% highlight sh %}
# cabal sandbox init
Writing a default package environment file to
.../husk/cabal.sandbox.config
Creating a new sandbox at .../husk/.cabal-sandbox
{% endhighlight %}

Next, install the package.

{% highlight sh %}
# cabal install
Resolving dependencies...
Configuring husk-0.0.0...
Building husk-0.0.0...
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Running Haddock for husk-0.0.0...
Preprocessing library husk-0.0.0...
haddock: No input file(s).
Installing library in
.../husk/.cabal-sandbox/lib/.../husk-0.0.0
Registering husk-0.0.0...
Installed husk-0.0.0
{% endhighlight %}

Alright!
The package is configured properly,
but it doesn't produce anything.
Let's fix that.

## Library

Your library code shouldn't live at the top level.
Create a `library` directory for it.
In there, make a module with the same name as your package.

For this example, it doesn't have to do anything interesting.
So we're going to have it export a function that simply returns the unit value.

{% highlight hs %}
-- library/Husk.hs
module Husk (husk) where

husk :: ()
husk = ()
{% endhighlight %}

Just writing the module isn't enough.
You have to let Cabal know about it.

{% highlight hs %}
-- husk.cabal
library
    hs-source-dirs:  library
    exposed-modules: Husk
{% endhighlight %}

This adds some new [build information][] to the library:

-   `hs-source-dirs`: List of directories to search for source files in.
-   `exposed-modules`: List of modules exposed by the package.

Now that Cabal's in the loop, you can fire up a REPL for your package.

{% highlight sh %}
# cabal repl
Preprocessing library husk-0.0.0...
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 1] Compiling Husk             ( library/Husk.hs, interpreted )
Ok, modules loaded: Husk.
*Husk>
{% endhighlight %}

The modules exposed by the package are already available.
You can play around with the functions they export.

{% highlight hs %}
*Husk> :type husk
husk :: ()
*Husk> husk
()
{% endhighlight %}

That's all that's necessary to make a Cabal package.
With only 14 lines of code,
you've already got more than `cabal init` provides.

## Executable

Now that we've got a library,
let's provide an executable.
Like the library, this shouldn't live at the top level.
Create an `executable` directory for it.

{% highlight hs %}
-- executable/Main.hs
module Main (main) where

import Husk (husk)

main :: IO ()
main = print husk
{% endhighlight %}

As before, Cabal needs to know about this.
Create a new section at the bottom of the Cabal file.

{% highlight hs %}
-- husk.cabal
executable husk
    build-depends:    base, husk
    default-language: Haskell2010
    hs-source-dirs:   executable
    main-is:          Main.hs
{% endhighlight %}

The only new property is `main-is`.
It points to the main entry point for the executable.
After adding that, you can run it!

{% highlight sh %}
# cabal run
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Linking dist/build/husk/husk ...
()
{% endhighlight %}

That's all it takes to make an executable with Cabal.

## Documentation

Now that you've got a library and an executable,
you should document them.
There are two things that need documentation:
the package itself
and the source of the package.

Documenting the package requires adding a few more [package properties] to the Cabal file.
If you're not going to distribute your package on [Hackage][],
you can probably skip this step.

{% highlight hs %}
-- husk.cabal
license:   MIT
copyright: 2014 Taylor Fausak <taylor@fausak.me>
synopsis:  An example package.
{% endhighlight %}

All of the properties are optional,
but I'd recommend supplying at least what's listed above.

To write documentation for the source,
you'll need to learn [Haddock][].
It's a simple markup language for annotating Haskell source.
Here's how the library looks with comments:

{% highlight hs %}
-- library/Husk.hs
-- | An example module.
module Husk (husk) where

{- |
    An alias for the unit value.

    >>> husk
    ()
-}
husk :: () -- ^ The unit type.
husk = ()
{% endhighlight %}

Now that it's documented,
let's create the actual HTML documentation.

{% highlight sh %}
# cabal haddock
Running Haddock for husk-0.0.0...
Preprocessing library husk-0.0.0...
Haddock coverage:
 100% (  2 /  2) in 'Husk'
Documentation created: dist/doc/html/husk/index.html
Preprocessing executable 'husk' for husk-0.0.0...
{% endhighlight %}

The resulting HTML file is basically what you'd see on [Haddock][].
But it's missing one thing:
links to the source.
Adding those requires an additional dependency.
Since not everyone that installs the package will be generating its documentation,
let's make this an optional dependency.

{% highlight hs %}
-- husk.cabal
flag documentation
    default: False

library
    if flag(documentation)
        build-depends: hscolour == 1.20.*
{% endhighlight %}

Enabling flags for Cabal commands is easy.
Add either `-fdocumentation` or `--flags=documentation`.
Using that flag, let's regenerate the documentation.

{% highlight sh %}
# cabal install --flags=documentation
# cabal configure
# cabal haddock --hyperlink-source
Running Haddock for husk-0.0.0...
Running hscolour for husk-0.0.0...
Preprocessing library husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Preprocessing library husk-0.0.0...
Haddock coverage:
 100% (  2 /  2) in 'Husk'
Documentation created: dist/doc/html/husk/index.html
Preprocessing executable 'husk' for husk-0.0.0...
{% endhighlight %}

Now it should have source links.
If you get a bunch of warnings,
you can ignore them.
Haddock is looking for the documentation for the standard library.
If you want to add it, install `haskell-platform-doc`.

## Testing

-   two types of testing needed
-   property checks, provided by quickcheck
-   unit tests, provided by hunit

-   instead of writing quickcheck and hunit directly we'll use hspec
-   it provides a nice wrapper around both of them
-   it also automatically discovers tests for you
-   both really great features
-   start by making a `tests` directory
-   then create a `Spec.hs`
-   this is the top-level test file
-   it finds and runs all the others

{% highlight haskell %}
-- tests/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{% endhighlight %}

-   read about how to lay out your specs for discovery
-   <http://hspec.github.io/hspec-discover.html>
-   now we need an actual test
-   so make another file
-   name it after your library
-   plus `Spec.hs` at the end

{% highlight haskell %}
-- tests/HaskeletonSpec.hs
module HaskeletonSpec (main, spec) where

import           Haskeleton            (haskeleton)
import           Test.Hspec            (Spec, describe, hspec, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.HUnit            ()
import           Test.QuickCheck       ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "haskeleton" $ do
        it "returns an empty string" $ do
            haskeleton 0 `shouldBe` ""

        it "returns the right number of haskeletons" $ do
            haskeleton 3 `shouldBe` "Haskeleton! Haskeleton! Haskeleton!"

        prop "is null for integers less than one" $
            \ n -> if n < 1 then null (haskeleton n) else not (null (haskeleton n))
{% endhighlight %}

-   a lot going on in here
-   imports: the library, hspec, hunit, and quickcheck
-   probably don't need hunit and quickcheck, but you might
-   then provide a `main` so the test can be run by itself
-   finally the meat of the file, `spec`
-   two hunit tests and one quickcheck property

-   next up is clueing cabal in
-   add a new section

{% highlight haskell %}
test-suite hspec
    build-depends:
        base == 4.*
      , haskeleton
      , hspec == 1.8.*
      , HUnit == 1.2.*
      , QuickCheck == 2.6.*
    default-language:
        Haskell2010
    ghc-options:
        -Wall
        -Werror
    hs-source-dirs:
        tests
    main-is:
        Spec.hs
    type:
        exitcode-stdio-1.0
{% endhighlight %}

-   depend on: base, lib, hspec, hunit and quickcheck
-   enable errors and make them warnings
-   point to the top-level spec
-   signal success through exit code 0
-   run the tests!

{% highlight sh %}
$ cabal install --enable-tests
$ cabal configure --enable-tests
$ cabal build
$ cabal test
Test suite hspec: RUNNING...
Test suite hspec: PASS
Test suite logged to: dist/test/haskeleton-0.0.0-hspec.log
{% endhighlight %}

## Benchmarks

-   going to use criterion, which is awesome
-   it does all the annoying work for you

-   make a new directory
-   `benchmarks` or `bench` or whatever
-   like the specs, we need a top level entry point
-   unfortunately there's no automatic discovery
-   so make `Bench.hs`

{% highlight haskell %}
-- benchmarks/Bench.hs
module Main (main) where

import           Criterion.Main  (bgroup, defaultMain)
import qualified HaskeletonBench

main :: IO ()
main = defaultMain
    [ bgroup "Haskeleton" HaskeletonBench.benchmarks
    ]
{% endhighlight %}

-   it depends on `HaskeletonBench`, which we haven't made yet
-   let's go make that

{% highlight haskell %}
module HaskeletonBench (benchmarks) where

import           Criterion.Main  (bench, nf)
import           Criterion.Types (Benchmark)
import           Haskeleton      (haskeleton)

benchmarks :: [Benchmark]
benchmarks =
    [ bench "haskeleton 0" (nf haskeleton 0)
    , bench "haskeleton 3" (nf haskeleton 3)
    ]
{% endhighlight %}

-   pretty straightforward
-   check out the docs
-   <http://hackage.haskell.org/package/criterion>
-   only weirdness is WHNF/NF
-   but that's haskell
-   not benchmarks in particular

-   should be getting used to this by now
-   gotta tell cabal what's up

{% highlight haskell %}
benchmark benchmarks
    build-depends:
        base == 4.*
      , criterion == 0.6.*
      , haskeleton
    default-language:
        Haskell2010
    hs-source-dirs:
        benchmarks
    main-is:
        Bench.hs
    type:
        exitcode-stdio-1.0
{% endhighlight %}

-   now able to run benchmarks

{% highlight sh %}
$ cabal install --enable-benchmarks
$ cabal configure --enable-benchmarks
$ cabal build
$ cabal bench
benchmarking Haskeleton/haskeleton 0
mean: 23.41356 ns, lb 23.40300 ns, ub 23.42692 ns, ci 0.950
std dev: 60.52601 ps, lb 48.55063 ps, ub 89.74390 ps, ci 0.950
{% endhighlight %}

## Code Quality

-   that covers everything for making a library
-   and binary
-   and making sure they're correct
-   and fast
-   i like to ensure the quality is up to par, too

### Test Documentation

-   our documentation had some example code in it
-   we should make sure that those examples are correct
-   if they're not, our documentation is wrong
-   that should be a build error
-   fortunately, thanks to the doctest package, it can be

-   make a new test file, `DocTest.hs`
-   all it does it run doctest against your source files

{% highlight haskell %}
-- tests/DocTest.hs
module Main (main) where

import           Test.DocTest (doctest)

arguments :: [String]
arguments =
    [ "library/Haskeleton.hs"
    ]

main :: IO ()
main = doctest arguments
{% endhighlight %}

-   you'll have to manually add source files to it
-   something like hspec-discover would be nice
-   you guessed it, gotta tell cabal

{% highlight haskell %}
test-suite doctest
    build-depends:
        base == 4.*
      , doctest == 0.9.*
    default-language:
        Haskell2010
    hs-source-dirs:
        tests
    main-is:
        DocTest.hs
    type:
        exitcode-stdio-1.0
{% endhighlight %}

-   run it same as the other tests

{% highlight sh %}
$ cabal install --enable-tests --only-dependencies
$ cabal configure --enable-tests
$ cabal build
$ cabal test
Test suite doctest: RUNNING...
Test suite doctest: PASS
Test suite logged to: dist/test/haskeleton-0.0.0-doctest.log
{% endhighlight %}

### Check Documentation Coverage

-   in addition to making sure the examples are correct
-   we should make sure everything is documented
-   there's no turn key solution for this
-   but it's easy enough to make one
-   make a new test file, `Haddock.hs`
-   it's going to run haddock against all the source files
-   and parse the output to check coverage

{% highlight haskell %}
-- tests/Haddock.hs
module Main (main) where

import           Data.List      (genericLength)
import           Data.Maybe     (catMaybes)
import           System.Exit    (exitFailure, exitSuccess)
import           System.Process (readProcess)
import           Text.Regex     (matchRegex, mkRegex)

arguments :: [String]
arguments =
    [ "haddock"
    ]

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    output <- readProcess "cabal" arguments ""
    if average (match output) >= expected
        then exitSuccess
        else putStr output >> exitFailure

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
{% endhighlight %}

-   tune the threshold by changing `expected`
-   don't have to modify this as you make new files
-   still gotta loop in cabal

{% highlight haskell %}
test-suite haddock
    build-depends:
        base == 4.*
      , process == 1.1.*
      , regex-compat == 0.95.*
    default-language:
        Haskell2010
    hs-source-dirs:
        tests
    main-is:
        Haddock.hs
    type:
        exitcode-stdio-1.0
{% endhighlight %}

-   run it with the rest of the suite

{% highlight sh %}
$ cabal install --enable-tests --only-dependencies
$ cabal configure --enable-tests
$ cabal build
$ cabal test
Test suite haddock: RUNNING...
Test suite haddock: PASS
Test suite logged to: dist/test/haskeleton-0.0.0-haddock.log
{% endhighlight %}

### Check Code Coverage

-   we're checking percent of documentation coverage
-   we should also be checking how much of our code is covered by our tests
-   we can measure it in pretty much the same way
-   we have to set up HPC, GHC's built in profiler
-   modify the `hspec` test-suite in the cabal file

{% highlight haskell %}
test-suite hspec
    ghc-options:
        -fhpc
    hs-source-dirs:
        library
        tests
    other-modules:
        Haskeleton
        HaskeletonSpec
    -- ...
{% endhighlight %}

-   tell GHC to enable HPC with `-fhpc`
-   then include the source directories
-   otherwise hpc won't know what we're talking about
-   similarly, include the library and specs
-   this sucks because the tests auto discover but the coverage doesnt

-   next up we need to create a new test case, `HPC.hs`

{% highlight haskell %}
-- tests/HPC.hs
module Main (main) where

import           Data.List      (genericLength)
import           Data.Maybe     (catMaybes)
import           System.Exit    (exitFailure, exitSuccess)
import           System.Process (readProcess)
import           Text.Regex     (matchRegex, mkRegex)

arguments :: [String]
arguments =
    [ "report"
    , "--include=Haskeleton"
    , "dist/hpc/tix/hspec/hspec.tix"
    ]

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    output <- readProcess "hpc" arguments ""
    if average (match output) >= expected
        then exitSuccess
        else putStr output >> exitFailure

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
{% endhighlight %}

-   it's like 90% the same as the haddock test
-   like the cabal file, you'll have to update this with new sources
-   use `--include=Something`

-   back to the cabal file
-   add another test suite
-   this *must* come after hspec
-   otherwise it either wont have any data
-   or it'll be run against the last iteration's data

{% highlight haskell %}
test-suite hpc
    build-depends:
        base == 4.*
      , process == 1.1.*
      , regex-compat == 0.95.*
    default-language:
        Haskell2010
    hs-source-dirs:
        tests
    main-is:
        HPC.hs
    type:
        exitcode-stdio-1.0
{% endhighlight %}

-   run the test suite

{% highlight sh %}
$ cabal install --enable-tests --only-dependencies
$ cabal configure --enable-tests
$ cabal build
$ cabal test
Test suite hpc: RUNNING...
Test suite hpc: PASS
Test suite logged to: dist/test/haskeleton-0.0.0-hpc.log
{% endhighlight %}

-   check out the coverage yourself

{% highlight sh %}
$ hpc report --include=Haskeleton dist/hpc/tix/hspec/hspec.tix
100% expressions used (5/5)
100% boolean coverage (0/0)
     100% guards (0/0)
     100% 'if' conditions (0/0)
     100% qualifiers (0/0)
100% alternatives used (0/0)
100% local declarations used (0/0)
100% top-level declarations used (1/1)
$ hpc markup --destdir=tmp --include=Haskeleton dist/hpc/tix/hspec/hspec.tix
# => tmp/hpc_index.html
{% endhighlight %}

### Lint Code

-   one last test suite to write
-   enforce code conventions with hlint
-   <http://community.haskell.org/~ndm/hlint/>
-   hlint shouldn't be applied blindly
-   but we can disable it inline with annotations
-   so it makes for a good test

{% highlight haskell %}
-- tests/HLint.hs
module Main (main) where

import           Language.Haskell.HLint (hlint)
import           System.Exit            (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "--color"
    , "--hint=HLint.hs"
    , "benchmarks"
    , "executable"
    , "library"
    , "tests"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
{% endhighlight %}

-   hlint recursively walks the directories
-   so you don't have to modify this
-   let cabal know it exists

{% highlight haskell %}
test-suite hlint
    build-depends:
        base == 4.*
      , hlint == 1.8.*
    default-language:
        Haskell2010
    hs-source-dirs:
        tests
    main-is:
        HLint.hs
    type:
        exitcode-stdio-1.0
{% endhighlight %}

-   run it

{% highlight sh %}
$ cabal install --enable-tests --only-dependencies
$ cabal configure --enable-tests
$ cabal build
$ cabal test
Test suite hlint: RUNNING...
Test suite hlint: PASS
Test suite logged to: dist/test/haskeleton-0.0.0-hlint.log
{% endhighlight %}

-   you can configure this at the project level
-   make `HLint.hs` in the root directory

{% highlight haskell %}
-- HLint.hs
{-# LANGUAGE PackageImports #-}

module HLint () where

import           "hint" HLint.Default
import           "hint" HLint.Dollar
import           "hint" HLint.Generalise
{% endhighlight %}

## Continuous Integration

-   all of this testing and code quality is no good if we don't run them
-   travis ci makes continuous integration a cinch
-   just add a `.travis.yml` file
-   and hook up your github project
-   <http://docs.travis-ci.com/user/getting-started/>

{% highlight yaml %}
# .travis.yml
language: haskell
{% endhighlight %}

## Conclusion

-   so much more code than i expected
-   a lot to it
-   check out the whole source
-   <https://github.com/tfausak/haskeleton>
-   hopefully in the future this will be a project template
-   or rolled into `cabal init`

[exercism.io]: https://github.com/tfausak/exercism-solutions/tree/master/haskell
[h-99]: https://github.com/tfausak/h99
[project euler]: https://github.com/tfausak/project-euler/tree/master/haskell
[haskeleton]: https://github.com/tfausak/haskeleton
[setup]: #setup
[the haskell platform]: http://www.haskell.org/platform/
[package properties]: http://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties
[semantic versioning]: http://semver.org/
[build information]: http://www.haskell.org/cabal/users-guide/developing-packages.html#build-information
[`base`]: http://hackage.haskell.org/package/base
[`haskell2010`]: http://www.haskell.org/onlinereport/haskell2010/
[library]: #library
[executable]: #executable
[documentation]: #documentation
[hackage]: http://hackage.haskell.org/
[haddock]: http://www.haskell.org/haddock/
