---
title: 'Haskeleton: a Haskell project skeleton'
---

I'm new to Haskell. I've learned enough to feel comfortable writing programs in
it. I can solve code katas like [exercism.io][1], [H-99][2], and [Project
Euler][3]. Yet I don't feel comfortable developing software with it. Writing
idiomatic, maintainable and well-tested Haskell code remains a mystery to me.

Cabal, the Haskell build tool, provides little guidance. For instance, `cabal
init` asks 11 questions and outputs two files totaling 26 lines. That's not the
best metric, but it shows that you aren't getting much out of it. To both
improve on that and educate myself, I built [Haskeleton][4], a Haskell project
skeleton.

I hope it replaces `cabal init` someday. For the time being, it's just an
example project. This post will walk you through setting up a project like
Haskeleton and explain the decisions I made along the way.

<aside>Update: Haskeleton has been implemented as a template for <a href="https://github.com/fujimura/hi">hi</a>.</aside>

-   [Setup](#setup)
-   [Library](#library)
-   [Executable](#executable)
-   [Documentation](#documentation)
-   [Tests](#tests)
-   [Benchmarks](#benchmarks)
-   [Code Quality](#code-quality)
    -   [Documentation Tests](#documentation-tests)
    -   [Documentation Coverage](#documentation-coverage)
    -   [Test Coverage](#test-coverage)
    -   [Static Analysis](#static-analysis)
-   [Continuous Integration](#continuous-integration)
-   [Notes](#notes)

## Setup

There's no reason to make new software with old technology. To get started,
make sure you have GHC 7.6.3 and Cabal 1.18.0.2 installed. You can get GHC
through [The Haskell Platform][18] and the latest version of Cabal with `cabal
install cabal-install`.

Now for the hardest part: thinking of a name for your package. I went with
"husk". (If you're following along, replace that with your package's name
throughout.) Make a directory for your package to get started.

``` sh
# mkdir husk
# cd husk
```

You only need one file to make a package: a Cabal file. It describes the
package and tells Cabal how to build it. It starts off pretty simple.

``` hs
-- husk.cabal
name:          husk
version:       0.0.0
build-type:    Simple
cabal-version: >= 1.18

library
    default-language: Haskell2010
```

The syntax is mix between Haskell and YAML. The [package properties][19] at the
top describe the package as a whole. In the library section, the [build
information][20] declares how to build the library. Here's what they all mean.

-   `name`: The package's name. This should be unique on [Hackage][21].
-   `version`: The package's version number. I recommend using [semantic
    versioning][22].
-   `build-type`: The type of build. Cabal provides a setup script if this is
    set to `Simple`. For some reason the default is `Custom`.
-   `cabal-version`: Cabal's version number. Use the major and minor parts of
    the version of Cabal used to build the package.
-   `default-language`: The version of the Haskell language report. The current
    state of the art is [`Haskell2010`][23].

With all the boilerplate out of the way, we can build the package. Before we
do, let's create a sandbox. It sets up a private environment separate from the
rest of your system.

``` sh
# cabal sandbox init
Writing a default package environment file to
.../husk/cabal.sandbox.config
Creating a new sandbox at .../husk/.cabal-sandbox
```

Now let's install the package into the sandbox.

``` sh
# cabal install
Resolving dependencies...
Configuring husk-0.0.0...
Building husk-0.0.0...
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Installing library in .../husk-0.0.0
Registering husk-0.0.0...
Installed husk-0.0.0
```

Alright, it worked! Six lines of code made a valid Cabal package. It doesn't do
anything yet, though. Let's fix that.

## Library

Your library code shouldn't live at the top level. Create a `library` directory
for it. In there, make a file with the same name as your package. For this
example, it doesn't have to do anything interesting. We're going to make it
export a function that returns the unit value.

``` hs
-- library/Husk.hs
module Husk (husk) where

husk :: ()
husk = ()
```

Just writing the module isn't enough. You have to let Cabal know about it.

``` hs
-- husk.cabal
library
    exposed-modules: Husk
    hs-source-dirs:  library
    build-depends:   base
```

This adds some new build information to the library:

-   `exposed-modules`: List of modules exposed by the package.
-   `hs-source-dirs`: List of directories to search for source files in.
-   `build-depends`: A list of needed packages. Every project will depend on
    [`base`][24], which provides the Prelude.

Now that Cabal's in the loop, you can fire up a REPL for your package. The
modules exposed by the package are already available. You can play around with
the functions they export.

``` sh
# cabal repl
*Husk> :type Husk.husk
Husk.husk :: ()
*Husk> husk
()
```

Now we've got a Cabal package with a library, which is more than `cabal init`
provides. And we did it with less code!

## Executable

Let's provide an executable that uses the library. It shouldn't live at the top
level either, so create an `executable` directory for it. Unlike the library,
don't name this after your package. Just call it `Main.hs` instead.

``` hs
-- executable/Main.hs
module Main (main) where

import Husk (husk)

main :: IO ()
main = print husk
```

As before, Cabal needs to know about this. Create a new section at the bottom
of the Cabal file.

``` hs
-- husk.cabal
executable husk
    build-depends:    base, husk
    default-language: Haskell2010
    hs-source-dirs:   executable
    main-is:          Main.hs
```

The only new property is `main-is`. It points to the main entry point for the
executable. After adding that, you can run it!

``` sh
# cabal run
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Linking dist/build/husk/husk ...
()
```

That's all it takes to make an executable with Cabal.

## Documentation

Now that you've got a library and an executable, you should document them.
There are two things that need documentation: the package itself and the source
of the package.

Documenting the package requires adding a few more package properties to the
Cabal file. If you're not going to distribute your package on Hackage, you can
skip this step.

``` hs
-- husk.cabal
copyright: 2014 Taylor Fausak
license:   MIT
synopsis:  An example package.
```

To write documentation for the source, you'll need to learn [Haddock][25]. It's
a simple markup language for annotating Haskell source. Here's how the library
looks with comments:

``` hs
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
```

Now that it's documented, let's generate the HTML documentation.

``` sh
# cabal haddock
Running Haddock for husk-0.0.0...
Preprocessing library husk-0.0.0...
Haddock coverage:
 100% (  2 /  2) in 'Husk'
Documentation created: dist/doc/html/husk/index.html
```

The output is like what you'd see on Hackage. It's missing one thing: links to
the source. Adding those requires another dependency. It should be optional.
Not everyone who installs the package will generate its documentation.

``` hs
-- husk.cabal
flag documentation
    default: False

library
    if flag(documentation)
        build-depends: hscolour == 1.20.*
```

Enabling flags for Cabal commands is easy. Add either `-fdocumentation` or
`--flags=documentation`. Using that flag, let's regenerate the documentation.

``` sh
# cabal install --flags=documentation
# cabal haddock --hyperlink-source
Running Haddock for husk-0.0.0...
Running hscolour for husk-0.0.0...
Preprocessing library husk-0.0.0...
Haddock coverage:
 100% (  2 /  2) in 'Husk'
Documentation created: dist/doc/html/husk/index.html
```

Now it should have source links. If you get a bunch of warnings, you can ignore
them. Haddock is looking for the documentation for the standard library. If you
want to add it, install `haskell-platform-doc`.

## Tests

You can test Haskell code in two different ways:

Unit tests using HUnit. Use these to test the behavior of your code. For
example, you could test that `+` returns `3` when given `1` and `2`.

``` hs
TestCase (assertEqual "1 + 2 = 3" 3 (1 + 2))
```

Property tests using QuickCheck. Use these to test the properties of your code.
For example, you could test that `+` always returns an even number when given
even arguments.

``` hs
quickCheck (\ x y -> even ((2 * x) + (2 * y)))
```

We're going to use [HSpec][26] instead of those libraries. It has a nicer
syntax and a uniform interface for both. Create a `test-suite` folder for the
tests. In there, create `Spec.hs`, the top-level entry point. HSpec discovers
and runs your tests using the `hspec-discover` GHC preprocessor.

``` hs
-- test-suite/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

It assumes you laid your tests out in the format it expects, so create a spec
file for your library. We don't have much functionality to test, but we can
write a unit test and a property test for the `husk` function.

``` hs
-- test-suite/HuskSpec.hs
module HuskSpec (spec) where

import Husk (husk)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "husk" $ do
        it "returns the unit value" $ do
            husk `shouldBe` ()

        prop "equals the unit value" $
            \ x -> husk == x
```

With that done, the only piece left is updating the Cabal file. Add a new
section at the end for the test suite.

``` hs
-- husk.cabal
test-suite hspec
    build-depends:    base, husk, hspec == 1.8.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          Spec.hs
    type:             exitcode-stdio-1.0
```

The only new build information here is `type`. The Cabal documentation
recommends the non-existent `detailed-1.0` type. Ignore that and use
`exitcode-stdio-1.0`, which uses the exit code to signify success or failure.

After doing all that, you should be able to run the tests.

``` sh
# cabal install --enable-tests
# cabal test
Test suite hspec: RUNNING...
Test suite hspec: PASS
Test suite logged to: dist/test/husk-0.0.0-hspec.log
```

## Benchmarks

Now that we've got tests to ensure our code works, let's write some benchmarks
to make sure it's fast. We're going to use [Criterion][27], an exceptional
benchmarking library. It handles all the setup for you and lets you focus on
writing benchmarks.

So let's make a new directory, `benchmark`, and do just that.

``` hs
-- benchmark/HuskBench.hs
module HuskBench (benchmarks) where

import Criterion (Benchmark, bench, nf)
import Husk (husk)

benchmarks :: [Benchmark]
benchmarks =
    [ bench "husk" (nf (const husk) ())
    ]
```

The only tricky part of this is `nf`. It evaluates the result of calling the
function with the given value. This is necessary since Haskell is lazy. If you
didn't do it, only a part of the result might get evaluated. Then your
benchmark wouldn't be accurate.

Now that we have a benchmark, we need a runner. Unlike HSpec, Criterion doesn't
auto-discover benchmarks. We have to wire it up.

``` hs
-- benchmark/Bench.hs
module Main (main) where

import Criterion.Main (bgroup, defaultMain)
import qualified HuskBench

main :: IO ()
main = defaultMain
    [ bgroup "Husk" HuskBench.benchmarks
    ]
```

We need to add a new section to the Cabal file for the benchmarks.

``` hs
benchmark criterion
    build-depends:    base, husk, criterion == 0.6.*
    default-language: Haskell2010
    hs-source-dirs:   benchmark
    main-is:          Bench.hs
    type:             exitcode-stdio-1.0
```

With that in place, we can now run the benchmarks.

``` sh
# cabal install --enable-benchmarks
# cabal bench
Benchmark criterion: RUNNING...
benchmarking Husk/husk
mean: 12.15392 ns, lb 11.89230 ns, ub 12.49891 ns, ci 0.950
std dev: 1.529236 ns, lb 1.229199 ns, ub 1.884049 ns, ci 0.950
found 17 outliers among 100 samples (17.0%)
  6 (6.0%) high mild
  11 (11.0%) high severe
variance introduced by outliers: 86.253%
variance is severely inflated by outliers
Benchmark criterion: FINISH
```

## Code Quality

That covers the basics for making a library and an executable, along with tests
and benchmarks for them. Another important part of software projects is code
quality. This includes things like code coverage and linting. Focusing on
quality helps you make maintainable and idiomatic software.

### Documentation Tests

Before we get to all that, there's one more thing that needs testing. When we
wrote our documentation, we included some example code. We should test that
code to make sure it's correct. Incorrect examples in documentation are
frustrating.

Thanks to [`doctest`][28], testing documentation is a cinch. We just need to
write a new test suite.

``` hs
-- test-suite/DocTest.hs
module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest (doctest)

main :: IO ()
main = glob "library/**/*.hs" >>= doctest
```

This uses globbing to avoid listing all the source files. That means you
shouldn't ever have to change it.

Next, create a new section in the Cabal file.

``` hs
-- husk.cabal
test-suite doctest
    build-depends:    base, doctest == 0.9.*, Glob == 0.7.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          DocTest.hs
    type:             exitcode-stdio-1.0
```

You can now run it along with the other test suites.

``` sh
# cabal install --enable-tests
# cabal test
Test suite doctest: RUNNING...
Test suite doctest: PASS
Test suite logged to: dist/test/husk-0.0.0-doctest.log
```

### Documentation Coverage

After checking our documentation for correctness, we should make sure that we
documented everything. Unfortunately there's no turnkey solution for this.
Making one isn't too hard since Haddock outputs coverage information already.
We just need a script for running it and parsing the results. So create a new
test suite for that.

``` hs
-- test-suite/Haddock.hs
module Main (main) where

import Data.List (genericLength)
import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)
import Text.Regex (matchRegex, mkRegex)

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    output <- readProcess "cabal" ["haddock"] ""
    if average (match output) >= expected
        then exitSuccess
        else putStr output >> exitFailure

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
```

This is the most complex code so far. Matching regular expressions in Haskell
isn't as easy as in scripting languages like Perl. But this code does what we
want and the only part that might change is the `expected` value.

To make this a bona fide test suite, we need to tell Cabal.

``` hs
-- husk.cabal
test-suite haddock
    build-depends:    base, process == 1.1.*, regex-compat == 0.95.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          Haddock.hs
    type:             exitcode-stdio-1.0
```

Finally we can run it.

``` sh
# cabal install --enable-tests
# cabal test
Test suite haddock: RUNNING...
Test suite haddock: PASS
Test suite logged to: dist/test/husk-0.0.0-haddock.log
```

### Test Coverage

We know how much of our code we documented, but we don't know how much of it we
tested. Let's fix that by modifying our `hspec` test suite to use [HPC][29].

``` hs
-- husk.cabal
test-suite hspec
    ghc-options:    -fhpc
    hs-source-dirs: test-suite library
    other-modules:  Husk, HuskSpec
```

What we're doing here is telling GHC to enable HPC. We also have to add all the
source and test files to `other-modules` so HPC can analyze them. This is kind
of annoying, especially since HSpec discovers our tests. But it's not too bad
because if you forget a file, HPC will yell at you and your test will fail.

Before it can fail, though, we need to write it. This new test suite looks a
lot like the last one.

``` hs
-- test-suite/HPC.hs
module Main (main) where

import Data.List (genericLength)
import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)
import Text.Regex (matchRegex, mkRegex)

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    output <- readProcess "hpc" ["report", "dist/hpc/tix/hspec/hspec.tix"] ""
    if average (match output) >= expected
        then exitSuccess
        else putStr output >> exitFailure

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
```

Just like the last one, we have to add it to the Cabal file.

``` hs
-- husk.cabal
test-suite hpc
    build-depends:    base, process == 1.1.*, regex-compat == 0.95.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          HPC.hs
    type:             exitcode-stdio-1.0
```

The order of things in the Cabal file doesn't matter. But it runs the test
suites in order of appearance. Since this test uses the output of the HSpec
suite, make sure it comes after that one. If it doesn't, it'll either run with
old data or no data.

``` sh
# cabal install --enable-tests
# cabal test
Test suite hspec: RUNNING...
Test suite hspec: PASS
Test suite logged to: dist/test/husk-0.0.0-hspec.log
Warning: Your version of HPC (0.6) does not properly handle multiple search
paths. Coverage report generation may fail unexpectedly. These issues are
addressed in version 0.7 or later (GHC 7.8 or later). The following search
paths have been abandoned: ["dist/hpc/mix/husk-0.0.0"]
Writing: hpc_index.html
Writing: hpc_index_fun.html
Writing: hpc_index_alt.html
Writing: hpc_index_exp.html
Test coverage report written to dist/hpc/html/hspec/hpc_index.html
Test suite hpc: RUNNING...
Test suite hpc: PASS
Test suite logged to: dist/test/husk-0.0.0-hpc.log
```

You can ignore HPC's warning about search paths. Everything works fine in spite
of it.

### Static Analysis

You might think we've got enough tests, but there's still one last suite to
write. It's going to enforce code conventions with [HLint][30].

``` hs
-- test-suite/HLint.hs
module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "benchmark"
    , "executable"
    , "library"
    , "test-suite"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure
```

Thanks to HLint's excellent interface, there's nothing too interesting going on
here. Let's tell Cabal about it.

``` hs
-- husk.cabal
test-suite hlint
    build-depends:    base, hlint == 1.8.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          HLint.hs
    type:             exitcode-stdio-1.0
```

All that's left to do now is run it!

``` sh
# cabal install --enable-tests
# cabal test
Test suite hlint: RUNNING...
Test suite hlint: PASS
Test suite logged to: dist/test/husk-0.0.0-hlint.log
```

## Continuous Integration

We've got all these tests now. They don't do us any good if nobody ever runs
them. Since it's all too easy to forget to run the tests when you're
developing, let's make a computer do it!

[Travis CI][31] makes continuous integration a cinch. Assuming your code is on
GitHub, all you have to do is make one file and add one line to it.

``` yaml
# .travis.yml
language: haskell
```

Now every time you push to GitHub, Travis will run your tests. You'll get an
email if they aren't green.

## Notes

This turned out to be much bigger than I anticipated. And I had to leave some
stuff out! For more details, check out [Haskeleton][4]. Hopefully some day it
will make this post obsolete.

In the meantime, [email me][32] if you have any questions. I'm happy to help!

I used a number of invaluable resources while writing this post, including:

-   The [Haskell wiki][33]. It contains a staggering amount of useful
    information and links. Sometimes the information is out of date, but it's
    helpful nonetheless.
-   Sebastiaan Visser's post, ["Towards a better Haskell package"][34]. It
    provides reasoned arguments for a number of subjective points about package
    development.
-   Kazu Yamamoto's [unit testing guide][35]. It helped me wrap my head around
    HUnit, QuickCheck, HSpec, and DocTest.
-   [hi][36], a Haskell package generator by Fujimura Daisuke. Haskeleton could
    end up as a template for this excellent utility.

[1]: https://github.com/tfausak/exercism-solutions/tree/master/haskell
[2]: https://github.com/tfausak/h99
[3]: https://github.com/tfausak/project-euler/tree/master/haskell
[4]: https://github.com/tfausak/haskeleton
[18]: http://www.haskell.org/platform/
[19]: http://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties
[20]: http://www.haskell.org/cabal/users-guide/developing-packages.html#build-information
[21]: http://hackage.haskell.org
[22]: http://semver.org
[23]: http://www.haskell.org/onlinereport/haskell2010/
[24]: http://hackage.haskell.org/package/base
[25]: http://www.haskell.org/haddock/
[26]: http://hspec.github.io
[27]: http://hackage.haskell.org/package/criterion
[28]: http://hackage.haskell.org/package/doctest
[29]: http://www.haskell.org/haskellwiki/Haskell_program_coverage
[30]: http://community.haskell.org/~ndm/hlint/
[31]: https://travis-ci.org
[32]: mailto:taylor+honeypot@fausak.me
[33]: http://www.haskell.org/haskellwiki/Category:Haskell
[34]: http://fvisser.nl/post/2013/may/28/towards-a-better-haskell-package.html
[35]: https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md
[36]: https://github.com/fujimura/hi
