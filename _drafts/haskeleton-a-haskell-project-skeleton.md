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
-   [Testing][]
-   [Benchmarks][]
-   [Code Quality]
    -   [Test Documentation][]
    -   [Check Documentation Coverage][]
    -   [Check Code Coverage][]
    -   [Lint Code][]
-   [Continuous Integration][]
-   [Notes][]

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

You can write two different kinds of tests in Haskell.
Unit tests, provided by HUnit, behave like in every other lanuage.
You can use them to test that one plus two is three, for example.
QuickCheck gives another kind of test: property tests.
They check things like "the sum of even numbers is even".

Instead of using those libraries directly,
we're going to use [HSpec][].
It's got a nicer syntax and a uniform interface for both libraries.

Create a new folder, `test-suite`, for the tests.
We don't have much functionality to test,
but we can write a unit test and a property test for the `husk` function.

{% highlight hs %}
-- test-suite/HuskSpec.hs
module HuskSpec (main, spec) where

import Husk (husk)
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "husk" $ do
        it "returns the unit value" $ do
            husk `shouldBe` ()

        prop "equals the unit value" $
            \ x -> husk == x
{% endhighlight %}

The `main` function is only there so the test can be run by itself.
You'll probably never do that because HSpec can automatically discover and run your tests.
All you need is a top-level entry point with the `hspec-discover` GHC preprocessor.

{% highlight hs %}
-- test-suite/Spec.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
{% endhighlight %}

With that done, the only piece left is updating the Cabal file.
Add a new section at the end for the test suite.

{% highlight hs %}
-- husk.cabal
test-suite hspec
    build-depends:    base, husk, hspec == 1.8.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          Spec.hs
    type:             exitcode-stdio-1.0
{% endhighlight %}

The only new [build information][] here is `type`.
The Cabal documentation erroneously recommends the non-existent `detailed-1.0` type.
Ignore that and use the `exitcode-stdio-1.0` type,
which uses the exit code to signify success or failure.

After doing all that, you should be able to run the tests.

{% highlight sh %}
# cabal install --enable-tests
# cabal test
Building husk-0.0.0...
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Linking dist/build/husk/husk ...
Preprocessing test suite 'hspec' for husk-0.0.0...
Linking dist/build/hspec/hspec ...
Running 1 test suites...
Test suite hspec: RUNNING...
Test suite hspec: PASS
Test suite logged to: dist/test/husk-0.0.0-hspec.log
1 of 1 test suites (1 of 1 test cases) passed.
{% endhighlight %}

## Benchmarks

Now that we've got tests to ensure our code works,
let's write some benchmarks to make sure it's fast.
We're going to use [Criterion][], an exceptional benchmarking library.
It handles all the annoying setup for you
and lets you focus on writing benchmarks.

So let's make a new directory, `benchmark`, and do just that.

{% highlight hs %}
-- benchmark/HuskBench.hs
module HuskBench (benchmarks) where

import Criterion
import Husk (husk)

benchmarks :: [Benchmark]
benchmarks =
    [ bench "husk" $ nf (\ _ -> husk) ()
    ]
{% endhighlight %}

The only tricky part of this is `nf`.
It fully evaluates the result of calling the function with the given value.
This is necessary since Haskell is lazy.
If you didn't do it, only a part of the result might get evaluated.
Then your benchmark wouldn't be accurate.

Now that we have a benchmark, we need a runner.
Unlike HSpec, Criterion doesn't auto-discover benchmarks.
We have to manually set it up.

{% highlight hs %}
-- benchmark/Bench.hs
module Main (main) where

import Criterion.Main
import qualified HuskBench

main :: IO ()
main = defaultMain
    [ bgroup "Husk" HuskBench.benchmarks
    ]
{% endhighlight %}

We need to add a new section to the Cabal file for the benchmarks.

{% highlight hs %}
benchmark criterion
    build-depends:    base, husk, criterion == 0.8.*
    default-language: Haskell2010
    hs-source-dirs:   benchmark
    main-is:          Bench.hs
    type:             exitcode-stdio-1.0
{% endhighlight %}

With that in place, we can now run the benchmarks.

{% highlight sh %}
# cabal install --enable-benchmarks
# cabal bench
Building husk-0.0.0...
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Linking dist/build/husk/husk ...
Preprocessing benchmark 'criterion' for husk-0.0.0...
Linking dist/build/criterion/criterion ...
Running 1 benchmarks...
Benchmark criterion: RUNNING...
warming up
estimating clock resolution...
mean is 2.904699 us (320001 iterations)
found 5207 outliers among 319999 samples (1.6%)
  2992 (0.9%) high severe
estimating cost of a clock call...
mean is 1.874305 us (17 iterations)
found 2 outliers among 17 samples (11.8%)
  2 (11.8%) high mild

benchmarking Husk/husk
mean: 12.31838 ns, lb 11.96851 ns, ub 12.88492 ns, ci 0.950
std dev: 2.235611 ns, lb 1.542356 ns, ub 3.186614 ns, ci 0.950
found 15 outliers among 100 samples (15.0%)
  5 (5.0%) high mild
  10 (10.0%) high severe
variance introduced by outliers: 92.590%
variance is severely inflated by outliers
Benchmark criterion: FINISH
{% endhighlight %}

## Code Quality

That covers the basics for making a library and an executable,
along with tests and benchmarks for them.
Another important part of software projects is code quality.
This includes things like code coverage and linting.
Focusing on quality helps you make maintainable, idiomatic software.

### Test Documentation

Before we get to all that,
there's one more thing that needs testing.
When we wrote our documentation,
we included some example code.
We should test that code to make sure it's correct.
Incorrect examples in documentation is frustrating.

Thanks to [`doctest`][], testing documentation is a cinch.
We just need to write a new test suite.

{% highlight hs %}
-- test-suite/DocTest.hs
module Main (main) where

import System.FilePath.Glob
import Test.DocTest

main :: IO ()
main = glob "library/**/*.hs" >>= doctest
{% endhighlight %}

This uses globbing to avoid listing all the source files.
That means you shouldn't ever have to modify it.

Next, create a new section in the Cabal file.

{% highlight hs %}
-- husk.cabal
test-suite doctest
    build-depends:    base, doctest == 0.9.*, Glob == 0.7.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          DocTest.hs
    type:             exitcode-stdio-1.0
{% endhighlight %}

You can now run it along with the other test suites.

{% highlight sh %}
# cabal install --enable-tests
# cabal test
Building husk-0.0.0...
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Linking dist/build/husk/husk ...
Preprocessing test suite 'hspec' for husk-0.0.0...
Linking dist/build/hspec/hspec ...
Preprocessing test suite 'doctest' for husk-0.0.0...
Running 2 test suites...
Test suite hspec: RUNNING...
Test suite hspec: PASS
Test suite logged to: dist/test/husk-0.0.0-hspec.log
Test suite doctest: RUNNING...
Test suite doctest: PASS
Test suite logged to: dist/test/husk-0.0.0-doctest.log
2 of 2 test suites (2 of 2 test cases) passed.
{% endhighlight %}

Sweet!
Now we know the examples in our documentation are correct.

### Check Documentation Coverage

In addition to checking our documentation for correctness,
we should make sure that everything is documented.
Unfortunately there's no turnkey solution for this.
Making one isn't too hard since Haddock outputs coverage information already.
We just need a script for running it and parsing the results.
So create a new test suite for that.

{% highlight hs %}
-- test-suite/Haddock.hs
module Main (main) where

import Data.List (genericLength)
import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)
import Text.Regex (matchRegex, mkRegex)

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    output <- readProcess "cabal" ["haddock"] ""
    if average (match output) >= expected
        then exitSuccess
        else putStr output >> exitFailure

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
{% endhighlight %}

This is the most complex code so far.
Matching regular expressions in Haskell is annoying.
But it does what we want
and the only part that might change is the `expected` value.

To make this a bona fide test suite, we need to tell Cabal.

{% highlight hs %}
-- husk.cabal
test-suite haddock
    build-depends:    base, process == 1.1.*, regex-compat == 0.95.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          Haddock.hs
    type:             exitcode-stdio-1.0
{% endhighlight %}

Finally we can run it.

{% highlight sh %}
# cabal install --enable-tests
# cabal test
Building husk-0.0.0...
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Linking dist/build/husk/husk ...
Preprocessing test suite 'hspec' for husk-0.0.0...
Linking dist/build/hspec/hspec ...
Preprocessing test suite 'doctest' for husk-0.0.0...
Preprocessing test suite 'haddock' for husk-0.0.0...
Preprocessing benchmark 'criterion' for husk-0.0.0...
Linking dist/build/criterion/criterion ...
Running 3 test suites...
Test suite hspec: RUNNING...
Test suite hspec: PASS
Test suite logged to: dist/test/husk-0.0.0-hspec.log
Test suite doctest: RUNNING...
Test suite doctest: PASS
Test suite logged to: dist/test/husk-0.0.0-doctest.log
Test suite haddock: RUNNING...
Test suite haddock: PASS
Test suite logged to: dist/test/husk-0.0.0-haddock.log
3 of 3 test suites (3 of 3 test cases) passed.
{% endhighlight %}

### Check Code Coverage

We know how much of our code is documented,
but we don't know how much of it is tested.
Let's fix that by modifying our `hspec` test suite to use [HPC][].

{% highlight hs %}
-- husk.cabal
test-suite hspec
    hs-source-dirs: library test-suite
    ghc-options:    -fhpc
    other-modules:  Husk, HuskSpec
    -- ...
{% endhighlight %}

What we're doing here is telling GHC to enable HPC.
We also have to add all the source and test files to `other-modules` so HPC can analyze them.
This is kind of annoying, especially since HSpec automatically discovers our tests.
But it's not too bad because if you forget a file, HPC will yell at you and your test will fail.

Before it can fail, though, we actually need to write it.
This new test suite looks a lot like the last one.

{% highlight hs %}
-- test-suite/HPC.hs
module Main (main) where

import Data.List (genericLength)
import Data.Maybe (catMaybes)
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)
import Text.Regex (matchRegex, mkRegex)

expected :: Fractional a => a
expected = 90

main :: IO ()
main = do
    output <- readProcess "cabal" ["report", "dist/hpc/tix/hspec/hspec.tix"] ""
    if average (match output) >= expected
        then exitSuccess
        else putStr output >> exitFailure

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
{% endhighlight %}

Just like the last one, we have to add it to the Cabal file.

{% highlight hs %}
-- husk.cabal
test-suite hpc
    build-depends:    base, process == 1.1.*, regex-compat == 0.95.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          HPC.hs
    type:             exitcode-stdio-1.0
{% endhighlight %}

Typically the order of things in the Cabal file doesn't matter.
However, it runs the test suites in order of appearance.
Since this test uses the output of the HSpec suite,
make sure it comes after that one.
If it doesn't, it'll either be run with old data or no data.

{% highlight sh %}
# cabal install --enable-tests
# cabal test
Building husk-0.0.0...
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Linking dist/build/husk/husk ...
Preprocessing test suite 'hspec' for husk-0.0.0...
Preprocessing test suite 'doctest' for husk-0.0.0...
Preprocessing test suite 'haddock' for husk-0.0.0...
Preprocessing test suite 'hpc' for husk-0.0.0...
Preprocessing benchmark 'criterion' for husk-0.0.0...
Linking dist/build/criterion/criterion ...
Running 4 test suites...
Test suite hspec: RUNNING...
Finished in 0.0279 seconds
2 examples, 0 failures
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
Test suite doctest: RUNNING...
Examples: 1  Tried: 1  Errors: 0  Failures: 0
Test suite doctest: PASS
Test suite logged to: dist/test/husk-0.0.0-doctest.log
Test suite haddock: RUNNING...
Test suite haddock: PASS
Test suite logged to: dist/test/husk-0.0.0-haddock.log
Test suite hpc: RUNNING...
Test suite hpc: PASS
Test suite logged to: dist/test/husk-0.0.0-hpc.log
4 of 4 test suites (4 of 4 test cases) passed.
{% endhighlight %}

You can ignore HPC's warning about search paths.
Everything works fine in spite of it.

### Lint Code

You might think we've got enough tests,
but there's still one last suite to write.
It's going to enforce code conventions with [HLint][].

{% highlight hs %}
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
{% endhighlight %}

Thanks to HLint's excellent inteface,
there's nothing too interesting going on here.
Let's tell Cabal about it.

{% highlight hs %}
-- husk.cabal
test-suite hlint
    build-depends:    base, hlint == 1.8.*
    default-language: Haskell2010
    hs-source-dirs:   test-suite
    main-is:          HLint.hs
    type:             exitcode-stdio-1.0
{% endhighlight %}

All that's left to do now is run it!

{% highlight sh %}
# cabal install --enable-tests
# cabal test
Building husk-0.0.0...
Preprocessing library husk-0.0.0...
In-place registering husk-0.0.0...
Preprocessing executable 'husk' for husk-0.0.0...
Linking dist/build/husk/husk ...
Preprocessing test suite 'hspec' for husk-0.0.0...
Preprocessing test suite 'doctest' for husk-0.0.0...
Preprocessing test suite 'haddock' for husk-0.0.0...
Preprocessing test suite 'hpc' for husk-0.0.0...
Preprocessing test suite 'hlint' for husk-0.0.0...
Preprocessing benchmark 'criterion' for husk-0.0.0...
Linking dist/build/criterion/criterion ...
Running 5 test suites...
Test suite hspec: RUNNING...
Finished in 0.0070 seconds
2 examples, 0 failures
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
Test suite doctest: RUNNING...
Examples: 1  Tried: 1  Errors: 0  Failures: 0
Test suite doctest: PASS
Test suite logged to: dist/test/husk-0.0.0-doctest.log
Test suite haddock: RUNNING...
Test suite haddock: PASS
Test suite logged to: dist/test/husk-0.0.0-haddock.log
Test suite hpc: RUNNING...
Test suite hpc: PASS
Test suite logged to: dist/test/husk-0.0.0-hpc.log
Test suite hlint: RUNNING...
No suggestions (2 ignored)
Test suite hlint: PASS
Test suite logged to: dist/test/husk-0.0.0-hlint.log
5 of 5 test suites (5 of 5 test cases) passed.
{% endhighlight %}

## Continuous Integration

We've got all these tests now.
They don't do us any good if nobody ever runs them.
Since it's all too easy to forget to run the tests when you're developing,
let's make a computer do it!

[Travis CI][] makes continuous integration a cinch.
Assuming your code is on GitHub,
all you have to do is make one file and add one line to it.

{% highlight yml %}
# .travis.yml
language: haskell
{% endhighlight %}

Now every time you push to GitHub, Travis will run your tests.
You'll get an email if they aren't green.

## Notes

This turned out to be much bigger than I anticipated.
This post is easily three time as long as any other post I've written.
And I had to leave some stuff out!
For more details, check out [Haskeleton][].
Hopefully some day it will make this post obsolete.

In the meantime,
[email me][] if you have any questions.
I'm happy to help!

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
[testing]: #testing
[hspec]: http://hspec.github.io/
[benchmarks]: #benchmarks
[criterion]: http://hackage.haskell.org/package/criterion
[code quality]: #code-quality
[test documentation]: #test-documentation
[`doctest`]: http://hackage.haskell.org/package/doctest
[check documentation coverage]: #check-documentation-coverage
[check code coverage]: #check-code-coverage
[hpc]: http://www.haskell.org/haskellwiki/Haskell_program_coverage
[lint code]: #lint-code
[hlint]: http://community.haskell.org/~ndm/hlint/
[travis ci]: https://travis-ci.org/
[continuous-integration]: #continuous-integration
[notes]: #notes
[email me]: mailto:taylor+honeypot@fausak.me
