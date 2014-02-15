---
layout: post
title: 'Haskeleton: A Haskell Project Skeleton'
---

-   <https://github.com/tfausak/haskeleton>

-   [Environment](#environment)
-   [Setup](#setup)
-   [Library](#library)
-   [Executable](#executable)
-   [Documentation](#documentation)
-   [Testing](#testing)
-   [Benchmarks](#benchmarks)
-   [Code Quality](#code-quality)
    -   [Test Documentation](#test-documentation)
    -   [Check Documentation Coverage](#check-documentation-coverage)
    -   [Check Code Coverage](#check-code-coverage)
    -   [Lint Code](#lint-code)
-   [Continuous Integration](#continuous-integration)
-   [Conclusion](#conclusion)

## Environment

-   GHC 7.6.3
-   Cabal 1.18.0.2

## Setup

-   `cabal init`
-   doesn't really give you that much
-   spits out a cabal file (`*.cabal`)
-   and a setup file (`Setup.hs`)

-   the generated cabal file has a lot of stuff
-   most of it is unnecessary at this point

{% highlight haskell %}
-- haskeleton.cabal
name: haskeleton
version: 0.0.0
cabal-version: >= 1.18
build-type: Simple

library
    build-depends: base == 4.*
    default-language: Haskell2010
{% endhighlight %}

-   the default setup file is fine
-   but i like to be more explicit about things

{% highlight haskell %}
-- Setup.hs
module Setup (main) where

import           Distribution.Simple (defaultMain)

main :: IO ()
main = defaultMain
{% endhighlight %}

-   with those two files, you're ready to go
-   `cabal sandbox init`
-   `cabal install`

## Library

-   currently installs an empty library
-   we're going to add a file
-   it won't do much
-   but it shows all the necessary machinery

-   create a directory for your library
-   something like `source` or `src` or `library` or `lib`
-   then make a file in there with the same name as your project
-   put whatever you want in there

{% highlight haskell %}
-- source/Haskeleton.hs
module Haskeleton (haskeleton) where

haskeleton :: Int -> String
haskeleton = unwords . flip replicate "Haskeleton!"
{% endhighlight %}

-   now that we've got a library we need to tell cabal about it
-   two things are necessary
-   which modules do you want to expose
-   and where are they
-   so open up the cabal file and add to the library section

{% highlight haskell %}
-- haskeleton.cabal
library
    exposed-modules: Haskeleton
    hs-source-dirs: source
{% endhighlight %}

-   now you've got a library
-   install it (into the sandbox) with `cabal install`
-   then launch GHCi with `cabal repl`

{% highlight sh %}
$ cabal repl
λ haskeleton 3
"Haskeleton! Haskeleton! Haskeleton!"
{% endhighlight %}

## Executable

-   now we want to write an executable
-   it just collects arguments and passes them off
-   it should be as simple as possible

-   you have two choices here
-   throw it in the `source` directory
-   or create a new directory for the executable
-   something like `executable` or `exec` or `binary` or `bin`
-   the only file you need in there is `Main.hs`
-   it's *not* named after your package

{% highlight haskell %}
-- executable/Main.hs
module Main (main) where

import           Haskeleton         (haskeleton)
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    mapM_ (putStrLn . haskeleton . read) args
{% endhighlight %}

-   as before, now we need to let cabal know
-   this time we're adding a new top-level section

{% highlight haskell %}
executable haskeleton
    build-depends: base == 4.* , haskeleton
    default-language: Haskell2010
    main-is: Main.hs
    hs-source-dirs: executable
{% endhighlight %}

-   after that, you should be able to build and run it

{% highlight sh %}
$ cabal install
$ cabal configure
$ cabal build
$ cabal run haskeleton 3
Haskeleton! Haskeleton! Haskeleton!
{% endhighlight %}

## Documentation

-   now that you've got a library and an executable, you should document them
-   there are two things that need documentation
-   the cabal package itself
-   and the source code

-   to document the cabal package, edit the cabal file
-   check out the cabal user guide for instructions
-   <http://www.haskell.org/cabal/users-guide/developing-packages.html#package-properties>
-   if you aren't distributing the package, this probably isn't necessary

-   to document the source, you'll need to learn haddock
-   the online documentation is good
-   <http://www.haskell.org/haddock/doc/html/markup.html>
-   let's see what our source looks like documented

{% highlight haskell %}
-- | This is the top-level module documentation.
module Haskeleton (haskeleton) where

{- | This is the documentation for the 'haskeleton' function.

>>> haskeleton 3 -- This is some example code.
"Haskeleton! Haskeleton! Haskeleton!" -}
haskeleton :: Int -- ^ The number of times you want to haskeleton.
  -> String -- ^ This is the return value, which happens to be a 'String'.
haskeleton = unwords . flip replicate "Haskeleton!"
{% endhighlight %}

-   next up is building the documentation
-   haddock can automatically generate and link to the source
-   this requires an additional dependency
-   so let's add it
-   but not make everyone install it

{% highlight haskell %}
flag documentation
    default: False

library
    if flag(documentation)
        build-depends: hscolour == 1.20.*
{% endhighlight %}

-   enabling flags through cabal is easy
-   either `-fdocumentation` or `--flags=documentation`

-   it can also link to stdlib documentation
-   but you need to install it
-   usually this is something like `sudo apt-get install -y haskell-platform-doc`
-   not necessary
-   if you don't do it, you'll get some warnings
-   which you can ignore

-   with all that out of the way
-   now you can generate docs

{% highlight sh %}
$ cabal install --flags=documentation
$ cabal configure
$ cabal build
$ cabal haddock --hyperlink-source
{% endhighlight %}

-   go to them
-   `dist/doc/html/haskeleton/index.html`

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
