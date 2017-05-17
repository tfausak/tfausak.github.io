---
title: Testing GHC release candidates with Stack
---

Yesterday Ben Gamari [announced the second GHC 8.2.1 release candidate][] on
behalf of the GHC team. Release candidates allow the community to test new
versions before they're officially released. Using the new compiler or
interpreter directly is easy enough, but what about building an entire project?
How can a Haskell developer test their code with the latest release candidate?
I'm going to show you how to use Stack to do just that.

## Compiler version

Assuming your project already uses Stack, your `stack.yaml` probably looks
something like this:

``` yaml
resolver: lts-8.14
```

You may be using a different resolver or have some other stuff in there. That's
fine! We're going to update your `stack.yaml` to include information about the
release candidate. For starters we'll tell it to use the GHC 8.2.1-rc2
compiler.

``` yaml
# Add this to your project's existing stack.yaml file.
compiler: ghc-8.2.0.20170507
compiler-check: match-exact
```

Don't be fooled by the compiler version! `ghc-8.2.0.20170507` is actually GHC
8.2.1-rc2. Note that we tell Stack to match the compiler version exactly. If we
didn't, it would try to use the previous release candidate if you had it
installed.

## Setup information

At this point you won't be able to build you project. Let's see what happens
when you try.

```
$ stack test
No compiler found, expected exact version ghc-8.2.0.20170507 (x86_64) (based on resolver setting in .../stack.yaml).
To install the correct GHC into .../x86_64-linux/, try running "stack setup" or use the "--install-ghc" flag.

$ stack setup
No information found for ghc-8.2.0.20170507.
Supported versions for OS key 'linux64': GhcVersion 7.8.4, GhcVersion 7.10.1, GhcVersion 7.10.2, GhcVersion 7.10.3, GhcVersion 8.0.1, GhcVersion 8.0.2
```

Stack is telling us that we don't have the correct compiler installed.
Furthermore it doesn't know how to install the version we want. That's okay! We
can tell Stack where to find the compiler we want.

``` yaml
# Add this to your project's existing stack.yaml file.
setup-info:
 ghc:
  linux64:
   8.2.0.20170507:
    url: https://downloads.haskell.org/~ghc/8.2.1-rc2/ghc-8.2.0.20170507-x86_64-deb8-linux.tar.xz
  macosx:
   8.2.0.20170507:
    url: https://downloads.haskell.org/~ghc/8.2.1-rc2/ghc-8.2.0.20170507-x86_64-apple-darwin.tar.xz
  windows64:
   8.2.0.20170507:
    url: https://downloads.haskell.org/~ghc/8.2.1-rc2/ghc-8.2.0.20170507-x86_64-unknown-mingw32.tar.xz
```

## Installation

Now that we've told Stack where to find the compiler we can run `stack setup`
to download and install it.

```
$ stack setup
Preparing to install GHC to an isolated location.
This will not interfere with any system-level installation.
No sha1 found in metadata, download hash won't be checked.
Downloaded ghc-8.2.0.20170507.               
Unpacking GHC into .../ghc-8.2.0.20170507.temp/
Installed GHC.
stack will use a sandboxed GHC it installed
For more information on paths, see 'stack path' and 'stack exec env'
To use this GHC and packages outside of a project, consider using:
stack ghc, stack ghci, stack runghc, or stack exec
```

Let's make sure everything worked by running `ghc` and `ghci` through Stack.

```
$ stack exec -- ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.2.0.20170507

$ stack exec ghci
GHCi, version 8.2.0.20170507: http://www.haskell.org/ghc/  :? for help
Prelude> :quit
Leaving GHCi.
```

Great! The new compiler works.

## Resolvers

Before we move on, a brief note about resolvers. We're telling Stack to use the
latest compiler along with a resolver meant for the previous compiler. This is
convenient because most packages will build against the latest compiler without
changing anything, but it is a little sloppy. We can tell Stack to only use
built-in packages by setting `resolver: ghc-8.2.0.20170507`. Then we can add
other packages using `stack solver`. However I won't be walking through how to
do that because it's pretty tedious.

## Version constraints

If we try to build our project we'll probably see something like this.

```
$ stack build
Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for array-0.5.1.1:
    base-4.10.0.0 must match >=4.5 && <4.10 (latest applicable is 4.9.1.0)
needed due to rattletrap-2.2.4 -> array-0.5.1.1

Plan construction failed.
```

The exact message will differ based on your project's dependencies, but the
idea is the same. The new version of the compiler ships with a new version of
the `base` package. Many other packages claim they won't work with that.
Fortunately we can tell these packages to ignore their version constraints and
try to build against the new version anyway. Simply add this to your
`stack.yaml`.

``` yaml
# Add this to your project's existing stack.yaml file.
allow-newer: true
```

Now when you try to build your project you'll see some warnings.

```
$ stack build
WARNING: Ignoring out of range dependency (allow-newer enabled): base-4.10.0.0. array requires: >=4.5 && <4.10
...
```

But hopefully it will start downloading, configuring, and building some
dependencies.

## Troubleshooting

It's pretty much guaranteed that you'll try to build the `deepseq` package.
Unfortunately it won't build with this release candidate out of the box. You'll
see this error.

```
.../deepseq-1.4.2.0/Control/DeepSeq.hs:420:10: error:
    • Illegal instance declaration for ‘NFData TypeRep’
        (All instance types must be of the form (T t1 ... tn)
         where T is not a synonym.
         Use TypeSynonymInstances if you want to disable this.)
    • In the instance declaration for ‘NFData TypeRep’
    |
420 | instance NFData TypeRep where
    |          ^^^^^^^^^^^^^^
```

You may be thinking that we're stuck, but we actually have two ways out of
this. The first way is to do what GHC suggests and tell `deepseq` to enable the
`TypeSynonymInstances` language extension. We can do this by adding the
following to our `stack.yaml`.

``` yaml
# Add this to your project's existing stack.yaml file,
# but prefer the next approach.
ghc-options:
 deepseq: -XTypeSynonymInstances
```

This enables the language extension for the entire package, so it's a pretty
heavy hammer. If we had to do this with an extension that could change program
semantics, like `OverloadedStrings`, then this probably wouldn't work because
it would introduce other build failures.

That brings us to our second way out of this: pulling the dependency from Git.
This problem has already been fixed in `deepseq` but a new version hasn't been
released. Fortunately we can tell Stack to grab it from GitHub.

``` yaml
# Add this to your project's existing stack.yaml file.
packages:
 - .
 - extra-dep: true
   location:
    git: https://github.com/haskell/deepseq
    commit: 0b22c9825ef79c1ee41d2f19e7c997f5cdc93494
```

You should prefer this solution because it's generally more robust and can
solve any problem, not just ones that can be fixed by GHC options.

Remember that you can always fork a package on GitHub to add support for a
release candidate and point Stack to your fork. In fact, this is a great way to
contribute to the Haskell ecosystem. Once your fork works with the release
candidate, open a pull request against the original repository. Then the next
person won't have to fix it themselves!

## Conclusion

With that, you should be able to test your Haskell project against GHC
8.2.1-rc2 using Stack. If your project has a lot of dependencies, you may need
to add a lot of `extra-dep`s to get it building. If you run into any problems
with the release candidate, be sure to [report a bug][]!

[announced the second GHC 8.2.1 release candidate]: https://mail.haskell.org/pipermail/ghc-devs/2017-May/014197.html
[This Gist]: https://gist.github.com/tfausak/534b2b7a5f2b412ad688b627f841529f/b95aaff47185612803c144132e46a9192f945f86
[report a bug]: https://ghc.haskell.org/trac/ghc/wiki/ReportABug
