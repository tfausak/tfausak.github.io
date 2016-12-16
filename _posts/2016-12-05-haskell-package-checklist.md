---
title: Haskell package checklist
---

This post covers everything you need to know about how to develop a Haskell package.
I decided to create it because I have made a few packages and nothing covers the entire process.
The [Cabal user guide](https://www.haskell.org/cabal/users-guide/developing-packages.html) provides good low-level information,
and Sebastiaan Visser's [Towards a better Haskell package](http://fvisser.nl/post/2013/may/28/towards-a-better-haskell-package.html) gives some nice high-level guidance.
This post covers both of those and then some.

- **Use Git for source control.**
  If it's not in source control, it doesn't exist.
  Git is the most popular choice,
  but its interface can be difficult to understand.
  Consider using a GUI like GitHub Desktop.

- **Host on GitHub.**
  GitHub allows other developers to easily contribute to your package.
  Compared to other hosts,
  you are more likely to receive contributions.
  Plus it integrates nicely with many other services.

- **Build with Stack.**
  Stack painlessly manages Haskell dependencies.
  It manages GHC installations
  and ensures you get a build plan that actually works.
  Plus it avoids bit rot by making sure your package will build tomorrow if it builds today.

- **Define with hpack.**
  The default Cabal package file format is custom, tedious, and verbose.
  hpack uses YAML and avoids unnecessary boilerplate.
  Stack integrates hpack to automatically convert your `package.yaml` into a `*.cabal` file when necessary.

- **Name with kebab-case.**
  Keep everything lowercase to avoid confusion.
  You don't want people trying to install `http` when they really meant `HTTP`.
  Use hyphens to separate words, but keep it short and memorable.
  Nobody wants to type out `hypertext-transfer-protocol`.

- **Use Semantic Versioning.**
  Unfortunately Hackage recommends the Package Versioning Policy.
  The PVP adds ambiguity by using two major version numbers.
  It also encourages packages to stay on major version 0, which looks bad.
  Many other languages use Semantic Versioning.
  SemVer matches how developers generally think about version numbers.

- **License your package.**
  Nobody can use a package without a license.
  The most popular license for Haskell is BSD 3-Clause, followed by MIT.
  Whichever license you choose, include the license file in your package (like `LICENSE.markdown`).

- **Write a README.**
  Most people will familiarize themselves with your package by reading your README.
  It should describe the problem that your package solves.
  Be sure to include at least one concrete example in it.
  And make it look nice by using Markdown (like `README.markdown`).

- **Keep a change log.**
  Most packages will use Git tags to mark releases.
  However reading diffs is not an acceptable way for users to discover changes.
  Put a human-readable summary of changes in the GitHub releases.
  Link to that from a CHANGELOG file (like `CHANGELOG.markdown`).

- **Write a `synopsis`.**
  This shows up when searching and viewing your package.
  Keep it short, imperative, and descriptive.
  Also write a `description`,
  which can be pretty much the same as the `synopsis`.
  For example:

      name:        aeson
      synopsis:    Encode and decode JSON.
      description: Aeson encodes and decodes JSON.

- **Avoid heavy dependencies.**
  Only add a dependency if your package needs it to function.
  Don't include stuff that's just nice to have.
  For example, you should probably avoid `lens` even though it makes code easier to write.
  In addition to avoiding heavy dependencies,
  you should avoid having too many dependencies.
  Think about how long it would take to install your package starting from scratch.

- **Include `extra-source-files`.**
  If a file is necessary for your package to build, it belongs in `extra-source-files`.
  This includes files that tests and benchmarks need.
  You should also include package metadata like your README and change log.
  Note that you don't need to include your license file if your package's `license-file` is set.

- **Fix package warnings.**
  Stack prints these out when you run `stack sdist`.
  You should fix all of them, even though some aren't that useful.
  For instance, Stack warns you if you don't set a category even though the categories on Hackage aren't that useful.
  To fix warnings about version bounds, consider using `--pvp-bounds=both`.

- **Put Haskell files in `source/`.**
  In other words, separate your package metadata from actual source files.
  This makes it easy to write scripts that work on every Haskell file, like formatting or counting lines of code.
  The exact names aren't important, but you should end up with a structure like this:

  - `source/library/YourPackage.hs`
  - `source/executables/Main.hs`
  - `source/tests/Main.hs`

- **Match package and module names.**
  If your package is named `tasty-burrito`, you should have a top-level module called `TastyBurrito`.
  Avoid the unnecessary module hierarchy like `Data.ByteString` or `Text.ParserCombinators.Parsec`.
  While the package name uses `kebab-case`, module names should use `CamelCase`.

- **Require one import.**
  Users should be able to get started with nothing more than `import YourPackage`.
  If necessary, re-export stuff from other packages.
  Design for qualified imports;
  don't be afraid to take common names like `singleton`.

- **Expose implementation details.**
  People will want to use your package in ways you didn't think of.
  Make everything public, but not necessarily part of your published API.
  Use `Internal` module names like `Data.Text.Internal` to signal that things aren't published.

- **Build `-Wall` clean.**
  GHC finds all kinds of problems with `-Wall`.
  Few of them are false positives and they generally help you write better code.
  But if you don't like one, disable it with `-fno-warn-whatever`.
  Be sure to use `stack build --pedantic` when developing your package.
  It will force you to fix warnings.

- **Follow most HLint suggestions.**
  Overall HLint is a great tool for improving code quality.
  However some suggestions aren't worth following.
  For example, the re-export shortcut suggestion breaks the Haddock documentation.
  Use your own judgment when deciding which suggestions to follow.

- **Format code with hindent.**
  hindent is the closest thing we have to a community style.
  Using it frees you from ever thinking about formatting again.
  If you don't like how something looks, fix it in hindent and everyone's formatting will improve.
  Using hindent also avoids pointless arguments about style in pull requests.

- **Write documentation with examples.**
  Types are not a substitute for documentation.
  Neither are laws.
  Usually functions are added to solve a specific problem.
  Show that problem in the documentation as an example.

- **Test with Tasty using Hspec.**
  Tasty provides a framework for running different kinds of tests with the same command line interface.
  It handles randomizing the test order, selecting which tests to run, and displaying their output.
  Hspec provides a library for writing human-readable tests.
  Use other testing libraries like QuickCheck when they make sense.

- **Run tests on Travis CI.**
  Travis CI is free for open source projects and integrates with GitHub.
  Every time you push a commit to GitHub, Travis CI will run your test suite.
  This makes it easy to keep your package buildable.
  By default Travis CI runs on Linux, but you can also run on macOS.
  If you want to run your test suite on Windows, consider using AppVeyor.

- **Keep executables small.**
  If your package provides an executable,
  define it in your library and re-export it.
  This allows other packages to use your executable's behavior from Haskell.
  Your executable should look like this:

      module Main (module YourPackage) where
      import YourPackage (main)

- **Benchmark with Criterion.**
  If your package needs to be fast, Criterion is the best tool for measuring it.
  On the other hand if your package doesn't need to be fast, there's no sense in maintaining benchmarks for it.

- **Automate releases.**
  Don't manually create distribution tarballs and upload them to Hackage.
  Instead, get Travis CI to do it.
  Travis CI sets the `TRAVIS_TAG` environment variable.
  If that's set, you can run `stack upload .` to upload your package.
  Travis CI will need your Hackage credentials, so be sure not to leak those into the build log.

If you're looking for a starting point that ticks most of these boxes,
consider my [Haskeleton]({% post_url 2014-03-04-haskeleton-a-haskell-project-skeleton %}) Stack template.
It will give you a good base to start from.
If you're looking for an actual package that follows the guidelines,
check out [Rattletrap]({% post_url 2016-11-15-parse-and-generate-rocket-league-replays-with-haskell %}), my Rocket League replay parser and generator.
It can show you exactly how some of these things are implemented.
