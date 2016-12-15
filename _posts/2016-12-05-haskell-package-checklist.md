---
title: Haskell package checklist
---

- **Use Git for source control.**
  If it's not in source control, it doesn't exist.
  Git is the most popular choice,
  but its interface can be difficult to understand.
  Consider using a GUI like GitHub Desktop.

- **Host your package on GitHub.**
  GitHub allows other developers to easily contribute to your package.
  Compared to other hosts,
  you are more likely to receive contributions.
  Plus it integrates nicely with many other services.

- **Build your package with Stack.**
  Stack painlessly manages Haskell dependencies.
  It manages GHC installations
  and ensures you get a build plan that actually works.
  Plus it avoids bit rot by making sure your package will build tomorrow if it builds today.

- **Define your package with hpack.**
  The default Cabal package file format is custom, tedious, and verbose.
  hpack uses YAML and avoids unnecessary boilerplate.
  Stack integrates hpack to automatically convert your `package.yaml` into a `*.cabal` file when necessary.

- **Name your package with kebab-case.**
  Keep everything lowercase to avoid confusion.
  You don't want people trying to install `http` when they really meant `HTTP`.
  Use hyphens to separate words, but keep it short and memorable.
  Nobody wants to type out `hypertext-transfer-protocol`.

- **Version your package with Semantic Versioning.**
  Unfortunately Hackage recommends the Package Versioning Policy.
  The PVP adds ambiguity by using two major version numbers.
  It also encourages packages to stay on major version 0, which looks bad.
  Many other languages use Semantic Versioning.
  SemVer matches how developers generally think about version numbers.

- **Choose a license for your package.**
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

      name:
        aeson
      synopsis:
        encode and decode JSON
      description:
        Aeson encodes and decodes JSON.

- **Avoid heavy dependencies.**
  Only add a dependency if your package needs it to function.
  Don't include stuff that's just nice to have.
  For example, you should probably avoid `lens` even though it makes easier to write.
  In addition to avoiding heavy dependencies,
  you should avoid having too many dependencies.
  Think about how long it would take to install your package starting from scratch.

- **Include `extra-source-files`.**
  If a file is necessary for your package to build, it belongs in `extra-source-files`.
  This includes files that tests and benchmarks need.
  You should also include package metadata like your README and change log.

- Avoid package warnings
  - Stack prints these out when you run `stack sdist`.
  - Some of the warnings, like category and description, are annoying.
  - Generally using `--pvp-bounds=both` is good.

- Put everything in `source/`
  - This separates Haskell files from package metadata.
  - Makes it easy to search and script Haskell files.
  - Looks like `source/library/Foo.hs`, `source/executables/Main.hs`, `source/tests/Main.hs`, `source/benchmarks/Main.hs`, and so on.
  - The exact names aren't important.

- Match module and package names
  - Module names use `CamelCase`, package names use `kebab-case`.
  - If you package is named `foo-bar`, you should have a top-level module called `FooBar`.
  - Not `Data.FooBar` or `Text.ParserCombinators.FooBar`.

- Require one import
  - Users should be able to hit the ground running with `import FooBar`.
  - If necessary, re-export functions from other packages.
  - Optimize for qualified imports, so don’t be afraid to take common names.

- Expose implementation details
  - People will use your package in ways you didn’t think of.
  - Make everything public, but not necessarily published.
  - Use `Internal` module names to signal that they’re private.

- Build `-Wall` clean
  - GHC can find all kinds of problems but doesn’t by default.
  - Few false positives, generally helps you write better code.
  - You can ignore specific warnings with `-fno-warn-whatever`.

- Follow most HLint suggestions
  - Overall a great tool that helps you write better software.
  - Some suggestions aren’t worth following.
  - In particular the re-export shortcut and anything involving operators.

- Format code with `hindent`
  - This frees you up to never think about formatting again.
  - Improving `hindent` improves formatting for everyone.
  - Side steps useless arguments in pull requests.

- Document with examples
  - Types are good, but they’re not documentation.
  - The same is true for laws.
  - Usually functions are added to solve a problem. Show that problem in the documentation as an example.

- Test with Tasty using Hspec
  - Tasty is a testing framework for running different kinds of tests.
  - Hspec tests are the nicest to write.
  - Use other providers like QuickCheck when they make sense.

- Run tests on Travis CI
  - Travis CI is free and integrates with GitHub.
  - You can run tests on Linux and macOS.
  - Consider also using AppVeyor for Windows builds.

- Build and test with `--pedantic`
  - This forces you to fix warnings.
  - Particularly useful as new versions of GHC and dependencies come out.

- **Keep executables small.**
  If your package provides an executable,
  define it in your library and re-export it.
  This allows other packages to use your executable's behavior from Haskell.
  Your executable should look like this:

      module Main (module YourPackage) where
      import YourPackage (main)

- Benchmark with Criterion
  - Only if your package needs to be fast!
  - Otherwise this is a lot of overhead to maintain.
  - Criterion is the gold standard for pretty much any language.

- Automate releases
  - Most CI services set an environment variable when building a Git tag.
  - Upload packages to Hackage from CI using `stack upload .`.
  - Be sure not to leak credentials into build log.
