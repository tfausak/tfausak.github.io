---
title: Haskell package checklist
---

- Make sure there are no warnings when running `stack sdist`.
- Avoid putting too much in your package's description. Either make it a copy of the synopsis or a link to the README.
- Use hpack's `package.yaml` instead of `*.cabal`.
- Use Stack instead of Cabal.
- Test on CI.
- Automate releases. Just `stack upload .` from your CI.
- Build `-Wall` clean. Use `--pedantic` on your CI.
- Put all your source files in a subdirectory, like src or library. This makes it easier to write scripts that avoid the project-level metadata like README.md.
  - TODO: What the preferred way to do this? `/source/library/Foo.hs` with `/source/executables/Main.hs` or `/library/Foo.hs` with `/executables/Main.hs`? I typically do the former but I see the appeal of the latter.
- Use HLint, but ignore some of it's dumb suggestions like "Use &&&".
- Use hindent all the time even if you think its output looks gross. Open an issue and make hindent better!
- Use tasty. And whichever providers you want, but probably hspec.

From Sebastiaan:

- Keep the module hierarchy flat.
- Use a short top-level namespace.
- One import, batteries included.
- Focus documentation on usage, not implementation.
- Think about the generated documentation.
- Be honest about the shortcomings.
- Create a website with a tutorial. [or create a tutorial module]
- Keep type signatures simple
- Avoid type classes
- Avoid useless identifier prefixes
- ~~Stick to the Package Versioning Policy~~ [use semver]
- Prune the dependencies
- Don't hide big impact changes behind minor bumps
- Have a changelog
- Expose internals
- Think about licensing
- Host on Github

From Haskeleton:

- benchmarks are nice, but not really generally applicable
- doctests can be nice, but there are a lot of gotchas
- documentation coverage isn't worth tracking
- same for test coverage
- both might be useful for some projects, but in general they're too much overhead
