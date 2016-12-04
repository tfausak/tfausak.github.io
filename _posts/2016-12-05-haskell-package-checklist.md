---
title: Haskell package checklist
---

- Host on GitHub.
- Build with Stack.
- Use hpack instead of raw Cabal files.
- Use kebab-case for package names.
- Use semver.
- Choose a license.
- Write a good README.
- Write a synopsis.
- Make the description link to the README.
- Include any non-source files necessary to build in `extra-source-files`.
- Make sure there are no warnings when running `stack sdist`.

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
