---
title: Haskell package checklist
---

- Make sure your package passes `cabal check`.
- Avoid putting a description in your Cabal file. Use the README instead.
- Use hpack's package.yaml instead of *.cabal.
- Use Stack instead of Cabal.
- Test on CI. Travis is free!
- Automate releases. Just stack upload . from your CI.
- Build -Wall clean. Use --pedantic on your CI.
- Put all your source files in a subdirectory, like src or library. This makes it easier to write scripts that avoid the project-level metadata like README.md.
- Use HLint, but ignore some of it's dumb suggestions like "Use &&&".
- Use hindent all the time even if you think its output looks gross. Open an issue and make hindent better!
