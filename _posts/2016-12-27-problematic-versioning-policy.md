---
title: Problematic versioning policy
---

My [Haskell package checklist][] recommends using [Semantic Versioning][] (SemVer) instead of the [Package Versioning Policy][] (PVP).
Many people disagree with that recommendation.
I stand by it.
In fact, I think the PVP is a bad policy
and the Haskell community should abandon it in favor of SemVer.

The main difference between these two versioning schemes is that
the PVP has an extra major version number for encoding breaking changes.
It recommends that versions follow a *major.major.minor* format.
SemVer differs by requiring versions to follow the *major.minor.patch* format.

I am aware of a few reasons why people prefer the PVP to SemVer.

-   Because the PVP allows for more version components,
    **it is more expressive**.
    In particular, it can differentiate between big and small breaking changes.
    Re-writing a package from the ground up is an example of a big breaking change.
    A small breaking change might be renaming a function.

    SemVer treats all breaking changes the same,
    which I think is the right approach.
    A small change from the package author's point of view might be a big change from the package user's point of view.
    Similarly, a ground-up rewrite might not introduce any breaking changes at all.

-   **The first major version number can be used for something else**
    because the PVP has two major version numbers.
    For example, it might be used for marketing.
    Or, a single SDL package could have both a 1.x and a 2.x series,
    targeting different versions of the underlying SDL library.

    SemVer forces these into the package name,
    which frees you from matching your major version number to the library you're wrapping.
    (What if you wanted to make a big breaking change to the 1.x series?)
    You might end up with an `sdl1` package and an `sdl2` package.
    If you wanted to share code between them, you might need an `sdl-base` package too.

-   Because the PVP has two major version numbers,
    **package authors can make backwards-incompatible changes to old versions**.
    Imagine that the current version of a package is 2.0 but some people are still using version 1.0.
    They discover a bug;
    the fix requires a breaking change.
    The PVP allows that bug fix to be released as version 1.1.

    SemVer makes users upgrade to the latest version for backwards-incompatible bug fixes.
    Or it encourages package authors to find backwards-compatible bug fixes.
    Either way, this is not a scenario that comes up often.

-   The PVP has **no special case for major version 0**.
    Going from version 0.x to 1.x does not change any of the rules.
    This means that major version 0 communicates neither (a lack of) stability nor maturity.

    By comparison, SemVer does not restrict major version 0 at all.
    Packages with major version 0 are in their initial phase of development.
    Users of those packages can make a case that the package should commit to stability and get on major version 1.

[Haskell package checklist]: {% post_url 2016-12-05-haskell-package-checklist %}
[Semantic Versioning]: https://github.com/mojombo/semver/blob/520670dc0e68cf3587549baf207d5d3da46ac87b/semver.md
[Package Versioning Policy]: https://github.com/haskell/pvp/blob/6b90cef4b14893180010b3202a8eb0ced1b5b295/pvp-specification.md

- The PVP gives freedom that most packages don't take advantage of.
  - It is not clear when to update one major version number versus another.
- Hackage has about 10,642 packages.
- Only 921 packages (9%) have changed their most significant version number.
  - Having more version numbers means the most significant one is less likely to change.
- 8,585 packages (81%) use `0` as their most significant version number.
  - Having more version numbers means the most significant one is more likely to be `0`.
- Version bounds are often not specific enough.
  - deepseq uses `base >= 4.3 && < 4.10` and `array >= 0.3 && < 0.6`
  - directory uses `base >= 4.5 && < 4.11` and `filepath >= 1.3 && <1.5` and `time >= 1.4 && < 1.8`
- Constraints are often loose.
  - containers uses `base >= 4.3 && < 5`
  - assert uses `base >= 4 && <= 9000`
- The PVP allows for any number of version components.
  - Different versions of the same package often don't have the same number of components.
  - The spec allows a breaking change from `1` to `1.0` because `1.0 > 1` according to `Data.Version`.
  - Hackage specifically checks for this scenario and rejects it.
  - This is problematic for version bounds since `>= A.B` isn't the same as `>= A.B.0`.
- SemVer requires exactly 3 components.
- 8,492 packages (80%) have less than 10 releases.
- Most other ecosystems use SemVer.
  - Required: Elixir, Elm, JavaScript, Rust
  - Recommended: Clojure, Java, Objective-C, PHP, PureScript, Python, Ruby, Swift
  - Unofficially recommended: .NET, C/C++
  - Not recommended: Go, Haskell, Perl
- The PVP allows for annoying releases that are difficult to constrain correctly.
  - For example `aeson-0.11.2.1` and `aeson-1.0.2.1` are broadly compatible.
  - Correct bounds would be `aeson (>= 0.11.2 && < 0.12) || (>= 1.0.2 && < 1.1)`.
  - Typical bounds are `aeson >= 0.11 && < 1.1`.
  - Technically 0.12 could be incompatible with 0.11 and 1.1.
  - For some people, this is a positive thing because you can keep developing the 0.x "series".
  - For others, this is negative because bounds are annoying and it maybe shouldn't be allowed at all.
- If you *must* use the PVP, just set the super-major version at 1 and treat the rest like SemVer.
- Some people claim that tooling depends on the PVP.
  - Cabal/Hackage does not; its solver can handle any version bounds.
  - Stack/Stackage does not; its snapshots use version bounds from packages.
- The PVP is older, but who cares?
  - The PVP was first introduced in November 2006, but it looked like SemVer at first.
  - Around October 2007 it changed into its current form.
  - SemVer appeared in December 2009.
- The PVP allows for non-breaking, non-adding changes to modify the major or minor version numbers.
  - For instance, a bug fix to 1.2.3 can be released as 1.2.3.0, 1.2.4, 1.3.0, or 2.0.0.
- The PVP allows for non-contiguous version ranges.
  - Example: 1.2.3 can become 1.2.5, 1.4.0, or 3.0.0, leaving a gap.
- The PVP does not require smaller components to reset.
  - Example: 1.2.3 can become 1.3.3 instead of 1.3.0, or 2.2.3 instead of 2.0.0.
- SemVer specifies pre-release and build metadata version components.
  - Cabal rejects them, so they don't matter for this discussion.
- The PVP also specifies how you should constrain your dependency versions.
  - SemVer doesn't spell this out, but `>= A.B && < (A+1)` follows naturally.
- The PVP considers deprecation to be the same as removal.
  - That means deprecating something requires a major version change.
  - SemVer requires only a minor version bump for deprecations.
- The PVP considers additions breaking changes if you use a "general" namespace.
  - SemVer says all additions are non-breaking.
  - Consequence of PVP: other packages are sort of part of the API you have to consider when cutting releases.
