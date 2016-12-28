---
title: Problematic versioning policy
---

My [Haskell package checklist][1] recommends using [Semantic Versioning][2]
(SemVer) instead of the [Package Versioning Policy][3] (PVP). Many people
disagree with that recommendation. I stand by it. In fact, I think the PVP is a
bad policy and the Haskell community should abandon it in favor of SemVer.

The main difference between these two versioning schemes is that the PVP has an
extra major version number for encoding breaking changes. It recommends that
versions follow a *major.major.minor* format. SemVer differs by requiring
versions to follow the *major.minor.patch* format.

I am aware of a few reasons why people prefer the PVP to SemVer.

-   Because the PVP allows for more version components, **it is more
    expressive**. In particular, it can differentiate between big and small
    breaking changes. Re-writing a package from the ground up is an example of
    a big breaking change. A small breaking change might be renaming a
    function.

    SemVer treats all breaking changes the same, which I think is the right
    approach. A small change from the package author's point of view might be a
    big change from the package user's point of view. Similarly, a ground-up
    rewrite might not introduce any breaking changes at all.

-   **The first major version number can be used for something else** because
    the PVP has two major version numbers. For example, it might be used for
    marketing. Or, a single SDL package could have both a 1.x and a 2.x series,
    targeting different versions of the underlying SDL library.

    SemVer forces these into the package name, which frees you from matching
    your major version number to the library you're wrapping. (What if you
    wanted to make a big breaking change to the 1.x series?) You might end up
    with an `sdl1` package and an `sdl2` package. If you wanted to share code
    between them, you might need an `sdl-base` package too.

-   Because the PVP has two major version numbers, **package authors can make
    backwards-incompatible changes to old versions**. Imagine that the current
    version of a package is 2.0 but some people are still using version 1.0.
    They discover a bug; the fix requires a breaking change. The PVP allows
    that bug fix to be released as version 1.1.

    SemVer makes users upgrade to the latest version for backwards-incompatible
    bug fixes. Or it encourages package authors to find backwards-compatible
    bug fixes. Either way, this is not a scenario that comes up often.

-   The PVP has **no special case for major version 0**. Going from version 0.x
    to 1.x does not change any of the rules. This means that major version 0
    communicates neither (a lack of) stability nor maturity.

    By comparison, SemVer does not restrict major version 0 at all. Packages
    with major version 0 are in their initial phase of development. Users of
    those packages can make a case that the package should commit to stability
    and get on major version 1.

I have many reasons for preferring SemVer to the PVP.

-   The PVP is **unique to Haskell**. Most other package ecosystems use SemVer.
    Elixir, Elm, JavaScript, and Rust require it. Clojure, Java, Objective-C,
    PHP, PureScript, Python, Ruby, and Swift recommend it. For .NET and C/C++,
    it's a little unclear, but they unofficially recommend it. Go and Perl are
    the only other ecosystems I can find that don't recommend SemVer.

-   The PVP **allows any number of version components**. The spec merely
    recommends that each version have at least 3 components. However all of
    these are valid PVP versions: 1, 1.2, 1.2.3, 1.2.3.4, and so on. There are
    a couple interesting consequences of this.

    Different versions of the same package do not have to have the same number
    of version components. For example, these are some of the lens package's
    versions: 4.15.1, 4.14, 4.13.2.1. This isn't a problem by itself, but it is
    a problem when specifying version bounds. Version 4.14 of lens would not be
    matched by a constraint of `>= 4.14.0`. That's because version 4.14 is less
    than 4.14.0 according to `Data.Version`.

    Another less obvious problem with this variable number of version
    components is that going from version 1 to version 1.0 is both allowed and
    a breaking change. This case is so surprising and treacherous that Hackage
    now prevents authors from uploading packages that only add ".0"s to the end
    of version numbers.

-   The PVP is **unclear when to change which major version number**. If you
    need to release a breaking change, do you go from 1.0 to 1.1 or 2.0?
    Consider the 4.8 release of the base package, which changed the superclass
    hierarchy of the `Monad` class. That seems like a big change; why was it
    communicated by going from 4.7 to 4.8 instead of 5.0? Which types of
    changes motivate version 5.0 of the base package to be released?

-   The PVP **encourages packages to stay on major version 0**. This follows
    from the previous point. At which point do you go from 0.x to 1.0? Making
    that change implies that there are big breaking changes in the release.
    Unlike SemVer, major version 0 doesn't imply instability. That leads to
    widely-used, rock-solid packages on major version 0. Look at the bytestring
    package, which has been around for 10 years. Why is it still on major
    version 0?

-   The PVP **considers deprecation a breaking change**. In other words, it
    considers deprecation the same as removal. Compare this to SemVer, which
    allows deprecations minor release.

-   Dependency **lower bounds are often incorrect** with the PVP. The correct
    minimum bound for a PVP package is `>= A.B.C`. Many packages only specify
    `>= A.B`, which implies (but isn't the same as) `>= A.B.0`. For example,
    the directory package depends on filepath `>= 1.3 && < 1.5`. Since new
    functionality can be introduced by the `C` version component, it's possible
    that version `1.3.1` would work but `1.3.0` would not.

-   Dependency **upper bounds are often incorrect** with the PVP. The correct
    upper bound for a PVP package is `< A.B`. Many packages only specify `< A`.
    For example, the containers package depends on base `>= 4.3 && < 5`. This
    is incorrect because version 4.9 of the base package could introduce
    breaking changes.

-   In addition to the previous two points, **bounds are often incorrect** with
    the PVP. For example, consider the aeson package. Its versions 0.11.2.1 and
    1.0.2.1 are broadly compatible. You might write `>= 0.11.2 && < 1.1` as a
    reasonable-looking version constraint that isn't a problem according to the
    previous two points. However, it's entirely possible that version 0.12 of
    aeson will be released and it will be neither backwards compatible with
    0.11 nor forwards compatible with 1.0. So the accurate version bounds for
    this scenario are `(>= 0.11.2 && < 0.12) || (>= 1.0.2 && < 1.1)`. That is
    tedious at best to write out, so often it will be lazily constrained as
    `>= 0.11 && < 1.1`.

-   The PVP **provides no guidance about patch releases**. For example,
    releasing a patch to version 1.2.3 could be any of: 1.2.3.0, 1.2.4, 1.3.0,
    or 2.0.0. All are allowed according to the PVP. This makes it difficult to
    tell if a major-looking release actually has anything major in it.

-   The PVP **allows non-contiguous version ranges**. The spec does not require
    version components to go up by 1. It only requires that they increase. So
    it's perfectly fine to go from version 1.2.3 to 3.0.0. This is not
    necessarily a problem, but it is potentially confusing.

-   The PVP **does not require smaller components to reset**. For example,
    going from version 1.2.3 to 2.2.3 is fine. Like the previous point, this is
    merely confusing.

-   The PVP **considers additions to be breaking if you use a "general" name
    space**. For example, if you want to add a module called `Data.Set.Mine`,
    it must be accompanied by a new major version number.

I have heard reasons for preferring the PVP that I consider invalid.

-   The PVP is older, but why does it matter? The PVP was introduced in
    November 2006, but it looked like SemVer at that time. Around October 2007
    it changed into its current form. SemVer was introduced in December 2009.

-   Tooling does no depend on the PVP. If you use Cabal and Hackage, everything
    works with version bounds and a solver. If you use Stack and Stackage,
    everything works with snapshots.

For the reasons given above, I dislike the PVP. In researching this post, I
kept finding things to dislike about the spec. I think the Haskell community
should move away from it, preferably toward SemVer.

[1]: {% post_url 2016-12-05-haskell-package-checklist %}
[2]: https://github.com/mojombo/semver/blob/520670dc0e68cf3587549baf207d5d3da46ac87b/semver.md
[3]: https://github.com/haskell/pvp/blob/6b90cef4b14893180010b3202a8eb0ced1b5b295/pvp-specification.md
