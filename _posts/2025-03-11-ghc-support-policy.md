---
title: GHC support policy
---

I maintain a small handful of Haskell packages.
I keep [a dashboard](https://github.com/tfausak/haskell-packages/blob/060d4bdd9384973fd70ecb8ecc8510ead3e6c5cc/README.md) of all of them.
I have been maintaining some of them for nearly a decade.
Naturally I have developed some opinions about how to maintain them.
[A recent discussion](https://discourse.haskell.org/t/how-much-effort-does-backwards-compatibility-require-from-library-authors/11584) on the Haskell Discourse prompted me to write down my thoughts.
This post will describe which versions of GHC I support for my Haskell packages and why.
I'm not trying to convince you that my way is the best way, and I'm not trying to be convinced of changing.
I'm just writing down my thoughts so I can point to them later.

## Why have a policy at all?

The main reason I publish my packages is so that other people can use them.
It's easier for people to use my packages if they know what to expect.
If I didn't have a policy, prospective users might be concerned that their version of GHC won't be supported.
With a policy, they can know (one way or the other).

## What is my policy?

My policy is to support the three latest major versions of GHC.
Right now that means 9.12, 9.10, and 9.8.
Since GHC releases are usually made every six months, this covers about a year and a half of GHC development.

This should allow people that depend on my packages to upgrade GHC without having to simultaneously upgrade my packages.

## Why not support older versions?

By removing support for older versions of GHC, I can remove anything specific to those versions.
This means workarounds, build-time conditionals (like `if impl(ghc >= 9.8)` in Cabal), and CPP.
I could accrete those things in order to maintain support, but I feel that it is better for the long-term quality of the codebase to shed them.

I can also use features that only appear in newer versions of GHC.
For example a future version of Witch might offer an interface that uses [the `RequiredTypeArguments` extension](https://downloads.haskell.org/~ghc/9.12.1/docs/users_guide/exts/required_type_arguments.html) once GHC 9.10 is the oldest version it supports.
By dropping support for older versions, I can use new features like this more quickly (without CPP).

Supporting many versions of GHC takes time, literally.
I run CI for all my packages.
A typical build uses about 30 CPU minutes.
Adding more configurations to the build matrix only increases that.
This happens to be free for me right now since my packages are open source and hosted on GitHub, but it still takes my time to wait for builds.

Similarly I prefer to be able to build and test all my packages on my machine.
Keeping around old versions of the compiler and dependencies takes up space.

## Why not support more versions?

Any cutoff I choose will exclude some people.
For the six years that I ran the Haskell survey, the three latest major versions of GHC covered the vast majority of the community
([2022](https://taylor.fausak.me/2022/11/18/haskell-survey-results/#s2q4),
[2021](https://taylor.fausak.me/2021/11/16/haskell-survey-results/#s2q4),
[2020](https://taylor.fausak.me/2020/11/22/haskell-survey-results/#s2q4),
[2019](https://taylor.fausak.me/2019/11/16/haskell-survey-results/#s2q4),
[2018](https://taylor.fausak.me/2018/11/18/2018-state-of-haskell-survey-results/#question-035),
[2017](https://taylor.fausak.me/2017/11/15/2017-state-of-haskell-survey-results/#question-18)).
A bigger support window for me means more work for less payoff.

## Why not support newer versions?

GHC has a few versions ahead of the latest major one.
There are nightlies, alphas, and release candidates.
If I'm on the ball enough, I try to test my packages with the RCs.
Otherwise I don't bother with alphas or nightlies.
Typically it's not worth it for me.
I'm unlikely to uncover problems and it requires a large amount of work.

## Why not let the community figure it out?

Another approach is to define packages with very loose bounds, like `base < 9`, and let the community alert you when it doesn't work.
I have two main problems with this approach.

First, this just goes against my sensibilities.
When I provide a package to someone, I want them to have confidence that it will work in the configurations that it claims to work in.
Failing to build a package is a bug in my eyes.

Second, doing this requires using revisions to prevent the ecosystem from getting too chaotic.
I personally dislike revisions and aim to never use them with my packages.
I think that getting a different build plan for the same package set depending on when exactly you ran the build command is madness.
I don't want to contribute to that.

---

I hope that provides some insight into my GHC support policy.
I encourage other package maintainers to adopt it if it makes sense to them.
Otherwise keep doing what you're doing.
Thanks for reading!
