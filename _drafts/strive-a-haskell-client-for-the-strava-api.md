---
title: 'Strive: a Haskell client for the Strava API'
---

![Strive logo][]

I'm proud to announce the release of [Strive][], a Haskell client for the [Strava V3 API][].
It hasn't yet reached [the 1.0 milestone][], but it's usable and stable.

## Motivation

I created it because after working on [Haskeleton][] and [hs2048][],
I wanted to create something real with Haskell.
I'm an avid cyclist and track all my rides on Strava.
I saw that they didn't have an official API library in Haskell,
so I decided to give it a go.

## Goals

Strive should be easy to use with a single import.
All you need to do is `import Strive`.
This means you don't have to use qualified imports.
Accomplishing this required [learning lenses][] so I could avoid long field names.

Strive should build fast.
Many libraries take a long time to build.
That's a price every user has to pay.
Accomplishing this will require using `zeroth` to remove Template Haskell.

Hand-in-hand with the last goal,
Strive should depend on the fewest number of packages possible.
Accomplishing this required avoiding heavy dependencies like `lens`.

Finally, Strive's source code should be beginner friendly.
Many packages are excessively clever,
either in types or implementation.
This makes it hard for people to contribute.

## Example

``` hs
import Strive
import Data.Text (unpack)

Right tokenExchangeResponse <- exchangeToken 1790 "secret" "code"
let token = unpack (get accessToken tokenExchangeResponse)
client <- buildClient token

Right currentAthlete <- getCurrentAthlete client
print (get firstname currentAthlete)
-- "Taylor"
```

[strive logo]: ../static/images/strive.png
[strive]: https://github.com/tfausak/strive
[strava v3 api]: http://strava.github.io/api/
[the 1.0 milestone]: https://github.com/tfausak/strive/milestones/v1.0.0
[haskeleton]: https://github.com/tfausak/haskeleton
[hs2048]: https://github.com/tfausak/hs2048
[learning lenses]: {% post-url 2014/08/03/lenses-from-the-ground-up %}
