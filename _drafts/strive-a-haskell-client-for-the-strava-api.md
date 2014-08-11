---
title: 'Strive: a Haskell client for the Strava API'
---

![Strive logo][]

I'm proud to announce the release of [Strive][], a Haskell client for the [Strava V3 API][].
It hasn't yet reached [the 1.0 milestone][], but it's usable and stable.

## Motivation

I created it because after working on [Haskeleton][] and [hs2048][],
I wanted to create something "real" with Haskell.

## Goals

- build fast
- minimal dependencies
- be easy to use with a single import
  - this required [learning lenses][]
- beginner-friendly source code

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
