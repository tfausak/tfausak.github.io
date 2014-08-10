---
title: 'Strive: a Haskell client for the Strava API'
---

![Strive logo][]

- <https://github.com/tfausak/strive>
- <http://strava.github.io/api/>

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
