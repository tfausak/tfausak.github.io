---
title: 'Strive: a Haskell client for the Strava API'
---

![Strive logo][1]

I'm proud to announce the release of [Strive][2], a Haskell client for the
[Strava V3 API][3]. Although it hasn't reached [the 1.0 milestone][4], it's
both usable and stable.

## Motivation

After working on [Haskeleton][5] and [hs2048][6], I wanted to use Haskell to
create something with real value. Seeing as I'm an avid cyclist and use Strava
to track my rides, it made sense to write a client for their API. Plus, they
didn't already have one in Haskell.

## Goals

In spite of still being an amateur Haskeller, I have some ideas about how
packages should be written. My post [introducing Haskeleton][7] probably made
that clear already. To that end, I tried to make Strive:

- Easy to use with a single import. All you need to do is `import Strive`. In
  order to do this, I had to [learn about lenses][8]. This makes the package
  easier to use and easier to play around with in GHCi.

- Quick to build. Installing Strive from scratch takes about ten minutes.
  Obviously it's not as fast as installing a Ruby gem, but it's pretty good as
  far as Haskell packages go. This is a cost that every user has to pay, so the
  faster the better.

- Depend on few other packages. Strive depends on 12 packages, many of which are
  included with the Haskell Platform. This not only makes installation faster,
  but it makes it easier to keep dependencies up to date.

- Be accessible to beginners. Anyone should be able to open any file in Strive
  and basically follow along. There are some exceptions, like lenses and
  Template Haskell, but they're necessary to achieve other goals. This makes it
  easier for anyone to contribute.

Hopefully these goals make for a package that is both easy to use and easy to
develop for.

## Example

I want to show you the bare minimum needed to get started. It covers Strava's
token exchange, which is necessary for getting a token. Once you have one, you
can perform any request against their API. I will show one here as an example.

{% highlight hs %}
import Strive
import Data.Default (def)
import Data.Text (unpack)

-- Each API application has a client ID and a client secret. Get them
-- both at <http://www.strava.com/settings/api>.
let clientId = 1790
let clientSecret = "..."

-- To get authorized, you need to build a URL and visit it.
let url = buildAuthorizeUrl clientId "http://localhost" def
putStrLn url

-- After visiting the above URL, copy the code out of the request
-- parameters.
let code = "..."

-- Finally you can exchange your code for a token, which can be used
-- to access the API.
Right response <- exchangeToken clientId clientSecret code
let token = unpack (get accessToken response)

-- Build a Strive client and use it to request the current athlete.
client <- buildClient token
Right athlete <- getCurrentAthlete client
print (get firstname athlete)
{% endhighlight %}

This example scratches the surface of what Strive provides. It covers 100% of
Strava's API. Check out [the readme][9] for a complete list of available
endpoints.

[1]: /static/images/2014/08/11/strive.png
[2]: https://github.com/tfausak/strive
[3]: http://strava.github.io/api/
[4]: https://github.com/tfausak/strive/milestones/v1.0.0
[5]: https://github.com/tfausak/haskeleton
[6]: https://github.com/tfausak/hs2048
[7]: {% post_url 2014-03-04-haskeleton-a-haskell-project-skeleton %}
[8]: {% post_url 2014-08-03-lenses-from-the-ground-up %}
[9]: https://github.com/tfausak/strive#readme
