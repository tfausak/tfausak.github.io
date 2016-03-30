---
title: Type safe web services in Haskell with Servant
---

I enjoy writing web services and working with Haskell.
So much so that I wrote about [creating a REST API in Haskell][] a while ago.
I am happy to say that I think there is a better way now.

I recently discovered [Servant][].
It is one of the coolest libraries I have ever seen.
It is for describing type-safe web services.
That means you know the types of the inputs and outputs for every endpoint.

Although it appears to be primarily designed for implementing API servers,
Servant can do a lot more.
Since it is a way to formally describe an API,
it can be used to generate documentation and clients as well.
With a single API description,
you can implement a server,
then get documentation and clients for free.

All of that can be accomplished with hardly any boilerplate.
Before I get into an example,
I should point out that [Servant's tutorial][] is great --- seriously.
If this post piques your interest,
go read the tutorial to get started.

So what does Servant look like?
Here is the description for a simple API.

``` hs
import Servant.API

type API
    -- GET /things
    = "things" :> Get '[JSON] [Thing]
    -- GET /things/:id
    :<|> "things" :> Capture "id" Integer :> Get '[JSON] Thing
```

That describes two endpoints, `GET /things` and `GET /things/:id`.
The former returns a list of `Thing`s and the latter returns a single `Thing`.
Both endpoints return JSON.
`Thing` is a normal Haskell data type that can be converted to JSON.

Although the API is represented as a type,
it is convenient to get at it from the value level.
To do that,
we can [use a proxy][].

``` hs
import Data.Proxy

api :: Proxy API
api = Proxy
```

Now that we have a description of our API,
we can go ahead and implement a server.
We will need to define two handlers.

``` hs
import Control.Monad.Trans.Either

-- This is Servant's default handler type.
type Handler a = EitherT ServantErr IO a

getThings :: Handler [Thing]
getThings = do
    things <- getThingsFromDB
    return things

getThing :: Integer -> Handler Thing
getThing thingID = do
    maybeThing <- getThingFromDB thingID
    case maybeThing of
        Just thing -> return thing
        Nothing -> left err404
```

Notice that the handlers don't really have to think about HTTP.
They get the input defined by the API and return something of the correct type.
Servant handles converting the inputs and serializing the outputs as JSON.

By combining the handlers,
we can create our complete server.

``` hs
server :: Server API
server
    -- GET /things
    = getThings
    -- GET /things/:id
    :<|> getThing
```

Note that we have to list the handlers in the same order as the API.
(This [connascence of position][] is the only thing I don't like about Servant so far.)
And to actually make the server available we need to serve it through Warp.

``` hs
import Network.Wai.Handler.Warp

main = run 8080 (serve api server)
```

As I mentioned before,
you can get documentation and client code from your API for free.
To see an example of this, check out [Factory][], my example Servant web service.
It does everything that Servant is capable of right now, as of version 0.4.

If you are interested in learning more about Servant,
I am giving [a talk this Wednesday][] and broadcasting it over Hangouts on Air.
If you can't make it,
the talk will be available afterwards on YouTube.
([The slides][] are now available on this site.)

[creating a rest api in haskell]: {% post_url 2014-10-21-building-a-json-rest-api-in-haskell %}
[servant]: http://haskell-servant.github.io
[servant's tutorial]: http://haskell-servant.github.io/tutorial/
[use a proxy]: http://stackoverflow.com/questions/27044209/haskell-why-use-proxy
[connascence of position]: https://en.wikipedia.org/wiki/Connascence_%28computer_programming%29#Connascence_of_Position_.28CoP.29
[factory]: https://github.com/tfausak/factory
[a talk this wednesday]: https://plus.google.com/events/ca830qjcae9570q7kgmq3fl5cmk
[the slides]: /static/pages/2015-08-26-type-safe-web-services-in-haskell-with-servant.html
