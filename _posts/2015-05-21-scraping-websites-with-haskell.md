---
title: Scraping websites with Haskell
---

I recently started a side project that involves scraping websites. Although I
would typically do that with a scripting language like Python or Ruby, I wanted
to use Haskell for its speed and type safety. It turned out to be easier than I
thought, thanks in large part to [the html-conduit package][1].

To show you how easy it is, let's look at an example. Say you want information
about [Magic cards][2]. For simplicity's sake, let's say you only want the name
of a card given its ID on [Gatherer][3]. We'll also ignore unusual cards that
are split or flipped.

From a high level, this problem breaks down into a few pieces:

1.  Get the list of IDs from the user. We are building a command-line
    application, so we'll get these from there.

2.  Build the URL to get. Each card has a URL on Gatherer that looks like
    `http://gatherer.wizards.com/Pages/Card/Details.aspx?multiverseid=ID`.

3.  Get the URL and parse the HTML. This is really two steps, but it's helpful
    to think of it as one unit of work. It takes a URL and returns some parsed
    HTML.

4.  Extract the name from the HTML.

5.  Show the names to the user. Since we're on the command line, this will just
    print them out.

Let's start with getting the IDs from the user. Even though they are integers,
we'll treat them as strings. Converting them to integers isn't worth it because
we'll just be putting them back into a URL.

``` hs
import System.Environment (getArgs)

getMultiverseIds :: IO [String]
getMultiverseIds = getArgs
```

Up next is building the URL. It is pretty easy, but the lack of string
interpolation makes it a little annoying. We could build an actual URI, but we
don't need that level of safety.

``` hs
buildUrl :: String -> String
buildUrl multiverseId = concat
  [ "http://gatherer.wizards.com"
  , "/Pages/Card/Details.aspx"
  , "?multiverseid="
  , multiverseId
  ]
```

Getting the URL and parsing the HTML is a little complicated. We are going to
use conduits, provided by [the conduit package][4]. They allow us to
efficiently stream data. We'll take the HTTP response and feed it into the HTML
parser.

``` hs
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-))
import Network.HTTP.Conduit (conduitManagerSettings, http, newManager, parseUrl, responseBody)
import Text.HTML.DOM (sinkDoc)
import Text.XML (Document)

makeRequest :: String -> IO Document
makeRequest url = do
  request <- parseUrl url

  -- Creating a new manager every time is expensive but simple.
  manager <- newManager conduitManagerSettings

  runResourceT $ do
    -- Actually make the request
    response <- http request manager
    -- Extract the response body.
    let body = responseBody response
    -- Parse the body as HTML.
    body $$+- sinkDoc
```

Now that we have an HTML document, we need to find the name in it.
Unfortunately there's not an easy way to get to it. It is one of many elements
at this CSS selector: `table.cardDetails td.rightCol div.row div.value`. We're
going to use [the xml-conduit package][] to express that as a series of
combinators. It's a lot more verbose, but also more powerful.

``` hs
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Text.XML.Cursor (attributeIs, content, element, fromDocument, ($//), (>=>), (&//))

getName :: Document -> Maybe Text
getName document = listToMaybe contents where
  contents = cursor
    -- table.cardDetails
    $// element "table"
      >=> attributeIs "class" "cardDetails"
    -- td.rightCol
    &// element "td"
      >=> attributeIs "class" "rightCol"
    -- div.row
    &// element "div"
      >=> attributeIs "class" "row"
    -- div.value
    &// element "div"
      >=> attributeIs "class" "value"
    &// content
  cursor = fromDocument document
```

With the name in hand, we can display it to the user. If, for whatever reason,
we couldn't find the name, we'll just print out a question mark.

``` hs
import Data.Text (strip, unpack)

printName :: Maybe Text -> IO ()
printName (Just name) = putStrLn (unpack (strip name))
printName Nothing = putStrLn "?"
```

Now that we have all the parts, we can combine them into a complete program.

``` hs
import Control.Monad (forM_)

main :: IO ()
main = do
  multiverseIds <- getMultiverseIds
  forM_ multiverseIds $ \ multiverseId -> do
    putStr (multiverseId ++ "\t")
    let url = buildUrl multiverseId
    document <- makeRequest url
    let name = getName document
    printName name
```

To run it, just pass a list of IDs you want to get from Gatherer.

``` sh
$ cabal run -- 383172 0
383172 Shivan Dragon
0      ?
```

So that was a pretty quick run through of scraping websites with Haskell. It's
tougher than doing the same thing in scripting languages, but hopefully easier
than you expected.

[1]: http://hackage.haskell.org/package/html-conduit-1.2.0
[2]: https://en.wikipedia.org/wiki/Magic:_The_Gathering
[3]: http://gatherer.wizards.com/Pages/Default.aspx
[4]: http://hackage.haskell.org/package/conduit-1.2.4.2
[5]: http://hackage.haskell.org/package/xml-conduit-1.3.0
