---
title: Scraping websites with Haskell
---

- i wanted to get and parse some html with haskell
- typically i use ruby or python for this
- i wanted to see how it was in haskell
- turns out, not that bad
- the html-conduit library does most of the heavy lifting
- and i get to use an xpath-like interface for traversal
- plus it's all fast and type safe

- let's consider scraping the gatherer for magic cards
- we will be given a multiverse id
- we should return the card name
- or nothing if it doesn't exist
- (ignore special cards like split or flip)

- this problem naturally breaks down into a few pieces
  1. get the ids from the user
  2. build the url to get
  3. get that url
  4. parse the html
  5. extract what you want from the html
  6. show the names to the user

- getting the ids from the user is easy
- we'll just use `getArgs`
- no type checking, but we're just throwing it into a url anyway

``` hs
import System.Environment (getArgs)

getMultiverseIds :: IO [String]
getMultiverseIds = getArgs
```

- building the url is also easy
- we'll use string concatenation
- unfortunately string interpolation is hard in haskell
- if you wanted to go all out, you could build a URI
- we don't need that level of safety here

``` hs
buildUrl :: String -> String
buildUrl multiverseId = concat
  [ "http://gatherer.wizards.com"
  , "/Pages/Card/Details.aspx"
  , "?multiverseid="
  , multiverseId
  ]
```

- getting the url is a little complicated
- we are going to use conduits
- these allow us to efficiently stream data
- they're a little more complex than just getting the data as a string

- due to how they work, we have to combine this with the next step
- the streaming response will be fed directly into a parser
- if you didn't want to do this, you could write out to a file or something

``` hs
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (($$+-))
import Network.HTTP.Conduit (conduitManagerSettings, http, newManager, parseUrl, responseBody)
import Text.HTML.DOM (sinkDoc)
import Text.XML (Document)

makeRequest :: String -> IO Document
makeRequest url = do
  manager <- newManager conduitManagerSettings
  request <- parseUrl url
  let actions = do
        response <- http request manager
        let body = responseBody response
        body $$+- sinkDoc
  runResourceT actions
```

- now we need to pull out the parts we're interested in
- the name isn't easy to find in the DOM
- it's one of many at this CSS selector: `table.cardDetails td.rightCol div.row div.value`

``` hs
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Text.XML.Cursor (attributeIs, content, element, fromDocument, ($//), (>=>), (&//))

getName :: Document -> Maybe Text
getName document = listToMaybe contents where
  contents = cursor
    $// element "table"
      >=> attributeIs "class" "cardDetails"
    &// element "td"
      >=> attributeIs "class" "rightCol"
    &// element "div"
      >=> attributeIs "class" "row"
    &// element "div"
      >=> attributeIs "class" "value"
    &// content
  cursor = fromDocument document
```

- now that we have the name, we should display it to the user

``` hs
import Data.Text (strip, unpack)

printName :: Maybe Text -> IO ()
printName (Just name) = putStrLn (unpack (strip name))
printName Nothing = putStrLn "?"
```

- we can combine all the pieces into a working program

``` hs
import Control.Monad (forM_)

main :: IO ()
main = do
  multiverseIds <- getMultiverseIds
  forM_ multiverseIds $ \ multiverseId -> do
    let url = buildUrl multiverseId
    document <- makeRequest url
    let name = getName document
    putStr (multiverseId ++ "\t")
    printName name
```

- and then we can run it
- (assuming we have the dependencies installed)

``` sh
$ cabal run -- 383172 0
383172 Shivan Dragon
0      ?
```
