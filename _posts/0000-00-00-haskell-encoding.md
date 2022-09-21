---
title: Haskell Encoding
---

<https://taylor.fausak.me/2021/07/13/witch/>
<https://github.com/tfausak/witch/issues/56>
<https://github.com/tfausak/witch/pull/58>

basic type class for conversions that can't fail:

``` hs
{-# language MultiParamTypeClasses #-}

class From source target where
  from :: source -> target
```

and an example instance:

``` hs
instance From Int Integer where
  from int =
    toInteger int
```

basic usage example

``` hs
>>> from (123 :: Int) :: Integer
123
```

another basic type class for conversions that can fail:

``` hs
class TryFrom source target where
  tryFrom :: source -> Maybe target
```

and another example instance:

``` hs
instance TryFrom Integer Int where
  tryFrom integer =
    let
      lo = toInteger (minBound :: Int)
      hi = toInteger (maxBound :: Int)
    in
      if lo <= integer && integer <= hi then
        Just (fromInteger integer)
      else
        Nothing
```

basic usage example, showing success and failure

``` hs
>>> tryFrom (123 :: Integer) :: Maybe Int
Just 123
>>> tryFrom (9223372036854775808 :: Integer) :: Maybe Int
Nothing
```

what if you want to convert from text to byte string?
you can assume utf-8

``` hs
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as Encoding

instance From Text ByteString where
  from text =
    Encoding.encodeUtf8 text
```

simple example encoding lambda &#955; U+03BB into utf-8 as 0xCE 0xBB

``` hs
import qualified Data.Text as Text

from (Text.pack "\955") :: ByteString
-- "\206\187"
```

and what about the other direction?
also assuming utf-8

``` hs
instance ByteString where
  tryFrom byteString =
    case Encoding.decodeUtf8' byteString of
      Right text -> Just text
      Left _ -> Nothing
```

simple example decoding lambda, and failing to decode 0xff

``` hs
import qualified Data.ByteString as ByteString

tryFrom (ByteString.pack [206, 187]) :: Maybe Text
-- Just "\955"
tryFrom (ByteString.pack [255]) :: Maybe Text.Tex
-- Nothing
```

but that's not satisfying.
what if you want to support other encodings, like utf-16?
you need to represent the encoding on the type level.
so let's introduce a way to tag values with encodings at the type level:

``` hs
newtype WithEncoding encoding value
  = WithEncoding value
  deriving (Eq, Show)
  
withEncoding :: encoding -> value -> WithEncoding encoding value
withEncoding _ value =
  WithEncoding value

withoutEncoding :: WithEncoding encoding value -> value
withoutEncoding (WithEncoding value) =
  value
```

and a corresponding encoding for utf-8:

``` hs
data Utf8
  = Utf8
  deriving (Eq, Show)
```

now we have the tools to define a better instance for encoding text.
but which value do we want to tag with the encoding?
we're converting text into a utf-8 byte string, so let's tag the byte string:

``` hs
{-# language FlexibleInstances #-}

instance From Text (WithEncoding Utf8 ByteString) where
  from text =
    withEncoding Utf8 (Encoding.encodeUtf8 text)
```

encoding isn't much harder, but we do need to select which encoding to use

``` hs
from (Text.pack "\955") :: WithEncoding Utf8 ByteString
-- WithEncoding "\206\187"
```

what about going in the other direction though?
when we encoding, `WithEncoding Utf8` means that's the encoding the byte string has.
but going on the other direction, it's the encoding we _want_:

``` hs
instance TryFrom (WithEncoding Utf8 ByteString) Text.Text where
  tryFrom byteStringWithEncoding =
    case Encoding.decodeUtf8' (withoutEncoding byteStringWithEncoding) of
      Right text -> Just text
      Left _ -> Nothing
```

decoding is still easy, though we have to pick the encoding to use

``` hs
tryFrom (withEncoding Utf8 (ByteString.pack [206, 187])) :: Maybe Text
-- Just "\955"
tryFrom (withEncoding Utf8 (ByteString.pack [255])) :: Maybe Text
-- Nothing
```

this feels like a reasonable approach, but i think it has a problem:
what does `WithEncoding Utf8 ByteString` mean?
when encoding it means that the byte string is valid utf-8.
but when decoding it means that the byte string should be interpreted as utf-8, although that may fail.
perhaps we could solve this by introducing more types, like `DefinitelyUtf8` and `ProbablyUtf8`.
but what if we tagged the text instead of the byte string?

``` hs
instance From (WithEncoding Utf8 Text) ByteString where
  from textWithEncoding =
    Encoding.encodeUtf8 (withoutEncoding textWithEncoding)
```

usage is similar to before

``` hs
from (withEncoding Utf8 (Text.pack "\955")) :: ByteString
-- "\206\187"
```

it also works for decoding

``` hs
instance TryFrom ByteString (WithEncoding Utf8 Text) where
  tryFrom byteString =
    case Encoding.decodeUtf8' byteString of
      Right text -> Just (withEncoding Utf8 text)
      Left _ -> Nothing
```

with similar ergonomics

``` hs
tryFrom (ByteString.pack [207, 187]) :: Maybe (WithEncoding Utf8 Text)
-- Just "\955"
tryFrom (ByteString.pack [255]) :: Maybe (WithEncoding Utf8 Text)
-- Nothing
```

and now the meaning is a little clearer.
`WithEncoding Utf8 Text` means it's just `Text`, but for encoding and decoding it will use UTF-8.

is this right?
should the byte string be tagged or the text?
and should i introduce a custom data type (`WithEncoding`) or just use `Tagged`?
and should i define custom data types for encodings (`Utf8`) or just use `"UTF-8"`?
