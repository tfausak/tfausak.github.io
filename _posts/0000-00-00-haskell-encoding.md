---
title: Haskell Encoding
---

<https://github.com/tfausak/witch/issues/56>
<https://github.com/tfausak/witch/pull/58>

---

I'm working on [Witch][], a Haskell library for ergonomic and safe conversions between types.
One common conversion is between `Text`, which represents strings of characters, and `ByteString`, which represents arrays of bytes.
Although Witch currently provides a conversion for this, it is potentially confusing.
In this post I will explain some of the problems with converting between `Text` and `ByteString`, along with some potential solutions.

[Witch]: https://taylor.fausak.me/2021/07/13/witch/

## Basics

You don't need to completely understand Witch for this post, but it will help to understand the basics.
The core of Witch is a type class for conversions that never fail.
It looks like this:

``` hs
{-# language MultiParamTypeClasses #-}

class From source target where
  from :: source -> target
```

An instance `From source target` means that you can convert from a value of type `source` into a value of type `target` unambiguously without anything going wrong.
For example it's always safe to convert an `Int` into an `Integer` because the latter is bigger than the former.
We can encode this with an instance like so:

``` hs
instance From Int Integer where
  -- from :: Int -> Integer
  from int =
    toInteger int
```

That defines how to convert from an `Int` into an `Integer`.
To actually perform the conversion, you simply call `from` with the proper types:

``` hs
from (123 :: Int) :: Integer
-- 123
```

Witch also has a companion type class for conversions that can fail.
It's called `TryFrom`.
Rather than converting directly from `source` to `target`, it allows for the possibility of failure.
We can represent that with `Maybe`.
(The real Witch library uses something else, but the differences aren't relevant for this post.)

``` hs
class TryFrom source target where
  tryFrom :: source -> Maybe target
```

Now we can define an instance for converting from an `Integer` into an `Int`.
Note that unlike the other direction, this conversion can sometimes fail.
That's because the `Integer` might be too big to represent as an `Int`.
That uncertainty is what the `TryFrom` type class expresses.

``` hs
instance TryFrom Integer Int where
  -- tryFrom :: Integer -> Maybe Int
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

Just like before, calling `tryFrom` with the types you want will do the conversion:

``` hs
tryFrom (123 :: Integer) :: Maybe Int
-- Just 123

maxBound :: Int
-- 9223372036854775807

tryFrom (9223372036854775808 :: Integer) :: Maybe Int
-- Nothing
```

So that's a basic API that's similar to what Witch exposes.
The key thing to note is that conversions are selected entirely by the types involved.

## Text

With that in mind, let's finally take a look at the core problem for this blog post: converting strings into and from bytes.
More commonly this is known as "encoding" and "decoding", respectively.
Encoding is simpler, so let's start with that.
For the time being we'll assume UTF-8.

Fortunately `Text` values can't contain any invalid Unicode characters, so this conversion can't fail.
That means we can represent encoding as a `From` instance:

``` hs
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as Encoding

instance From Text ByteString where
  -- from :: Text -> ByteString
  from text =
    Encoding.encodeUtf8 text
```

To show that it works, here it is converting a lambda character (U+03BB, decimal 955).
The UTF-8 encoding of that character is `0xCE BB` (decimal 206 187).

``` hs
import qualified Data.Text as Text

from (Text.pack "\955") :: ByteString
-- "\206\187"
```

But what about going in the other direction: decoding?
Since there are sequences of bytes that aren't valid UTF-8, decoding can fail.
That means we can use a `TryFrom` instance to represent it:

``` hs
instance ByteString where
  -- tryFrom :: ByteString -> Maybe Text
  tryFrom byteString =
    case Encoding.decodeUtf8' byteString of
      Right text -> Just text
      Left _ -> Nothing
```

Armed with that, we can decode the lambda we just encoded.
And we can verify that `0xFF` (decimal 255) is indeed an invalid byte in UTF-8:

``` hs
import qualified Data.ByteString as ByteString

tryFrom (ByteString.pack [206, 187]) :: Maybe Text
-- Just "\955"

tryFrom (ByteString.pack [255]) :: Maybe Text
-- Nothing
```

Now we have a simple API for encoding and decoding text.
It works reasonably well.
In fact, this is what Witch has today!
However as we'll see, it has some problems.

## Encodings

The main problem with this approach is that we've somewhat arbitrarily chosen UTF-8 as the encoding.
It's a popular encoding, so maybe it's not that bad.
But what happens if you want to encode some text as UTF-16?
Or if you need to decode some CP-1252 bytes?
With the current API you simply can't do that.

So we need a better API.
Since the conversions are selected by the types involved, it seems like our only choice is to mess with the types.
And if we're going to do that, we basically have two choices.
We can add a bunch of type wrappers, or we can add one polymorphic type wrapper.
In other words, we can do this: `newtype Utf8 = Utf8 ByteString`;
or we can do this: `newtype Tagged tag val = Tagged val`.

These are roughly equivalent in terms of expressive power, so I'm going to choose the "tagged" approach.
It's more flexible, which will be useful for this post, and it doesn't require quite as much boilerplate.
However I will choose a different name for the `Tagged` type.
Hopefully it's more expressive:

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

We'll also need a corresponding data type to represent the UTF-8 encoding itself:

``` hs
data Utf8
  = Utf8
  deriving (Eq, Show)
```

With those generic types defined, now we can talk about some useful types.
Although we do have to choose if we want to tag the `Text` or the `ByteString`.
It doesn't seem to make much sense to say "a UTF-8 text", so let's go with the byte string for now.
The type `WithEncoding Utf8 ByteString` represents, unsurprisingly, a UTF-8 encoded byte string.
And since that's a distinct type, we can write a `From` instance for it.

``` hs
{-# language FlexibleInstances #-}

instance From Text (WithEncoding Utf8 ByteString) where
  -- from :: Text -> WithEncoding Utf8 ByteString
  from text =
    withEncoding Utf8 (Encoding.encodeUtf8 text)
```

This is basically the same as our first attempt, except that now we have to explicitly choose the UTF-8 encoding.
It's no longer implied.

``` hs
from (Text.pack "\955") :: WithEncoding Utf8 ByteString
-- WithEncoding "\206\187"
```

That's easy enough, but what about decoding?
When we encode, the type `WithEncoding Utf8 ByteString` means that the byte string is definitely valid UTF-8 because we just converted it from some text.
But when we're decoding, that same type means that the byte string should have UTF-8 in it, but it might not.

``` hs
instance TryFrom (WithEncoding Utf8 ByteString) Text where
  -- tryFrom :: WithEncoding Utf8 ByteString -> Maybe Text
  tryFrom byteStringWithEncoding =
    case Encoding.decodeUtf8' (withoutEncoding byteStringWithEncoding) of
      Right text -> Just text
      Left _ -> Nothing
```

Just like with encoding, we now have to explicitly select UTf-8 for decoding.

``` hs
tryFrom (withEncoding Utf8 (ByteString.pack [206, 187])) :: Maybe Text
-- Just "\955"

tryFrom (withEncoding Utf8 (ByteString.pack [255])) :: Maybe Text
-- Nothing
```

This feels like a reasonable approach.
It's for sure better than before.
We no longer assume which encoding you want, and we allow for new encodings to be added later.

## Another Approach

But what does the `WithEncoding Utf8 ByteString` type mean?
When we encode, it means the byte string is definitely valid UTF-8.
But when we decode, it means the byte string should be UTF-8.
(Or, equivalently, that we'll treat it like UTF-8.)

That ambiguity feels weird to me.
Perhaps we could solve it by introducing even more types for tagging, like `DefinitelyUtf8` and `ProbablyUtf8`.
But that doesn't feel satisfying to me.
We don't have types like `DefinitelyPositive` and `ProbablyPositive` for handling positive integer, we just have `Natural`.
I want something like that but for text conversions.

I thought about this for a while and played around with some different approaches.
One thing I tried that I ended up liking was tagging the text rather than the byte string.
It feels backwards, but it seems to work out pretty well.
So the type `WithEncoding Utf8 Text` means that the text will be encoded as UTF-8.

``` hs
instance From (WithEncoding Utf8 Text) ByteString where
  -- from :: WithEncoding Utf8 Text -> ByteString
  from textWithEncoding =
    Encoding.encodeUtf8 (withoutEncoding textWithEncoding)
```

Actually using the instance is pretty nice too.
You start with a regular `Text` value, then you go through this type that selects the encoding, and you end up with a regular `ByteString`.

``` hs
from (withEncoding Utf8 (Text.pack "\955")) :: ByteString
-- "\206\187"
```

But the great thing about this approach is that the wrapped type makes just as much sense when you're going in the other direction.

``` hs
instance TryFrom ByteString (WithEncoding Utf8 Text) where
  -- tryFrom :: ByteString -> WithEncoding Utf8 Text
  tryFrom byteString =
    case Encoding.decodeUtf8' byteString of
      Right text -> Just (withEncoding Utf8 text)
      Left _ -> Nothing
```

The end result is wrapped, but it's easy enough to unwrap it using `withoutEncoding`.

``` hs
tryFrom (ByteString.pack [207, 187]) :: Maybe (WithEncoding Utf8 Text)
-- Just "\955"

tryFrom (ByteString.pack [255]) :: Maybe (WithEncoding Utf8 Text)
-- Nothing
```

I think that the meaning of `WithEncoding Utf8 Text` is clearer than the old `WithEncoding Utf8 ByteString`.
The wrapped `Text` value means that it's a regular `Text` value except that encoding or decoding will use UTF-8.
And you can more easily do things like decode a byte string as CP-1252, then turn around and encode it as UTF-8.

## Questions

I feel like this is a better approach, but I wanted to solicit feedback before committing to it in the Witch library.
So please let me know what you think!
The core question I'm seeking an answer to is:
Should I tag `ByteString` or `Text` values?

I'm also wondering if I should use a custom data type like `WithEncoding` or just go with the `Tagged` type from the `tagged` library.
Similarly I don't know if it would be better to use data types like `Utf8` for encoding or to enable `DataKinds` and use symbols like `"UTF-8"`.
What do you think?
