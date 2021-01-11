---
title: How to define JSON instances quickly
---

Working on a large Haskell project means grappling with GHC's long compile times. This can be especially frustrating when doing iterative development. To help with that, Matt Parsons recently posted [some tips for keeping compilation fast](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html). He suggests ways to organize types, instances, and modules in order to speed up the build. In a similar vein, a few years ago I posted about how [deriving type class instances](https://taylor.fausak.me/2017/08/09/deriving-type-classes-in-haskell-is-slow/) takes longer than you might think. 

Today I'm going to explore various methods for defining type class instances and their relative performance. Is it faster to write instances by hand or to derive them? Should you put everything in one module or put each type in its own? Is it always faster to use more threads? How long does optimization take? Let's find out.

## Data types

If we're going to talk about the performance of deriving type class instances, we'll need some types to derive the instances for. Fortunately I maintain [Rattletrap](https://hackage.haskell.org/package/rattletrap-9.1.1), a command-line program that parses Rocket League replays and outputs JSON. As you might expect, it defines a bunch of types. There's nothing too surprising about them. They fall into three main categories: simple type wrappers, records with fields, and enums with constructors. For example:

``` hs
-- simple type wrapper
newtype Word32le
  = Word32le Word32

-- record with fields
data Version = Version
  { major :: Word32le
  , minor :: Word32le
  , patch :: Maybe Word32le }

-- enum with constructors
data RemoteId
  = PlayStation Text [Word8]
  | Steam Word64le
  | Xbox Word64le
```

## Type classes

There are many type classes that you can define instances for. We're going to focus on [Aeson](https://hackage.haskell.org/package/aeson-1.4.7.1)'s `ToJSON` and `FromJSON` type classes. Since Aeson is the most popular JSON library on Hackage, you're probably going to need these type classes at some point. Plus they're easy to implement but still interesting enough to avoid being trivial. If you're not already familiar with them, here they are:

``` hs
class ToJSON a where
  toJSON :: a -> Value
  toEncoding :: a -> Encoding

class FromJSON a where
  parseJSON :: Value -> Parser a
```

The extra `toEncoding` method on the `ToJSON` type class is there for performance reasons. It encodes directly without creating an intermediate value. 

## Instances

I won't get into what the instances do too much since that's not really the point of this post. But when it comes to implementing the instances, you have at least three options:

1.  Write the instances by hand. This is tedious and error prone, but also straightforward and easy to understand.

    ``` hs
    instance ToJSON Version where
      toJSON x = object
        [ "major" .= major x
        , "minor" .= minor x
        , "patch" .= patch x ]
      toEncoding x = pairs
        ( "major" .= major x
        <> "minor" .= minor x
        <> "patch" .= patch x )

    instance FromJSON Version where
      parseJSON = withObject "Version" $ \ x -> Version
        <$> x .: "major"
        <*> x .: "minor"
        <*> x .: "patch"
    ```

2.  Derive the instances using generics. This requires the `DeriveGeneric` and `DeriveAnyClass` extensions, along with the `GHC.Generics` module. It doesn't require any special syntax and behaves the same as deriving more typical type classes like `Eq` and `Show`.

    ``` hs
    {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
    import GHC.Generics
    data Version = Version
      { -- omitted, but same as before
      } deriving (Generic, ToJSON, FromJSON)
    ```

3.  Derive the instances using Template Haskell. Unsurprisingly this requires the `TemplateHaskell` extension. It uses a different syntax and runs custom code at compile time.

    ``` hs
    {-# LANGUAGE TemplateHaskell #-}
    $( deriveJSON defaultOptions ''Version )
    ```

## Details

Since this is a benchmark, I'm going to be pedantic about how I'm running things. Feel free to skip this section if you don't care. 

I extracted 77 data types from the Rattletrap project. I put them into a Haskell script for generating code in various configurations. I uploaded the code to [this Gist](https://gist.github.com/tfausak/a5cae9e41e5ccd0b0a5b4e49f1e2104d/55e603e732f3fb07b6fd9f164f119177cfd2229e) so you can run it for yourself. 

I ran the code using Stack 2.3.1 with the LTS-15.13 snapshot, which includes Aeson 1.4.7.1. Instead of using the provided version of GHC, I upgraded to 8.10.1. 

I ran the code on two desktops: a Windows machine with an Intel Core i5-8400 (6 cores, 6 threads) and an Ubuntu machine with an AMD Ryzen 5 2400G (4 cores, 8 thread). Both machines use SSD storage.

All measurements are given in seconds. Times are taken from all the configurations listed in this post. Initially I ran each configuration three times, but the times didn't change much between runs. So in the end I ran each configuration only once.

All compilation was done from scratch using `-fforce-recomp` along with `-v0` and `-w`.

## Optimization

GHC offers three optimization levels, from least optimized to most: `-O0`, `-O1`, and `-O2`. Usually development builds are done with `-O0` and production builds are either done with `-O1` or `-O2`. 

`-On` | Minimum | Average | Maximum
--- | --- | --- | ---
0 | 1.75 | 4.74 | 8.03
1 | 15.93 | 30.09 | 48.57
2 | 19.32 | 32.79 | 52.25

Unsurprisingly `-O0` is by far the fastest. For quick feedback from the compiler, make sure you're disabling optimizations by setting `-O0`. For production builds, this workload didn't show a big difference between `-O1` and `-O2`. (Note that I only analyzed compile time performance, not run time.) 

## Layout

When writing code, you can choose to put everything in one module or to put each thing in its own module. (You can, and probably do, do something in between, but I didn't benchmark that.) Working in a single module can be easier to reason about, since you don't have to jump around to different files.

Layout | Minimum | Average | Maximum
--- | --- | --- | ---
Single module | 2.34 | 25.28 | 52.25
Multiple modules | 1.75 | 19.81 | 48.55

Splitting up your code into multiple modules will almost always make your code faster to compile. Even if it didn't, it would still be a good idea because after the first compile you'd only have to recompile the things that changed. 

The only time that a single module is faster than multiple modules is when you only have one core/thread at your disposal. And even then it's not too much faster.

## Jobs

GHC can use multiple threads when compiling. This seems like it should help a lot, since compiling each module can be done more or less in isolation. 

`-jn` | Minimum | Average | Maximum
--- | --- | --- | ---
1 | 2.34 | 26.75 | 49.56
2 | 2.33 | 22.50 | 48.92
4 | 1.75 | 20.17 | 50.23
8 | 1.81 | 20.75 | 52.25

As expected, using more cores is faster, up to a point. On my machine, going from 4 to 8 threads slowed things down rather than speeding them up. 

Averaging these measurements downplay how significant of an impact they can have on your compile times. For example when compiling multiple modules with hand written instances at `-O1`, going from `-j1` to `-j4` makes the build nearly twice as fast (from 31 seconds to 17). 

## Implementation

As explained in the intro, I looked at three different implementation strategies: writing instances by hand, deriving them with Template Haskell, and deriving them with generics. 

Implementation | Minimum | Average | Maximum
--- | --- | --- | ---
Manual | 1.75 | 17.37 | 35.72
TH | 3.79 | 21.27 | 40.79
Generics | 4.32 | 29.00 | 52.25

In spite of its reputation as being a heavyweight, Template Haskell easily beats out generics when it comes to speed of deriving instances. However both of them are absolutely crushed by hand written instances. If you want compilation to be as fast as possible, you should write your own instances. Otherwise you should probably use TH.

That being said, Template Haskell does have one major downside when compared to generics: It recompiles more often. Let's say module `A` depends on module `B`. If you change module `B`, module `A` will be compiled if it contains TH. If it doesn't contain TH, it may not be recompiled. (It depends on what changes you made to `B`.) So if you frequently change modules below ones with TH in the hierarchy, you may be better off using generics.

## Conclusion

When it comes to type class instances in Haskell, if you want compilation to be fast you should:

- Compile with `-O0` for development.
- Put each type in its own module.
- Use lots of threads, but not too many. (`-j4` is probably a good default.)
- Prefer Template Haskell over generics. (Write them by hand if you don't mind the tedium.) 

## Future work

Are there other build configurations I should have benchmarked? Let me know and I'll try them out! 

I had a hard time slicing and dicing these numbers. If you want to take a deeper look at the data, I encourage you to open [the spreadsheet](https://docs.google.com/spreadsheets/d/1jqtvvMjwKXJmLV7Nc5LRaGbwkcGr6Q03Xvo4LtqtxFI/edit?usp=sharing) and poke around. 

I think it would be interesting to explore using [GHC source plugins](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/extending_ghc.html#source-plugins) to derive instances quickly without forcing recompilation as often as Template Haskell. 
