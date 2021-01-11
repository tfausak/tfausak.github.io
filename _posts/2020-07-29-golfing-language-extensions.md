---
title: Golfing language extensions
---

Recently Baldur Blöndal posted [an interesting challenge](https://twitter.com/iceland_jack/status/1287682449863041024) on Twitter:

> Challenge: trigger many language extensions using fewest characters

When I first saw this there were already a few responses. I played with them a little to produce this monstrosity:

``` haskell
x# ∷_=id @(_∷_)mdo if|let?_=(0x0.0,'(),)→[0b_0^0e0| !_←[]|_←[],then\case]
```

Those 73 characters require 21 language extensions to compile correctly! This blog post aims to quickly explain what that code does by showing how to require various language extensions with the least amount of code.

Before we get started, a couple notes:

- The challenge was to require the most language extensions with the fewest characters. I would be happy to be proven wrong, but I think you can't do much better than about 10 extensions with 23 characters. Most things have so much syntactic overhead that it screws up the ratio.

- Because of that, I decided to change the challenge into roughly: "How many extensions can you require from a single 80-character line of code?" This allows showing off more extensions even though they require more than about two characters.

- I'm doing all of this with GHC 8.10 on Ubuntu. My goal is to produce a program that compiles. I don't care about warnings, runtime errors, or portability. The code should fail to compile without each extension. Duplicate extensions like `RankNTypes` and `Rank2Types` don't count.

Alright. Let's get started.

## Baseline

Since we're playing some variant of code golf here, the idea is to use the fewest number of characters. We should set a baseline for the smallest program that will compile. Since GHC will compile an empty program, that's a total of zero characters. But that's not very interesting.

The original challenge looked for a high ratio of extensions to characters, so I thought it made sense to use a regular value declaration, like this:

``` hs
-- shortest non-empty program ghc compiles
x=0
```

There's almost no overhead with that. You have to come up with a name and a value, but those can probably be used later. The equals sign is true overhead, but it's less than other approaches like types or classes.

``` hs
-- these ones are a little longer
type T=()
class C a
instance Show()
```

## [MagicHash](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-MagicHash)

The first thing we can do is add a hash to the end of our value's name to require the `MagicHash` extension. Like many of the extensions we'll encounter, this is purely syntactic. It simply allows you to name things with a hash after them.

``` hs
x#=0
```

## [ScopedTypeVariables](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-ScopedTypeVariables)

Typically you would give a top-level declaration a standalone type signature. Or perhaps you would annotate its value. But did you know that the `ScopedTypeVariables` extension allows you to provide the signature inline? It's true!

``` hs
x#::Int=0
```

Normally this extension is used to add type signatures to polymorphic local bindings, but it works just as well for our purposes. Here's an example of its more typical usage:

``` hs
-- typical scoped type variable usage
main = do
  let zero :: Int = 0
  line :: String <- getLine
  print (zero, line)
```

## [UnicodeSyntax](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-UnicodeSyntax)

That pesky type signature is taking up two whole characters! We can do better. The `UnicodeSyntax` language extension allows `::` to be replaced by `∷`, which is only a single character.

``` hs
x#∷Int=0
```

Note that code golf usually counts the number of bytes to discourage exploiting Unicode like this. 

## [PartialTypeSignatures](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-PartialTypeSignatures)

It's kind of annoying that we have `Int` in there. It takes up three characters without really requiring any language extensions itself. Fortunately `PartialTypeSignatures` lets us save two characters and require another extension.

``` hs
x# ∷_=0
```

Note that we have to put a space after the hash otherwise this would compile without the `MagicHash` extension because `#∷` would be interpreted as an operator.

This gets GHC to infer the type even though we have an explicit type signature. In normal usage this is used to specify a small part of a larger type.

## [TypeApplications](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-TypeApplications)

We've been focused on the left side of the equals sign. Let's turn to the right side. First let's introduce an unnecessary call to `id`. It doesn't do anything by itself, but we'll expand on it in a second.

``` hs
x# ∷_=id 0
```

Since `id` is polymorphic, we can use `TypeApplications` to pick a concrete type for it. But just like before we don't want to actually pick a type because that will take too many characters to write out. Instead we'll let GHC infer it.

``` hs
x# ∷_=id @_ 0
```

Although the type application looks like it would require `PartialTypeSignatures`, it doesn't. Note that there must be a space before the type application otherwise it won't compile.

## [KindSignatures](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-KindSignatures)

We have one last type-level trick up our sleeve. In addition to giving type signatures to values, we can give kind signatures to types. Since we have two type signatures, we have to pick which one to decorate with a kind signature. Each case would require parentheses, so we'll add it to the type application since it will allow us to remove the space before the `0`.

``` hs
x# ∷_=id @(_∷_)0
```

Curiously this kind signature doesn't require `PartialTypeSignatures` either. Also note that we're still using the Unicode `∷` instead of `::`.

## [BlockArguments](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-BlockArguments)

Normally if you want to pass a `do` block to a function you either have to use parentheses (`f (do ...)`) or some operator (`f $ do ...`). The recent `BlockArguments` extension allows you to avoid any decoration around your argument.

``` hs
x# ∷_=id @(_∷_)do 0
```

## [RecursiveDo](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-RecursiveDo)

I've never used this extension, so I don't have much to say about it. By changing our `do` to an `mdo`, we can require another extension. The mechanics of regular versus recursive `do` binds don't matter since we're not using them anyway.

``` hs
x# ∷_=id @(_∷_)mdo 0
```

## [BinaryLiterals](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-BinaryLiterals)

At long last we come to that `0` at the far right side. It's not requiring any extensions. Let's fix that! 

By default Haskell lets you write octal (`0o0`) and hexadecimal (`0x0`) literals. With the `BinaryLiterals` extension you can also write numbers in binary.

``` hs
x# ∷_=id @(_∷_)mdo 0b0
```

## [NumericUnderscores](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-NumericUnderscores)

The `NumericUnderscores` extension lets you put underscores in numeric literals, as you might have guessed from the name. We can take advantage of this by adding another underscore that will require another extension.

``` hs
x# ∷_=id @(_∷_)mdo 0b_0
```

## [NumDecimals](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-NumDecimals)

There's another extension to the numeric literal syntax that allows you to write integral values using scientific notation. This is useful for writing really big numbers without adding a bunch of zeroes.

Unfortunately this one is a little tricky to actually require. Simply writing a number like `1e2` will infer a `Fractional` constraint and compile just fine without the extension. We need a way to write a number like `1e2` but also forbid it from being `Fractional`. And we need to do all that without eating up too many characters.

Fortunately there's a handy operator in the `Prelude` that puts the right constraints on its arguments:

``` hs
(^) :: (Num a, Integral b) => a -> b -> a
```

So we can put a number that requires the `NumDecimals` extension as the second argument to `(^)` and we should be good.

``` hs
x# ∷_=id @(_∷_)mdo 0b_0^0e0
```

## [ImplicitParams](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-ImplicitParams)

At this point it starts to feel like we've run out of easy wins. Time to reach for something that adds a bunch of characters but also adds a bunch of flexibility: `ImplicitParams`.

This is going to add a bunch of characters because we're forced to also add a `let ... in ...` expression in order to bind the implicit parameter. That's alright though, we'll make up for it later.

``` hs
x# ∷_=id @(_∷_)mdo let?_=()in 0b_0^0e0
```

## [MultiWayIf](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-MultiWayIf)

It would be nice if we could replace that `let ... in ...` with something more exotic. Fortunately multi-way `if`s don't have a lot of overhead themselves and allow us to exploit a quirk of Haskell's syntax. This will take a little explaining.

When you use guard syntax, the expression must return a `Bool`. You're allowed to put any expression in there, including something with `let ... in ...` as part of it. For example:

``` hs
unit = case "example" of
  string
    | let size = 7 in length string == size
      -> ()
```

The weird thing about this is that you can have `let` without `in` when you're in a guard clause. The following example is completely valid and compiles without warnings:

``` hs
unit = case "example" of
  string
    | let size = 7
      -> ()
```

The guard clause is effectively empty and always evaluates to `True` even though we never produced a `Bool` from it. We can use this trick to introduce a `let` binding without paying for the corresponding `in`.

However that would hardly be worth it if we had to use `case` because that introduces so many other characters. Thankfully multi-way `if`s are more compact. Bringing it all together we can replace our boring `let?_=()in` with the novel `if|let?_=()→`.

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=()→0b_0^0e0
```

Note that we're using the Unicode `→` arrow instead of the ASCII `->` to save a character.

## [HexFloatLiterals](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-HexFloatLiterals)

Once again we are going to abuse numeric literals for our game of code golf. This time we'll be writing a floating point literal using hexadecimal syntax, which requires an extension.

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=0x0.0→0b_0^0e0
```

## [TupleSections](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-TupleSections)

Why stop there? Now that we have an unused binding just sitting there, we can cram a bunch of interesting values into it. We'll start by converting it into a tuple section, which is a concise way of writing a lambda that produces a tuple.

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=(0x0.0,)→0b_0^0e0
```

## [TemplateHaskellQuotes](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-TemplateHaskellQuotes)

Somehow we've made it this far without introducing any Template Haskell or quasi-quotes. As far as I can tell the easiest way to require some form of TH is to use the quoted name syntax. We'll be grabbing the name of `()` because it's the shortest constructor name available.

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=(0x0.0,'(),)→0b_0^0e0
```

## [BangPatterns](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-BangPatterns)

This one requires a bit of a walk. It feels like we should have been able to require a bang pattern by now, but I don't think they're allowed on any of the binders we have. We need to introduce a new binder that can have a bang pattern on it. We're not going to do this in the most efficient way, but hopefully it'll be worth it later.

We're going to use a list comprehension to introduce a new binding. As mentioned, this is a bunch extra characters.

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=(0x0.0,'(),)→[0b_0^0e0| !_<-[]]
```

Note that we need a space before the exclamation point otherwise `|!` would be interpreted as an operator.

## [ParallelListComp](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-ParallelListComp)

Fortunately for us, list comprehensions have a few associated language extensions. By adding five more characters we can introduce a parallel list comprehension. 

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=(0x0.0,'(),)→[0b_0^0e0| !_←[]|_←[]]
```

## [TransformListComp](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-TransformListComp)

I've got to say, this is one of the weirder extensions. It augments list comprehensions with a SQL-like syntax for querying data. But that doesn't really matter for us. All we care about is how many characters it takes to require another extension. Answer: eight.

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=(0x0.0,'(),)→[0b_0^0e0| !_←[]|_←[],then id]
```

## [LambdaCase](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-LambdaCase)

We're nearly running out of room on our 80-character line. How can we add more language extensions? There's another lightweight syntactic extension to reach for: `LambdaCase`. It lets us write a short lambda and `case` together without naming the lambda's variable.

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=(0x0.0,'(),)→[0b_0^0e0| !_←[]|_←[],then\case x→x]
```

## [EmptyCase](https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#extension-EmptyCase)

The actual body of the `case` is kind of annoying. It's taking up five characters without contributing anything! Wouldn't you know it, the `EmptyCase` extension lets us get rid of that.

``` hs
x# ∷_=id @(_∷_)mdo if|let?_=(0x0.0,'(),)→[0b_0^0e0| !_←[]|_←[],then\case]
```

---

So there you have it. That's as many language extensions as I can figure out how to require on a single 80-character line of Haskell code. Can you do better? Are there tricks that I'm missing? Let me know! 

---

Here's the whole thing written with slightly more normal formatting:

``` hs
{-# language BangPatterns #-}
{-# language BinaryLiterals #-}
{-# language BlockArguments #-}
{-# language EmptyCase #-}
{-# language HexFloatLiterals #-}
{-# language ImplicitParams #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language MagicHash #-}
{-# language MultiWayIf #-}
{-# language NumDecimals #-}
{-# language NumericUnderscores #-}
{-# language ParallelListComp #-}
{-# language PartialTypeSignatures #-}
{-# language RecursiveDo #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskellQuotes #-}
{-# language TransformListComp #-}
{-# language TupleSections #-}
{-# language TypeApplications #-}
{-# language UnicodeSyntax #-}

x# ∷ _ = id @(_ ∷ _) mdo
  if | let ?_ = ( 0x0.0, '(), ) →
      [ 0b_0 ^ 0e0
      | !_ ← []
      | _ ← []
      , then\case
      ]
```
