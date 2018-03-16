---
title: Record fields break smart constructors
---

In Haskell many data types are correct by construction.
However it's not always desirable to push invariants into the type system.
For example let's say we want a type that only contains even numbers.
We could use something like Peano numbers to make the type correct by construction.
Unfortunately that would could up being a little clunky.

A common alternative is what's known as smart constructors.
Instead of exposing a type's constructors directly,
we will only expose functions for working with that type.
Those functions are responsible for maintaining any invariants.
Here's an example of a type for even numbers using smart constructors.

``` haskell
module EvenModule
  ( EvenType
  , evenField
  , evenConstructor
  ) where

newtype EvenType = UnsafeEvenConstructor
  { evenField :: Integer
  } deriving Show

evenConstructor :: Integer -> Maybe EvenType
evenConstructor number =
  if even number
    then Just (UnsafeEvenConstructor number)
    else Nothing
```

By hiding the actual constructor and only exposing the smart constructor function,
we can ensure that only even values occupy this type.

``` haskell
>>> evenConstructor 1
Nothing
>>> evenConstructor 2
Just (UnsafeEvenConstructor { evenField = 2 })
```

Furthermore it's possible to get the underlying number from the type by using the exported record field.

``` haskell
>>> let Just valid = evenConstructor 2
>>> valid
UnsafeEvenConstructor { evenField = 2 }
>>> evenField valid
2
```

Unfortunately the record field also allows us to do a record update,
which can break the invariants imposed by our smart constructor.
This means we can put an odd number into this type that is only supposed to hold even numbers.

``` haskell
>>> let invalid = valid { evenField = 3 }
>>> invalid
UnsafeEvenConstructor { evenField = 3 }
>>> evenField invalid
3
```

This effectively defeats the entire point of using smart constructors.
But fear not!
This problem is very easy to fix.
Instead of exporting the record field,
you can export a simple function.
Even if the function is an alias for the record field,
it will not allow record updates.
Here's how you could redefine the data type given above to avoid this problem.

``` haskell
newtype EvenType = UnsafeEvenConstructor
  { unsafeEvenField :: Integer
  } deriving Show

evenField :: EvenType -> Integer
evenField = unsafeEvenField
```

You could also avoid record fields entirely by using positional arguments and pattern matching.
The end result is the same,
so use whichever one you prefer.

``` haskell
newtype EvenType
  = UnsafeEvenConstructor Integer
  deriving Show

evenField :: EvenType -> Integer
evenField (UnsafeEvenConstructor number) = number
```

In either case you should now have a smart constructor that can't be subverted by record updates.

``` haskell
>>> let Just valid = evenConstructor 2
>>> let invalid = valid { evenField = 3 }
<interactive>:2:23: error:
    • ‘evenField’ is not a record selector
```

As shown, record fields can break smart constructors in Haskell.
So if you're defining a smart constructor,
be sure not to export any record fields that could break it!
