---
layout: post
title: Cloning 2048 in Haskell
---

I wrote a clone of 2048 in Haskell.
It's called hs2048 and it's on Hackage.
I had fun doing it and want to share what I learned along the way.
So let's get started!

The most basic part of the game is the tiles.
Each tile is either empty or has a number.
That sounds like a perfect fit for a `Maybe` type.

``` hs
type Tile = Maybe Int
```

There's no need for a full-blown type.
A type synonym will do the job.
We can also get away with using `Int`s instead of `Integer`s.
They work for values up to 2^29-1, which is more than twice as much as we need.

The next most basic component is a row or column of tiles.
The only difference between them is their orientation.
That means we can represent them as the same type.
Let's call it a `Vector`.
It's nothing more than a list of tiles.

``` hs
type Vector = [Tile]
```

The last thing we'll need a type for is the board itself.
It's just a list of vectors.
So let's make another type synonym.

``` hs
type Board = [Vector]
```

Alright!
With three lines of code we've got all the types covered.
Let's move on to the behavior.

The obvious starting point is moving the board.
Each row or column can be considered individually.
So let's write a function that takes a vector and returns it moved to the left.
We'll get around to moving in the other directions soon.

``` hs
shift :: Vector -> Vector
```

A simple enough type signature.
We're off to a good start.

How does movement work in the game?
All the non-empty tiles move to the left.
This is the same as removing the `Nothing`s from the list.

``` hs
import Data.Maybe
-- catMaybes :: [Maybe a] -> [a]
shift v = catMaybes v
```

Now our vector has no `Nothing`s in it.
The next step of the move is combining like tiles.
Specifically, the first two like tiles of any run of similar tiles are added together.
That means `2 2 2 2` gives you `4 2 2` and `2 2 4 4` gives you `4 8`.

This step can be broken in two.
First let's group like tiles together.

``` hs
import Data.List
-- group :: Eq a => [a] -> [[a]]
shift v = group (catMaybes v)
```

We've transformed our vector into a list of lists of tiles.
The values of the tiles in each sub-list are the same.
For example, `[2, 2, 4, 4]` becomes `[[2, 2], [4, 4]]`.

Let's write a helper function that takes a list like that and adds the first two elements together if they exist.

``` hs
add :: Num a => [a] -> [a]
add (x : y : rest) = x + y : rest
add xs = xs
```

Let's use that in our `shift` function.

``` hs
shift v = map add (group (catMaybes v))
```

Not too bad!
Believe it or not, the hard part is done.
We still have two things left to do, though.
We need to transform this into the right type.
Right now we have `[[a]]` and we want `[Tile]`.
After that we'll need to pad the vector out with the right number of tiles.

First let's get rid of a level of nesting by concatenating our lists together.

``` hs
shift v = concatMap add (group (catMaybes v))
```

Now that we've got a list of `Int`s, we can transform them back into `Tile`s.
Remember that `Tile` is just a synonym for `Maybe Int`.

``` hs
shift v = map Just (concatMap add (group (catMaybes v)))
```

Sweet!
Now we've got our list of tiles.
The only problem is it's the wrong length.
If we started with `2 2 4 4`, we ended up with `4 8`.
We're two short!

We can fix that by padding the vector with `Nothing`.
The length we need is the length of our input.
Let's define a helper function for this.
It takes a list, an element, and a length.
It returns a new list of the specified length, padded with the given element.

``` hs
pad :: [a] -> a -> Int -> [a]
pad xs x n = take n (xs ++ repeat x)
```

Now let's use this function to get our vector to the right length.

``` hs
shift v = pad v' Nothing (length v)
  where
    v' = map Just (concatMap add (group (catMaybes v)))
```

And we're done!
Another nine lines of code was all it took for the core game logic.

- moving in other directions
- score
