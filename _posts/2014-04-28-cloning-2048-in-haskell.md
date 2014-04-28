---
layout: post
title: Cloning 2048 in Haskell
---

![A screenshot of hs2048][1]

I wrote a clone of [2048][2] in Haskell. It's called [hs2048][3] and it's on
Hackage. I had fun doing it and want to share what I learned along the way.

We're going to start with the types, then implement the core functionality, and
finally write some helpers to make things easier. Let's get started!

## Types

The most basic part of the game is a tile. It can be empty or have a number in
it. That sounds like a perfect fit for the `Maybe` type.

{% highlight hs %}
type Tile = Maybe Int
{% endhighlight %}

Up next are rows and columns. They're fundamentally the same, so let's
represent them with the same type. It's nothing more than a list of tiles.

{% highlight hs %}
type Vector = [Tile]
{% endhighlight %}

Last up is the whole board. It's nothing more than a list of vectors.

{% highlight hs %}
type Board = [Vector]
{% endhighlight %}

By convention, we'll consider it to be row-major. That means the vectors
represent rows. Therefore the top left tile is the first element in the first
vector.

## Logic

You can move in any of the cardinal directions. Each direction follows the same
rules, so we'll implement one direction and generalize later. Furthermore, the
rows (or columns) don't interact with each other when you move. That means we
can write the logic for a single vector. After we've done that, we can apply it
to the whole board.

Let's start with the type signature. We'll take a vector and return the vector
moved toward the left.

{% highlight hs %}
shift :: Vector -> Vector
{% endhighlight %}

We're going to build this function up piece by piece. Each step isn't going to
type check, but that's ok.

The first thing we want to do is remove all the blank tiles. This effectively
slides all the tiles to the left.

{% highlight hs %}
import Data.Maybe (catMaybes)
-- catMaybes :: [Maybe a] -> [a]
shift v = catMaybes v
-- shift [Nothing, Nothing, Nothing, Just 2] =
--   [Just 2]
{% endhighlight %}

After that, we need to group up all the similar tiles. This makes it easy to
combine them in the next step.

{% highlight hs %}
import Data.List (group)
-- group :: Eq a => [a] -> [[a]]
shift v = group (catMaybes v)
-- shift [Just 2, Just 2, Just 4, Just 4] =
--   [[2, 2], [4, 4]]
{% endhighlight %}

Now that all the like tiles are together, we need to add the first two tiles of
each group. Let's write a little function to help us out here.

{% highlight hs %}
add :: [Int] -> [Int]
add (x : y : rest) = x + y : rest
add ts = ts
-- add [2, 2, 2, 2] = [4, 2, 2]
{% endhighlight %}

Now let's apply that function to our grouped tiles.

{% highlight hs %}
shift v = map add (group (catMaybes v))
-- shift [Just 2, Just 2, Just 4, Just 4] =
--   [[4], [8]]
{% endhighlight %}

Not bad, but we ended up with a list of lists. We only want a list. Thankfully,
`concat` is all it takes to fix that.

{% highlight hs %}
shift v = concat (map add (group (catMaybes v)))
-- shift [Just 2, Just 2, Just 4, Just 4] =
--   [4, 8]
{% endhighlight %}

We can do a little better, though. `concatMap` does both in one step while
being a little more idiomatic. ([HLint][4] will let you know when you should
use it.)

{% highlight hs %}
shift v = concatMap add (group (catMaybes v)))
-- shift [Just 2, Just 2, Just 4, Just 4] =
--   [4, 8]
{% endhighlight %}

We're one step away from type checking! We need to turn our list of integers
into a list of tiles. We can do that by wrapping each one in `Just`.

{% highlight hs %}
shift v = map Just (concatMap add (group (catMaybes v))))
-- shift [Just 2, Just 2, Just 4, Just 4] =
--   [Just 4, Just 8]
{% endhighlight %}

Even though our function type checks now, it still isn't right. The output
vector isn't the same length as the input vector. We need to pad it with
`Nothing`.

Let's write another little function to help us out. It should take a list, an
element, and a length. Then it should return the list padded to the length with
the element.

{% highlight hs %}
pad :: [a] -> a -> Int -> [a]
pad xs x n = take n (xs ++ repeat x)
-- pad [Just 4, Just 8] Nothing 4 =
--   [Just 4, Just 8, Nothing, Nothing]
{% endhighlight %}

Armed with that, we can finish our function. Let's take our output list and pad
it to get our output vector.

{% highlight hs %}
shift v = pad
    (map Just (concatMap add (group (catMaybes v)))))
    Nothing
    (length v)
-- shift [Just 2, Just 2, Just 4, Just 4] =
--   [Just 4, Just 8, Nothing, Nothing]
{% endhighlight %}

That does the trick, but it is ugly. We can make it a lot cleaner by pulling
out the list we're padding.

{% highlight hs %}
shift v = pad ts Nothing (length v)
  where
    ts = map Just (concatMap add (group (catMaybes v)))
-- shift [Just 2, Just 2, Just 4, Just 4] =
--   [Just 4, Just 8, Nothing, Nothing]
{% endhighlight %}

That's all there is to the game logic! Everything else sits on top of that to
make things easier.

## Helpers

Moving one vector at a time isn't too useful. We want to move the whole board
at once. Since the board is a list of vectors, we can map our function over it.

{% highlight hs %}
shift' :: Board -> Board
shift' = map shift
-- shift' [[Just 2, Just 2], [Just 4, Just 4]] =
--   [[Just 2, Nothing], [Just 4, Nothing]]
{% endhighlight %}

So now we can move the board left. What about the other directions? They can be
handled by rotating the board, moving it, then rotating it back. Let's write
another little function to rotate the board.

{% highlight hs %}
import Data.List (transpose)
-- transpose :: [[a]] -> [[a]]
rotate :: Board -> Board
rotate = map reverse . transpose
-- rotate [[Nothing, Just 2], [Just 4, Just 8]] =
--   [[Just 4, Nothing], [Just 8, Just 2]]
{% endhighlight %}

It happens to rotate the board clockwise. If we want to move down, we have to
rotate, then shift, then undo the rotation by rotating three times. That's
tedious and annoying, though. If you want to see the next step, check out [the
source][5] on GitHub. It's got all that and more.

[1]: /static/images/2014-04-28-hs2048.png
[2]: https://github.com/gabrielecirulli/2048
[3]: http://hackage.haskell.org/package/hs2048
[4]: http://community.haskell.org/~ndm/hlint/
[5]: https://github.com/tfausak/hs2048
