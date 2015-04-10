---
title: Write more understandable Haskell with Flow
---

![Flow's logo][]

Last week, I [announced Blunt][],
a tool for converting Haskell expressions between the pointfree and pointful styles.
I have a secret, though:
I don't like the pointfree style.

I don't like it because I think it's hard to read.
Pointfree expressions don't mention all of their arguments,
which forces you to know the arity of the functions they call.
And [the `.` operator][] reads from right to left,
meaning your eyes have to jump back and forth when reading code that uses it.

I think Haskell can do better.
That's why I created [Flow][],
a library for writing more understandable Haskell.
Flow provides alternatives to common operators in [the base package][],
like function application with `$` and function composition with `.`.
Here's an overview of the functions and operators it defines:

Flow            | Base
--------------- | -------------
`apply x f`     | `f x`
`x |> f`        | `x & f`
`f <| x`        | `f $ x`
`compose f g x` | `g (f x)`
`f .> g`        | `f >>> g`
`g <. f`        | `g . f`
`apply' x f`    | `seq x (f x)`
`x !> f`        | -
`f <! x`        | `f $! x`

Flow borrows from F#,
which defines pipe forward (`|>`) and pipe backward (`<|`) operators.
It also borrows the compose forward (`>>`) and compose backward (`<<`) operators,
although I renamed them since Haskell already defines `>>`.

In addition to operators,
Flow provides functions for application and composition.
I did this for two reasons:
One, every operator should have a name.
And two, regular functions are more convenient to use with higher-order functions.
Compare `map (apply 2)` to `map ($ 2)` for example.

Given all that,
how can Flow be used to write more understandable Haskell?
Let's look at [problem 8][] from Project Euler for an example.

> Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?

I wrote [a solution][] years ago when I was learning Haskell.
I also recently re-wrote that solution in a more idiomatic, pointfree style.
I'll present both solutions here
and convert them to the Flow style.

The first part of the solution is the `takes` function.
It slides a window of the given size over a list.
Let's see an example.

    >>> takes 2 [1 .. 3]
    [[1,2],[2,3],[3],[]]

When I first wrote this function,
I used explicit recursion.

    takes :: Int -> [a] -> [[a]]
    takes _ [] = [[]]
    takes n xs@(_ : ys) = take n xs : takes n ys

Since then,
I learned about the pointfree style and the [`tails`][] function.
Using both leads to the most idiomatic definition of this function.

    takes n = map (take n) . tails

So how can you convert this function to the Flow style,
and what does it get you?
You have two options for converting.
If you want to keep the pointfree style,
you can use the compose forward operator.

    takes n = tails .> map (take n)

If, like me, you don't want to keep the pointfree style,
you can use the pipe forward operator.

    takes n xs = xs |> tails |> map (take n)

Either way, what you get is a function that reads naturally from left to right.
And if you use the pipe forward operator,
your function looks like something you might have written in GHCi.
Consider the following interpreter session.

    >>> let xs = [1 .. 3]
    >>> xs
    [1,2,3]
    >>> xs |> tails
    <interactive>:3:7: Not in scope: ‘tails’
    >>> import Data.List (tails)
    >>> xs |> tails
    [[1,2,3],[2,3],[3],[]]
    >>> xs |> tails |> map (take n)
    <interactive>:6:26: Not in scope: ‘n’
    >>> let n = 2
    >>> n
    2
    >>> xs |> tails |> map (take n)
    [[1,2],[2,3],[3],[]]

This shows how Flow helps you build up complex expressions one piece at a time.
So let's move on to the next piece of this problem, the `euler8` function.
Given a window size *n* and a string, it returns the largest product of *n* consecutive numbers in the string.
Let's take a look at a simple example.

    >>> euler8 2 "123"
    6

The first time I wrote this function,
I didn't know about function composition.
I wrote it with a bunch of parentheses,
then learned about [the `$` operator][] to remove them.

    euler8 :: Int -> String -> Int
    euler8 n x
        = maximum
        $ map product
        $ takes n
        $ map digitToInt
        $ filter isDigit x

Now that I know about the `.` operator,
I would write this function differently.
Like the pointfree definition of `takes` above,
this is the most idiomatic way to write this function.

    euler8 n
        = maximum
        . map product
        . takes n
        . map digitToInt
        . filter isDigit

In spite of that, this looks backwards to me.
You have to read it starting from the bottom in order to understand it.
With Flow, you can read it naturally from top to bottom.

    euler8 n x = x
        |> filter isDigit
        |> map digitToInt
        |> takes n
        |> map product
        |> maximum

[flow's logo]: /static/images/2015/04/09/flow.svg
[announced blunt]: {% post_url 2015-04-02-announcing-blunt-a-pointless-haskell-tool %}
[the `.` operator]: https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
[flow]: /flow/
[the base package]: https://hackage.haskell.org/package/base
[problem 8]: https://projecteuler.net/problem=8
[a solution]: https://github.com/tfausak/project-euler/blob/f3903d4/haskell/pe008.hs
[`tails`]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-List.html#v:tails
[the `$` operator]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:-36-
