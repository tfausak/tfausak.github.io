---
title: Write more understandable Haskell with Flow
---

Last week, I [announced Blunt][],
a tool for converting Haskell expressions between the pointfree and pointful styles.
I have a secret, though:
I don't like the pointfree style.

I don't like it because I think it's harder to read.
Pointfree expressions don't even mention all of their arguments,
which forces you to know the arity of the functions they call.
And [the `.` operator][] reads from right to left,
meaning your eyes have to jump back and forth when reading code that uses it.

I think Haskell can do better.
That's why I created [Flow][],
a library for writing more understandable Haskell.
Flow provides alternatives to common operators in [the base package][],
like function application with `$`, function composition with `.`, and strict function application with `$!`.
Here's an overview of the functions and operators it defines:

Flow            | Base
--------------- | -------------
`apply x f`     | `f x`
`x |> f`        | `x & f`
`f <| x`        | `f $ x`
`compose f g x` | `g (f x)`
`f .> g`        | `f >>> g`
`g <. f`        | `f . g`
`apply' x f`    | `seq x (f x)`
`x !> f`        | -
`f <! x`        | `f $! x`

Flow borrows from F#,
which defines "pipe forward" and "pipe backward" operators (`|>` and `<|`).
It also borrows "compose forward" and "compose backward" (`>>` and `<<`),
although I renamed them since Haskell already defines `>>`.

In addition to operators,
Flow provides normal functions for application and composition.
I did this for two reasons:
One, every operator needs a name.
The easiest way to do that is to define it as a function.
And two, regular functions can be more convenient with higher-order functions.
Compare `map (apply 2)` to `map ($ 2)` for example.

So that's what Flow provides.
How can we use that to write more understandable Haskell?
Let's look at an example.
[Problem 8][] from Project Euler is a good candidate.
I wrote [a solution][] years ago when I was learning Haskell.
I also recently re-wrote that solution in a more idiomatic, pointfree style.

The first part of the solution is the `takes` function.
It takes a list and returns a sliding window of a given size over the list.
For example:

    >>> takes 2 [1 .. 3]
    [[1,2],[2,3],[3],[]]

When I first wrote this function, I used explicit recursion.

    takes :: Int -> [a] -> [[a]]
    takes _ [] = [[]]
    takes n xs@(_ : ys) = take n xs : takes n ys

Since then, I learned about the pointfree style and the [`tails`][] function.
I would use both if I wrote this function today.

    takes n = map (take n) . tails

So how would you write this function with Flow?
If you want to keep it in the pointfree style,
you can with the compose forward operator.

    takes n = tails .> map (take n)

That's not what I prefer, though.
I like mentioning all of the arguments and showing how they flow through the function.
You can do this with the pipe forward operator.

    takes n xs = xs |> tails |> map (take n)

That may not look like a huge win,
but it fits my mental model much better.
Plus it looks like code that I could've written in GHCi.

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

Programming with Flow works great with bigger functions too.
Let's look at the next piece of this problem, `euler8`.
It takes a window size and a string.
It returns the maximum product of a run of numbers of the given size in the string.

    >>> euler8 2 "123"
    6

The first time I wrote this function,
I didn't know about function composition.
I wrote it with a bunch of parentheses,
then learned about the `$` operator to remove them.

    euler8 :: Int -> String -> Int
    euler8 n x
        = maximum
        $ map product
        $ takes n
        $ map digitToInt
        $ filter isDigit x

Now that I know about the `.` operator,
I would write this function differently.

    euler8 n
        = maximum
        . map product
        . takes n
        . map digitToInt
        . filter isDigit

This looks backwards to me.
To understand it, you have to read bottom to top.
With Flow, you can read it naturally from top to bottom.

    euler8 n x = x
        |> filter isDigit
        |> map digitToInt
        |> takes n
        |> map product
        |> maximum

[announced blunt]: {% post_url 2015-04-02-announcing-blunt-a-pointless-haskell-tool %}
[the `.` operator]: https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
[flow]: /flow/
[the base package]: https://hackage.haskell.org/package/base
[problem 8]: https://projecteuler.net/problem=8
[a solution]: https://github.com/tfausak/project-euler/blob/f3903d4/haskell/pe008.hs
[`tails`]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-List.html#v:tails
