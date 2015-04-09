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

[announced blunt]: {% post_url 2015-04-02-announcing-blunt-a-pointless-haskell-tool %}
[the `.` operator]: https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
[flow]: /flow/
[the base package]: https://hackage.haskell.org/package/base

- first up is `takes`, a sliding window function
- naively might write this:
  ``` hs
  takes :: Int -> [a] -> [[a]]
  takes _ [] = [[]]
  takes n xs@(_ : ys) = take n xs : takes n ys
  ```
- someone more familiar with haskell might write this instead:
  ``` hs
  import Data.List (tails)
  takes n = map (take n) . tails
  ```
- i'm proposing this style with flow
  ``` hs
  import Flow
  takes' n xs = xs |> tails |> map (take n)
  -- takes' n = tails .> map (take n) -- equivalently
  ```
- this closely follows how i read and develop functions
- now for the meat of the problem
- in traditional pointfree style
  ``` hs
  import Data.Char (digitToInt, isDigit)
  euler8 :: Int -> String -> Int
  euler8 n = maximum . map product . takes n . map digitToInt . filter isDigit
  ```
- and in the flow style
  ``` hs
  euler8' n x = x |> filter isDigit |> map digitToInt |> takes n |> map product |> maximum
  ```
