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

[announced blunt]: {% post_url 2015-04-02-announcing-blunt-a-pointless-haskell-tool %}
[the `.` operator]: https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#v:.
[flow]: /flow/
[the base package]: https://hackage.haskell.org/package/base

- it borrows from f# (and therefore elm and elixir)
- it defines some functions (`apply`, `compose`, and `apply'`)
- these are usually only available as operators
- having functions make some higher-order functions clearer
- for example `map (apply 2) [(+ 2), (* 2), (^ 2)]`
- it also defines some operators
- for `apply`: `|>` (pipe forward) and `<|` (pipe backward)
- for `compose`: `.>` (compose forward) and `<.` (compose backward)
- for `apply'`: `!>` (strict pipe forward) and `<!` (strict pipe backward)
- that's it
- i understand haskell's style and how to make and read pointfree functions
- but i often prefer writing code that shows how data flows through it
- for a more concrete example, let's check out project euler #8
- <https://github.com/tfausak/project-euler/blob/42d0c19/haskell/pe008.hs>
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
- flow style reads left-to-right instead of right-to-left
- could be written with `&` from lens or most recent prelude
- the `$` and `&` don't mean anything to me, so it's hard to grok
