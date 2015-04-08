---
title: Write more understandable Haskell with Flow
---

- announced blunt last week
- dont actually like pointfree style
- built a library to help
- its called flow
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
