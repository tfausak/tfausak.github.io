---
title: Let's make a standard library!
---

- {% post_url 2015-10-22-better-know-a-language-purescript %}
- https://github.com/tfausak/purescript-neon
- 0.0.x basically only changed names
- 0.1.x removed class hierarchy
- 0.2.x reordered function arguments and added `..` operator
- 0.3.x replaced `..` with `:` and increased the precedence

``` purescript
import Neon (..)
main :: Eff (console :: CONSOLE) Unit
main = 1
  :upTo 999
  :filter (divisibleBy 3 || divisibleBy 5)
  :sum
  :print
```
