---
title: Neon: An alternative PureScript prelude
---

- {% post_url 2015-10-22-better-know-a-language-purescript %}
- https://github.com/tfausak/purescript-neon
- 0.0.x basically only changed names
- for instance, Semigroup became HasAdd
- 0.1.x removed class hierarchy
- so HasZero (Monoid) does not require HasAdd (Semigroup)
- 0.2.x reordered function arguments and added `..` operator
- put "subject" last, so `add y x` means `x + y`, not `y + x`
- `..` operator made things read better as `x .. add y`
- 0.3.x replaced `..` with `:` and increased the precedence
- much more compact than previous operator `x :add y`
- behaves better with operators `a :add b ^ c :add d`
- gets parsed as `(a + b) ^ (c + d)`

``` purescript
import Neon
main :: Eff (console :: CONSOLE) Unit
main = 1
  :upTo 999
  :filter (divisibleBy 3 || divisibleBy 5)
  :sum
  :print
```

- Functions should take their subject last. This means `add x y` is really
  `y + x`. Consider calling functions with `:`, like `y :add x`.

- Type classes should be lawless. This means `zero` doesn't have to be the
  additive identity. That's recommended, but not necessary.

- There should be no type class hierarchy. This means `Zero` does not imply
  `Add`. If you need both, add both to your type signature.

- There should be as few operators as possible. This means `<$>` does not
  exist. Use `map` instead.

- There should be one obvious way to do things. This means `return` is not an
  alias for `wrap`. In fact, it doesn't exist at all.

- Functions should be defined in type classes. This means `add` can be used for
  both numbers and strings.

- Type classes should be as small as possible. This means the `Bounded` type
  class is split into `Bottom` and `Top`.

- Type classes should be designed for programmers, not mathematicians. This
  means `Add` is a semigroup, but it's not called `Semigroup`.

- Pure functions should not throw exceptions. This means `fromInt` returns a
  `Maybe` value. Pure functions that throw exceptions should be marked unsafe.

- Qualified imports are annoying, and fewer imports are better. This mean
  `import Neon` is enough. No need for tens of lines of imports.
