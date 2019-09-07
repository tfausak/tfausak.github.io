---
title: Announcing the `list-singleton` package
---

Have you ever found yourself typing the five-character Haskell expression
`(:[])` and thinking to yourself, "I wish I could add an external dependency
for this"? If so, great news! I published [the `list-singleton` package][1]
today and it does just that.

Seriously though, I'm on a quest to add this `singleton` function to
`Data.List` (and `Data.List.NonEmpty`):

``` haskell
singleton :: a -> [a]
singleton a = [a]
```

You're probably thinking that there are plenty of ways to make singleton lists
already. You're right! However I think it would be useful to have a documented,
monomorphic function for this exact use case. Check out [the `singleton`
documentation][2] for a deep dive.

I submitted [a proposal][3] to the Core Libraries Committee nearly a month ago.
Although it generated a fair amount of discussion, it doesn't seem to me like
the CLC is any closer to deciding on it.

That inaction made me grumpy, so I decided to create something rather than
complain about it. Thus `list-singleton` was born! I don't really expect anyone
to depend on this library for one tiny function. I made it to show what the
implementation and documentation could look like. Also it should show up when
someone searches Hoogle for `singleton` or `a -> [a]`, and then the
documentation can point that person in the right direction.

[1]: https://hackage.haskell.org/package/list-singleton
[2]: https://hackage.haskell.org/package/list-singleton-1.0.0.0/docs/Data-List-Singleton.html#v:singleton
[3]: https://mail.haskell.org/pipermail/libraries/2019-August/029801.html
