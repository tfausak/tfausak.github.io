---
title: You should be using Hackage tokens
---

rather than using `cabal upload --username foo --password bar`, you should be using `cabal upload --token qux`.
this is more secure, especially on CI, because you don't have to store your password in plaintext.
also tokens can be generated and revoked at will.

api tokens were implemented in the hackage server in 2018: https://github.com/haskell/hackage-server/pull/534

the `--token` option was added to cabal's upload sub-command in 2023: https://github.com/haskell/cabal/pull/9058

you can create tokens here: https://hackage.haskell.org/user/mercury/manage
