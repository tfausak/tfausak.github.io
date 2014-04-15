---
layout: post
title: Cloning 2048 in Haskell
---

https://github.com/tfausak/hs2048

## types

- tile can either be a number or nothing

``` hs
type Tile = Maybe Int
```

- a vector (row or column) of tiles is a list of tiles

``` hs
type Vector = [Tile]
```

- a board is a list of vectors

``` hs
type Board = [Vector]
```

## logic

- there are two real pieces of game logic
- moving (shifting) the board
- adding new tiles

- shifting isn't too bad
- assume you're always shifting toward the head
- that means to the left

``` hs
shift :: Vector -> Vector
shift v = take n v'
  where
    n = length v
    v' = (concatMap go . group . catMaybes v) ++ repeat Nothing
    go (Just a : Just b : ts) = Just (a + b) : go ts
    go ts = ts
```
