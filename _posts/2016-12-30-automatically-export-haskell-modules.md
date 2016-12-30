---
title: Automatically export Haskell modules
---

I am happy to announce the 1.0.0 release of [Autoexporter][1], a tool for
automatically exporting Haskell modules.

What does that mean? Haskell modules can re-export other modules in addition to
exporting things defined directly in them. In fact, my [Haskell package
checklist][2] recommends providing a single module for users to import. But
defining everything in one module could make it difficult to understand. So you
will often see top-level modules like `Data.Time` that simply re-export modules
below them in the hierarchy.

``` hs
module Data.Time
  ( module Data.Time.Calendar
  , module Data.Time.Clock
  , module Data.Time.LocalTime
  , module Data.Time.Format
  ) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
```

Doing this by hand is tedious and error-prone. Every time you add a new module,
you must remember to come back to this top-level module and re-export it. Also
you have to spell it correctly not once but twice.

It is possible to make this a little better by using the so-called
"import/export shortcut". This aliases the imports so that you only have to
import one thing. The `Data.Time` module could be cleaned up with this
shortcut.

``` hs
module Data.Time
  ( module Export
  ) where

import Data.Time.Calendar as Export
import Data.Time.Clock as Export
import Data.Time.LocalTime as Export
import Data.Time.Format as Export
```

This solves the problem of having to write out each module name twice, but it
introduces a new problem: The generated documentation is completely blank. This
is a long-standing bug in Haddock, the Haskell documentation tool.

Now there is a better way! Autoexporter does this tedious work for you. Instead
of manually listing the modules you want to export, add Autoexporter as a
dependency. Then replace the entire module with this:

``` hs
{-# OPTIONS_GHC -F -pgmF autoexporter #-}
```

That will generate code for you that looks like the first example. It will
automatically include all direct children of the module. So if that's in
`Data.Time`, it will include `Data.Time.Calendar` but not
`Data.Time.Calendar.Julian`.

I find this to be very convenient, more so for applications that libraries. But
in both cases it reduces the friction of adding more modules to a Haskell
package.

[1]: https://github.com/tfausak/autoexporter
[2]: {% post_url 2016-12-05-haskell-package-checklist %}
