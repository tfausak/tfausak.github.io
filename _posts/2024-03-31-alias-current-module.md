---
title: Alias the current module with Imp
---

Sometimes when writing a Haskell module you want to use an identifier that would be ambiguous.
For example perhaps you're writing a logger and want to call a function `log`.
That's a problem because the `Prelude` also defines a function called `log`.
You can't use either one without disambiguating.
Usually you will disambiguate by qualifying the identifier like `MyLogger.log`.
This is fine but it is a little annoying because it duplicates the name of the module.
If you later refactor this module by changing its name, you'll also have to update any qualified identifiers.
I'm happy to announce that my GHC plugin [Imp][1] now has a solution to this problem.

TL;DR:

``` hs
{-# OPTIONS_GHC
  -fplugin=Imp
  -fplugin-opt=Imp:--alias=_:This #-}
print = putStrLn . show
main = This.print ()
```

---

To provide a concrete example, consider the following module that defines a very basic logger:

``` hs
module MyLogger where

data Severity
  = Info
  deriving (Show)

log :: Severity -> String -> IO ()
log severity string =
  putStrLn (show severity <> ": " <> show string)

info :: String -> IO ()
info string =
  log Info string
```

This won't compile as written because the reference to `log` in the `info` function is ambiguous.
GHC will give you an error like this:

```
MyLogger.hs:13:3: error: [GHC-87543]
    Ambiguous occurrence ‘log’.
    It could refer to
       either ‘Prelude.log’,
              imported from ‘Prelude’ at MyLogger.hs:1:8-15
              (and originally defined in ‘GHC.Float’),
           or ‘MyLogger.log’, defined at MyLogger.hs:8:1.
   |
13 |   log Info string
   |   ^^^
```

Typically you would solve this problem by qualifying the identifier.
If you did that, the updated definition of `info` would look like this:

``` hs
info string =
  MyLogger.log Info string
```

That's a reasonable solution and it works fine but I think there are a couple problems with it.
One, it gets to be more annoying when your module name is longer.
Imagine something like `My.Really.Deeply.Nested.Logger` instead of `MyLogger`.
And two, if you rename the module then you have to go through and update any qualified identifiers as well.

I've often wished that GHC would provide a special module name like `Current` or `This` or `Self` that could be used in place of the actual module name.
Now you can use my [Imp][1] plugin to provide that for yourself!
Imp already had the ability to alias modules, which allowed you to use `String` in place of `Data.String` for example.
But now it can also provide an alias for the module that's currently being compiled.

So if you added the following pragma to the top of `MyLogger`:

``` hs
{-# OPTIONS_GHC
  -fplugin=Imp
  -fplugin-opt=Imp:--alias=_:This #-}
```

Then you could change the definition of `info` to use `This.log` instead of `MyLogger.log`:

``` hs
info string =
  This.log Info string
```

And Imp will replace `This` with `MyLogger` for you.

I hope you find this useful!
Thanks for reading, and please check out my [Imp][1] plugin.

[1]: https://github.com/tfausak/imp
