---
title: Default exception handler in Haskell
---

Have you ever wanted to install a default exception handler in Haskell?
Until recently I didn't think that was possible.
Then I found Ivan Gromakovsky's [Handling of Uncaught Exceptions in Haskell][] blog post,
which describes how to print exceptions using [`displayException`][] rather than [`show`][].
Ivan accomplishes that with a little known and under documented function called [`setUncaughtExceptionHandler`][].

This post will explain why you might want a default exception handler, how to install one, and point out a tricky edge case to watch out for.

## Motivation

When you throw an exception from the main thread in a Haskell,
the program will print out the exception and crash.
For example:

``` hs
main1 = fail "oh no"
-- Main.exe: user error (oh no)
```

Often you want to do something different.
Perhaps you want to decorate the exception with more information for debugging,
or report it to some third party service.
Fortunately it's easy enough to define a custom exception handler.
I'll keep it simple for demonstration purposes:

``` hs
myHandler (SomeException e) =
  putStrLn $ "[caught] " <> show e
```

The handler doesn't do anything by itself.
You have to wrap your code in something like [`handle`][] in order for your handler to be called.
For an application, typically one of the first things your program will do is set up exception handling:

``` hs
main2 = handle myHandler $ fail "oh no"
-- [caught] user error (oh no)
```

This works great on the main thread, but things get more complicated when you start spawning other threads.
You have to remember to attach your custom exception handler to any threads that you spawn:

``` hs
main3 = forkIO . handle myHandler $ fail "oh no"
-- [caught] user error (oh no)
```

That's not terribly difficult to do, but it is easy to forget.
But what's worse is that you can have code that looks like it handles exceptions properly even though it doesn't.
For example, the following code does not handle the exception on the forked thread with the custom exception handler in spite of the fact that it looks like it should:

``` hs
main4 = handle myHandler . void . forkIO $ fail "oh no"
-- Main.exe: user error (oh no)
```

That's unfortunate.
You could write a helper function that forks a thread with your custom exception handler.
There are lots of ways to fork threads,
so you would end up with lots of helper functions.
Plus if you call any third party code that forks threads,
they clearly won't be using your custom exception handler.
So what can you do?

## Solution

Fortunately GHC provides a solution to this problem: [`setUncaughtExceptionHandler`][].
If an exception is thrown and nothing else handles it,
eventually it will be handled by the uncaught exception handler.
This makes it easy to customize exception handling without wrapping everything in [`handle`][].
The following example showcases that but is otherwise the same as `main2`:

``` hs
main5 = do
  setUncaughtExceptionHandler myHandler
  fail "oh no"
-- [caught] user error (oh no)
```

Crucially the uncaught exception handler works on all threads, not just the main thread.
That means the following exception thrown on another thread is handle by the custom exception handler, just like in `main3`:

``` hs
main6 = do
  setUncaughtExceptionHandler myHandler
  forkIO $ fail "oh no"
-- [caught] user error (oh no)
```

So there you have it.
If you want to install a default exception handler that gets used for all unhandled exceptions on all threads, use [`setUncaughtExceptionHandler`][].

## Problems

That's not quite the end of the story though.
There is one thing to watch out for:
exceptions thrown from the handler.

Let's say your exception handler itself throws an exception.
This could be due to a bug, or perhaps something went wrong attempting to report the exception to some third party service.
Regardless of the cause, the result is the same:
you're throwing an exception from your default exception handler.
What happens?

``` hs
badHandler (SomeException e) = do
  putStrLn $ "[caught] " <> show e
  fail "boom"
```

If the original exception was thrown from the main thread, GHC notices this and prints out a helpful message for you:

``` hs
main7 = do
  setUncaughtExceptionHandler badHandler
  fail "oh no"
-- Main.exe: encountered an exception while trying to report an exception.
-- One possible reason for this is that we failed while trying to encode
-- an error message. Check that your locale is configured properly.
```

However if the original exception was thrown from a different thread, it will get stuck in a loop for a while before eventually terminating:

``` hs
main8 = do
  setUncaughtExceptionHandler badHandler
  forkIO $ fail "oh no"
-- [caught] user error (oh no)
-- [caught] user error (boom)
-- [caught] user error (boom)
-- ...
```

Neither case is ideal, so what can you do?
You could write a more basic exception handler and use that to handle exceptions from your other, more complicated exception handler.
But why bother writing another one when you can use the original one?
This is what I would recommend:

``` hs
main9 = do
  originalHandler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler $ handle originalHandler . badHandler
  fail "oh no"
-- [caught] user error (oh no)
-- Main.exe: user error (boom)
```

## Conclusion

Hopefully that explains why you could want a default exception handler, how to install one, and how to deal with exceptions thrown from your handler.
I encourage you to install a default exception handler in your Haskell applications to make sure that forked threads aren't throwing exceptions that you don't know about.

Please let me know if you have any thoughts about this approach.
Thanks for reading!

[Handling of Uncaught Exceptions in Haskell]: https://serokell.io/blog/uncaught-exception-handling
[`displayException`]: https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:displayException
[`show`]: https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Show.html#v:show
[`setUncaughtExceptionHandler`]: https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Conc.html#v:setUncaughtExceptionHandler
[`handle`]: https://hackage.haskell.org/package/base-4.15.0.0/docs/Control-Exception.html#v:handle
[`ExitCode`]: https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Exit.html#t:ExitCode
