---
title: Monad transformers
---

In my post about [building a JSON REST API in Haskell][1], I glossed
over monad transformers. That was unfortunate. Monad transformers
are both useful and hard for me to wrap my head around. To help my
solidify my understanding (and to fill the gap left by my other
post) I'll try to explain them here.

Before I can do that, I need to introduce a few monads. They are
all simple and together they can be used to show how transformers
work.

The first is [`Identity`][2]. As the name implies, it does almost
nothing. Like the `identity` function, it isn't too useful by itself
but becomes it can be handy in certain situations. Here's an example
of how to use it:

{% highlight hs %}
import Data.Functor.Identity (Identity, runIdentity)

type Output = Integer

anIdentity :: Identity Output
anIdentity = do
    x <- return 3
    let y = x * 2
    return y

>>> runIdentity anIdentity
6
{% endhighlight %}

The next monad we'll look at is [`Reader`][3]. It adds some read-only
data. It can be a convenient way to add configuration information
to an otherwise pure function. Here's how it looks in action:

{% highlight hs %}
import Control.Monad.Trans.Reader (Reader, ask, runReader)

type Input = Integer
type Output = String

aReader :: Reader Input Output
aReader = do
    x <- ask
    let s = "The input was " ++ show x
    return s

>>> runReader aReader 3
"The input was 3"
{% endhighlight %}

Finally we'll take a look at the [`Writer`][4] monad. It is the
opposite of the reader monad. It adds write-only data. It is most
often used to add logging to a function. Like the reader, it is
easy to use:

{% highlight hs %}
import Control.Monad.Trans.Writer (Writer, tell, runWriter)

type Output = [String]
type Result = Integer

aWriter :: Writer Output Result
aWriter = do
    let x = 3
    tell ["The number was " ++ show x]
    return x

>>> runWriter aWriter
(3,["The number was 3"])
{% endhighlight %}

Now that we've covered all the monads, there's one more piece we
need for transformers. The [`lift`][5] function takes a function
that works in one monad and allows you to use it in another. You
can `ask` for something inside a `Reader`, but if you're in a
transformer stack that contains a reader, you have to `lift ask`
for it. It's easier to understand through an example. So let's build
up a monad transformer stack.

At the bottom will be an `Identity` monad. It doesn't really do
anything, but it lets us put more stuff on top of it. (Another
popular base is the `IO` monad.) Above that, we'll layer a `Reader`
monad. In real life this would probably contain some kind of
application configuration. For our purposes, it'll hold a number.
And at the top we'll have a `Writer` monad. We'll have it accumulate
a list of strings, which may be what you'd use it for in real life.

{% highlight hs %}
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity (Identity, runIdentity)
import Control.Monad.Trans.Reader (ReaderT, ask, runReader)
import Control.Monad.Trans.Writer (WriterT, tell, runWriter)

type Input = Integer
type Output = [String]
type Result = Integer

stack :: WriterT Output (ReaderT Input Identity) Result
stack = do
    x <- lift ask
    tell ["The input was " ++ show x]
    return x
{% endhighlight %}

So that's the whole stack. We can use the outer `Writer` monad
directly with `tell`. But we have to wrap calls to the inner `Reader`
monad with `lift`. This is the crux of the power of monad transformers.
They allow you to use any monad in the stack with a single call to
`lift`. Even if you have a stack of 10 monads, one `lift` is all
you need to get all the way to the one you want.

The only downside is that you need to run all of these monads. Doing
so can be a little tedious.

{% highlight hs %}
>>> let newReader = runWriterT stack
>>> let newIdentity = runReaderT newReader 3
>>> runIdentity newIdentity
(3,["The number was 3"])
{% endhighlight %}

This example is trivial, but I hope it shows how monad transformers
work. They are a powerful way to combine monadic actions.

[1]: {% post_url 2014-10-21-building-a-json-rest-api-in-haskell %}
[2]: https://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Functor-Identity.html
[3]: https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Reader.html
[4]: https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Writer-Lazy.html
[5]: https://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Class.html#v:lift
