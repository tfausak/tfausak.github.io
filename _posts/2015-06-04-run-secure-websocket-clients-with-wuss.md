---
title: Run secure WebSocket clients with Wuss
---

While working on [pressing the button with Haskell][1], I needed to establish a
secure WebSocket (WSS) connection. Unfortunately the existing [`websockets`
package][2] did not support secure WebSockets. I looked around and found [an
issue][3] addressing it. It had a few workarounds by [@jaspervdj][4],
[@mpickering][5], and [@elfenlaid][6]. They got the job done, but weren't
nearly as easy to use as the insecure `runClient` function. I figured that
other people could benefit from having a turnkey WSS solution, so I packaged it
up and released it as [Wuss][7].

I was surprised by how easy it was to add support for secure clients to the
`websockets` package. It only took [five lines of code][8]. That is a testament
to the design of that package, the strength of the ecosystem, and the
expressiveness of Haskell as a language.

{% highlight hs %}
runSecureClientWith host port path options headers app = do
    context <- initConnectionContext
    connection <- connectTo context (connectionParams host port)
    stream <- makeStream (reader connection) (writer connection)
    runClientWithStream stream host path options headers app
{% endhighlight %}

[1]: {% post_url 2015-04-23-pressing-the-button-with-haskell %}
[2]: http://hackage.haskell.org/package/websockets
[3]: https://github.com/jaspervdj/websockets/issues/41
[4]: https://gist.github.com/jaspervdj/7198388
[5]: https://gist.github.com/mpickering/f1b7ba3190a4bb5884f3
[6]: https://gist.github.com/elfenlaid/7b5c28065e67e4cf0767
[7]: http://taylor.fausak.me/wuss/
[8]: https://github.com/tfausak/wuss/blob/v1.0.2/Wuss.hs#L117-L121
