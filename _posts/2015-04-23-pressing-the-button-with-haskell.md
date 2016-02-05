---
title: Pressing the button with Haskell
---

On April Fools' Day, [Reddit announced the button][1]. They explained that:

> A button and timer will become active at [/r/thebutton][2]. The timer will
> count down from 60 seconds. If the button is pressed the timer will reset to
> 60 seconds and continue counting down. Only users logged into accounts
> created before 2015-04-01 can press the button.
>
> **You may only press the button once.**

In the 22 days since then, the timer on the button has not dipped below 14
seconds. Over 800,000 users have pressed the button.

I have not yet pressed the button. As a programmer, I wanted to understand how
the button works. So I wrote a Haskell program that watches the button and
presses it when it hits the time you want. I called this program [Hutton][3].

![][4]

Assuming that you have Haskell installed, you can install and run Hutton like
this:

``` sh
$ cabal update
$ cabal install hutton
# hutton threshold username password
$ hutton 10 taylorfausak secret
```

That will connect to Reddit and listen to the button, printing out its state
every second. Once the timer goes below the threshold, it will press the
button. Hutton will get you the time that you want; if someone else presses it
before you, you won't end up with a 60-second press.

So how does Hutton work? First is uses Reddit's API to log into your account.
It grabs a modhash and a cookie. Both of these are necessary to use the API,
which is how the button is pressed.

After that, it loads [/r/thebutton][2] to get two values: the current timestamp
and a hash. Both of these are necessary to listen to the button. This is the
last step of the setup.

Once it's done that, it connects to a secure WebSocket. (Haskell's [websockets
packge][5] does not support secure WebSockets. I wrote [Wuss][6], a wrapper
that does support WSS.) The socket sends messages about the current state of
the button. They look like this:

``` json
{
  "type": "ticking",
  "payload": {
    "participants_text": "810,483",
    "tick_mac": "9e32acfb0c1fb176424b124ece9325a1a19a2576",
    "seconds_left": 31.0,
    "now_str": "2015-04-23-14-21-21"
  }
}
```

Hutton parses those messages and displays them to you in a tab-separated table.

If the `seconds_left` field is ever less than or equal to the threshold you
set, Hutton will post to Reddit's API. This is the same mechanism that the
JavaScript on [/r/thebutton][2] uses. There is one key difference though: On
[/r/thebutton][2], the timer updates continuously. So if someone presses the
button a second before you, the timer will reset and you will have pressed it
at 60 seconds. This cannot happen with Hutton.

And that's all it does. If you're curious about exactly how it works, I
encourage you to read [the source][7]. Hopefully it is approachable. If there
is any interest, I can provide compiled binaries of Hutton.

If you're still trying to figure out what the big deal is with the button, I
can't help you. I don't know either.

[1]: http://www.redditblog.com/2015/04/the-button.html
[2]: https://www.reddit.com/r/thebutton
[3]: https://github.com/tfausak/hutton
[4]: /static/images/2015/04/23/hutton.png
[5]: http://hackage.haskell.org/package/websockets
[6]: /wuss/
[7]: https://github.com/tfausak/hutton/blob/v1.0.0/Main.hs#L30
