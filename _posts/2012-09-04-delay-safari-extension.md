---
title: Delay Safari extension
---

[![Let Go][1]][2]

Recently, I caught myself spending too much time on websites like
Hacker News and Reddit. You know the routine: do some work, then
reward yourself with a picture of [a cat in comically oversized
tennis shoes][3]. Running unit tests only takes about a minute, but
I'd be lost on Reddit for a lot longer than that waiting for them
to finish.

So I wanted to keep myself from doing that. Monitoring and outright
blocking never worked for me. Randall Munroe's [distraction affliction
correction extension][4] looked promising. Unfortunately, my laptop
boots too quickly for that. The comments there recommend several
Firefox and Chrome extensions, but I use Safari and couldn't find
one like those. So I wrote my own.

## Delay

[![Delay][5]][6]

[Delay][6] is a Safari extension that delays the loading of
time-wasting websites. It's very simple. Set up a list of hostnames
to blacklist and they'll be delayed for however long you want. When
you visit a site that matches the blacklist, it will hide the whole
page and show a countdown timer instead. The timer only runs when
the window has focus, so you can't open a bunch of tabs to sidestep
it.

### How It Works

Delaying a site proved to be harder than blocking it. To block a
site, you can either empty `location.href` or call `window.stop`.
I tried a bunch of different things for delaying and settled on
styling the root element (`<html>`). The root element is always
present, unlike `<body>`, which doesn't show up until later. After
my script identifies the site as a time-waster, it updates the page
to look like this:

``` html
<html delay="30">
    <!-- the rest of the DOM -->
</html>
```

Styling is handled by adding an attribute to the root element and
then matching it with the `[attr]` selector in CSS. The first time
around, I set `display: none`, but that screwed up page layout so
I went with `visibility: hidden` instead. Flash elements don't obey
`visibility`, so I had to set `opacity: 0` for them.

The countdown timer is created in CSS using the `:before` pseudo-element
and the `attr()` function.

``` css
[delay]:before {
    content: attr(delay);
    /* ... */
}
[delay] * {
    opacity: 0 !important;
    visibility: hidden !important;
}
```

[1]: /static/images/2012/09/04/let-go.png
[2]: http://xkcd.com/862/
[3]: http://www.reddit.com/r/aww/comments/zabw9/my_running_buddy/
[4]: http://blog.xkcd.com/2011/02/18/distraction-affliction-correction-extensio/
[5]: /static/images/2012/09/04/delay.png
[6]: https://github.com/tfausak/delay
