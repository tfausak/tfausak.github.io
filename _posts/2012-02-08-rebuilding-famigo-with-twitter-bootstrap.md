---
layout: post
title: Rebuilding Famigo with Twitter Bootstrap
---

Over the past week, I rebuilt [Famigo's website][1] using the
[Twitter Bootstrap][2]. I'm happy to announce that version 2.0 of
our website is going live today.

[![Famigo][3]][1]

The bootstrap made it easy to quickly put together a great-looking
website. I loved working with it and look forward to continuing
development with it.

## Other Frameworks

Before settling on the Twitter Bootstrap, I looked around for other
frameworks. The most important requirement was a responsive layout.
More than two-thirds of our clients are browsing from a mobile
device. We need to serve mobile-friendly content to them, but we
also want a good user experience on the desktop.

Up until today, we used user-agent sniffing to serve different
content to mobile clients. That meant we duplicated a lot of stuff
on the back end. We tried to mitigate it using [Django's template
inheritance][4], which worked okay. But it's always easier to worry
about one file instead of two.

[![Reflexions][5]][14]

Of the many CSS frameworks out there, only a handful use responsive
layouts. [Foundation][6], [Skeleton][7], and [Less][8] were the
other front-runners. Ultimately we chose the Twitter Bootstrap
because it came with the most batteries included.

## Updating Widgets

Three or four of us at Famigo routinely work on parts of the website.
We're not all intimately familiar with the entire codebase. As a
result, we sometimes end up duplicating functionality.

For instance, we render a list of applications as a grid of tiles.
As time went on, little bits of HTML, CSS, and JS crept into disparate
parts of the code base. And we'd need one of those bits for some
other widget on our site, like [applists][9].

![Comparison of app tiles][10]

Now the tiles have been refactored into a separate template file.
They're styled with reusable styles defined in one CSS file. A
single JavaScript file handles every user interaction with them.

## Moving Forward

We still have a lot of work to do. One of our top priorities at
this point is not looking like a "bootstrap site". Not to say that
we're not a fan of how it looks --- we are. But as anyone who has
used the [default WordPress theme][11] can tell you, you need to
be unique.

We're looking forward to customizing the bootstrap, though. It was
so easy to get everything set up with the defaults that we're
assuming tinkering with it will be a piece of cake.

[![Best Free Games][12]][15]

Please send feedback about the new site to [taylor@fausak.me][13].
I'd love to hear what you think about it!

[1]: http://www.famigo.com
[2]: http://twitter.github.com/bootstrap/
[3]: /static/images/2012/02/08/famigo-bootstrap-comparison.png
[4]: https://docs.djangoproject.com/en/1.3/topics/templates/#template-inheritance
[5]: /static/images/2012/02/08/famigo-bootstrap-responsive.png
[6]: http://foundation.zurb.com
[7]: http://www.getskeleton.com
[8]: http://lessframework.com
[9]: {% post_url 2011-11-02-applists-playlists-for-your-apps %}
[10]: /static/images/2012/02/08/famigo-app-tile-comparison.png
[11]: http://wordpress.org/extend/themes/default
[12]: /static/images/2012/02/08/famigo-best-free-games.png
[13]: mailto:taylor@fausak.me
[14]: http://www.famigo.com/app/reflexions/
[15]: http://www.famigo.com/best-free-games/
