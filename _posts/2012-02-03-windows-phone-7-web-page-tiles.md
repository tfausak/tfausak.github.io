---
title: Windows Phone 7 web page tiles
---

<aside>

Update: You should use [My Pinned Site][] by Microsoft to create web page
tiles.

</aside>

Adding a web page to the home screen of an iOS device uses a
screenshot of the page or [a web page icon][1] if it's available.
Similarly, adding a web page to a home screen on an Android device
uses a generic bookmark icon or [a web page icon][2] if it's
available.

However, [it's impossible to set an icon][3] for a web page on the
home screen of a Windows Phone 7 device. Instead, the home screen
icon is always a screenshot of the current page as-is. The desktop
version of Internet Explorer allows developers to create [pinned
sites][4]. The lack of a comparable feature in the mobile version
is curious.

So how does Google [make pretty home screen tiles][5]? They don't,
really. They show WP7 users a link to a tile-able page that looks
nice when it's pinned to the home screen. All it does is redirect
them back to Google's home page.

Showing the link to only WP7 users is relatively simple. Either
user-agent sniffing or [conditional comments][6] will handle that
nicely. The pretty page that redirects is the hard part. Fortunately
for you, I've done all the hard work.

I made a [tile-able page for WP7][7]. It handles everything on the
client side, so it's just a static HTML file. You can customize the
title, icon, and destination URL with the query string parameters
`title`, `icon`, and `url`, respectively.

The first time you visit the page, you'll see the icon, title, and
instructions. The next time you visit the page, it'll redirect you
to the URL you specified. This allows you to load the page on a WP7
device and pin it to your home screen. Every time you tap it on
your home screen, it'll just redirect you.

![Example home screen tiles][8]

The page is available [as a Gist on GitHub][9]. (The example pages
above are for [Google][10], [Facebook][11], and [Twitter][12].)

[1]: https://developer.apple.com/library/IOs/#documentation/AppleApplications/Reference/SafariWebContent/ConfiguringWebApplications/ConfiguringWebApplications.html
[2]: http://stackoverflow.com/questions/1951381/configuring-android-web-applications
[3]: http://stackoverflow.com/questions/5994380/windows-phone-7-internet-shortcut-icon
[4]: http://msdn.microsoft.com/en-us/library/IE/gg491731(v=vs.85).aspx
[5]: http://www.russellbeattie.com/blog/creating-a-pinnable-windows-phone-7-tile-for-your-website-like-google
[6]: http://blogs.msdn.com/b/iemobile/archive/2010/12/08/targeting-mobile-optimized-css-at-windows-phone-7.aspx
[7]: /static/pages/2012-02-03-wp7-tile.html
[8]: /static/images/2012/02/03/web-page-tiles.png
[9]: https://gist.github.com/tfausak/1727983
[10]: /static/pages/2012-02-03-wp7-tile.html?title=Google&icon=http://i.imgur.com/4UNSv.png&url=https://www.google.com
[11]: /static/pages/2012-02-03-wp7-tile.html?title=Facebook&icon=http://i.imgur.com/wXxZR.png&url=https://www.facebook.com
[12]: /static/pages/2012-02-03-wp7-tile.html?title=Twitter&icon=http://i.imgur.com/lfyve.png&url=https://twitter.com
[13]: http://www.buildmypinnedsite.com