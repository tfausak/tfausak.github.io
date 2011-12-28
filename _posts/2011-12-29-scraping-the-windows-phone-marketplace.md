---
layout: post
title: Scraping the Windows Phone Marketplace
---

[Web scraping][xaa] makes the world go round. For services that
don't supply an API, it's the only way to reliably and repeatedly
get the information you want out of a website.

I'm fairly well versed in writing scrapers, so I decided to have
some fun over Christmas break and write one for the [Windows Phone
Marketplace][xab]. I wrote scrapers for the iTunes Music Store and
the Android Market before, but I have a Windows Phone now.

Unfortunately, naively grabbing an application's market page doesn't
work too well. For instance, getting the page for [Tentacles][xad]
(using Python's [`urlopen`][xac]) returns this cryptic HTML:

    <html>
        <head>
            <meta http-equiv="REFRESH" content="0; URL=..." />
            <script type="text/javascript">
                function OnBack() {}
            </script>
        </head>
    </html>

[xaa]: http://en.wikipedia.org/wiki/Web_scraping
[xab]: http://www.windowsphone.com/en-US/marketplace
[xad]: http://www.windowsphone.com/en-US/apps/6651b8fe-0da1-e011-986b-78e7d1fa76f8
[xac]: http://docs.python.org/library/urllib2.html#urllib2.urlopen

* * *

The Windows Phone Marketplace sets a cookie and does some redirection.
See this StackOverflow answer:

    http://stackoverflow.com/a/8047329

So we can use Python's CookieJar class to load the required cookies.
Then we can begin to parse the page. We don't want to use regular
expressions <http://stackoverflow.com/a/1732454>, so we'll go with
BeautifulSoup <http://www.crummy.com/software/BeautifulSoup/>.

Most of the information is pretty easy to get. We can just extract
it right out of the DOM. Some of the data takes a little finagling.

Rating in particular takes some work, since its only expressed as
a class name, and the class name isn't numerical. We'll write a
function to handle transforming the psuedo-english class name into
a number.

Now we've got all the information about this app in an easy-to-use
format. But what if you want to get information about lots of apps?
The current function will make two requests every time: one to set
the cookie, and another to get the actual data. So let's just get
the cookie once.

Now you can quickly scrape lots of app data from the Windows Phone
Marketplace. The full code from this post is available on GitHub:
https://gist.github.com/1518335
