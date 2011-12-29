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
            <meta http-equiv="REFRESH" content="0; URL=http://www.windowsphone.com/en-US/apps/6651b8fe-0da1-e011-986b-78e7d1fa76f8" />
            <script type="text/javascript">
                function OnBack() {}
            </script>
        </head>
    </html>

Turns out that the Windows Phone Marketplace sets a cookie and
redirects back to the same page. If the cookie isn't present, it
returns the ugly chunk of HTML above. If it is, the full page is
returned. (I didn't discover this on my own; a [StackOverflow
question][xae] pointed me in the right direction.)

Setting the cookies isn't as easy as just getting the URL, but it's
not too much trouble with Python's [`CookieJar`][xaf] class. The
first request just sets the cookies, so the response is unneeded.
The second request returns the actual page.

    import cookielib, urllib2
    jar = cookielib.CookieJar()
    handler = urllib2.HTTPCookieProcessor(jar)
    opener = urllib2.build_opener(handler)
    opener.open(url)
    response = opener.open(url)

After getting the raw HTML, it's time to parse it. My tool of choice
is [Beautiful Soup][xag], but there are others. ([Don't use regular
expressions][xah]!)

    import BeautifulSoup
    soup = BeautifulSoup.BeautifulSoup(response)

Most of the information can be extracted right out of the DOM. The
HTML is surprisingly easy to navigate and pull data out of. The
only thing that's particularly tricky is the average rating. It's
displayed as a part of a sprite, and it's specified by a class like
"fourPtFive". Translating that pseudo-English into a number isn't
too hard, though.

    def parse_rating(rating):
        values = {'zero': 0, 'one': 1, 'two': 2,
            'three': 3, 'four': 4, 'five': 5}
        integer, fraction = rating.split('Pt')
        integer = values[integer]
        fraction = values[fraction.lower()]
        return integer + (fraction / 10.0)

(I avoided putting all the boring data extraction inline with this
post because it's not very interesting. If you're interested in the
nitty-gritty of it, my scraper is available as [a Gist on GitHub][xai].)

At this point, the whole page has been parsed into an easy-to-use
format. But grabbing multiple pages in a row will be really slow
because each one will have to do the cookie handshake. That means
getting `n` pages will require `2n` requests. We can do better.
Much better, in fact: `1 + n`.

    if jar is None:
        jar = cookielib.CookieJar()
    handler = urllib2.HTTPCookieProcessor(jar)
    opener = urllib2.build_opener(handler)
    if not jar:
        opener.open(url)
    response = opener.open(url)

The first time a page is requested, the cookie jar will be created
and filled. Every time after that, it'll just use the existing
cookie jar and save a request.

So, there you have it. Scraping the Windows Phone Marketplace through
its website. The [full source code][xai] is available if you want
to poke around.

[xaa]: http://en.wikipedia.org/wiki/Web_scraping
[xab]: http://www.windowsphone.com/en-US/marketplace
[xad]: http://www.windowsphone.com/en-US/apps/6651b8fe-0da1-e011-986b-78e7d1fa76f8
[xac]: http://docs.python.org/library/urllib2.html#urllib2.urlopen
[xae]: http://stackoverflow.com/q/8046907
[xaf]: http://docs.python.org/library/cookielib.html#cookielib.CookieJar
[xag]: http://www.crummy.com/software/BeautifulSoup/
[xah]: http://stackoverflow.com/a/1732454
[xai]: https://gist.github.com/1518335
