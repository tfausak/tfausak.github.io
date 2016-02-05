---
title: Scraping the Windows Phone Marketplace
---

[Web scraping][1] makes the world go round. For services that don't
supply an API, it's the only way to reliably and repeatedly get the
information you want out of a website.

I'm fairly well versed in writing scrapers, so I decided to have
some fun over Christmas break and write one for the [Windows Phone
Marketplace][2]. I wrote scrapers for the iTunes Music Store and
the Android Market before, but I have a Windows Phone now.

Unfortunately, naively grabbing an application's market page doesn't
work too well. For instance, getting the page for [Tentacles][3]
(using Python's [`urlopen`][4]) returns this cryptic HTML:

``` html
<html>
    <head>
        <meta http-equiv="REFRESH" content="0; URL=http://www.windowsphone.com/en-US/apps/6651b8fe-0da1-e011-986b-78e7d1fa76f8" />
        <script type="text/javascript">
            function OnBack() {}
        </script>
    </head>
</html>
```

Turns out that the Windows Phone Marketplace sets a cookie and
redirects back to the same page. If the cookie isn't present, it
returns the ugly chunk of HTML above. If it is, the full page is
returned. (I didn't discover this on my own; a [StackOverflow
question][5] pointed me in the right direction.)

Setting the cookies isn't as easy as just getting the URL, but it's
not too much trouble with Python's [`CookieJar`][6] class. The first
request just sets the cookies, so the response is unneeded. The
second request returns the actual page.

``` python
import cookielib, urllib2
jar = cookielib.CookieJar()
handler = urllib2.HTTPCookieProcessor(jar)
opener = urllib2.build_opener(handler)
opener.open(url)
response = opener.open(url)
```

After getting the raw HTML, it's time to parse it. My tool of choice
is [Beautiful Soup][7], but there are others. ([Don't use regular
expressions][8]!)

``` python
import BeautifulSoup
soup = BeautifulSoup.BeautifulSoup(response)
```

Most of the information can be extracted right out of the DOM. The
HTML is surprisingly easy to navigate and pull data out of. The
only thing that's particularly tricky is the average rating. It's
displayed as a part of a sprite, and it's specified by a class like
"fourPtFive". Translating that pseudo-English into a number isn't
too hard, though.

``` python
def parse_rating(rating):
    values = {'zero': 0, 'one': 1, 'two': 2,
        'three': 3, 'four': 4, 'five': 5}
    integer, fraction = rating.split('Pt')
    integer = values[integer]
    fraction = values[fraction.lower()]
    return integer + (fraction / 10.0)
```

(I avoided putting all the boring data extraction inline with this
post because it's not very interesting. If you're interested in the
nitty-gritty of it, my scraper is available as [a Gist on GitHub][9].)

At this point, the whole page has been parsed into an easy-to-use
format. But grabbing multiple pages in a row will be really slow
because each one will have to do the cookie handshake. That means
getting `n` pages will require `2n` requests. We can do better.
Much better, in fact: `1 + n`.

``` python
if jar is None:
    jar = cookielib.CookieJar()
handler = urllib2.HTTPCookieProcessor(jar)
opener = urllib2.build_opener(handler)
if not jar:
    opener.open(url)
response = opener.open(url)
```

The first time a page is requested, the cookie jar will be created
and filled. Every time after that, it'll just use the existing
cookie jar and save a request.

So, there you have it. Scraping the Windows Phone Marketplace through
its website. The [full source code][9] is available if you want to
poke around.

[1]: http://en.wikipedia.org/wiki/Web_scraping
[2]: http://www.windowsphone.com/en-us/store
[3]: http://www.windowsphone.com/en-us/store/app/tentacles/6651b8fe-0da1-e011-986b-78e7d1fa76f8
[4]: http://docs.python.org/2/library/urllib2.html#urllib2.urlopen
[5]: http://stackoverflow.com/questions/8046907/how-come-i-cant-download-this-webpage-in-python
[6]: http://docs.python.org/2/library/cookielib.html#cookielib.CookieJar
[7]: http://www.crummy.com/software/BeautifulSoup/
[8]: http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454
[9]: https://gist.github.com/tfausak/1518335
