---
title: Serving Atom feeds with GitHub Pages
---

I recently checked on my blog through [Google's Webmaster Tools][1].
Surprisingly, my sitemap couldn't be read because it contained an
error. That was weird because it worked fine for a long time.
Google's error message didn't help at all:

> We were unable to read your Sitemap. It may contain an entry we are
> unable to recognize. Please validate your Sitemap before resubmitting.

My sitemap is an Atom feed of all my posts. Since Atom is XML, I
can check it with a more useful validator, like [the W3C's feed
validator][2]. It came back with this, which was much more helpful:

> Missing "charset" attribute for "text/xml" document.
>
> Sorry, I am unable to validate this document because on line 86 it
> contained one or more bytes that I cannot interpret as us-ascii (in
> other words, the bytes found are not valid values in the specified
> Character Encoding). Please check both the content of the file and
> the character encoding indication.
>
> The error was: ascii "\xC2" does not map to Unicode

There are actually two problems here. One is that the `charset`
attribute isn't being set on the `Content-Type` header. It should
be `utf-8`, but [it defaults to `us-ascii`][3]. The other problem
is that `text/xml` is deprecated and should be replaced with
`application/xml`. Or, better yet, `application/atom+xml`.

Fortunately, solving the second problem solves the first problem
too. The default `charset` for `application/xml` is `utf-8`.

Unfortunately, my blog is hosted on [GitHub Pages][4]. I can't
modify the headers. I can, however, modify the filenames. GitHub
doesn't know that my sitemap is an Atom feed since it has an `.xml`
extension. Changing it to `.atom` clues them in, and they serve it
with the right header. See for yourself:

    $ curl -v 'http://taylor.fausak.me/sitemap.atom'
    * About to connect() to taylor.fausak.me port 80 (#0)
    *   Trying 204.232.175.78... connected
    * Connected to taylor.fausak.me (204.232.175.78) port 80 (#0)
    > GET /sitemap.atom HTTP/1.1
    > User-Agent: curl/7.21.4 (universal-apple-darwin11.0) libcurl/7.21.4 OpenSSL/0.9.8r zlib/1.2.5
    > Host: taylor.fausak.me
    > Accept: */*
    >
    < HTTP/1.1 200 OK
    < Server: nginx/1.0.13
    < Date: Thu, 26 Apr 2012 16:47:37 GMT
    < Content-Type: application/atom+xml
    < Content-Length: 188478
    < Last-Modified: Thu, 26 Apr 2012 16:44:12 GMT
    < Connection: keep-alive
    < Expires: Fri, 27 Apr 2012 16:47:37 GMT
    < Cache-Control: max-age=86400
    < Accept-Ranges: bytes
    <

In short, to get GitHub pages to serve your Atom feed with the right
MIME type, use `.atom` instead of `.xml`. (The same thing goes for
RSS: use `.rss` instead of `.xml`.)

Renaming the sitemap means that anyone subscribed to that feed won't
receive updates. I could keep both of them around indefinitely, but
I don't want to duplicate the content. Instead, I'll create a feed
at the old URL with one entry that points to the new URL.

[1]: https://www.google.com/webmasters/tools/home?hl=en
[2]: http://validator.w3.org/feed/
[3]: http://annevankesteren.nl/2005/03/text-xml
[4]: http://pages.github.com
