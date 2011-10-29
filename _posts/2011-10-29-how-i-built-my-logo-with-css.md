---
layout: post
title: How I Built My Logo with CSS
---

I'm a big fan of [Jon Tan][1]'s [text logo][2]. When I was designing
this site, I knew I wanted something similar.

The actual design comes from a doodle I drew a few years ago. It
looks a little bit like a lightning bolt and conveniently combines
my initials. It has a simple shape and can be rendered easily with
any number of colors. It was a no-brainer to choose as my logo.

Rendering it was a more interesting problem. My options were: a
raster image, like PNG; a vector image, like SVG; or some type of
HTML, CSS, and/or JS trickery.

A raster image is the most traditional solution, but it's problematic
because it doesn't scale. Changing the text size makes the browser
resize the image, which isn't pretty. And I'd have to supply a
double sized version for high-density devices like the iPhone. So
that was out.

You don't often see vector images on webpages, even though [you can
use SVG][3] in almost every browser. However, it's not supported
in very recent versions of Internet Explorer and the stock Android
browser. Since my logo isn't that complicated, I wanted it to be
visible on basically every device without issue. So no SVG.

I ended up using a variation of [Eric Meyer][4]'s [Slantastic][5]
CSS demo. The trick is to use CSS borders to build triangles. Looking
at my logo, I see that it can be created using nine triangles (or
four triangles and two squares). Further, it fits nicely into a
three-by-three grid of squares. This made writing the HTML easy:

{% highlight html %}
<span id="logo">
    <span id="tl"></span><span id="tc"></span><span id="tr"></span>
    <span id="cl"></span><span id="cc"></span><span id="cr"></span>
    <span id="bl"></span><span id="bc"></span><span id="br"></span>
</span>
{% endhighlight %}

As you can see, just a handful of `<span>` elements with short IDs
describing their position ("tl" for "top left", and so on).
Semantically gibberish, but not much can be done about that; it's
a logo.

The CSS is a little more complicated. I essentially needed the
elements to behave like table cells. Unfortunately, that's [not
supported very well][6]. Limiting the width and floating them all
to one side gets the job done, though.

{% highlight css %}
#logo {
    height: 3em;
    width: 3em; }
#logo span {
    display: block;
    float: left;
    height: 0;
    width: 0; }
{% endhighlight %}

The next step is to make each cell render itself using two triangles.
Due to the way my logo is shaped, the edge between them needs to
run from bottom left to top right. This can be achieved using the
border on the bottom and left of the element.

{% highlight css %}
#logo span {
    border-color: transparent;
    border-style: solid;
    border-width: 0 0 1em 1em; }
{% endhighlight %}

Now that everything's being drawn as triangles, the only thing left
to do is color them. I like the [Tango Desktop Project][7]'s color
palette, so I pulled some colors from that.

{% highlight css %}
#logo #tl {
    border-bottom-color: #ce5c00; }
#logo #tr, #logo #cr {
    border-left-color: #ce5c00; }
#logo #tc, #logo #cc {
    border-bottom-color: #d3d7cf;
    border-left-color: #d3d7cf; }
#logo #bc {
    border-left-color: #d3d7cf; }
{% endhighlight %}

That's it! This method works in every browser I can get my hands
on. It also compares favorably to the other methods ([raster][8]
or [vector][9] image) in terms of size. The raster image is the
largest at 646 bytes. The CSS-based version is in the middle at 557
bytes for both the markup and styles. The vector image is the
smallest at 311 bytes. However, the CSS-based version doesn't require
any extra HTTP requests.

[1]: http://jontangerine.com/
[2]: http://jontangerine.com/log/2007/11/complex-type-css-fix-cleartype-miss
[3]: http://caniuse.com/svg
[4]: http://meyerweb.com/
[5]: http://meyerweb.com/eric/css/edge/slantastic/demo.html
[6]: http://www.quirksmode.org/css/display.html
[7]: http://en.wikipedia.org/wiki/Tango_Desktop_Project
[8]: /static/images/og-image.png
[9]: /static/images/logo.svg
