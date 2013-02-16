---
layout: post
title: Making the Famigo Logo with CSS
---

The [Famigo][1] [logo][2] is relatively simple. Some colorful, jumbled
text and a dot tumbling across them. I figured it was possible to render
the whole thing in HTML and CSS. [The result][3] isn't pixel-perfect,
but it's pretty close.

![Comparison between logo and CSS rendition][4]

[The starting markup][5] is basic. It's my usual HTML5 boilerplate,
plus an `<h1>` element, which is where all the magic happens. Each
letter in the logo is a different color, so each letter needs its own
element. Not very clean, but it works well enough. (The alternative is
to use CSS pseudo elements, which aren't very well supported.)

Putting each letter into its own element puts some space between the
letters, since there's whitespace between the elements. There are a couple
ways to avoid this (put the whitespace in the tags or run all the tags
together), but commenting out the whitespace keeps the markup readable.

At this point, [the markup][6] is a good baseline to work on. Each letter
can be styled individually but the whole thing just looks like a word.

{% highlight html %}
<h1 class="famigo-logo">
    <span class="famigo-logo-f">F</span><!--
 --><span class="famigo-logo-a">a</span><!--
 --><span class="famigo-logo-m">m</span><!--
 --><span class="famigo-logo-i">i</span><!--
 --><span class="famigo-logo-g">g</span><!--
 --><span class="famigo-logo-o">o</span>
</h1>
{% endhighlight %}

It's worth noting that the Famigo logo uses [Clarendon][7]. It's not a
web font, but [Georgia][8] is pretty close and basically ubiquitous.

![Rough initial logo style][9]

Each letter has three properties to set: color, rotation, and spacing. The
colors are easy enough to extract from the logo image. Rotating and
spacing are a bit more difficult, but they can be eyeballed to get
close enough.

The CSS [rotate transform][10] handles rotating the letters, and
[`letter-spacing`][11], unsurprisingly, handles the spacing. For brevity,
I've only included properties with `-webkit-` prefixes. To support other
browsers, `-moz-`, `-ms-`, and `-o-` should be added.

(Each letter also needs to have the property `display: inline-block`,
otherwise the rotations won't work. I've ignored that here, but it's in
[the code][12].)

{% highlight css %}
.famigo-logo-f {
    color: #8dc63f;
    letter-spacing: -0.075em;
    -webkit-transform: rotate(-9deg); }
.famigo-logo-a {
    color: #f4911e;
    letter-spacing: -0.05em;
    vertical-align: 0.05em;
    -webkit-transform: rotate(5deg); }
.famigo-logo-m {
    color: #ee3124;
    vertical-align: 0.05em;
    -webkit-transform: rotate(-4deg); }
.famigo-logo-i {
    color: #b0006e;
    letter-spacing: -0.075em;
    vertical-align: 0.15em;
    -webkit-transform: rotate(7deg); }
.famigo-logo-g {
    color: #deb406;
    -webkit-transform: rotate(-10deg); }
.famigo-logo-o {
    color: #1ab7ea;
    vertical-align: 0.075em;
    -webkit-transform: rotate(9deg); }
{% endhighlight %}

![Logo with proper color, rotation, and spacing][13]

At this point, I realized that the [tittle][14] over the "i" is the wrong
color. It's supposed to be red, not purple. CSS doesn't allow coloring
arbitrary parts of letters, so it needs to be split up somehow.

Fortunately, Unicode's got me covered. The "i" can be split into two
components: the bottom part, a [dotless "i"][15], and the top part,
a [dot diacritic][16]. Their code points are U+0131 (&#x131;) for the
lower case dotless "i" and U+02D9 (&#x2d9;) for the dot diacritic.

{% highlight html %}
<h1 class="famigo-logo">
    <span class="famigo-logo-f">F</span><!--
 --><span class="famigo-logo-a">a</span><!--
 --><span class="famigo-logo-m">m</span><!--
 --><span class="famigo-logo-i">&#x131;</span><!--
 --><span class="famigo-logo-g">g</span><!--
 --><span class="famigo-logo-o">o</span>
    <span class="famigo-logo-tittle">&#x307;</span>
</h1>
{% endhighlight %}

Each part gets styled just the same as the "i", but the dot either
has to be moved over to fit over the "i" or be absolutely positioned
on top of it. I initially did it the first way but ended up with the
second. Absolutely positioning it requires the top-level container to
be positioned, too.

{% highlight css %}
.famigo-logo {
    position: relative
    /* ... */ }
.famigo-logo-tittle {
    color: #ee3124;
    left: 2.8em;
    position: absolute;
    vertical-align: 0.15em;
    -webkit-transform: rotate(7deg); }
{% endhighlight %}

![Logo with tittle colored properly][17]

Now that the letters are all the right color and in the right spot, the
next thing to tackle is the lighting effect. Unfortunately, CSS doesn't
support lighting effects. Overlaying a gradient works as a fake light
source, though.

Since I want to do this without images, I'll use CSS [gradients][18]
and [masks][19] for this effect.

{% highlight css %}
.famigo-logo {
    -webkit-mask-image: -webkit-linear-gradient(
        transparent, black);
    /* ... */ }
{% endhighlight %}

![Logo with gradient mask][20]

It's not perfect, but it gets pretty close and it's relatively
simple. Unfortunately, it makes this next part a huge pain.

The stroke around the letters is unaffected by the lighting. A stroke
by itself is easy (using [`text-stroke`][21]), but it will get covered
by the gradient mask.

I tried all kinds of ways to get around this, but to no avail. The
best solution I found was to duplicate the content and only apply the
mask to one of them (the foreground) and apply the stroke to the other
(the background).

This solution also requires the background to be transparent, since
the foreground mask makes parts of the foreground transparent. It would
be counter-productive to put an opaque letter behind a partially
transparent one.

{% highlight css %}
.famigo-logo-background {
    height: 0;
    -webkit-text-stroke: 1px; }
.famigo-logo-foreground {
    -webkit-mask-image: -webkit-linear-gradient(
        transparent, black); }
.famigo-logo-background .famigo-logo-f {
    color: transparent;
    -webkit-text-stroke-color: #8dc63f; }
.famigo-logo-background .famigo-logo-a {
    color: transparent;
    -webkit-text-stroke-color: #f4911e; }
.famigo-logo-background .famigo-logo-m {
    color: transparent;
    -webkit-text-stroke-color: #ee3124; }
.famigo-logo-background .famigo-logo-i {
    color: transparent;
    -webkit-text-stroke-color: #b0006e; }
.famigo-logo-background .famigo-logo-g {
    color: transparent;
    -webkit-text-stroke-color: #deb406; }
.famigo-logo-background .famigo-logo-o {
    color: transparent;
    -webkit-text-stroke-color: #1ab7ea; }
.famigo-logo-background .famigo-logo-tittle {
    color: transparent;
    -webkit-text-stroke-color: #ee3124; }
{% endhighlight %}

{% highlight html %}
<h1 class="famigo-logo">
    <div class="famigo-logo-background">
        <span class="famigo-logo-f">F</span><!--
     --><span class="famigo-logo-a">a</span><!--
     --><span class="famigo-logo-m">m</span><!--
     --><span class="famigo-logo-i">&#x131;</span><!--
     --><span class="famigo-logo-g">g</span><!--
     --><span class="famigo-logo-o">o</span>
        <span class="famigo-logo-tittle">&#x307;</span>
    </div>
    <div class="famigo-logo-foreground">
        <span class="famigo-logo-f">F</span><!--
     --><span class="famigo-logo-a">a</span><!--
     --><span class="famigo-logo-m">m</span><!--
     --><span class="famigo-logo-i">&#x131;</span><!--
     --><span class="famigo-logo-g">g</span><!--
     --><span class="famigo-logo-o">o</span>
        <span class="famigo-logo-tittle">&#x307;</span>
    </div>
</h1>
{% endhighlight %}

![Logo with stroke][22]

The dotted trail across the letters is a nice flourish. CSS doesn't do
[Bézier curves][23], so I had to fake it with circles.

That's all there is to it! The [complete source is on GitHub as a
Gist][24]. It's not a pixel-perfect remake of the logo, but it's pretty
dang close. My original plan was to animate the ball bouncing across the
letters and have them rotate when they're hit. CSS animations don't play
nice with all the other stuff I'm doing, though. Letters get clipped in
weird places, gradients and strokes disappear. It's ugly.

[1]: http://www.famigo.com
[2]: http://www.famigo.com/static/images/famigo-logo.png
[3]: https://gist.github.com/tfausak/1644047#file-famigo-logo-png
[4]: /static/images/2012-01-20-famigo-logo-comparison.png
[5]: https://gist.github.com/tfausak/1644047/75d290bceb5c397e28aaa81b6aa2d678bffde936
[6]: https://gist.github.com/tfausak/1644047/ce27e0bad757ac8a56a74784a7bf5d7b4763754c
[7]: http://en.wikipedia.org/wiki/Clarendon_(typeface)
[8]: http://en.wikipedia.org/wiki/Georgia_(typeface)
[9]: /static/images/2012-01-20-famigo-logo-1.png
[10]: https://developer.mozilla.org/en-US/docs/CSS/transform#rotate
[11]: https://developer.mozilla.org/en-US/docs/CSS/letter-spacing
[12]: https://gist.github.com/tfausak/1644047/0c4397aca8d7dee8028388ad04c17b4ec13625c3
[13]: /static/images/2012-01-20-famigo-logo-2.png
[14]: http://en.wikipedia.org/wiki/Tittle
[15]: http://en.wikipedia.org/wiki/Dotted_and_dotless_I
[16]: http://en.wikipedia.org/wiki/Dot_(diacritic)
[17]: /static/images/2012-01-20-famigo-logo-3.png
[18]: https://developer.mozilla.org/en-US/docs/CSS/linear-gradient
[19]: https://developer.mozilla.org/en-US/docs/CSS/mask
[20]: /static/images/2012-01-20-famigo-logo-4.png
[21]: https://www.webkit.org/blog/85/introducing-text-stroke/
[22]: /static/images/2012-01-20-famigo-logo-5.png
[23]: http://en.wikipedia.org/wiki/B%C3%A9zier_curve
[24]: https://gist.github.com/tfausak/1644047
