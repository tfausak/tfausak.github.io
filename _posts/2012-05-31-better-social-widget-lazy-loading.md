---
layout: post
title: Better Social Widget Lazy Loading
---

Last month, I streamlined my blog by [lazy loading social widgets][1].
I liked the results but felt that I could do better. A [Hacker News
comment][2] tipped me off to [Socialite.js][3]. That script reminded
me that `onhover` is more appropriate than `onscroll`. So I updated
my script.

I held off writing a post describing what I changed because the
core idea is the same: don't load the widgets until the user wants
them. However, a couple people emailed me about my new approach.
In addition, Oliver Reichenstein urged webmasters to [sweep the
sleaze][4]; lazy loading widgets addresses the technical concerns
(privacy, load times, scrolling performance) with ease.

## Markup

Typically, loading a social widget like the Google +1 button looks
like this:

{% highlight html %}
<div class="g-plusone" data-size="tall"></div>
{% endhighlight %}

Waiting to load the widget until the user hovers over it requires
some additional markup. The widget needs a placeholder, which will
be used until the user hovers, and for clients with JavaScript
disabled. Both the widget and the placeholder need to be wrapped
in a container.

{% highlight html %}
<div class="social-widget google-widget">
    <a href="#google" id="google-widget">
        +1
    </a>
    <div class="g-plusone" data-size="tall"></div>
</div>
{% endhighlight %}

Of course, `#google` should be replaced with a link to Google's
sharer. Stack Overflow covers [adding a Google +1 link without
JavaScript][5]. That answer also covers Facebook and Twitter.

## Styles

Before the actual widgets load, the placeholder needs to look pretty.
Fortunately, each widget has a distinctive color and verb associated
with it. Facebook's Like is blue (`#3b5b99`), Google's +1 is
orange (`#dd4b39`), and Twitter's Tweet is light blue (`#33ccff`).
Also, the widgets are all about the same size. They'll all fit in
a 62-by-55 pixel rectangle.

{% highlight css %}
#facebook-widget,
#google-widget,
#twitter-widget {
    color: #ffffff;
    display: block;
    line-height: 62px;
    text-align: center;
    text-decoration: none;
    width: 55px;
}
#facebook-widget { background: #3b5b99; }
#google-widget   { background: #dd4b39; }
#twitter-widget  { background: #33ccff; }
{% endhighlight %}

Now the placeholders won't be an eyesore before the actual widgets load.

## Scripts

Since the markup for the widgets is already in the page, all that
needs to be done is load the appropriate JavaScript library. So
when the user hovers over the +1 widget, load Google's +1 library.
In addition, the placeholder should be removed, since it's obsoleted
by the actual widget.

{% highlight js %}
var element, script;
element = document.getElementById('google-widget');
element.onmouseover = function () {
    this.onmouseover = null;
    this.parentNode.removeChild(this);
    script = document.createElement('script');
    script.async = true;
    script.src = '//apis.google.com/js/plusone.js';
    document.body.appendChild(script);
};
{% endhighlight %}

That's all there is to it! Loading the other social networks' widgets
is very similar.

## Post Script

I'm very happy with this approach. It has none of the drawbacks of
my previous version. It also plays nicely with clients that don't
run JavaScript, including text-only browsers. It works great on
mobile, too, where the first tap is interpreted as a hover.

This blog uses this technique, so just scroll down for an example.
If you want to see exactly how it's implemented, [check the source][6].
If you have any complaints or recommendations, please let me know.

[1]: /2012/04/29/lazy-loading-social-widgets/
[2]: http://news.ycombinator.com/item?id=3907424
[3]: http://socialitejs.com
[4]: http://informationarchitects.net/blog/sweep-the-sleaze/
[5]: http://stackoverflow.com/questions/7157411/adding-a-google-plus-one-or-share-link-to-an-email-newsletter
[6]: https://github.com/tfausak/tfausak.github.com
