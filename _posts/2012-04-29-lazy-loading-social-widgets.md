---
layout: post
title: Lazy Loading Social Widgets
---

<aside>Update: <a href="/2012/05/31/better-social-widget-lazy-loading/">Better Social Widget Lazy Loading</a></aside>

I added social widgets (Facebook like, Google +1, and Twitter tweet
buttons) to this site about three months ago. I had no idea if
anyone would use them or not. Turns out, they do.

I have one big problem with them, though: they take forever to load!
An entire page of mine takes about 750 milliseconds to load. Adding
social widgets drags that down to almost 3 seconds --- more than
three times longer!

I like my pages to load fast. I have just over 6 kB of assets (CSS
and JS, no images). Each post has about 3 kB of Markdown. So how
can I keep my site performant while providing these social widgets
for people that want them?

# Lazy Loading

The answer is [lazy loading][1]: "defer\[ing\] initialization of an
object until the point at which it is needed". So now I need to
know when the social widgets are needed. Better yet, when are they
*not* needed?

If they're not visible, they aren't needed. Simple, right? Turns
out, determining if an element is in the viewport is difficult if
you don't want to use a framework. (I don't want to start using a
framework, since that adds a big request and kind of defeats the
whole point.)

So, what can I do instead? The [scroll event][2] is well-supported
and doesn't require a framework. Loading the social widgets when
the user scrolls is nearly as good as loading when they should be
visible. You need to scroll to see them, anyway.

{% highlight js %}
window.onscroll = function () {
    window.onscroll = null;

    // Facebook JS SDK
    script = document.createElement('script');
    script.async = true;
    script.id = 'facebook-jssdk';
    script.src = '//connect.facebook.net/en_US/all.js';
    document.body.appendChild(script);

    // Google +1 button
    script = document.createElement('script');
    script.async = true;
    script.src = '//apis.google.com/js/plusone.js';
    document.body.appendChild(script);

    // Twitter widgets
    script = document.createElement('script');
    script.async = true;
    script.id = 'twitter-wjs';
    script.src = '//platform.twitter.com/widgets.js';
    document.body.appendChild(script);
};
{% endhighlight %}

Note that the first part of the `onscroll` function is to unassign
it. That's because the scripts for these social widgets only need
to be loaded once per page. Originally I used a boolean flag to
know if the scripts had been loaded, but that's unnecessary since
the function can be reassigned from within the function body.

## Special Cases

That's all fine and dandy, but what happens if the viewport is
taller than the document? Then scrolling is impossible and the
social widgets never get loaded.

My initial goal was to reduce page load time by avoiding unnecessary
elements. In this case, I'm okay with the old behavior of just
loading the widgets anyway. So I can just call the `onscroll`
function if the viewport is tall enough.

{% highlight js %}
if (window.innerHeight >= document.body.clientHeight) {
    window.onscroll();
}
{% endhighlight %}

This method is [reasonably well-supported][3], but it won't work
in Internet Explorer before version 9. That's not a problem for my
site, since those versions of IE account for less than 1% of all
visits.

## Improvements

As I mentioned above, a JavaScript framework would make this better
by delaying the load until the element is visible. Another potential
improvement in my case is delaying the load until the viewport has
been scrolled all the way to the bottom, where the widgets are.

## Ancillary Benefits

Although it wasn't the motivation for exploring this technique,
this improves my site's performance in the eyes of [Googlebot][4].
It loads the page but doesn't scroll around so the social widgets
aren't loaded. I don't know what it's viewport is, so it could end
up calling `onscroll` due to the workaround for tall viewports.

[1]: http://en.wikipedia.org/wiki/Lazy_loading
[2]: http://www.quirksmode.org/dom/events/scroll.html
[3]: http://www.quirksmode.org/dom/w3c_cssom.html
[4]: http://en.wikipedia.org/wiki/Googlebot
