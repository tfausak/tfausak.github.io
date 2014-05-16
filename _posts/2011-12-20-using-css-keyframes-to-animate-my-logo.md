---
layout: post
title: Using CSS Keyframes to Animate My Logo
---

A couple months ago, I explained [how I built my logo with CSS][1].
Today, I made a change to it that was only possible because I used
CSS: I animated it.

![Exploded logo animation][2]

(That's not to say I couldn't have an animated logo without CSS. An
animated GIF would technically work, but it wouldn't stay synchronized
with the colored bar across the top. And the color palette would
be pretty limited.)

The key to animating in CSS is [keyframes][3]. They're just a list
of properties to apply at a given time. Since the color of the
header bar and my logo both inherit from their parent's `color`
attribute, that's the only one that needs animating.

{% highlight css %}
@keyframes solarized {
    0%, 100% { color: #dc322f; }
    12.5%    { color: #cb4b16; }
    25%      { color: #b58900; }
    37.5%    { color: #859900; }
    50%      { color: #2aa198; }
    62.5%    { color: #268bd2; }
    75%      { color: #6c71c4; }
    87.5%    { color: #d33682; }
}
{% endhighlight %}

Unfortunately, that won't work in any browsers yet. CSS animation
is still a working draft, so the vendor prefixes are required. And
the keyframe definition must be repeated for every vendor prefix.

``` css
@-moz-keyframes    solarized { /* ... */ }
@-ms-keyframes     solarized { /* ... */ }
@-o-keyframes      solarized { /* ... */ }
@-webkit-keyframes solarized { /* ... */ }
@keyframes         solarized { /* ... */ }
```

Once the keyframes have been defined, they need to be applied. It's
also a good idea to set the element's styles to match the first
keyframe so nothing flashes when the page loads.

~~~ css
.header {
    color: #dc322f;
       -moz-animation: solarized 14s infinite;
        -ms-animation: solarized 14s infinite;
         -o-animation: solarized 14s infinite;
    -webkit-animation: solarized 14s infinite;
            animation: solarized 14s infinite;
}
~~~

I chose a leisurely pace for my animation since I didn't want to
draw a whole lot of attention to it. It's more of an easter egg
than an in-your-face effect.

[1]: {% post_url 2011-10-29-how-i-built-my-logo-with-css %}
[2]: /static/images/2011-12-20-my-animated-logo.png
[3]: https://developer.mozilla.org/en-US/docs/CSS/@keyframes
