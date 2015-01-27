---
title: iOS 8 web apps
layout: post
---

Apple released the iPhone 6 and 6 Plus a few months ago.
They also released iOS 8 along with it.
Unfortunately they didn't update [their web app documentation][] at the same time.
That leaves the official docs woefully out of date.

Apple has done this in the past.
I filled in the gaps for configuring web apps with [iOS 7][], [iOS 6][], and [iOS 5][].
This post will do the same for iOS 8.
But instead of making you flip between four guides,
I've put everything here.
This information works for all versions of iOS on all iPhones, iPads, and iPod Touches.

Check out [my example web app][] on an iOS device to see the finished product.

[their web app documentation]: https://developer.apple.com/library/safari/documentation/AppleApplications/Reference/SafariWebContent/ConfiguringWebApplications/ConfiguringWebApplications.html#//apple_ref/doc/uid/TP40002051-CH3
[ios 5]: {% post_url 2012-03-27-ios-web-app-icons-and-startup-images %}
[ios 6]: {% post_url 2012-09-20-iphone-5-web-app-startup-image %}
[ios 7]: {% post_url 2013-11-01-ios-7-web-apps %}
[my example web app]: /static/pages/YYYY-MM-DD-ios-8-web-app.html

## Configuration

### `apple-mobile-web-app-capable`

{% highlight html %}
<meta name="apple-mobile-web-app-capable"
      content="yes">
{% endhighlight %}

This tag allows the page to be run full screen.
Note that this only works when it has been added to the home screen.

### `apple-mobile-web-app-title`

{% highlight html %}
<meta name="apple-mobile-web-app-title"
      content="iOS Web App">
{% endhighlight %}

This tag sets a custom title.
If it's missing, iOS will use the `<title>` tag.
If that is missing too, it will default to "Favorites".
This is limited to about 8 to 12 characters.

### `apple-mobile-web-app-status-bar-style`

{% highlight html %}
<meta name="apple-mobile-web-app-status-bar-style"
      content="black">
{% endhighlight %}

This tag changes the color of the status bar.
There are three options: `default`, `black`, and `black-translucent`.
The best bet for maximum compatibility is `black`;
`default` and `black-translucent` behave differently between iOS 6 and 7.
(Sometimes on iOS 7+, the status bar starts as white-on-white or black-on-black.
Restarting the web app fixes this problem.)

### `viewport`

{% highlight html %}
<meta name="viewport"
      content="initial-scale=1">
{% endhighlight %}

This tag sets the size of the browser's viewport.
That means it determines how wide the virtual window is.
By setting `initial-scale` to 1, the virtual window will be the same size as the physical device.
That makes it the only setting you need here.
Other settings, like `device-width` just cause trouble.
(In particular, `device-width=320` will cause letterboxing on iOS 7+.)

{% highlight html %}
<!-- Only for web apps pretending to be native. -->
<meta name="viewport"
      content="initial-scale=1,minimum-scale=1,maximum-scale=1">
{% endhighlight %}

If you want to pretend like you're a native app,
set `minimum-scale=1` and `maximum-scale=1`.
Be warned that this means users can't scale your app at all.

### `format-detection`

{% highlight html %}
<meta name="format-detection"
      content="telephone=no">
{% endhighlight %}

This tag prevents Safari from automatically linking phone numbers.

## Icons

{% highlight html %}
<!-- iPad retina icon -->
<link href="apple-touch-icon-precomposed-152.png"
      sizes="152x152"
      rel="apple-touch-icon-precomposed">
<!-- iPad retina icon (iOS < 7) -->
<link href="apple-touch-icon-precomposed-144.png"
      sizes="144x144"
      rel="apple-touch-icon-precomposed">
<!-- iPad non-retina icon -->
<link href="apple-touch-icon-precomposed-76.png"
      sizes="76x76"
      rel="apple-touch-icon-precomposed">
<!-- iPad non-retina icon (iOS < 7) -->
<link href="apple-touch-icon-precomposed-72.png"
      sizes="72x72"
      rel="apple-touch-icon-precomposed">
<!-- iPhone 6 Plus icon -->
<link href="apple-touch-icon-precomposed-180.png"
      sizes="120x120"
      rel="apple-touch-icon-precomposed">
<!-- iPhone retina icon (iOS < 7) -->
<link href="apple-touch-icon-precomposed-114.png"
      sizes="114x114"
      rel="apple-touch-icon-precomposed">
<!-- iPhone non-retina icon (iOS < 7) -->
<link href="apple-touch-icon-precomposed-57.png"
      sizes="57x57"
      rel="apple-touch-icon-precomposed">
{% endhighlight %}

You'll need seven icon sizes:

- 152x152 (76@2x) for iPad retina on iOS 7+
- 144x144 (72@2x) for iPad retina on iOS 6
- 76x76 for iPad on iOS 7+
- 72x72 for iPad on iOS 6
- 180x180 (60@3x) for iPhone 6 Plus
- 144x144 (57@2x) for iPhone retina on iOS 6
- 57x57 for iPhone on iOS 6

There are two icon sizes you might think you need but actually don't:

- 120x120 (60@x) for iPhone retina on iOS 7+: There's no way to tell this apart from the 60@3x icon for the iPhone 6 Plus.
- 60x60 for iPhone on iOS 7+: There are no devices that support this combination.

If you set the `rel` to `apple-touch-icon` instead of `apple-touch-icon-precomposed`, iOS 6 will apply a glossy icon finish.
For consistency with iOS 7+, use precomposed icons.

By default, the icon is a screenshot of the page.
If there are no icon tags, Safari will try the following URLs:

1.  `/apple-touch-icon-180x180-precomposed.png`
2.  `/apple-touch-icon-180x180.png`
3.  `/apple-touch-icon-precomposed.png`
4.  `/apple-touch-icon.png`

The exact dimensions will depend on the device.

## Startup images

{% highlight html %}
<!-- iPad retina portrait startup image -->
<link href="/static/images/apple-touch-startup-image-1536x2008.png"
      media="(device-width: 768px) and (device-height: 1024px)
             and (-webkit-device-pixel-ratio: 2)
             and (orientation: portrait)"
      rel="apple-touch-startup-image">
<!-- iPad retina landscape startup image -->
<link href="/static/images/apple-touch-startup-image-1496x2048.png"
      media="(device-width: 768px) and (device-height: 1024px)
             and (-webkit-device-pixel-ratio: 2)
             and (orientation: landscape)"
      rel="apple-touch-startup-image">
<!-- iPad non-retina portrait startup image -->
<link href="/static/images/apple-touch-startup-image-768x1004.png"
      media="(device-width: 768px) and (device-height: 1024px)
             and (-webkit-device-pixel-ratio: 1)
             and (orientation: portrait)"
      rel="apple-touch-startup-image">
<!-- iPad non-retina landscape startup image -->
<link href="/static/images/apple-touch-startup-image-748x1024.png"
      media="(device-width: 768px) and (device-height: 1024px)
             and (-webkit-device-pixel-ratio: 1)
             and (orientation: landscape)"
      rel="apple-touch-startup-image">
<!-- iPhone 6 Plus portrait startup image -->
<link href="/static/images/apple-touch-startup-image-1242x2148.png"
      media="(device-width: 414px) and (device-height: 736px)
             and (-webkit-device-pixel-ratio: 3)
             and (orientation: portrait)"
      rel="apple-touch-startup-image">
<!-- iPhone 6 Plus landscape startup image -->
<link href="/static/images/apple-touch-startup-image-1182x2208.png"
      media="(device-width: 414px) and (device-height: 736px)
             and (-webkit-device-pixel-ratio: 3)
             and (orientation: landscape)"
      rel="apple-touch-startup-image">
<!-- iPhone 6 portrait startup image -->
<link href="/static/images/apple-touch-startup-image-750x1294.png"
      media="(device-width: 375px) and (device-height: 667px)
             and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPhone retina portrait startup image -->
<link href="/static/images/apple-touch-startup-image-640x1096.png"
      media="(device-width: 320px) and (device-height: 568px)
             and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPhone < 5 retina portrait startup image -->
<link href="/static/images/apple-touch-startup-image-640x920.png"
      media="(device-width: 320px) and (device-height: 480px)
             and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPhone < 5 non-retina portrait startup image -->
<link href="/static/images/apple-touch-startup-image-320x460.png"
      media="(device-width: 320px) and (device-height: 480px)
             and (-webkit-device-pixel-ratio: 1)"
      rel="apple-touch-startup-image">
{% highlight html %}

- you need ten:
  1. ipad retina portrait (768x1004@2x / 1536x2008)
  2. ipad retina landscape (748x1024@2x / 1496x2048)
  3. ipad portrait (768x1004)
  4. ipad landscape (748x1024)
  5. iphone 6 plus portrait (414x716@3x / 1242x2148)
  6. iphone 6 plus landscape (394x736@3x / 1182x2208)
  7. iphone 6 retina portrait (375x647@2x / 750x1294)
  8. iphone 5 retina portrait (320x548@2x / 640x1096)
  9. iphone retina portrait (320x460@2x / 640x920)
  10. iphone portrait (320x460)
- you *don't* need:
  1. landscape for non plus iphones because they can't start in that orientation
  2. iphone 5 or 6 form factor non-retina because no such devices exist
- requires web app capable
- landscape images are rotated 90 degrees clockwise
- start stretched on ios 7 and pop under the status bar
  - in ios 8 it doesnt pop any more, but the resolution is still shorter than it should be
- defaults to plain white
- doesn't try anything automatically
- last one is fallback
- must be the exact resolution - no scaling

## Hacks

{% highlight html %}
<style>
  html {
    -webkit-text-size-adjust: 100%;
  }
</style>
{% endhighlight %}

- `-webkit-text-size-adjust` prevents font from getting huge when rotating to landscape
