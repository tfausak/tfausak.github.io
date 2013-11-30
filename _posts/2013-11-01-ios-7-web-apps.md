---
layout: post
title: iOS 7 Web Apps
---

Apple released iOS 7 a few weeks ago. They updated [their web app
documentation][1], but it's still not comprehensive. The fact of
the matter is, building a web app can be a frustrating trial-and-error
experience.

I'm here to help. I've written about iOS web apps before, for [iOS
5][2] and the [iPhone 5][3]. Along with those two posts, this is a
complete reference for configuring web apps on any version of iOS
up to and including 7.0.3.

Check out the [example web app][4] before digging in!

To get started, put this in your `<head>`:

{% highlight html %}
<meta name="apple-mobile-web-app-capable"
      content="yes">
{% endhighlight %}

The `apple-touch-fullscreen` alias is obsolete. It was never
officially supported. As of iOS 7, it no longer works. Don't use
it.

## Title

![Edit title dialog][5]

The default title is "Favorites". That's not too helpful, but you
probably won't ever see it. If you have the `<title>` attribute
set, Safari will use that instead. You can override that by setting
the `apple-mobile-web-app-title` meta attribute.

{% highlight html %}
<title>Normal title</title>
<meta name="apple-mobile-web-app-title"
      content="iOS title">
{% endhighlight %}

Regardless of how you set the title, you should try to make it
between 8 to 12 characters long.

## Icons

![Icons on the home screen][6]

The default icon is plain white. If you don't link to any icons,
Safari will try the following URLs:

1.  `/apple-touch-icon-152x152-precomposed.png`
2.  `/apple-touch-icon-152x152.png`
3.  `/apple-touch-icon-precomposed.png`
4.  `/apple-touch-icon.png`

The dimensions (`152x152`) will vary by device and iOS version.
They will be one of:

-   `152x152` for retina iPads on iOS 7.
-   `144x144` for retina iPads on iOS 6.
-   `120x120` for retina iPhones & iPod touches on iOS 7.
-   `114x114` for retina iPhones & iPod touches on iOS 6.
-   `76x76` for iPads on iOS 7.
-   `72x72` for iPads on iOS 6.
-   `60x60` for iPhones & iPod touches on iOS 7, although no such devices exist.
-   `57x57` for iPhones & iPod touches on iOS 6.

To specify a custom URL for icons, add a `<link>` with
`rel="apple-touch-icon-precomposed"` or `rel="apple-touch-icon"`.
If you give both, the precomposed icon will be preferred. iOS 7
doesn't apply any effects to icons, so I recommend using precomposed
icons for consistency across all versions of iOS.

{% highlight html %}
<!-- iPad, iOS 7, retina -->
<link href="apple-touch-icon-152x152.png"
      sizes="152x152"
      rel="apple-touch-icon">
<!-- iPad, iOS 6, retina -->
<link href="apple-touch-icon-144x144.png"
      sizes="144x144"
      rel="apple-touch-icon">
<!-- iPhone, iOS 7, retina -->
<link href="apple-touch-icon-120x120.png"
      sizes="120x120"
      rel="apple-touch-icon">
<!-- iPhone ,iOS 6, retina -->
<link href="apple-touch-icon-114x114.png"
      sizes="114x114"
      rel="apple-touch-icon">
<!-- iPad, iOS 7 -->
<link href="apple-touch-icon-76x76.png"
      sizes="76x76"
      rel="apple-touch-icon">
<!-- iPad, iOS 6 -->
<link href="apple-touch-icon-72x72.png"
      sizes="72x72"
      rel="apple-touch-icon">
<!-- iPhone, iOS 7 -->
<link href="apple-touch-icon-60x60.png"
      sizes="60x60"
      rel="apple-touch-icon">
<!-- iPhone, iOS 6 -->
<link href="apple-touch-icon-57x57.png"
      sizes="57x57"
      rel="apple-touch-icon">
{% endhighlight %}

Older versions of iOS ignore the `sizes` attribute, so the last
icon should be a fallback. If none of the icons match the expected
dimensions, Safari will use the smallest icon larger than the correct
size. However if none of them are larger, it'll use the largest
available.

## Startup Images

![Example startup image][7]

In order to use a startup image, your page must be web app capable.
That means setting the `apple-mobile-web-app-capable` meta attribute.

The default startup image is plain white. Unlike the icons, Safari
doesn't try anything by default. To specify a startup image, you
must add a `<link>` with `rel="apple-touch-startup-image"`.

{% highlight html %}
<!-- iPad, retina, portrait -->
<link href="apple-touch-startup-image-1536x2008.png"
      media="(device-width: 768px) and (device-height: 1024px)
         and (orientation: portrait)
         and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPad, retina, landscape -->
<link href="apple-touch-startup-image-1496x2048.png"
      media="(device-width: 768px) and (device-height: 1024px)
         and (orientation: landscape)
         and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPad, portrait -->
<link href="apple-touch-startup-image-768x1004.png"
      media="(device-width: 768px) and (device-height: 1024px)
         and (orientation: portrait)
         and (-webkit-device-pixel-ratio: 1)"
      rel="apple-touch-startup-image">
<!-- iPad, landscape -->
<link href="apple-touch-startup-image-748x1024.png"
      media="(device-width: 768px) and (device-height: 1024px)
         and (orientation: landscape)
         and (-webkit-device-pixel-ratio: 1)"
      rel="apple-touch-startup-image">
<!-- iPhone 5 -->
<link href="apple-touch-startup-image-640x1096.png"
      media="(device-width: 320px) and (device-height: 568px)
         and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPhone, retina -->
<link href="apple-touch-startup-image-640x920.png"
      media="(device-width: 320px) and (device-height: 480px)
         and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPhone -->
<link href="apple-touch-startup-image-320x460.png"
      media="(device-width: 320px) and (device-height: 480px)
         and (-webkit-device-pixel-ratio: 1)"
      rel="apple-touch-startup-image">
{% endhighlight %}

Older iPhones ignore media queries and just load the last startup
image. So just like the icons, the last one should be a fallback.

If none of the startup images are the right resolution, Safari will
simply display a white screen. Note that all startup images are
actually in portrait orientation. Landscape images should be rotated
90 degrees clockwise. There are no landscape startup images for
iPhones since they always start in the same orientation.

Annoyingly, startup images will be stretched on iOS 7. They initially
cover the entire screen. Once the status bar is rendered, they
shrink to their actual size. There is no way to specify a startup
image with the correct resolution.

## Status bar

![Some status bars](/static/images/2013-11-01-status-bar.png)

Just like with startup images, you page must be web app capable to
modify the status bar.

By default, the status bar has black text on a black background.
That makes it essentially useless. To fix it, set the
`apple-mobile-web-app-status-bar-style`.

{% highlight html %}
<meta name="apple-mobile-web-app-status-bar-style"
      content="black">
{% endhighlight %}

The only other allowable value is `black-translucent`. It's a bit
of a misnomer, though. The background is completely translucent and
the text is white. For consistency across all versions of iOS, I
recommend setting this attribute to `black`.

## Notes

Unlike iOS 6, web apps are never letterboxed on iOS 7. That means
you can set the viewport to whatever you want. However, I would
recommend using the workaround anyway for maximum compatibility.

{% highlight html %}
<meta name="viewport"
      content="initial-scale=1">
{% endhighlight %}

By default, Safari resizes text on orientation change. To disable
this, add the `-webkit-text-size-adjust` CSS property.

{% highlight html %}
<style>
    -webkit-text-size-adjust: 100%;
</style>
{% endhighlight %}

[1]: https://developer.apple.com/library/ios/documentation/AppleApplications/Reference/SafariWebContent/ConfiguringWebApplications/ConfiguringWebApplications.html#//apple_ref/doc/uid/TP40002051-CH3-SW3
[2]: {% post_url 2012-03-27-ios-web-app-icons-and-startup-images %}
[3]: {% post_url 2012-09-20-iphone-5-web-app-startup-image %}
[4]: /static/pages/2013-11-01-ios-7-web-app.html
[5]: /static/images/2013-11-01-title.png
[6]: /static/images/2013-11-01-icons.png
[7]: /static/images/2013-11-01-startup-images.png
