---
title: iOS Web App Icons & Startup Images
---

<aside>Update: <a href="{% post_url 2013-11-01-ios-7-web-apps %}">iOS 7 Web Apps</a></aside>

<aside>Update: <a href="{% post_url 2012-09-20-iphone-5-web-app-startup-image %}">iPhone 5 Web App Startup Image</a></aside>

Apple makes it a piece of cake to develop web apps for iOS.
Unfortunately, they haven't updated [their web app documentation][1]
since October of last year. That wouldn't be a big deal, but they
wrote it before the release of the new iPad. They need to update
it to explain how to configure web apps on the new iPad and
retina iPhones.

Until they do, I hope this post can help you configure your web app
on iOS devices. I tested using the iOS simulator running iOS 5.0
on an iPhone, iPad, and retina iPhone. I also tested on a physical
iPod touch and iPad, both with retina displays running iOS 5.1. All
the techniques described here should work with any iOS device running
at least iOS 3.0.

## Icons

Although Apple's documentation pretty much covers all the bases,
they don't explicitly say how to supply an icon for a retina iPad.
They do, however, show you how to add icons for iPads and retina
iPhones. All you have to do is set the `sizes` attribute to `72x72`
for iPads and `114x114` for retina iPhones. Since retina iPads have
twice the resolution of regular iPads, it makes sense that their
icons are `144x144`, twice that of the regular iPad.

{% highlight html %}
<!-- iPad (Retina) -->
<link rel="apple-touch-icon"
      sizes="144x144"
      href="apple-touch-icon-144x144.png">
{% endhighlight %}

The official icon sizes are listed in Apple's [icon creation
guidelines][2]. If you want to support all iOS devices, you need
the following sizes: `57x57`, `72x72`, `114x114`, and `144x144`.

## Startup Images

The documentation for startup images only covers portrait-oriented
iPhones. There's no mention of retina iPhones, iPads, or landscape
images.

### iPhones

iPhones only support portrait orientation. This is because web apps
cannot start in landscape. The home screen is always oriented the
same way, so web apps always start the same way. Once they finish
loading, they rotate to whatever orientation the device is in.

That means iPhone startup images must be in the portrait orientation.
The resolution Apple recommends, `320x460`, is the correct one.
Note that it's 20 pixels shorter than the iPhone's resolution of
`320x480`. The status bar is always visible while a web app is
loading, and it's exactly 20 pixels tall.

Startup images for retina iPhones should be twice the resolution,
`640x920`. Unfortunately, linking to a retina startup image isn't
as easy as the icon. The `sizes` attribute doesn't work for startup
images. Looking for a workaround to this lead me to a Gist about iOS web
app configuration. It reminded me that startup images are just `<link>`
elements that can be targeted using media queries.

All iPhones report their resolution as `320x480`. This means a media
query of `(device-width: 320px)` will match all iPhones. Retina
iPhones report their [pixel ratio][3] as `2`, so a
`(-webkit-device-pixel-ratio: 2)` will match retina iPhones.

{% highlight html %}
<!-- iPhone -->
<link rel="apple-touch-startup-image"
      media="(device-width: 320px)"
      href="apple-touch-startup-image-320x460.png">
<!-- iPhone (Retina) -->
<link rel="apple-touch-startup-image"
      media="(device-width: 320px)
         and (-webkit-device-pixel-ratio: 2)"
      href="apple-touch-startup-image-640x920.png">
{% endhighlight %}

### iPads

Unlike iPhones, iPads can start web apps in any orientation.
Fortunately, the `orientation` media query makes specifying startup
images easy as pie. The technique is the same as the one described
for iPhones, except iPads report their resolution as `768x1024`.

{% highlight html %}
<!-- iPad (portrait) -->
<link rel="apple-touch-startup-image"
      media="(device-width: 768px)
         and (orientation: portrait)"
      href="apple-touch-startup-image-768x1004.png">
<!-- iPad (landscape) -->
<link rel="apple-touch-startup-image"
      media="(device-width: 768px)
         and (orientation: landscape)"
      href="apple-touch-startup-image-748x1024.png">
<!-- iPad (Retina, portrait) -->
<link rel="apple-touch-startup-image"
      media="(device-width: 768px)
         and (orientation: portrait)
         and (-webkit-device-pixel-ratio: 2)"
      href="apple-touch-startup-image-1536x2008.png">
<!-- iPad (Retina, landscape) -->
<link rel="apple-touch-startup-image"
      media="(device-width: 768px)
         and (orientation: landscape)
         and (-webkit-device-pixel-ratio: 2)"
      href="apple-touch-startup-image-1496x2048.png">
{% endhighlight %}

Note that landscape images are not actually defined as such ---
they're taller than they are wide. When you're creating your assets,
make them as landscape images. Then, when you're ready to publish,
rotate them 90° clockwise.

## Complete Example

I've created [a minimal web app][4] that includes all icons and
startup images for iOS devices. It's also available [a Gist][5].
Please let me know if you notice any problems or want to suggest
an improvement.

## Other Platforms

Although I don't have any devices to test it on, apparently the
icons work on Android. As far as I understand it, startup images
are not supported. Unfortunately the Android documentation doesn't
mention anything about icons or startup images for web apps.

Icons and startup images are completely unsupported on Windows Phone
7. However, it is possible to create [mobile web app tiles on Windows
Phone 7][6] using completely unrelated techniques.

[1]: http://developer.apple.com/library/ios/#DOCUMENTATION/AppleApplications/Reference/SafariWebContent/ConfiguringWebApplications/ConfiguringWebApplications.html
[2]: http://developer.apple.com/library/ios/#DOCUMENTATION/UserExperience/Conceptual/MobileHIG/IconsImages/IconsImages.html%23//apple_ref/doc/uid/TP40006556-CH14
[3]: https://developer.mozilla.org/en-US/docs/CSS/Media_queries#-moz-device-pixel-ratio
[4]: /static/pages/2012-03-27-web-app.html
[5]: https://gist.github.com/tfausak/2222823
[6]: {% post_url 2012-02-03-windows-phone-7-web-page-tiles %}
