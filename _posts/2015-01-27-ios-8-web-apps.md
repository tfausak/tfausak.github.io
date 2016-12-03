---
title: iOS 8 web apps
---

<aside>Some parts of this do not (and apparently cannot) work in iOS 9 or 10. See <a href="https://gist.github.com/tfausak/2222823">this Gist</a> for a lot of discussion.</aside>

Apple released the iPhone 6 and 6 Plus a few months ago. They also
released iOS 8 along with it. Unfortunately they didn't update
[their web app documentation][1] at the same time. That leaves the
official docs woefully out of date.

Apple has done this in the past. I filled in the gaps for configuring
web apps with [iOS 7][2], [iOS 6][3], and [iOS 5][4]. This post
will do the same for iOS 8. But instead of making you flip between
four guides, I've put everything here.

This information works for all versions of iOS on all iPhones,
iPads, and iPod Touches. Check out [my example web app][5] on an
iOS device to see the finished product.

## Configuration

### `apple-mobile-web-app-capable`

``` html
<meta name="apple-mobile-web-app-capable"
      content="yes">
```

This tag allows the page to be run full screen. Note that this only
works when it has been added to the home screen.

### `apple-mobile-web-app-title`

``` html
<meta name="apple-mobile-web-app-title"
      content="iOS Web App">
```

This tag sets a custom title. If it's missing, iOS will use the
`<title>` tag. If that is missing too, it will default to "Favorites".
This is limited to about 8 to 12 characters.

### `apple-mobile-web-app-status-bar-style`

``` html
<meta name="apple-mobile-web-app-status-bar-style"
      content="black">
```

This tag changes the color of the status bar. There are three
options: `default`, `black`, and `black-translucent`. The best bet
for maximum compatibility is `black`; `default` and `black-translucent`
behave differently between iOS 6 and 7. (Sometimes on iOS 7+, the
status bar starts as white-on-white or black-on-black. Restarting
the web app fixes this problem.)

### `viewport`

``` html
<meta name="viewport"
      content="initial-scale=1">
```

This tag sets the size of the browser's viewport. That means it
determines how wide the virtual window is. By setting `initial-scale`
to 1, the virtual window will be the same size as the physical
device. That makes it the only setting you need here. Other settings,
like `device-width` just cause trouble. (In particular, `device-width=320`
will cause letterboxing on iOS 7+.)

``` html
<!-- Only for web apps pretending to be native. -->
<meta name="viewport"
      content="initial-scale=1,minimum-scale=1,maximum-scale=1">
```

If you want to pretend like you're a native app, set `minimum-scale=1`
and `maximum-scale=1`. Be warned that this means users can't scale
your app at all.

### `format-detection`

``` html
<meta name="format-detection"
      content="telephone=no">
```

This tag prevents Safari from automatically linking phone numbers.

## Icons

``` html
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
```

You'll need seven icon sizes:

-   152x152 (76@2x) for iPad retina on iOS 7+
-   144x144 (72@2x) for iPad retina on iOS 6
-   76x76 for iPad on iOS 7+
-   72x72 for iPad on iOS 6
-   180x180 (60@3x) for iPhone 6 Plus
-   144x144 (57@2x) for iPhone retina on iOS 6
-   57x57 for iPhone on iOS 6

There are two icon sizes you might think you need but actually
don't:

-   120x120 (60@x) for iPhone retina on iOS 7+: There's no way to
    tell this apart from the 60@3x icon for the iPhone 6 Plus.
-   60x60 for iPhone on iOS 7+: There are no devices that support
    this combination.

If you set the `rel` to `apple-touch-icon` instead of
`apple-touch-icon-precomposed`, iOS 6 will apply a glossy icon
finish. For consistency with iOS 7+, use precomposed icons.

By default, the icon is a screenshot of the page. If there are no
icon tags, Safari will try the following URLs:

1.  `/apple-touch-icon-180x180-precomposed.png`
2.  `/apple-touch-icon-180x180.png`
3.  `/apple-touch-icon-precomposed.png`
4.  `/apple-touch-icon.png`

The exact dimensions will depend on the device.

## Startup images

``` html
<!-- iPad retina portrait startup image -->
<link href="apple-touch-startup-image-1536x2008.png"
      media="(device-width: 768px) and (device-height: 1024px)
             and (-webkit-device-pixel-ratio: 2)
             and (orientation: portrait)"
      rel="apple-touch-startup-image">
<!-- iPad retina landscape startup image -->
<link href="apple-touch-startup-image-1496x2048.png"
      media="(device-width: 768px) and (device-height: 1024px)
             and (-webkit-device-pixel-ratio: 2)
             and (orientation: landscape)"
      rel="apple-touch-startup-image">
<!-- iPad non-retina portrait startup image -->
<link href="apple-touch-startup-image-768x1004.png"
      media="(device-width: 768px) and (device-height: 1024px)
             and (-webkit-device-pixel-ratio: 1)
             and (orientation: portrait)"
      rel="apple-touch-startup-image">
<!-- iPad non-retina landscape startup image -->
<link href="apple-touch-startup-image-748x1024.png"
      media="(device-width: 768px) and (device-height: 1024px)
             and (-webkit-device-pixel-ratio: 1)
             and (orientation: landscape)"
      rel="apple-touch-startup-image">
<!-- iPhone 6 Plus portrait startup image -->
<link href="apple-touch-startup-image-1242x2148.png"
      media="(device-width: 414px) and (device-height: 736px)
             and (-webkit-device-pixel-ratio: 3)
             and (orientation: portrait)"
      rel="apple-touch-startup-image">
<!-- iPhone 6 Plus landscape startup image -->
<link href="apple-touch-startup-image-1182x2208.png"
      media="(device-width: 414px) and (device-height: 736px)
             and (-webkit-device-pixel-ratio: 3)
             and (orientation: landscape)"
      rel="apple-touch-startup-image">
<!-- iPhone 6 startup image -->
<link href="apple-touch-startup-image-750x1294.png"
      media="(device-width: 375px) and (device-height: 667px)
             and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPhone 5 startup image -->
<link href="apple-touch-startup-image-640x1096.png"
      media="(device-width: 320px) and (device-height: 568px)
             and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPhone < 5 retina startup image -->
<link href="apple-touch-startup-image-640x920.png"
      media="(device-width: 320px) and (device-height: 480px)
             and (-webkit-device-pixel-ratio: 2)"
      rel="apple-touch-startup-image">
<!-- iPhone < 5 non-retina startup image -->
<link href="apple-touch-startup-image-320x460.png"
      media="(device-width: 320px) and (device-height: 480px)
             and (-webkit-device-pixel-ratio: 1)"
      rel="apple-touch-startup-image">
```

You'll need ten startup image sizes:

-   1536x2008 (768x1004@2x) for iPad retina portrait
-   1496x2048 (748x1024@2x) for iPad retina landscape
-   768x1004 for iPad portrait
-   748x1024 for iPad landscape
-   1242x2148 (414x716@3x) for iPhone 6 Plus portrait
-   1182x2208 (394x736@3x) for iPhone 6 Plus landscape
-   750x1294 (375x647@2x) for iPhone 6
-   640x1096 (320x548@2x) for iPhone 5
-   640x920 (320x460@2x) for iPhone retina
-   320x460 for iPhone

Note that you only need portrait and landscape images for iPads and
the iPhone 6 Plus. All other iPhones only start web apps in portrait.

In order for these to work, your web app must be
`apple-mobile-web-app-capable`. They will only show up when launching
your web app from the home screen.

Unlike icons, startup images don't have a default. If you don't
provide one, the screen will be plain white. And Safari doesn't try
anything automatically. You have to add the meta tags.

Landscape images aren't actually in landscape; they're taller than
they are wide. They should be rotated 90 degrees clockwise from
their correct orientation.

On iOS 7+, startup images are stretched when the app launches. On
iOS 7, the image pops under the status bar once the app loads.
Unfortunately there's no way to provide a startup image with the
correct dimensions. It will always be stretched on iOS 7+.

That being said, startup images must be the exact resolution listed
here. Safari will not perform any scaling.

## Hacks

``` html
<style>
  html {
    -webkit-text-size-adjust: 100%;
  }
</style>
```

This style prevents fonts from getting bigger when rotating to
landscape.

[1]: https://developer.apple.com/library/content/documentation/AppleApplications/Reference/SafariWebContent/ConfiguringWebApplications/ConfiguringWebApplications.html
[2]: {% post_url 2013-11-01-ios-7-web-apps %}
[3]: {% post_url 2012-09-20-iphone-5-web-app-startup-image %}
[4]: {% post_url 2012-03-27-ios-web-app-icons-and-startup-images %}
[5]: /static/pages/2015-01-27-ios-8-web-app.html
