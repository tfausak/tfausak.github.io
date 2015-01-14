---
title: iOS 8 web apps
layout: post
---

[example](/static/pages/YYYY-MM-DD-ios-8-web-app.html)

Past posts:

- [iOS 5]({% post_url 2012-03-27-ios-web-app-icons-and-startup-images %})
- [iOS 6]({% post_url 2012-09-20-iphone-5-web-app-startup-image %})
- [iOS 7]({% post_url 2013-11-01-ios-7-web-apps %})

supports ios >=3, any iphone and any ipad

- [Configuration](#configuration)
- [Icons](#icons)
- [Startup images](#startup-images)
- [Hacks](#hacks)

## Configuration

<https://developer.apple.com/library/safari/documentation/AppleApplications/Reference/SafariHTMLRef/Articles/MetaTags.html>

### apple-mobile-web-app-capable

- allows the page to be run full screen
- only works when added to the home screen

### apple-mobile-web-app-title

- defaults to "Favorites"
- specify a different page name
- this is like the app name
- usually 8-12 characters

### apple-mobile-web-app-status-bar-style

- change the color of the status bar
- `black-translucent` is really white text on transparent
- `black` is the best bet
- sometimes starts white-on-white or black-on-black
  - restarting fixes

### viewport

- `initial-scale` is all you need
- `device-width=320` causes letterboxing
- carefully consider `minimum-scale` and `maximum-scale`
  - should be for emulating apps only!

### format-detection

- set `telephone=no` to not automatically link phone numbers

## Icons

- you need seven:
  1. 76@2x for ipad retina >= ios 7
  2. 72@2x for ipad retina < ios 7
  3. 76 for ipad >= ios 7
  4. 72 for ipad < ios 7
  5. 60@3x for iphone 6 plus
  6. 57@2x for iphone retina < ios 7
  7. 57 for iphone < ios 7
- you *don't* need:
  1. 60@2x for iphone retina >= ios 7 because there's no way to differentiate it from the 6 plus, which is higher resolution
  2. 60 for iphone >= ios 7 because there are no such devices
- precomposed vs not
  - prefers precomposed if both given
- automatically tries some
- last one is fallback

## Startup images

- you need ten:
  1. ipad retina portrait
  2. ipad retina landscape
  3. ipad portrait
  4. ipad landscape
  5. iphone 6 plus portrait
  6. iphone 6 plus landscape
  7. iphone 6 retina portrait
  8. iphone 5 retina portrait
  9. iphone retina portrait
  10. iphone portrait
- you *don't* need:
  1. landscape for non plus iphones because they can't start in that orientation
  2. iphone 5 or 6 form factor non-retina because no such devices exist
- requires web app capable
- landscape images are rotated 90 degrees clockwise
- stretched on ios 7. deal with it.
  - fixed in ios 8
- defaults to plain white
- doesn't try anything automatically
- last one is fallback
- must be the exact resolution - no scaling

## Hacks

- `-webkit-text-size-adjust` prevents font from getting huge when rotating to landscape
