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
  1. 76@2x (152) for ipad retina >= ios 7
  2. 72@2x (144) for ipad retina < ios 7
  3. 76 for ipad >= ios 7
  4. 72 for ipad < ios 7
  5. 60@3x (180) for iphone 6 plus
  6. 57@2x (114) for iphone retina < ios 7
  7. 57 for iphone < ios 7
- you *don't* need:
  1. 60@2x (120) for iphone retina >= ios 7 because there's no way to differentiate it from the 6 plus, which is higher resolution
  2. 60 for iphone >= ios 7 because there are no such devices
- precomposed vs not
  - prefers precomposed if both given
- automatically tries some
- last one is fallback

## Startup images

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

- `-webkit-text-size-adjust` prevents font from getting huge when rotating to landscape
