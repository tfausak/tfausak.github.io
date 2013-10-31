---
layout: post
title: iOS 7 Web App
---

## Title

-   defaults to "Favorites"
-   uses value of title if available
-   apple-mobile-web-app-title takes precedence

## Icon

-   fetches icon when hitting the share button
    -   and when launching (as a web app)
-   defaults to plain white
-   automatically tries (if nothing specified):
    -   /apple-touch-icon-120x120-precomposed.png
    -   /apple-touch-icon-120x120.png
    -   /apple-touch-icon-precomposed.png
    -   /apple-touch-icon.png
-   prefers precomposed if both given
-   rescales available size to fit if no exact match
    -   uses highest resolution available
-   need 7 sizes to cover iphone & ipad on ios 6 & 7
    -   152 144 120 114 76 72 57

## Startup Image

-   must be web app capable
-   defaults to plain white
-   doesn't try anything automatically
-   won't show if wrong resolution
-   will be stretched on ios 7. CANNOT specify correct resolution
-   fetches image when hitting the share button
    -   and when launching (as a web app)
-   always have to be portait
    -   rotated 90 degrees CW for "landscape"
-   iphone always launches in portrait
-   iphone 3 loads last startup image, so put that at bottom

## Status bar

-   text is black by default (i.e., hidden)
-   same if you explicitly set it to "default"
-   "black" makes the text white
-   "black-translucent" isn't black, but it is translucent
    -   text is still white
-   for consistency with ios 6, "black" is the best bet

## Notes

-   web apps never letterboxes on ios 7
-   apple-touch-fullscreen doesn't work on ios 7
-   title: 8+ characters
-   prevent text resize on orientation change with -webkit-text-size-adjust:100%
-   ios 7 safari bookmark icon is drawn from the largest apple-touch-icon
