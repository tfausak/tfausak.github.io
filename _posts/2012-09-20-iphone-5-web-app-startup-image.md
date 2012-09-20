---
layout: post
title: iPhone 5 Web App Startup Image
---

![iPhone comparison][1]

iphone 5 released
different height
nothing else has changed

old startup images wont work
requests them but doesnt display them
just shows a black white page
pixel ratio doesnt matter

iphone 5 is 1136 pixels tall @2x
logical height is 568 pixels
subtract 20 for the status bar
startup image must be 1096 pixels tall (and 640 wide)

{% highlight html %}
<link rel="apple-touch-startup-image"
      media="(device-width: 320px)
         and (device-height: 568px)
         and (-webkit-device-pixel-ratio: 2)"
      href="apple-touch-startup-image-640x1096.png">
{% endhighlight %}

over-specific media queries
prevent unnecessary downloads
without them some devices would download
but wouldnt display
waste of bandwidth!

[1]: /static/images/2012-09-20-iphone-comparison.png
[2]: /2012/03/27/ios-web-app-icons-and-startup-images/
[3]: /static/pages/2012-09-20-web-app.html
[4]: https://gist.github.com/2222823
