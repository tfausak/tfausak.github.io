---
layout: post
title: Adding Pinterest's "Pin It" Button
---

At [Famigo][1], we want to make it as easy as possible to share
your favorite [free apps][2] with your family and friends. When I
[rebuilt our website with Twitter Bootstrap][3], I included social
widgets on every page to make them easy to share.

[![Cut the Rope][4]][5]

They were pretty easy to drop in. A chunk of JavaScript and a snippet
of HTML for each one and they're ready to go. Recently, I decided
to add Pinterest's ["Pin It"][6] button to the row of social widgets.
Adding it was considerably harder than the others.

For starters, the button doesn't use the page's current URL
automatically. A strange omission, but not particularly hard to
correct. Django makes specifying the URL a piece of cake with
[`build_absolute_uri`][7]:

    http://pinterest.com/pin/create/button/
    ?url={% literal %}{{ request.build_absolute_uri }}{% endliteral %}

Every pin on Pinterest has a description. They recommend setting
the description with the "Pin It" button because "it lowers the
friction for your users to pin your products". Unfortunately, they
don't automatically get the page's meta or [Open Graph][8] description.
Fortunately, this allows us to tailor the description for Pinterest.

We want several bits of information about the app in the description:
name, rating, price, and our review. Since we like to keep things
positive, we'll include the "cool" part of our review and leave out
the "drool" part.

    &description={% literal %}{{ application.name }}{% endliteral %}
                 {% literal %}{{ application.rating }}{% endliteral %}
                 {% literal %}{{ application.price }}{% endliteral %}
                 {% literal %}{{ application.cool }}{% endliteral %}

Now, on to the hardest part: the image. By this point, I'm not
surprised that Pinterest doesn't load any media automatically. No
favicon, no Open Graph image, no images from the DOM, nothing.

Luckily that lets us pick the best image for the app. The first
choice is the banner image from the Android Market. If an app doesn't
have that, a screenshot is the next best thing. For the rare app
that has neither, the icon will have to do.

Since that logic is a little complicated for a template, let's throw
it in a method. Here it is as pseudo-code:

{% highlight python %}
def pinterest_media(self):
    if self.banner:
        return self.banner
    if self.screenshots:
        return self.screenshots[0]
    return self.icon
{% endhighlight %}

Adding it to the URL for the "Pin It" button is a piece of cake
now.

    &media={% literal %}{{ application.pinterest_media }}{% endliteral %}

[![Talking Tom Cat][9]][10]

Curiously, the button doesn't show the number of pins. I haven't
figured that out yet.

[1]: http://www.famigo.com/
[2]: http://www.famigo.com/free-apps/
[3]: /2012/02/08/rebuilding-famigo-with-twitter-bootstrap/
[4]: /static/images/2012-02-25-figure-1.png
[5]: http://www.famigo.com/app/cut-the-rope/
[6]: http://pinterest.com/about/goodies/#button_for_websites
[7]: https://docs.djangoproject.com/en/dev/ref/request-response/#django.http.HttpRequest.build_absolute_uri
[8]: http://ogp.me/
[9]: /static/images/2012-02-25-figure-2.png
[10]: http://www.famigo.com/app/talking-tom-cat/
