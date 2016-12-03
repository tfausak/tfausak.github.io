---
title: Blunt is now pointfree.info
---

<aside>I am no longer hosting Blunt. Consider using <a href="http://pointfree.io">pointfree.io</a> instead.</aside>

Last year I [announced Blunt][1], a pointless Haskell tool. By "pointless", I
mean that it converts Haskell expressions between pointfree and pointful
styles. The point*free* style is sometimes called point*less*.

This year I am happy to say that Blunt now lives pointfree.info, which is
much more memorable than its old URL. The guts of the site have stayed mostly
the same, but the way that it is deployed is completely different.

I used to deploy Blunt to Heroku for two reasons. One, it was free. And two, it
was easy, once it was set up. But setting it up was a huge pain and not
something that I wanted to do again. And Heroku changed their free plan to only
allow [18 hours per 24 hours][2]. Even though Blunt wasn't exactly high
availability, I wanted it to be available whenever.

So I needed a new deployment solution. Being a fan of [Docker][3] containers, I
looked for somewhere to host them and found [Joyent][4]'s Triton
infrastructure. For less than $3 a month they could run a container for me.

I spent about an hour setting up both Docker and Joyent. Then I spent another
hour waiting for my newly purchased domain name's DNS changes to propagate.
Finally I waiting a third hour for [Docker Hub][5] to build my image. Once all
that was done, I had my container running and available at pointfree.info.

I really enjoyed containerizing my Haskell application and deploying it to
Triton. I highly recommend it!

[1]: {% post_url 2015-04-02-announcing-blunt-a-pointless-haskell-tool %}
[2]: https://blog.heroku.com/archives/2015/5/7/heroku-free-dynos
[3]: https://www.docker.com
[4]: https://www.joyent.com
[5]: https://hub.docker.com
