---
title: The importance of documentation
---

Yesterday, I spent a couple hours scouring an old machine for a
project I wrote a few years ago. I had long since forgotten where
the project might be, what it was called, or how it was laid out.
Searching every folder in my home directory was the only sure-fire
way to find it.

I quickly got frustrated with my past self. Very few of the folders
contained any kind of documentation. Often the only recognizable
file would be `index.php`, but of course there were no comments in
it. So I had to re-immerse myself in every project that I opened
just to figure out if it was the one I was looking for.

By this point, I was beating myself for not littering my top-level
directories with `README`s. Without them, understanding old code
bases turns into archaeology. I can see why [GitHub][1] recommends
them for every repository they host.

But what makes a good `README`? Just having the file doesn't get
you anywhere. You need to fill it with useful information. I try
to answer the [Five Ws][2] in mine.

-   *Who* created it? This is the person to bother if anything
    doesn't work like it should. Alternatively, if the creator isn't
    appropriate, go with the maintainer.

-   *What* does it do? The shorter the description, the better. Try
    to think in terms of what it takes as input and what it produces
    as output.

-   *When* was it created? And when was the last change made? Source
    control and file modification times help here, but they're easy
    to overlook. Knowing when it was created gives you context. A
    project started in 1999 is going to look different than one
    created in 2009.

-   *Where* is it hosted? And, if appropriate, where's the live demo?
    Many projects have extensive online documentation, but it's
    useless if you can't figure out where it is.

-   *Why* was it created? Is it a one-off script or an enterprise
    application? Did you make it to learn a particular technology
    or is it your magnum opus? Was it created for a particular
    platform or is it meant to be used everywhere?

-   *How* does it work? Specifically, how do you get it to work.
    Dependencies, environment, and compilation should all be covered.
    It should be possible to get it working using nothing more than
    this.

If my past self had followed this advice, hunting for my old
project would've been a piece of cake. I eventually found it, added
a `README`, and uploaded it to GitHub where it lives as [bdbxml-jukebox][3].

[1]: https://github.com
[2]: http://en.wikipedia.org/wiki/Five_Ws
[3]: https://github.com/tfausak/bdbxml-jukebox
