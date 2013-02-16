---
layout: post
title: Syntax Highlighting for Sheet Music
---

A couple weeks ago, I read Kyle Isom's [:syntax off][1] blog post
about disabling syntax highlighting. It was interesting to hear his
opinion because every developer I've ever known took syntax
highlighting as a given.

However, I'm more interested about this statement he makes: "When
I read a book, I don’t want parts of speech highlighted in different
colours." I thought it would be interesting to read a color-coded
book, but his remark reminded me of a story I heard in high school.

I played saxophone in the band. In the band hall one day, a private
lesson teacher was talking about a student they had that could only
read color-coded music. I forget the exact scheme, but I think it
was red for sharps and blue for flats. Since almost all printed
music is strictly black and white, the student had to be weaned of
her habit.

But music doesn't have to be black and white! We have powerful
engraving software like [LilyPond][2] available to us. I spent a
little while playing around with it and came up with this simple
color scheme:

![Musical scales][3]

The scaffolding, like bar lines and key signatures, are subtly muted
while sharps and flats are highlighted in red and blue, respectively.

Scales aren't very interesting as far as accidentals go, so let's
try something a little more complex. Here's an excerpt from Wilhelm
Ferling's Famous Studies (#13):

![Ferling's Famous Studies #13][4]

I haven't actually played any music in a few years, so I don't know
if this is an improvement or not. I think it looks better, though.
(Check out [the source][5] to see how I did it.)

[1]: http://www.kyleisom.net/blog/2012/10/17/syntax-off/
[2]: http://lilypond.org
[3]: /static/images/2012-11-15-musical-scales.png
[4]: /static/images/2012-11-15-ferlings-famous-studies-13.png
[5]: https://gist.github.com/tfausak/4083481
