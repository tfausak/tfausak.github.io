---
title: Relate intervals with Rampart
---

Have you ever needed to know if two intervals overlap?
I wrote [Rampart][], a small Haskell library to help with that.

The other day at work we were working on a system that needed to select accounts that were active during a certain timeframe.
This seemingly simple problem prompted a bunch of discussion.
We spent some time puzzling through inequalities before we landed on something that everyone agreed would do what we wanted.
I figured we could save ourselves and others time in the future by making a library to handle the tricky interval logic.

You may be thinking that this is a trivial problem.
Did you know that there are 13 different ways that two intervals can relate to each other?
Here they are:

![interval relations][]

[Rampart][] is a tiny Haskell module that can take two intervals and tell you how they relate.
For example, how does the interval from 2 to 4 relate to the interval from 3 to 5?

    >>> import Rampart
    >>> relate (toInterval (2, 4)) (toInterval (3, 5))
    Overlaps

It's easy to show off with numbers, but it works with anything that's orderable: dates, times, money, and so on.
So if you find yourself asking questions about intervals, consider using [Rampart][] to answer them.

You can find the documentation here:
<https://hackage.haskell.org/package/rampart-1.0.0.1/docs/Rampart.html>.

[Rampart]: https://hackage.haskell.org/package/rampart
[interval relations]: /static/images/2020/03/13/interval-relations.svg
