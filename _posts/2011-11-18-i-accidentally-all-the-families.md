---
layout: post
title: I Accidentally All the Families
published: false
---

Yesterday, I went to [MongoDallas][1] with [Cody][2] and [Shaun][3],
my coworkers at [Famigo][4]. We first heard about it a couple months
ago and figured it'd be a fun trip. We use MongoDB for nearly
everything, and Cody gave a presentation about that.

The conference went pretty well, nothing really out of the ordinary
as far as conferences go. (10gen supplied an absurd number of mugs,
just like at the last event.) During lunch, we shot the shit with
some folks from GameStop. One of them asked what the worst thing
we'd done to a production server was. I didn't really have an answer,
but Cody told a story about how everything crashed immediately after
pushing a complete rewrite to production.

Turns out, I would make my worst production blunder later that day.

Before lunch, in between two presentations, I checked on our server
to make sure everything was going smoothly. I noticed an error
related to saving unique field values. Somehow a race condition in
our API caused two family accounts to be created with the same email
address, which is required to be unique.

I quickly debugged the error, made a note in our bug tracker to fix
the root cause, and found the offending family account. I deleted
the account since no data was associated with it and our client
would automatically recreate it next time they logged in.

Then I wanted to make sure that there weren't any other offending
family accounts in our database. I looped over everyone in our
database and saved them (without making any changes); that would
raise the error if anything was amiss. I was working in Python's
interactive shell, so I don't have the actual code, but it looked
something like this:

    from mongoengine import connect
    from models import Family
    connect('the-production-database')
    for family in Family.objects:
        family.save()

That finished without issue, so I shut my laptop and turned my
attention back to the conference. A couple hours later, Cody was
getting a bunch of emails about our server responding slowly. He
popped open his laptop and I glanced over at the screen. I nearly
had a heart attack when I saw:

    >>> Family.objects.count()
    38

That number was off by a couple orders of magnitude! We quietly
freaked out, then excused ourselves from the conference room.

It quickly became clear that our family accounts, and only our
family accounts, got clobbered. I looked at the least recent family
account and compared their join date to my latest commit date. They
were suspiciously close.

Unfortunately [my screen session][5] doesn't have enough scrollback
to see what I entered earlier in the day. And since I was working
in the interactive prompt, I didn't have any history. My best guess,
though, is that I did something like this, instead:

    for family in Family.objects:
        family.delete()

D'oh!

(Post mortem: We recovered from a backup and didn't end up losing
that much. I feel like a total dunce, but there wasn't much lasting
damage.)

[1]: http://www.10gen.com/events/mongo-dallas-2011
[2]: http://www.codypowell.com/
[3]: https://twitter.com/#!/shaundubs
[4]: http://www.famigo.com/
[5]: /2011/10/27/be-productive-use-screen/
