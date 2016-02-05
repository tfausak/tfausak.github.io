---
title: I accidentally deleted all our data
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
but Cody told a story from his old job. Everything crashed immediately
after pushing a complete rewrite to production.

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

``` python
from mongoengine import connect
from models import Family
connect('the-production-database')
for family in Family.objects:
    family.save()
```

That finished without issue, so I shut my laptop and turned my
attention back to the conference. A couple hours later, Cody was
getting a bunch of emails about our server responding slowly. He
popped open his laptop and I glanced over at the screen. I nearly
had a heart attack when I saw:

``` python
>>> Family.objects.count()
38
```

That number was off by a couple orders of magnitude! We quietly
freaked out, then excused ourselves from the conference room.

It quickly became clear that our family accounts, and only our
family accounts, got clobbered. I looked at the least recent family
account and compared their join date to my latest commit date. They
were suspiciously close.

Unfortunately [my screen session][5] doesn't have enough scrollback
to see what I entered earlier in the day. And since I was working
in the interactive shell, I didn't have any history. My best guess,
though, is that I did something like this, instead:

``` python
for family in Family.objects:
    family.delete()
```

D'oh! Both `save` and `delete` don't return anything, so I didn't
notice anything wrong when I saw the families scroll by. (I'm not
convinced this is what actually happened, but it's the simplest
explanation. Occam's razor is up against my ego here.)

The internet connection at the conference was spotty at best, and
power outlets were nowhere to be found, so we packed up our stuff
and hustled to find a coffee shop. Turns out, all the coffee shops
in downtown Dallas close at about four in the afternoon. They offer
free wifi 24 hours a day, though, so we set up shop outside of a
Starbucks and got to work.

(It was much colder in Dallas than we anticipated. When we left
Austin, it was 80 degrees and sunny. In Dallas, it was 40, cloudy,
and windy. The three of us were huddled around a laptop in jeans
and t-shirts, trying to quickly fix the problem before we froze.)

We had a backup from earlier in the week, so we transferred that
over to our development box and Cody restored it to a separate
database. Then I wrote a script to transfer the missing family
accounts from one to the other. Fortunately, everything went smoothly
and all the families were restored.

There's still some work to be done to make sure all the references
in our database point to the right things, but most of the fires
have been put out. November 17 will live on as our [backup awareness
day][6].

[1]: http://www.10gen.com/events/mongodb-dallas-2011
[2]: http://www.codypowell.com
[3]: https://twitter.com/shaundubs
[4]: http://www.famigo.com
[5]: {% post_url 2011-10-27-be-productive-use-screen %}
[6]: http://www.codinghorror.com/blog/2009/12/international-backup-awareness-day.html
