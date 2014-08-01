---
title: How four characters caused 579 errors
---

When I started work this morning, my goal was to eliminate errors
from our server log. We have a couple functions that occasionally
throw errors when given weird input. Typically they don't matter
because no data gets messed up and the user doesn't see anything.

However, they are problematic because they pollute the logs, making
it harder to find real errors. After looking at them every day for
a while, I decided to get rid of them. Unfortunately, in doing so
I managed to introduce a real error. It didn't affect any users,
but it did affect our data.

So I started combing through the logs and looking for errors. Most
of them were easy to fix. Throwing in an `if`/`else` or `try`/`except`
block usually got the job done. Other times preconditions that used
to be true weren't true any more.

The trickiest ones, though, were logic errors. Take this chunk of
code for example (we use [Django][1], backed by [MongoEngine][2]):

{% highlight python %}
if 'family_id' in request.GET:
    family = Family.objects(id=request.GET['family_id']).first()
    if family:
        request.session['family_id'] = family.id
{% endhighlight %}

Generally speaking, calling `objects(...).first()` instead of
`objects.get(...)` is a good way to avoid handling `DoesNotExist`
and `MultipleObjectsReturned` errors. However, the parameters have
to be valid, otherwise it'll return a `ValidationError`.

I saw a bunch of `ValidationError`s in the logs, and this was the
culprit. Turns out that `family_id` was sometimes set to `None`,
which is an invalid ID. I knew it was an easy fix, so I changed it
to this:

{% highlight python %}
try:
    family = Family.objects(id=request.GET.get('family_id'))
except (ValidationError, Family.DoesNotExist):
    pass
if family is not None:
    request.session['family_id'] = getattr(family, 'id', None)
{% endhighlight %}

That took care of the `ValidationError`s, but I introduced two more
subtle bugs. So subtle, in fact, that our unit tests missed them.
I pushed my commit and went on with my day.

A couple hours later, [Cody][3] let me know that the logs were
exploding with errors. The messages were cryptic, but they started
around the time I pushed that commit. My heart sunk. I immediately
started thinking about the time [I accidentally deleted all our
data][4] and hoped it wasn't that bad.

I looked over my commits. ([GitHub's compare view][5] helped
immensely.) At first, nothing seemed amiss. I finally got it down
to three possible commits and scrutinized every bit of them. That's
when I saw it: I wrote `Family.objects(...)` instead of
`Family.objects.get(...)`. That missing `.get` meant I was returning
a `QuerySet` instead of a `Family` object.

That wouldn't have been so bad if it wasn't for the other change I
made further down. Instead of using `family.id`, which would've
raised an `AttributeError` when called on a `QuerySet`, I used
`getattr(family, 'id', None)`, which silently returns `None`.

I quickly changed the offending code and pushed a new commit. The
final working code looked like this:

{% highlight python %}
try:
    family = Family.objects.get(id=request.GET.get('family_id'))
except (ValidationError, Family.DoesNotExist):
    pass
if family is not None:
    request.session['family_id'] = family.id
{% endhighlight %}

During the six hours that the code was missing that very important
`.get`, it managed to cause 579 errors. Luckily they didn't affect
any users and only had a minimal impact on our data.

After slamming my head on the desk for a couple minutes, I wrote a
handful of test cases to make sure this problem never happened
again.

[1]: https://www.djangoproject.com
[2]: http://mongoengine.org
[3]: http://www.codypowell.com
[4]: {% post_url 2011-11-18-i-accidentally-deleted-all-our-data %}
[5]: https://github.com/blog/612-introducing-github-compare-view
