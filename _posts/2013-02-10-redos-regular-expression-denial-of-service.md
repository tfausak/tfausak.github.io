---
layout: post
title: 'ReDoS: Regular Expression Denial of Service'
published: false
---

On Thursday, I ran into a puzzling issue at work. It first occurred
three weeks ago and remained uninvestigated because it appeared to
be a duplicate of another issue. When I deployed the fix for the
other issue, I expected it to fix this one, too. Turns out, it
didn't.

So I took a closer look at it. A particular page sometimes timed
out (in other words, returned a 502 after 30 seconds) for some
users. I suspected some sub-optimal SQL queries were to blame, so
I reproduced it locally. That's when things got weird.

The Rails logger stayed silent, meaning nothing was querying the
database. I popped open OS X's activity monitor and saw 100% CPU
utilization but no RAM or disk usage. "That's strange," I thought.
"Do we have an infinite loop in our code somewhere?"

I dropped a debugger into the view, reloaded the page, and ...
nothing happened. It pegged my processor before it reached the break
point. That meant a before filter was to blame, so I threw a debugger
into that and chased down the offending line of code. It turned out
to be totally benign:

{% highlight ruby %}
req.save if req.new_record? || req.changed?
{% endhighlight %}

Nothing particularly concerning about that line of code. The trouble
lies in the validators that get called before saving the record.
In particular, that `req` object has a URL field. We make sure it's
a valid URL with the following regular expression:

{% highlight ruby %}
%r{
  https://              # protocol
  (?:.+\.)+             # subdomains
  [a-z]+/?              # top level domain
  (?:[^./]+/?)*         # path
  (?:[^.]+\.[0-9a-z]+)? # file
  (?:[#?].*)?           # query string or fragment
}ix
{% endhighlight %}

(Before you quote [Zawinski][], I know this isn't ideal ---
[`URI::regexp`][] is better. That's not the point.)

If you have an eagle eye for regexes, you might spot the problem.
Heck, it's listed as an example of an evil regex on [Wikipedia's
ReDoS page][]. The problem is with the path component: `(?:[^./]+/?)*`.
It applies repetition with `*` to a complex subexpression that
applies repetition with `+`.

I don't have an eagle eye for regexes, so I didn't immediately spot
the problem. I played with both the regex and the offending URL
until I found the evil part. I ended up with this, which takes about
five seconds to execute on my machine:

{% highlight ruby %}
/^([^.\/]+\/?)*$/.match('0123456789012345678901234//')
{% endhighlight %}

The solution was to rewrite the regex without any evil bits:

{% highlight ruby %}
%r{
  https?://             # protocol
  (?:[-0-9A-Za-z]+\.)+  # subdomains
  [A-Za-z]+             # top level domain
  (?:
    /[^#?]*             # path
    (?:\?[^#]*)?        # query string
    (?:#.*)?            # fragment
  )?
}ix
{% endhighlight %}

## Debugging

If you want to see how in particular this regex is behaving as it
matches, try running this Perl script:

{% highlight perl %}
use re 'debug';
'0123456789012345678901234//' =~ /^([^.\/]+\/?)*$/;
{% endhighlight %}

[zawinski]: http://en.wikiquote.org/wiki/Jamie_Zawinski
[uri::regexp]: http://www.ruby-doc.org/stdlib-1.9.3/libdoc/uri/rdoc/URI.html#method-c-regexp
[wikipedia's redos page]: http://en.wikipedia.org/wiki/ReDoS
