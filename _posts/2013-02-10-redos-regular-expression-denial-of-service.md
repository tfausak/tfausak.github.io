---
title: 'ReDoS: regular expression denial of service'
---

[![Regular expression problems][1]][2]

On Thursday, I ran into a puzzling issue at work. It first occurred
three weeks ago and remained uninvestigated because it appeared to
be a duplicate of another issue. When I deployed the fix for the
other issue, I expected it to fix this one too. Turns out, it
didn't.

So I took a closer look at it. A particular page sometimes timed
out --- returned a 502 after 30 seconds. I suspected some sub-optimal
SQL queries were to blame, so I reproduced it locally. That's when
things got weird.

The Rails logger stayed silent, meaning nothing was querying the
database. I popped open OS X's activity monitor and saw 100% CPU
utilization but no RAM or disk usage. That was strange. It's not
often that our application hits a CPU bottleneck.

I dropped a debugger into the view, reloaded the page, and ...
nothing happened. It pegged my processor before it reached the break
point. That meant a before filter was to blame, so I threw a debugger
into those and chased down the offending line of code. It appeared
completely benign:

``` ruby
req.save if req.new_record? || req.changed?
```

Nothing particularly concerning about that. The validators that get
called before saving the record are another matter entirely, though.
In particular, that `req` object has a URL field. It's checked
against the following regular expression:

``` ruby
%r{
  https?://             # protocol
  (?:.+\.)+             # subdomains
  [a-z]+/?              # top level domain
  (?:[^./]+/?)*         # path
  (?:[^.]+\.[0-9a-z]+)? # file
  (?:[#?].*)?           # query string or fragment
}ix
```

(Before you pull out that [Zawinski][3] quote, I know this isn't
ideal. [`URI::regexp`][4] is better. That's not the point.)

If you have an eagle eye for regexes, you might spot the problem
right away. I didn't, so I played with both the regex and URL until
I whittled it down to this evil part:

``` ruby
/^([^.\/]+\/?)*$/.match('0123456789012345678901234//')
```

That's the path component of the pattern trying to match against a
pathological string. I figured the problem was with nested repetition
--- a `+` inside a group with a `*` isn't a good sign. In fact,
it's listed as an example of an evil regex on [Wikipedia's ReDoS
page][5].

I was curious about exactly what it was doing, so I fired up Perl
to debug the regex, since Ruby doesn't have a regex debugger. This
script confirmed my suspicions:

``` perl
use re 'debugcolor';
'0123456789012345678901234//' =~ /^([^.\/]+\/?)*$/;
```

The solution was to rewrite the evil part of the pattern. The
original regex tried too hard to break the URL into components,
especially considering they weren't being saved into match groups.
The rewrite is a little more lax in that regard.

``` ruby
%r{
  https?://          # protocol
  (?:[-0-9a-z]+\.)+  # subdomains
  [a-z]+             # top level domain
  (?:
    /[^#?]*          # path
    (?:\?[^#]*)?     # query string
    (?:#.*)?         # fragment
  )?
}ix
```

[1]: /static/images/2013/02/10/regular-expression-problems.png
[2]: http://xkcd.com/1171/
[3]: http://en.wikiquote.org/wiki/Jamie_Zawinski
[4]: http://www.ruby-doc.org/stdlib-1.9.3/libdoc/uri/rdoc/URI.html#method-c-regexp
[5]: http://en.wikipedia.org/wiki/ReDoS
