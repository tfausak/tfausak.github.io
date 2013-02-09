---
layout: post
title: ReDoS: Regular Expression Denial of Service
published: false
---

hit this issue (#2501)
thought it had to do with cloning forms
expected an unrelated fix to take care of it

it didnt
checked it locally
pegged a cpu for 20 minutes
showed no signed of stopping
wondered what would peg a cpu without hitting ram or disk

debugging got me down to a url validator
for this url

    http://www.dentistry.uiowa.edu/missions/education/predental_club.shtmlhttp://

and this is what was trying to parse it

    $URL_VALIDATION_STRING = [
      'https?:\/\/',
      '(?:.+\.)+',                # subdomains and domains
      '[a-zA-Z]+\/?',             # suffix
      '(?:[^.\/]+\/?)*',          # directories
      '(?:[^.]+\.[a-zA-Z0-9]+)?', # file
      '(?:[#?].*)?'               # #anchor or ?args=bla&...
    ].join('')

if you have an eagle eye for regexes, you might see it
the problem is the "directories" bit
here's a minimal test case showing how poorly it performs
this takes about 5 seconds on my machine

    /^([^.\/]+\/?)*$/.match('0123456789012345678901234//')

at this point i remembered that perl can debug regexes for you
but i couldnt remember the right syntax since i havent touched perl in years
stackoverflow to the rescue!
http://stackoverflow.com/a/2348725/1274282
so i ran this for shits and giggles

    perl -Mre=debug -e'"0123456789012345678901234//"=~/^([^.\/]+\/?)*$/'

so i rewrote the regular expression

    $URL_VALIDATION_STRING = [
      'https?:\\/\\/',         # protocol
      '(?:[-0-9A-Za-z]+\\.)+', # subdomains
      '[A-Za-z]+',             # top level domain
      '(?:',
        '\\/[^#?]*',           # path
        '(?:\\?[^#]*)?',       # query string
        '(?:#.*)?',            # fragment
      ')?'
    ].join('')

before you go quoting zawinski on me, we have to use regexes
we want the same validation client- and server-side
so we pass the validator off to
