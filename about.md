---
layout: base
title: About
---

# {{ page.title }}

Howdy! My name is Taylor Fausak. I am a 25-year-old software developer
living in Dallas, Texas. I currently work for [OrgSync][1]. I studied
Computer Science at [UT Austin][2] for a while, but I don't have
anything to show for it.

The best way to reach me is by sending an email to [taylor@fausak.me][3]. To
find me at other places on the web, check [my Keybase profile][4].

![Taylor Fausak][5]

## Projects

- [ActiveInteraction][] (Ruby):
  ActiveInteraction confidently manages business logic. It is an implementation
  of the command pattern in Ruby. [Aaron Lasseigne][] and I developed it to add
  a little bit of static typing to Ruby. It is also good for reusing code in
  different parts of an application.

- [Stoplight][] (Ruby):
  Stoplight controls code like traffic. It is an implementation of the circuit
  breaker pattern in Ruby. [Cameron Desautels][] and I created it as a way to
  wrap sections of code that occasionally fail. It prevents failures from
  cascading by short-circuiting problematic calls.

- [Haskeleton][] (Haskell):
  Haskeleton is a project skeleton for Haskell packages. It provides a complete
  and idiomatic starting point. I made it to codify best practices. It also
  encourages package authors to create tests and benchmarks.

- [Blunt][] (Haskell):
  Blunt converts Haskell expressions between the pointfree and pointful styles.
  It is a web front end to the `pointfree` and `pointful` executables extracted
  from the Lambdabot. I created it as a more convenient way to use those tools.
  I also learned how to create WebSocket clients and servers for this service.

- [Flow][] (Haskell):
  Flow allows writing more understandable Haskell. It borrows function
  application and composition operators from F#, Elm, and Elixir. I got fed up
  with cryptic, backwards operators and decided to create my own. It is a hard
  sell with the Haskell community, though.

- [Erudite][] (Ruby):
  Erudite helps turn documentation into tests. It is a Ruby version of Python's
  `doctest` module. I started working on it after getting frustrated with the
  existing gems that did this. Also it is strange to me that doctests are not
  as popular in Ruby as they are in Python.

- [Strive][] (Haskell):
  Strive wraps the Strava V3 API. It provides a convenient way to consume the
  API from Haskell. I built this to see how writing real-world stuff in Haskell
  felt. I liked it, and I learned about lenses along the way.

- [Delay][]:
  Delay slows down time-wasting websites. It is a Safari extension that adds a
  delay to the loading of websites like Facebook and Reddit. I made it to curb
  my own usage of such sites. There are lots of Firefox addons and extensions
  that do the same thing, but I don't know of any other Safari extensions.

- [Wuss][] (Haskell):
  Wuss supports secure WebSockets. It handles the WSS protocol in Haskell. I
  wrote it to scratch an itch; the standard WebSocket library did not support
  WSS and I needed it for a project. Instead of making it only for myself, I
  released it as a package.

- [Ledger][] (Haskell, JavaScript):
  Ledger tracks expenses. It is an alternative to manually entering
  transactions into a spreadsheet. I used this project as a way to learn how to
  build an API with Haskell. And the front end is powered completely by React.

- [Moxen][] (Python):
  Moxen catalogs Magic: the Gathering cards. It offers an interface for
  managing a collection. I created it as an alternative to the Gatherer, but I
  never actually deployed it. I did get pretty good at writing web scrapers
  because of this.

- [Mad Minute][] (Objective-C):
  Mad Minute is a fast-paced math game. It is an iOS version of a game I played
  in second grade. I wrote this as an example consumer of the [Famigo][] API.
  It was in the App Store for a brief while.

- [bdbxml-jukebox][] (PHP):
  bdbxml-jukebox plays music in the browser. It is a glorified web front end to
  the getID3 library. I made this way back in high school as a way to listen to
  music on my phone without using up all the storage space. I also happened to
  learn about document-based data stores and the XML toolchain, including XPath
  and XSLT.

[1]: http://www.orgsync.com
[2]: http://www.utexas.edu
[3]: mailto:taylor+honeypot@fausak.me
[4]: https://keybase.io/taylorfausak
[5]: /static/images/taylor-fausak.jpg

[activeinteraction]: http://devblog.orgsync.com/active_interaction/
[aaron lasseigne]: http://aaronlasseigne.com
[stoplight]: http://devblog.orgsync.com/stoplight/
[cameron desautels]: http://camdez.com
[haskeleton]: /haskeleton/
[blunt]: https://blunt.herokuapp.com
[flow]: /flow/
[erudite]: /erudite/
[strive]: /strive/
[delay]: /delay/
[wuss]: /wuss/
[ledger]: https://assembly.com/ledger
[moxen]: /moxen/
[mad minute]: https://github.com/tfausak/MadMinute
[famigo]: http://www.famigo.com/
[bdbxml-jukebox]: https://github.com/tfausak/bdbxml-jukebox
