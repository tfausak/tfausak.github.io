---
title: Parse and generate Rocket League replays with Haskell
---

I am happy to announce version 1.0.0 of [Rattletrap][], a Rocket League replay parser/generator!
It is a command-line utility for converting replays to and from JSON.
[Binaries][] are available for every platform Rocket League supports: Windows, macOS, and Linux.

This post is split into two parts:
[Usage][] covers how to actually use Rattletrap and what you can do with it;
[Development][] talks about how Rattletrap works and how it was made.

## Usage

To parse a replay, run Rattletrap in decode mode.

``` sh
> rattletrap decode path/to/a.replay parsed-replay.json
```

That will read the raw replay from `path/to/a.replay` and write the JSON to `parsed-replay.json`.
It will parse most replays in about 2 seconds.
The JSON output is minified, so if you want to read it you should probably pretty-print it.
Be warned, the output is large!
A 1 MB replay turns into about 100 MB of pretty JSON.

Why might you want to parse Rocket League replays?
Parsing can be useful for getting stats about a match.
Anything you see on the scoreboard in-game is also in the replay.
You can even [analyze boost usage][] or [watch the replay in your browser][]!

To generate a replay, run Rattletrap in encode mode.

``` sh
> rattletrap encode parsed-replay.json output.replay
```

That reads some JSON from `parsed-replay.json` and writes a replay to `output.replay`.
Generating is a lot slower than parsing.
It takes about 15 seconds to generate the average replay.
If the input JSON came from `rattletrap decode`, then the output replay will be identical to the input.
In other words, replays can be converted to and from JSON without losing any information.

And why would you generate a replay?
It's not quite as useful as parsing them, but you can still have some fun.
For example, you could [remove your wheels][] or [wear unusual items][].
I would like to see something that stitches replays together to make a highlight reel.
Making cinematic camera paths (["smooths"][]) would be cool too.

That's pretty much all there is to know about using Rattletrap.
I hope it's useful to you, and let me know if you run into any problems!

## Development

Rattletrap is not the first Rocket League replay parser.
There are [at least a dozen of them][].
I borrowed heavily from the other parsers, particularly [jjbott][]'s C# parser and [Galile0][]'s Python one.
I literally could not have done this without them.

Rattletrap is written in Haskell.
Haskell is known for many things, but for this project, one is very important: speed.
Because it's written in Haskell, Rattletrap is fast.
Compared to the Python parser, Rattletrap is about 30 times faster.
And compared to the C# parser, it's 6 times faster.

I was able to make Rattletrap so fast thanks to the great tools around Haskell.
In particular, [Stack][] makes profiling as easy as `stack build --profile`.
Haskell's runtime system shows you how your program runs when you call it with `+RTS -p`.
And you can turn that raw data into a useful chart with [ghc-prof-flamegraph][].

![CPU flame graph][]

This graph shows that Rattletrap spends less than 10% of its time generating JSON.
Also there don't appear to be any obvious hot spots.
[Learn more about flame graphs][] from FP Complete.

Profiling memory usage is just as easy as profiling CPU usage.
After building with profiling enabled, run with `+RTS -hc`.
That will generate a heap profile that you can visualize with `hp2ps`.

![Memory usage graph][]

This graph shows the memory usage over time for parsing a 1 MB replay.
It reaches a peak of about 30 MB and most of that is used by `CompressedWord` values.
That makes sense because it's the most common value in a replay.
[Learn more about memory profiling][] from Pusher.

Rattletrap has never crashed at runtime, and I doubt it ever will.
It does have some known failure modes, like if it finds some game data it hasn't seen before.
But aside from those it should never fail at runtime.
I'm confident in it because it has very high code coverage in its test suite.

![Code coverage report][]

This coverage report was generated with `stack test --coverage`.
It shows that 79% of expressions and 42% of top-level definitions are covered by the test suite.
Those numbers are a little misleading for two reasons.
One, they consider derived instances (like `deriving Eq`) to not be covered.
(I don't bother testing those because the compiler writes them for me.)
And two, the failure modes I mentioned above aren't exercised in the test suite.
You can [read more about code coverage][] from Real World Haskell.

Because Haskell is such a high-level language, Rattletrap is a pretty small project.
It weighs in at about 3,000 lines of code.
For comparison, the C# parser/generator is about 4,300 lines of code.
The Python parser is only about 1,000 lines of code, but it is not able to generate replays.

If you're wondering what real-world Haskell looks like, I encourage you to [read the source][].
I avoided using any advanced language features or weird operators, so it should be approachable even if you don't know Haskell.

[Rattletrap]: https://github.com/tfausak/rattletrap
[Binaries]: https://github.com/tfausak/rattletrap/releases/tag/1.0.0
[Usage]: #usage
[analyze boost usage]: https://www.rocketleaguereplays.com/news/2016/apr/16/boost-stats-and-stream-data/
[watch the replay in your browser]: https://www.rocketleaguereplays.com/news/2016/apr/2/in-browser-replay-simulation/
[remove your wheels]: https://gfycat.com/FineOldHammerheadshark
[wear unusual items]: https://gfycat.com/CoarseMellowIchneumonfly
["smooths"]: https://www.youtube.com/watch?v=9AFXPESOEz0
[Development]: #development
[at least a dozen of them]: https://github.com/rocket-league-replays/rocket-league-replays/wiki/Rocket-League-Replay-Parsers/13e3ae1273acc531ccc46d08f805c08e7a700d3d
[jjbott]: https://github.com/jjbott
[Galile0]: https://github.com/Galile0
[Stack]: https://docs.haskellstack.org/en/stable/README/
[ghc-prof-flamegraph]: https://hackage.haskell.org/package/ghc-prof-flamegraph
[CPU flame graph]: /static/images/2016/11/14/cpu-flame-graph.png
[Learn more about flame graphs]: https://www.fpcomplete.com/blog/2015/04/ghc-prof-flamegraph
[Memory usage graph]: /static/images/2016/11/14/memory-usage-graph.png
[Learn more about memory profiling]: https://blog.pusher.com/memory-profiling-in-haskell/
[Code coverage report]: /static/images/2016/11/14/code-coverage-report.png
[read more about code coverage]: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html#id629497
[read the source]: https://www.stackage.org/haddock/nightly-2016-11-13/rattletrap-0.4.1/src/Rattletrap.Replay.html
