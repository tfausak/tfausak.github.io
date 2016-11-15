---
title: Parse and generate Rocket League replays with Haskell
---

I am happy to announce version 1.0.0 of [Rattletrap][], a Rocket League replay parser and generator!
It is a command-line utility for converting replays to and from JSON.
[Binaries][] are available for every platform Rocket League supports, including Windows, macOS, and Linux.

This post is split into two parts:
[Usage][] covers how to use Rattletrap and what you can do with it;
[Development][] talks about how Rattletrap works and how it was made.

## Usage

### Parse

``` sh
> rattletrap decode input.replay replay.json
```

That will read the raw replay from `input.replay` and write the JSON to `replay.json`.
It will parse most replays in about 2 seconds.
The JSON output is minified, so if you want to read it you should probably pretty-print it.
But be warned, the output is large!
A 1 MB replay turns into about 100 MB of pretty JSON.

Why might you want to parse Rocket League replays?
Parsing can be useful for getting stats about a match.
Anything you see on the scoreboard in-game is also in the replay.
You can even [analyze boost usage][] or [watch the replay in your browser][]!
I'm sure there are other neat things to be done with the replay data.
I'd love to see what you come up with!

### Generate

``` sh
> rattletrap encode replay.json output.replay
```

That reads some JSON from `replay.json` and writes a replay to `output.replay`.
Generating is a lot slower than parsing.
It takes about 15 seconds to generate the average replay.
If the input JSON came from `rattletrap decode`, then the output replay will be identical to the input.
In other words, replays can be converted to and from JSON without losing any information.

### Modify

``` sh
> rattletrap decode original.replay original.json
> your-program-here original.json modified.json
> rattletrap encode modified.json modified.replay
```

Those three commands parse a replay into JSON, modify that JSON, then generate a replay from it.
By using Rattletrap to handle the annoying replay file format, you can work with easy-to-use JSON.

Why would you want to modify a replay?
It's not quite as useful as parsing one, but you can still have some fun.
For example, you could [remove your wheels][] or [wear unusual items][].
You can also force every car to look the same.
I would like to see something that stitches replays together to make a highlight reel, or something that makes cinematic camera paths (["smooths"][]) for videos.

That's pretty much all there is to know about using Rattletrap.
I'm excited to see what you can do with it!

## Development

Rattletrap is not the first Rocket League replay parser.
There are [at least a dozen of them][].
I borrowed heavily from the other parsers, particularly [jjbott][]'s C# parser and [Galile0][]'s Python one.
I literally could not have done this without them.
So thanks y'all!

Rattletrap is written in Haskell.
Because it's written in Haskell, Rattletrap is fast.
Compared to the Python parser, Rattletrap is about 30 times faster.
And compared to the C# parser, it's about 6 times faster.

I was able to make Rattletrap so fast thanks to the great tools available in Haskell.
In particular, [Stack][] makes profiling as easy as `stack build --profile`.
Haskell's runtime system gives you a window into your program's behavior with `+RTS -p`.
That will give you a raw profiling file that you can turn into a chart with [ghc-prof-flamegraph][].

![CPU flame graph][]

This graph shows that Rattletrap spends less than 10% of its time generating JSON.
Also there don't appear to be any obvious hot spots.
You can [learn more about flame graphs][] from FP Complete.

Profiling memory usage is just as easy as profiling CPU usage.
After building with profiling enabled, run your program with `+RTS -hc`.
That will generate a heap profile that you can visualize with `hp2ps`.

![Memory usage graph][]

This graph shows the memory usage over time for parsing a 1 MB replay.
It reaches a peak of about 30 MB and most of that is used by `CompressedWord` values.
That makes sense because it's the most common value in a replay.
For example, this replay has about 600,000 `CompressedWord` values.
The Pusher blog goes into more detail [about memory profiling][].

Rattletrap has never crashed at runtime, and I doubt it ever will.
It does have some known failure modes, like if it finds some game data it hasn't seen before.
But aside from those it should never fail at runtime.
I'm confident in it because Haskell is statically typed and its test suite has very high code coverage.

![Code coverage report][]

This coverage report was generated with `stack test --coverage`.
It shows that 79% of expressions and 42% of top-level definitions are covered by the test suite.
Those numbers are a little misleading for two reasons:
One, they consider derived instances like `deriving Eq` to not be covered;
and two, the failure modes I mentioned above aren't exercised in the test suite.
Rattletrap's effective code coverage is much higher.
You can [read more about code coverage][] from Real World Haskell.

Because Haskell is such a high-level language, Rattletrap is a pretty small project.
It weighs in at about 3,000 lines of code.
For comparison, the C# parser and generator is about 4,300 lines of code.
The Python parser is only about 1,000 lines of code, but it is not able to generate replays.

If you're wondering what real-world Haskell looks like, I encourage you to [read the source][].
I avoided using any advanced language features or weird operators, so it should be approachable even if you don't know Haskell.
To give you a taste of what you're in for, here is the code for parsing a player's camera settings:

``` hs
getCamSettingsAttribute = do
  fov         <- getFloat32Bits
  height      <- getFloat32Bits
  angle       <- getFloat32Bits
  distance    <- getFloat32Bits
  stiffness   <- getFloat32Bits
  swivelSpeed <- getFloat32Bits
  return (CamSettingsAttribute fov height angle distance stiffness swivelSpeed)
```

I have really enjoyed working on this project.
It has allowed me to dive deep into different areas of Haskell.
And in the end I wrote a tool that I use every day to make sense of my replays.
I hope you like it!

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
[learn more about flame graphs]: https://www.fpcomplete.com/blog/2015/04/ghc-prof-flamegraph
[Memory usage graph]: /static/images/2016/11/14/memory-usage-graph.png
[about memory profiling]: https://blog.pusher.com/memory-profiling-in-haskell/
[Code coverage report]: /static/images/2016/11/14/code-coverage-report.png
[read more about code coverage]: http://book.realworldhaskell.org/read/testing-and-quality-assurance.html#id629497
[read the source]: https://www.stackage.org/haddock/nightly-2016-11-13/rattletrap-0.4.1/src/Rattletrap.Replay.html
