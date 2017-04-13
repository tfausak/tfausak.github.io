---
title: Better Haskell packages with hpack
---

Writing a Haskell package requires putting the package metadata in a Cabal file.
Unfortunately the Cabal file format is custom, tedious, and verbose.
Simon Hengel's [hpack][] tool provides a better way to define Haskell packages.
That's why my [Haskell package checklist][] recommends using hpack.

Cabal uses a custom file format.
As far as I know, the only way to parse a Cabal file is with Haskell using the `Cabal` library.
This makes it hard to use the package files from other languages for routine editing, extracting information, or pretty printing.
By comparison, hpack is just YAML.
Chances are that you, your editor, and the other languages you use are already familiar with YAML.

Another problem with the Cabal file format is that it requires a lot of unnecessary boilerplate.
For example, when you create a new module in your package,
you have to add it to either the `exposed-modules` or `other-modules`.
If you don't, that module won't be available and you'll get strange linking errors.
hpack automatically finds your modules and exposes them,
but it still gives you control and the ability to hide certain modules.

In addition to removing boilerplate,
hpack adds some nice shortcuts for common functionality.
For instance, you may have a bunch of template files that need to be included with your package.
With Cabal, you'd have to manually list them and remember to keep it up to date as you add and remove templates.
With hpack, you can say something like `extra-source-files: templates/*` and forget about it.

hpack also drastically reduces duplication.
It's very common for a package to define a library, an executable, and a test suite that all need the same dependencies.
Cabal forces you to list these dependencies in each section, easily tripling the size of the package file.
hpack lets you put your dependencies at the top level so they're shared between the sections.
(Cabal may eventually get similar functionality, but this has been a known problem since at least 2012.)

hpack provides all these nice-to-have features without restricting what you can configure.
You're not losing any power by switching from Cabal to hpack.
In fact, it's possible to mechanically convert a Cabal file into hpack with [hpack-convert][].

Stack has hpack built right in.
So if you're using Stack to build your package
(and you probably should be)
then you get hpack for free as part of `stack build`.
And since hpack generates a Cabal file,
it's easy to go back to using Cabal if you need to do that for whatever reason.

In short, use [hpack][] to make your life as a Haskell package author easier.

[hpack]: https://github.com/sol/hpack
[Haskell package checklist]: {% post_url 2016-12-05-haskell-package-checklist %}
[hpack-convert]: https://github.com/yamadapc/hpack-convert
