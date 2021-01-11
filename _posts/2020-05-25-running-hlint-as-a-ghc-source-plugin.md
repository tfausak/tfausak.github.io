---
title: Running HLint as a GHC source plugin
---

HLint is a static analyzer for Haskell that suggests how to improve your code and make it more idiomatic. Normally it's run as a standalone executable or an extra test suite. Thanks to [GHC 8.6](https://www.haskell.org/ghc/blog/20180922-ghc-8.6.1-released.html) and [HLint 3](https://neilmitchell.blogspot.com/2020/05/hlint-30.html) it's now possible to run HLint as part of GHC by using a source plugin. This post introduces [Splint](https://github.com/tfausak/splint), which does exactly that.

## Motivation

Running HLint by itself is a fine workflow, and you may still prefer doing that even if it's available as a source plugin. But why would you want a linter plugin in the first place?

The main motivation is parsing. It used to be that HLint used its own custom parser called `haskell-src-exts` rather than the one GHC uses to actually compile your code. This can lead to annoying bugs where GHC parses your code just fine but HLint has trouble with it. 

However since version 3 HLint uses GHC for parsing instead of `haskell-src-exts`. It seems like this should fix the problem, right? Well, yes and no. You might call GHC with some options, like `-XTemplateHaskell` to enable the Template Haskell extension. Even though HLint is using GHC's parser, you still have to make sure it sets up GHC in the same way, otherwise your module may fail to parse. This is possible to do, but it can be tedious to keep things in sync.

This is why source plugins are nice. You can hook into the compilation process right after the module has been parsed. Then you can pass that parsed module directly to HLint, without parsing it again or doing any serialization. This way you can be sure the module parsed correctly, and you can avoid doing any extra work. 

## Inspiration

Hopefully you're convinced that running HLint as a GHC source plugin is at least not a bad idea. In fact it was a good idea even before HLint switched over to using GHC's parser. Two years ago Ollie Charles made [hlint-source-plugin](https://github.com/ocharles/hlint-source-plugin) as a proof of concept. 

Wait, what? If hlint-source-plugin already exists, why did I make my own project and write this blog post? As mentioned, Ollie created his project back when HLint used its own parser. That means the plugin has to re-parse modules after GHC has parsed them. It could be upgraded to avoid re-parsing, but the plugin itself is so small that it seemed easier to make a new one from scratch.

At any rate, thanks to Ollie for showing that something like this was possible! 

## Usage

To use Splint, pass `-fplugin=Splint` to GHC. Any ideas suggested by HLint will be reported as warnings by GHC. For example, if you define `Main.hs` as:

``` hs
main = print . concat $ map pure [ 'a' .. 'z' ]
```

You would expect HLint to tell you to use `concatMap`. Normally you would need to both compile your module with GHC and lint it with HLint. However with Splint you can compile it and get suggestions from HLint all at once by running:

``` sh
ghc -fplugin=Splint Main.hs
```

Among all the usual output from GHC, you should see this new warning:

```
Main.hs:1:8: warning:
    Use concatMap
    Perhaps: print (concatMap pure ['a' .. 'z'])
  |
1 | main = print . concat $ map pure [ 'a' .. 'z' ]
  |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

And that's all there is to it! HLint suggestions as part of your normal build process. What a time to be alive.

If you want to pass arguments through to HLint, you can use `-fplugin-opt=Splint:arg`. For example you can ignore the warning above with `-fplugin-opt=Splint:'--ignore=Use concatMap'`. Usually this won’t be necessary because Splint will use your `.hlint.yaml` configuration file.

## Trade offs

Running HLint as a GHC source plugin has some upsides:

-   Modules are only linted when they're compiled, so if they haven't changed they won't be linted again.

-   HLint's suggestions are reported just like GHC's warnings. They're formatted the same way and they're reported at the same time.

-   Each module is only parsed once.

-   Parsing is done by GHC instead of something like `haskell-src-exts`. HLint already works like this, but by using a plugin you can be sure that all of the versions and options line up correctly.

However it's also got some downsides:

-   Using Splint means adding it as a dependency to the targets you want to lint. Normally HLint is either a test dependency or just installed on the system.

    You may be able to lessen the impact of this by providing a flag to control linting. That way you can enable it locally and in CI, but not require everything downstream of you to depend on HLint.

    ``` cabal
    flag lint
      default: False
      manual: True
    library
      if flag(lint)
        build-depends: splint
        ghc-options: -fplugin=Splint
    ```

-   It's slower. I've found that it adds about a tenth of a second per module.

-   You can't use the automated refactorings that HLint provides.

-   Using plugins marks every module as unsafe.

## Conclusion

Hopefully this post has explained why you might want to integration HLint into your build process as a GHC source plugin. [Splint](https://hackage.haskell.org/package/splint-1.0.0.0) makes it a cinch, so please check it out! 
