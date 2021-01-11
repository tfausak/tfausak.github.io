---
title: Match URI templates with Burrito
---

URI templates are a powerful way to succinctly describe URLs. For example the URI template `https://hackage.haskell.org/package/{name}` shows you how to find a Haskell package on Hackage. By expanding that template with `name := "parsec"` you can get the full URL of a package: `https://hackage.haskell.org/package/parsec`.

``` hs
>>> let Just template = parse "https://hackage.haskell.org/package/{name}"
>>> expand [("name", stringValue "parsec")] template
"https://hackage.haskell.org/package/parsec"
```

[RFC 6570](https://tools.ietf.org/html/rfc6570) specifies how URI templates work. Unfortunately it doesn't describe how to go in the other direction. That is, how to take a full URL and parse it according to a template. For example you may want to take a package URL and match it against the template to get the package name. This is kind of like doing template expansion in reverse.

``` hs
>>> match "https://hackage.haskell.org/package/parsec" template
[[("name", String "parsec")]]
```

I'm happy to announce that [version 1.1.0.0 of Burrito](https://hackage.haskell.org/package/burrito-1.1.0.0), my URI template library for Haskell, can now perform this matching operation! So if you find yourself needing to expand or match URI templates, please check it out.
