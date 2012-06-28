This is my blog. There are many like it, but this one is mine.

Powered by [GitHub Pages][1]. And, by extension, [Jekyll][2]. Run
it with:

```sh
jekyll --auto
# or
sudo jekyll --auto --server 80
```

Technically speaking, the combined and minified CSS and JS files
shouldn't be in this repository, since they can be generated on the
fly. However, they're necessary to serve everything through GitHub
Pages.

Rebuilding them requires the [YUI Compressor][3] for minification.
Combine and minify them like so:

```sh
# CSS
cat static/styles/{reset,main,syntax}.css |
java -jar yuicompressor-2.4.7.jar --type css > \
static/styles/all.min.css

# JS
cat static/scripts/main.js |
java -jar yuicompressor-2.4.7.jar --type js > \
static/scripts/all.min.js
```

[1]: http://pages.github.com/
[2]: http://jekyllrb.com/
[3]: http://developer.yahoo.com/yui/compressor/
