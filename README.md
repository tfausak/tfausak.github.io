This is my blog. There are many like it, but this one is mine.

Powered by [GitHub Pages][1]. And, by extension, [Jekyll][2]. Run
it on port 80 with:

    $ sudo jekyll --server 80 --auto

Technically speaking, the combined and minified CSS and JS files
shouldn't be in this repository, since they can be generated on the
fly. However, they're necessary to serve everything through GitHub
Pages.

Rebuilding them requires the [YUI Compressor][3] for minification.
Combine and minify them like so:

    cat static/styles/{reset,main,syntax}.css >static/styles/all.css
    java -jar yuicompressor-2.4.7.jar -o static/styles/all.min.css static/styles/all.css
    rm static/styles/all.css

    cat static/scripts/{analytics,social}.js >static/scripts/all.js
    java -jar yuicompressor-2.4.7.jar -o static/scripts/all.min.js static/scripts/all.js
    rm static/scripts/all.js

[1]: http://pages.github.com/
[2]: http://jekyllrb.com/
