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

    cat static/styles/{reset,main,syntax}.css >tmp.css
    java -jar yuicompressor-2.4.7.jar tmp.css >static/styles/all.min.css
    rm tmp.css

    cat static/scripts/{analytics,social}.js >tmp.js
    java -jar yuicompressor-2.4.7.jar tmp.js >static/scripts/all.min.js
    rm tmp.js

It's also a good idea to make sure all other assets are as small
as possible. To wit, PNG images can be minimized with [Pngcrush][4]:

    pngcrush -rem alla -rem allb -rem text -brute -d tmp static/images/*.png
    mv tmp/*.png static/images/.
    rmdir tmp

[1]: http://pages.github.com/
[2]: http://jekyllrb.com/
[3]: http://developer.yahoo.com/yui/compressor/
[4]: http://pmt.sourceforge.net/pngcrush/
