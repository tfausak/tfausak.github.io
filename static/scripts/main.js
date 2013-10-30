var _gaq = [
    ['_setAccount', 'UA-26535645-1'],
    ['_trackPageview']
];

(function () {
    'use strict';
    var script;

    // Google Analytics
    script = document.createElement('script');
    script.async = true;
    script.src = (/^https/.test(location) ? '//ssl' : '//www') +
        '.google-analytics.com/ga.js';
    document.body.appendChild(script);
}());
