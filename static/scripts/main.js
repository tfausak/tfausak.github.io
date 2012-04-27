var _gaq = [
    ['_setAccount', 'UA-26535645-1'],
    ['_trackPageview']
];

(function () {
    "use strict";
    var script;

    // Google Analytics
    script = document.createElement('script');
    script.async = true;
    script.src = (/^https/.test(location) ? '//ssl' : '//www') + '.google-analytics.com/ga.js';
    document.body.appendChild(script);

    // Delay loading social widgets until the user scrolls.
    window.onscroll = function () {
        window.onscroll = null;

        // Facebook JS SDK
        script = document.createElement('script');
        script.async = true;
        script.id = 'facebook-jssdk';
        script.src = '//connect.facebook.net/en_US/all.js#xfbml=1&appId=133083533456136';
        document.body.appendChild(script);

        // Google +1 button
        script = document.createElement('script');
        script.async = true;
        script.src = '//apis.google.com/js/plusone.js';
        document.body.appendChild(script);

        // Twitter widgets
        script = document.createElement('script');
        script.async = true;
        script.id = 'twitter-wjs';
        script.src = '//platform.twitter.com/widgets.js';
        document.body.appendChild(script);
    };
}());
