(function () {
    'use strict';
    var script;

    // Google Analytics
    window._gaq = [
        ['_setAccount', 'UA-26535645-1'],
        ['_trackPageview']
    ];
    script = document.createElement('script');
    script.async = true;
    script.src = (/^https/.test(window.location) ? '//ssl' : '//www') +
        '.google-analytics.com/ga.js';
    document.body.appendChild(script);

    // Piwik
    window._paq = [
        ['trackPageView'],
        ['enableLinkTracking'],
        ['setTrackerUrl', '//piwik-fausak.rhcloud.com/piwik.php'],
        ['setSiteId', 1]
    ];
    script = document.createElement('script');
    script.async = true;
    script.defer = true;
    script.src = '//piwik-fausak.rhcloud.com/piwik.js';
    document.body.appendChild(script);
}());
