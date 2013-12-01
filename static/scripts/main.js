(function () {
    'use strict';
    var script;

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
