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
    script.src = (/^https/.test(window.location) ? '//ssl' : '//www') +
        '.google-analytics.com/ga.js';
    document.body.appendChild(script);
}());

var _paq = _paq || [];
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function() {
var u=(("https:" == document.location.protocol) ? "https" : "http") + "://piwik-fausak.rhcloud.com//";
_paq.push(['setTrackerUrl', u+'piwik.php']);
_paq.push(['setSiteId', 1]);
var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0]; g.type='text/javascript';
g.defer=true; g.async=true; g.src=u+'piwik.js'; s.parentNode.insertBefore(g,s);
})();
