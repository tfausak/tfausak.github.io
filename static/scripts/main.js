var _gaq = [
    ['_setAccount', 'UA-26535645-1'],
    ['_trackPageview']
];

(function () {
    'use strict';
    var script, element;

    // Google Analytics
    script = document.createElement('script');
    script.async = true;
    script.src = (/^https/.test(location) ? '//ssl' : '//www') + '.google-analytics.com/ga.js';
    document.body.appendChild(script);

    // Facebook like widget
    element = document.getElementById('facebook-widget');
    if (element) {
        element.onmouseover = function () {
            this.onmouseover = null;
            this.parentNode.removeChild(this);
            _gaq.push(['_trackEvent', 'Social', 'Facebook - Like', element.getAttribute('data-label')]);

            script = document.createElement('script');
            script.async = true;
            script.id = 'facebook-jssdk';
            script.src = '//connect.facebook.net/en_US/all.js';
            document.body.appendChild(script);

            window.fbAsyncInit = function () {
                FB.init({
                    appId: '133083533456136',
                    channelUrl: '//taylor.fausak.me/channel.html',
                    xfbml: true
                });
            };
        };
    }

    // Google +1 widget
    element = document.getElementById('google-widget');
    if (element) {
        element.onmouseover = function () {
            this.onmouseover = null;
            this.parentNode.removeChild(this);
            _gaq.push(['_trackEvent', 'Social', 'Google - +1', element.getAttribute('data-label')]);

            script = document.createElement('script');
            script.async = true;
            script.src = '//apis.google.com/js/plusone.js';
            document.body.appendChild(script);
        };
    }

    // Twitter tweet widget
    element = document.getElementById('twitter-widget');
    if (element) {
        element.onmouseover = function () {
            this.onmouseover = null;
            this.parentNode.removeChild(this);
            _gaq.push(['_trackEvent', 'Social', 'Twitter - Tweet', element.getAttribute('data-label')]);

            script = document.createElement('script');
            script.async = true;
            script.id = 'twitter-wjs';
            script.src = '//platform.twitter.com/widgets.js';
            document.body.appendChild(script);
        };
    }
}());
