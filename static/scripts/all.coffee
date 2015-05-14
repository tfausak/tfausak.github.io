---
---

url = '//piwik.fausak.me'

window._paq = [
  ['trackPageView']
  ['enableLinkTracking']
  ['setTrackerUrl', "#{url}/piwik.php"]
  ['setSiteId', 1]
]

script = document.createElement 'script'
script.async = true
script.defer = true
script.src = '#{url}/piwik.js'
document.body.appendChild script
