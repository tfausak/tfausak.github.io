---
title: Automatically generating PureScript documentation
---

-   generate a new personal access token on github
-   https://github.com/settings/tokens/new
-   deselect all scopes

-   set up travis ci
-   http://docs.travis-ci.com/user/languages/javascript-with-nodejs/
-   this can depend on your project
-   most use pulp, so that's what i'll cover
-   run `psc-publish --dry-run` as part of your test suite
-   add an `after_success` to your travis config

-   travis sets the `TRAVIS_TAG` environment variable when testing a tag
-   you can use that as a flag for publishing documentation
-   if it's there, you'll want to run `psc-publish` to generate documentation
-   then upload it to pursuit using your github token for authorization

-   add your github token to travis as an environment variable
-   do this on your project settings page
-   https://travis-ci.org/tfausak/PACKAGE/settings
-   make sure it is hidden in the build log

-   create a new release as usual
-   this means creating a new tag
-   if the tests succeed, travis will generate and upload your documentation to pursuit

- see purescript-batteries for specific examples
- https://github.com/tfausak/purescript-batteries/blob/v0.2.4/package.json
- https://github.com/tfausak/purescript-batteries/blob/v0.2.4/.travis.yml
