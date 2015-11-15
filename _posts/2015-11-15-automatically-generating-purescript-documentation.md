---
title: Automatically generating PureScript documentation
---

Recently I have been pouring a lot of time into PureScript projects. The
canonical source for PureScript packages and documentation is [Pursuit][]. It
annoyed me that I had to manually generate and upload my documentation whenever
I released a new version. So I figured out how to automate it and thought I'd
share my workflow.

To get started, you'll need a Personal Access Token from GitHub. You can create
one at <https://github.com/settings/tokens/new>. Make sure that you deselect
all scopes. This token shouldn't be allowed to do anything because it's only
used for authorization.

After you create it, copy the token and head over to Travis CI. you'll want to
add the token as an environment variable in your project's settings. You can do
that at `https://travis-ci.org/OWNER/REPO/settings`. Name your token
`GITHUB_TOKEN`. Make sure that it is hidden in the build log so that other
people can't pretend to be you.

Now that your Travis build can authenticate as you on GitHub, it needs to
generate and upload documentation. This isn't something you want to do on every
build, though. You only want documentation built when you release a new
version. For PureScript packages, you make new versions with Git tags.
Fortunately Travis runs a separate build for tags and it sets the `TRAVIS_TAG`
environment variable for those builds. That means if `TRAVIS_TAG` is set, the
build should generate and upload documentation.

So open up your `.travis.yml` and add an `after_success` section that looks
like this:

{% highlight yaml %}
after_success:
- >-
  test $TRAVIS_TAG &&
  npm run psc-publish \
    | tail -n 1 \
    > output/documentation.json &&
  curl -X POST http://pursuit.purescript.org/packages \
    -d @output/documentation.json \
    -H 'Accept: application/json' \
    -H "Authorization: token ${GITHUB_TOKEN}"
{% endhighlight %}

That does a lot, so here's a breakdown:

1.  First it checks for the `TRAVIS_TAG` environment variable. If it's not set,
    it won't build documentation.

2.  Then it generates the documentation with `psc-publish`. For this to work,
    `psc-publish` must be in the `"scripts"` section of your `package.json`.

    -   It is necessary to `tail` the output because `npm run` outputs garbage
        before the actual output of the script. Since `psc-publish` always
        outputs a single line of JSON, only the last line of the output is
        necessary.

    -   The documentation must be written to a file that is ignored by Git.
        Otherwise `psc-publish` will complain that your Git working tree is
        dirty. Since most PureScript packages ignore the `output` directory,
        that's a safe place to put the documentation.

3.  Finally it uploads to documentation to Pursuit. Pursuit itself recommends
    using `curl -v`, but that would be bad here because it would expose your
    `GITHUB_TOKEN`.

Now when you want to release a new version, all you have to do is create a new
Git tag. Everything else will happen automatically!

If you want to see a complete, working example of this, check out [Neon][], one
of my PureScript packages. You should be able to copy its [`package.json`][]
and [`.travis.yml`][] into your own package. Take a look at [this build][] to
see how it looks on Travis.

[pursuit]: http://pursuit.purescript.org
[neon]: https://github.com/tfausak/purescript-neon
[`package.json`]: https://github.com/tfausak/purescript-neon/blob/v0.0.25/package.json
[`.travis.yml`]: https://github.com/tfausak/purescript-neon/blob/v0.0.25/.travis.yml
[this build]: https://travis-ci.org/tfausak/purescript-neon/builds/91235573#L1817-L1823
