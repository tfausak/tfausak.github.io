---
title: Add files to GitHub releases
---

I develop some executables in Haskell. As with most of my projects, I host the
code and manage the releases on GitHub. And I run the tests on Travis CI.

When I release a new version, I want the compiled executables to be available
on the releases page. That way users don't have to set up a local Haskell
development environment to start using it.

I don't want to do this manually because undoubtedly I would forget or do I
would do it wrong. Also I wouldn't be able to consistently provide executables
for more than one operating system. Fortunately Travis CI allows me to do this
automatically.

Travis, like most other continuous integration services, runs separate builds
for Git tags. And it sets an environment variable when it runs one of those
builds. So if the build was for a tag and it succeeds, it should go ahead and
upload the executable to that tag.

GitHub has allowed adding files to releases for as long as they've had
releases. Unfortunately it's not very easy. You'd need more than a couple lines
of Bash to get it done. In the past I used [github-release][1], a Go
executable, to do this. But where's the fun in using a Go program to deploy a
Haskell program?

That's why I made [GitHub Release][2]. It's a Haskell version of (part of) the
Go program. You can use it to upload files to tags on your GitHub project. It's
self-hosting, meaning it uploads files to its own releases.

If you want to use GitHub Release for your own Haskell project, here's a
template for how to use it with Travis CI.

``` yaml
after_success:
- | # Add binary to GitHub release.
  if test "$TRAVIS_TAG"
  then
    stack --resolver nightly-2016-05-08 install github-release
    stack --resolver nightly-2016-05-08 exec -- github-release upload \
      --token "$GITHUB_TOKEN" \
      --owner your-name \
      --repo your-project \
      --tag "$TRAVIS_TAG" \
      --file "$(stack path --local-install-root)/bin/your-project" \
      --name "your-project-$TRAVIS_TAG-$TRAVIS_OS_NAME"
  fi
```

(If your project uses a nightly resolver newer than 2016-05-07, you can leave
out the `--resolver ...` options.)

Make sure you replace `your-name` with your GitHub username and `your-project`
with your GitHub repository name. Also be sure to set the `GITHUB_TOKEN`
environment variable and hide it from the build log. You can make a token on
the [personal access tokens][3] page of your GitHub settings.

If you don't want to build it yourself, you can download the latest binary from
GitHub. This is what you should do for non-Haskell projects.

``` yaml
after_success:
- | # Add binary to GitHub release.
  if test "$TRAVIS_TAG"
  then
    curl --location "https://github.com/tfausak/github-release/releases/download/0.1.8/github-release-0.1.8-$TRAVIS_OS_NAME.gz" > github-release.gz
    gunzip github-release.gz
    ./github-release upload \
      --token "$GITHUB_TOKEN" \
      --owner your-name \
      --repo your-project \
      --tag "$TRAVIS_TAG" \
      --file "$(stack path --local-install-root)/bin/your-project" \
      --name "your-project-$TRAVIS_TAG-$TRAVIS_OS_NAME"
  fi
```

I hope GitHub Release is useful for you! Please open an issue if you hit any
problems or if you want it to do anything else with GitHub releases.

[1]: https://github.com/aktau/github-release
[2]: https://github.com/tfausak/github-release
[3]: https://github.com/settings/tokens
