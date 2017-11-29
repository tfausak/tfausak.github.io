---
title: 2017 state of Haskell survey results
---

I am excited to announce the results of the [2017 state of Haskell survey](https://haskellweekly.news/surveys/2017.html),
published by [Haskell Weekly](https://haskellweekly.news)!
To the 1,335 people that responded to the survey:
Thank you!
I appreciate your feedback.

You can download the anonymized responses as [JSON](/static/pages/2017-11-29-responses-anonymized.json.zip) or [CSV](/static/pages/2017-11-29-responses-anonymized.csv.zip).
The responses are licensed under the [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) license.

**November 29, 2017 update:**
The anonymized responses, both JSON and CSV, now include the answers to free response questions.

Before we look at the responses themselves,
let's take a look at when they were submitted.

![](/static/images/2017/11/15/chart-date.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-date.svg) or a [PNG](/static/images/2017/11/15/chart-date.png).*

I opened the survey in the morning on Wednesday, November 1, 2017.
It stayed open for one week.
Most people responded in the first few days.
A significant number of people responded on the last day.
Keeping the survey open for a week felt right.
I think I'll do that again next year.

Now let's take a look at the survey responses.

1.  [What is your email address?](#question-1)
2.  [Do you use Haskell?](#question-2)
3.  [If you stopped using Haskell, how long did you use it before you stopped?](#question-3)
4.  [If you stopped using Haskell, why did you stop?](#question-4)
5.  [If you don't use Haskell, why not?](#question-5)
6.  [How long have you been using Haskell?](#question-6)
7.  [How frequently do you use Haskell?](#question-7)
8.  [Where do you use Haskell?](#question-8)
9.  [How do you use Haskell at work?](#question-9)
10. [If you don't use Haskell at work, why not?](#question-10)
11. [What is the total size of all the Haskell projects you work on?](#question-11)
12. [Which platforms do you develop on?](#question-12)
13. [Which platforms do you target?](#question-13)
14. [Which Haskell compilers do you use?](#question-14)
15. [If you use GHC, how do you install it?](#question-15)
16. [If you use GHC, has upgrading it broken your code in the last year?](#question-16)
17. [If upgrading GHC broke your code in the last year, how did it break?](#question-17)
18. [If you use GHC, which versions of it do you use?](#question-18)
19. [Have you ever contributed to GHC?](#question-19)
20. [If you have not contributed to GHC, why not?](#question-20)
21. [Which language extensions would you like to be enabled by default?](#question-21)
22. [Which build tools do you use?](#question-22)
23. [What is your preferred build tool?](#question-23)
24. [Which editors do you use for Haskell?](#question-24)
25. [Which version control systems do you use for Haskell?](#question-25)
26. [Where do you get Haskell packages from?](#question-26)
27. [Have you contributed to an open source Haskell project?](#question-27)
28. [If you have not contributed to an open source Haskell project, why not?](#question-28)
29. [How do you interact with the Haskell community?](#question-29)
30. [How would you rate your proficiency in Haskell?](#question-30)
31. [I am satisfied with Haskell as a language.](#question-31)
32. [I am satisfied with Haskell's build tools (such as cabal-install or Stack).](#question-32)
33. [I am satisfied with Haskell's package repositories (such as Hackage or Stackage).](#question-33)
34. [I can find Haskell libraries for the things that I need.](#question-34)
35. [I think Haskell libraries are high quality.](#question-35)
36. [I think Haskell libraries are well documented.](#question-36)
37. [I can easily compare Haskell libraries to select the best one.](#question-37)
38. [I find Haskell libraries easy to use.](#question-38)
39. [I find that Haskell libraries provide a stable API.](#question-39)
40. [I find that Haskell libraries work well with other libraries.](#question-40)
41. [I find that Haskell libraries perform well.](#question-41)
42. [I can easily reason about the performance of my Haskell programs.](#question-42)
43. [In your opinion, what is Haskell's biggest problem?](#question-43)
44. [If you could change anything about Haskell's community, what would it be?](#question-44)
45. [Do you have any other comments about Haskell, its ecosystem, its community, or this survey?](#question-45)

<h2 id="question-1"><a href="#question-1">1</a>: What is your email address? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-email.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-email.svg) or a [PNG](/static/images/2017/11/15/chart-email.png).*

Almost exactly 75% of people provided their email address.
That number feels low.
For next year's survey, I will be clearer about how your email address will be used.

<h2 id="question-2"><a href="#question-2">2</a>: Do you use Haskell? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-use-haskell.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-use-haskell.svg) or a [PNG](/static/images/2017/11/15/chart-use-haskell.png).*

Not surprisingly, most of the responders use Haskell.
Although the survey mainly targeted Haskell users,
I am grateful to the non-users who responded.
Hopefully some of you will consider using Haskell again!

<h2 id="question-3"><a href="#question-3">3</a>: If you stopped using Haskell, how long did you use it before you stopped? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-stopped-after.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-stopped-after.svg) or a [PNG](/static/images/2017/11/15/chart-stopped-after.png).*

Most people used Haskell for a significant amount of time before stopping.
Haskell has a reputation for being hard to learn.
I think this data supports that reputation.
Even if you have been using Haskell for a year,
you might still give up on it because it's either too hard or simply not worth it.

<h2 id="question-4"><a href="#question-4">4</a>: If you stopped using Haskell, why did you stop? <a href="#">&#x2191;</a></h2>

> Fascinating language and I really wanted to like it, but:
> not beginner-friendly;
> hard to debug errors;
> hard to reason about performance;
> relative lack of use in industry;
> fewer libraries;
> lack of a well-supported and actively maintained way to target JS.

A common thread in these answers was finishing a Haskell project and moving on to something else.
Sometimes that meant completing a school assignment where the next thing was in a different language.
Other times it meant wrapping up a project and deciding that it wasn't worth it to keep using Haskell.

Another common response was a lack of support at work.
Usually this meant that Haskell didn't make the short list of blessed languages.
Sometimes it meant that Haskell couldn't be introduced to the rest of the team
because it's not familiar enough or too difficult to teach.

In both cases it was common to mention switching to another language.
Scala, F#, Rust, and PureScript were popular choices.

<h2 id="question-5"><a href="#question-5">5</a>: If you don't use Haskell, why not? <a href="#">&#x2191;</a></h2>

> The barrier to entry is huge.
> The install size of the tooling is huge.
> Installing or building from source any tool built in Haskell inevitably involves downloading hundreds of megabytes of stuff.
> The fascination with concepts and naming of concepts that don't appear elsewhere is off-putting to beginners.

The most common reason for not using Haskell is that it's hard to learn.
In particular, the documentation is bad
and there is a lack of tutorials covering beginner and intermediate topics.

Problems with tooling also contribute to people not using Haskell.
For example, there is no good Haskell IDE.
Also the whole Haskell toolchain uses a lot of disk space.

It was also common to see complaints about Haskell being too academic or not practical enough for real world usage.

<h2 id="question-6"><a href="#question-6">6</a>: How long have you been using Haskell? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-user-for.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-user-for.svg) or a [PNG](/static/images/2017/11/15/chart-user-for.png).*

Most people have been using Haskell for a long time.
This is encouraging because it means learning and using Haskell is worth it to those people.
However, the relative lack of new users is not great.
Only 22% of Haskell users have been using it for less than a year.
For next year's survey, I will be splitting the "more than a year" choice into more discrete choices.

<h2 id="question-7"><a href="#question-7">7</a>: How frequently do you use Haskell? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-usage-frequency.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-usage-frequency.svg) or a [PNG](/static/images/2017/11/15/chart-usage-frequency.png).*

Most people use Haskell at least once a week.
This is impressive since most people don't use Haskell at work.
That means Haskell is compelling enough to use again and again for hobby projects.

<h2 id="question-8"><a href="#question-8">8</a>: Where do you use Haskell? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-used-at.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-used-at.svg) or a [PNG](/static/images/2017/11/15/chart-used-at.png).*

Almost everyone uses Haskell at home.
Haskell is also a popular choice at school.
Unfortunately it's not as popular at work, but see the next question.

<h2 id="question-9"><a href="#question-9">9</a>: How do you use Haskell at work? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-work-usage.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-work-usage.svg) or a [PNG](/static/images/2017/11/15/chart-work-usage.png).*

I'm not sure why fewer than 200 people said they use Haskell at work in the previous question
but more than 600 said they use Haskell at work at least some of the time in this question.
Regardless, there are more people who use Haskell at work than those who don't.
That's great!

<h2 id="question-10"><a href="#question-10">10</a>: If you don't use Haskell at work, why not? <a href="#">&#x2191;</a></h2>

> We usually do not have the need for anything faster (in execution) than Python
> and Python cuts development times by offering multitude of libraries.
> Static typing usually also gets trumped by functional tests.

Overall the answers to this question were similar to [question 4](#question-4).
One thing worth pointing out here is that many people are the only Haskell users at their workplace.
Convincing the rest of their team to use Haskell doesn't seem feasible.

<h2 id="question-11"><a href="#question-11">11</a>: What is the total size of all the Haskell projects you work on? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-total-size.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-total-size.svg) or a [PNG](/static/images/2017/11/15/chart-total-size.png).*

Small to medium size Haskell projects are the most popular.
That being said, there are a fair number of large to huge Haskell projects out there.

<h2 id="question-12"><a href="#question-12">12</a>: Which platforms do you develop on? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-build-platforms.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-build-platforms.svg) or a [PNG](/static/images/2017/11/15/chart-build-platforms.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-build-platforms.csv).*

Linux is far and away the most popular build platform.
macOS comes in second with Windows bringing up the rear.
A BSD was the most common choice for people not using one of the big three.

<h2 id="question-13"><a href="#question-13">13</a>: Which platforms do you target? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-target-platforms.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-target-platforms.svg) or a [PNG](/static/images/2017/11/15/chart-target-platforms.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-target-platforms.csv).*

This mirrors the build platforms.
It's interesting to note that more people target Windows than build on it.
The opposite is true for macOS.

<h2 id="question-14"><a href="#question-14">14</a>: Which Haskell compilers do you use? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-compilers.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-compilers.svg) or a [PNG](/static/images/2017/11/15/chart-compilers.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-compilers.csv).*

GHC won, hands down.
For all intents and purposes, Haskell is GHC.
The most popular "other" response was GHCJS.

<h2 id="question-15"><a href="#question-15">15</a>: If you use GHC, how do you install it? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-compiler-install.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-compiler-install.svg) or a [PNG](/static/images/2017/11/15/chart-compiler-install.png).*

Stack is clearly the preferred way to install GHC.
It's more than three times as popular as operating system packages in second place.
I did receive feedback that the official binaries and Nix were missing.
I'll be sure to add those to next year's survey.

<h2 id="question-16"><a href="#question-16">16</a>: If you use GHC, has upgrading it broken your code in the last year? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-broken-by-compiler.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-broken-by-compiler.svg) or a [PNG](/static/images/2017/11/15/chart-broken-by-compiler.png).*

Upgrading GHC was not a problem for 84% of people in the past year.
This makes sense because the 8.2.1 release focused on performance and stability.
However it also added some new big ticket features like the Backpack module system.

<h2 id="question-17"><a href="#question-17">17</a>: If upgrading GHC broke your code in the last year, how did it break? <a href="#">&#x2191;</a></h2>

> GHC fixed several issues around overlapping instances lately,
> that triggered warning in my code base.
> Since we use `-Wall` and `-Werror` as a policy with the code base,
> the builds broke.
> Never the less I was really happy to find out errors in my understanding around the issues
> and fixing my code actually improved it very much.

Most of the breakage included basic stuff like AMP, FTP, and dependency resolution.
Finding a set of dependencies that works with the latest compiler is a common problem.

The next biggest chunk of breakage included advanced features like
the GHC API, Template Haskell, generics, and language extensions.
Many users admitted that breaking changes weren't surprising here,
but they still needed to be dealt with.

A significant number of people mentioned that they build with `-Werror`
and upgrading introduced new warnings that broke their build.
Most were happy to have new warnings and didn't see this as a problem.

<h2 id="question-18"><a href="#question-18">18</a>: If you use GHC, which versions of it do you use? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-ghc-versions.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-ghc-versions.svg) or a [PNG](/static/images/2017/11/15/chart-ghc-versions.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-ghc-versions.csv).*

Most people use a version of GHC that was released in the past year.
Going back two years to GHC 7.10.3 covers 90% of the user base.
My takeaway here is that even though operating system packages might provide older versions of GHC,
most people will be using a newer version.
Don't spend too much time supporting old versions of GHC.

<h2 id="question-19"><a href="#question-19">19</a>: Have you ever contributed to GHC? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-ghc-contributor.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-ghc-contributor.svg) or a [PNG](/static/images/2017/11/15/chart-ghc-contributor.png).*

Only 1 out of every 10 Haskell users have contributed to GHC.
I am not too surprised by these results,
given that GHC already has the features most people want and it's an intimidating code base.

<h2 id="question-20"><a href="#question-20">20</a>: If you have not contributed to GHC, why not? <a href="#">&#x2191;</a></h2>

> Organizing everything through mailing lists seems so complicated and outdated.
> It would be much easier to get involved if everything was on GitHub.
> By comparison, I have contributed to `rustc` multiple times.

Most people didn't see a need to contribute to GHC.
For those that did, they either didn't have enough time
or felt they lacked the experience necessary to contribute.
Many people felt that they needed to be an expert Haskell developer to contribute to GHC.

Complaints about GHC's development and build processes were also common.
Many people expressed an interest in GHC being hosted on GitHub.

<h2 id="question-21"><a href="#question-21">21</a>: Which language extensions would you like to be enabled by default? <a href="#">&#x2191;</a></h2>

Votes | Language extension
---   | ---
512   | `OverloadedStrings`
367   | `BangPatterns`
359   | `GADTs`
337   | `DeriveFunctor`
336   | `DeriveGeneric`
331   | `LambdaCase`
317   | `ScopedTypeVariables`
276   | `DeriveFoldable`
276   | `RankNTypes`
268   | `GeneralizedNewtypeDeriving`

*Download these responses as a [CSV](/static/pages/2017-11-15-language-extensions.csv).*

These are the top ten desired language extensions.
Overloaded strings is by far the most popular.
However I encourage you to look at the CSV,
as 42 language extensions got more than 100 votes.

<h2 id="question-22"><a href="#question-22">22</a>: Which build tools do you use? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-build-tools.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-build-tools.svg) or a [PNG](/static/images/2017/11/15/chart-build-tools.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-build-tools.csv).*

Stack is used by more than twice as many people as Cabal.
Both Nix and raw `ghc-pkg` are used by a surprising number of people.
The most popular build tool other than the top four is Mafia.

<h2 id="question-23"><a href="#question-23">23</a>: What is your preferred build tool? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-preferred-build-tool.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-preferred-build-tool.svg) or a [PNG](/static/images/2017/11/15/chart-preferred-build-tool.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-preferred-build-tool.csv).*

In spite of being a relative newcomer,
Stack is clearly the preferred build tool for Haskell users.
It was first released a little over two years ago.
Now five times the number of people use it as their preferred build tool compared to Cabal.

<h2 id="question-24"><a href="#question-24">24</a>: Which editors do you use for Haskell? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-editors.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-editors.svg) or a [PNG](/static/images/2017/11/15/chart-editors.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-editors.csv).*

Vim and Emacs, the old standbys, are the favorites here.
Interestingly Haskell users are split evenly between the two editors.
The Electron-based Visual Studio Code and Atom editors are both pretty popular.
The most popular editor not listed here is IntelliJ IDEA.

<h2 id="question-25"><a href="#question-25">25</a>: Which version control systems do you use for Haskell? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-source-controllers.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-source-controllers.svg) or a [PNG](/static/images/2017/11/15/chart-source-controllers.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-source-controllers.csv).*

Git is easily the most popular version control system for Haskell users.
Interestingly the new VCS Pijul was the most popular "other" choice.

<h2 id="question-26"><a href="#question-26">26</a>: Where do you get Haskell packages from? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-package-repositories.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-package-repositories.svg) or a [PNG](/static/images/2017/11/15/chart-package-repositories.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-package-repositories.csv).*

Stackage barely edged out Hackage as the most popular package repository.
That means some people never need to get a package that isn't in their Stackage snapshot.
Downloading source is also a surprisingly popular method of getting Haskell packages.
The most popular choice not listed here was Nix.

<h2 id="question-27"><a href="#question-27">27</a>: Have you contributed to an open source Haskell project? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-open-source-contributor.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-open-source-contributor.svg) or a [PNG](/static/images/2017/11/15/chart-open-source-contributor.png).*

More people are open source contributors than not,
but it's almost an even split.

<h2 id="question-28"><a href="#question-28">28</a>: If you have not contributed to an open source Haskell project, why not? <a href="#">&#x2191;</a></h2>

> Very unclear how to engage.
> I don't know if my contributions would be taken seriously
> or will be reprimanded because of my lack of knowledge on Haskell.
> Just to be honest,
> I don't know how much I don't know
> and feel my contributions would be useless.

A lack of time was the biggest hurdle to contributing to open source Haskell projects.
This makes sense when you consider that most Haskell users are either hobbyists or students.
They are likely to spend their time on their own projects.

Many people wanted to contribute to something
but felt that they lacked the skill to make a meaningful contribution.
As the maintainer of several open source Haskell projects, I want to take a moment to say:
I welcome contributions from Haskell users of all skill levels!
Please open an issue or a PR, even if you just started learning Haskell today.

<h2 id="question-29"><a href="#question-29">29</a>: How do you interact with the Haskell community? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-communities.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-communities.svg) or a [PNG](/static/images/2017/11/15/chart-communities.png). Download the "other" responses as a [CSV](/static/pages/2017-11-15-other-communities.csv).*

Reddit is the main hub for the Haskell community.
Twitter, IRC, Stack Overflow, and mailing lists are pretty much in a four way tie for second place.
The most popular choices not listed here are GitHub and Gitter.

<h2 id="question-30"><a href="#question-30">30</a>: How would you rate your proficiency in Haskell? <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-proficiency.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-proficiency.svg) or a [PNG](/static/images/2017/11/15/chart-proficiency.png).*

Most people consider themselves intermediate Haskellers.
Haskell is a big, complicated language, and few people consider themselves experts of it.

<h2 id="question-31"><a href="#question-31">31</a>: I am satisfied with Haskell as a language. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-language-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-language-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-language-satisfaction.png).*

(Questions 31 through 42 rated things on a Likert scale.
By summing the responses, the twelve questions can be compared against each other.)
Rank: 1 of 12 (best).
Haskell users are extremely satisfied with the language itself.

<h2 id="question-32"><a href="#question-32">32</a>: I am satisfied with Haskell's build tools (such as cabal-install or Stack). <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-build-tool-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-build-tool-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-build-tool-satisfaction.png).*

Rank: 4 of 12.
Haskell users are satisfied with the build tools.

<h2 id="question-33"><a href="#question-33">33</a>: I am satisfied with Haskell's package repositories (such as Hackage or Stackage). <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-package-repository-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-package-repository-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-package-repository-satisfaction.png).*

Rank: 3 of 12.
Haskell users are satisfied with the package repositories.

<h2 id="question-34"><a href="#question-34">34</a>: I can find Haskell libraries for the things that I need. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-library-existence-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-library-existence-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-library-existence-satisfaction.png).*

Rank: 5 of 12.
Haskell users are satisfied with the library selection.
I was surprised to see this one ranked so high given that Haskell doesn't have nearly as many libraries as other languages like JavaScript or Rust.

<h2 id="question-35"><a href="#question-35">35</a>: I think Haskell libraries are high quality. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-library-quality-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-library-quality-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-library-quality-satisfaction.png).*

Rank: 2 of 12.
Haskell users are extremely satisfied with the quality of libraries.
It's interesting to compare this result to the next two questions.
Haskell libraries are high quality but poorly documented and hard to choose between.

<h2 id="question-36"><a href="#question-36">36</a>: I think Haskell libraries are well documented. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-library-documentation-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-library-documentation-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-library-documentation-satisfaction.png).*

Rank: 10 of 12.
Haskell users are unsatisfied with library documentation.
This is a common complaint.

<h2 id="question-37"><a href="#question-37">37</a>: I can easily compare Haskell libraries to select the best one. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-library-comparison-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-library-comparison-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-library-comparison-satisfaction.png).*

Rank: 12 of 12 (worst).
Haskell users are extremely unsatisfied with the ability to compare libraries.
I am not at all surprised to see this at the bottom of the list.
There is no good way to compare libraries that solve the same problem.
In fact, there's often no good way to even discover other similar libraries.

<h2 id="question-38"><a href="#question-38">38</a>: I find Haskell libraries easy to use. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-library-ease-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-library-ease-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-library-ease-satisfaction.png).*

Rank: 9 of 12.
Haskell users are unsatisfied with the ease of libraries.
The lack of good documentation no doubt contributes to this.

<h2 id="question-39"><a href="#question-39">39</a>: I find that Haskell libraries provide a stable API. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-library-stability-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-library-stability-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-library-stability-satisfaction.png).*

Rank: 7 of 12.
Haskell users are satisfied with library stability.

<h2 id="question-40"><a href="#question-40">40</a>: I find that Haskell libraries work well with other libraries. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-library-compatibility-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-library-compatibility-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-library-compatibility-satisfaction.png).*

Rank: 8 of 12.
Haskell users are satisfied with library compatibility.

<h2 id="question-41"><a href="#question-41">41</a>: I find that Haskell libraries perform well. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-library-performance-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-library-performance-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-library-performance-satisfaction.png).*

Rank: 6 of 12.
Haskell users are satisfied with library performance.

<h2 id="question-42"><a href="#question-42">42</a>: I can easily reason about the performance of my Haskell programs. <a href="#">&#x2191;</a></h2>

![](/static/images/2017/11/15/chart-program-performance-satisfaction.svg)

*Download this image as an [SVG](/static/images/2017/11/15/chart-program-performance-satisfaction.svg) or a [PNG](/static/images/2017/11/15/chart-program-performance-satisfaction.png).*

Rank: 11 of 12.
Haskell users are extremely unsatisfied with their ability to reason about the performance of their programs.
This is interesting to compare to the previous question
because even though Haskell libraries are performant,
making performant programs is still hard.
This is a common complaint about Haskell that its pervasive laziness contributes to.

<h2 id="question-43"><a href="#question-43">43</a>: In your opinion, what is Haskell's biggest problem? <a href="#">&#x2191;</a></h2>

> Haskell's strongest point is its biggest problem at the same time:
> it's too powerful.
> One can express pretty much anything,
> but there are no strongly accepted best practices or design patterns for a lot of tasks.

Almost 1,000 people answered this question, giving more than 20,000 words of feedback.
It's nearly impossible to distill it down into key points.

Most of the big problems were covered by other questions and expanded upon here.

<h2 id="question-44"><a href="#question-44">44</a>: If you could change anything about Haskell's community, what would it be? <a href="#">&#x2191;</a></h2>

> I think the community has a lot of very grown-up, intelligent, and thoughtful people, generally open to exchange.
> Having said that, I also have the impression that we tend to over-complicate a lot of things instead of trying to get the simplest possible solutions to a lot of problems.

Most people want the Haskell community to be bigger.
To that end, many people want the community to be less divisive (for example Stack versus Cabal)
or less elitist (for example putting down other languages).
Also more beginner and intermediate documentation and tutorials would help.

<h2 id="question-45"><a href="#question-45">45</a>: Do you have any other comments about Haskell, its ecosystem, its community, or this survey? <a href="#">&#x2191;</a></h2>

> Too bad the questions forced me to show the bad side of Haskell.
> This is by far my preferred language.
> The one I think in when I close my eyes.
> When I code in Haskell after a day having coding in Clojure I'm so relieved.

It's hard to summarize these responses as they varied quite a bit.
Even so, most of them were positive and hopeful.
In spite of the gripes,
people generally like Haskell and its community.
