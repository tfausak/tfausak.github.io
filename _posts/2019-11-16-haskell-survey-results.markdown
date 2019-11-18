---
title: 2019 State of Haskell Survey results
---

The third annual State of Haskell Survey closed a couple days ago.
This post will analyze the results, graph them, and compare them to previous years ([2018][], [2017][]).
I will not attempt to summarize the answers to free-response questions.
I encourage any interested parties to download the raw results, do their own analysis, and share their results.

You can download the anonymized responses in [CSV][] or [JSON][] formats.
Both are licensed under the [ODbL 1.0 license][].

[2018]: {% post_url 2018-11-18-2018-state-of-haskell-survey-results %}
[2017]: {% post_url 2017-11-15-2017-state-of-haskell-survey-results %}
[CSV]: /static/pages/2019-11-16-state-of-haskell-survey-results.csv.zip
[JSON]: /static/pages/2019-11-16-state-of-haskell-survey-results.json.zip
[ODbL 1.0 license]: https://opendatacommons.org/licenses/odbl/

<style>
  .row {
    position: relative;
  }
  .bar {
    background: plum;
    height: 100%;
    left: 0;
    position: absolute;
    top: 0;
  }
  .percent, .count, .choice {
    display: inline-block;
    position: relative;
  }
  .percent, .count {
    text-align: right;
    width: 3em;
  }
  .choice {
    padding-left: 1em;
  }
</style>

<ul>
  <li> <a href='#s0q0'> What is your email address? </a> </li>
  <li>
    <a href='#s0'> Haskell usage </a>
    <ul>
      <li> <a href='#s0q1'> Do you use Haskell? </a> </li>
      <li> <a href='#s0q2'> If you stopped using Haskell, how long did you use it before you stopped? </a> </li>
      <li> <a href='#s0q3'> If you do not use Haskell, why not? </a> </li>
      <li> <a href='#s0q4'> How long have you been using Haskell? </a> </li>
      <li> <a href='#s0q5'> How frequently do you use Haskell? </a> </li>
      <li> <a href='#s0q6'> How would you rate your proficiency in Haskell? </a> </li>
      <li> <a href='#s0q7'> Where do you use Haskell? </a> </li>
      <li> <a href='#s0q8'> Do you use Haskell at work? </a> </li>
      <li> <a href='#s0q9'> If you do not use Haskell at work, why not? </a> </li>
      <li> <a href='#s0q10'> Which programming languages other than Haskell are you fluent in? </a> </li>
      <li> <a href='#s0q11'> Which types of software do you develop with Haskell? </a> </li>
      <li> <a href='#s0q12'> Which industries do you use Haskell in? </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s1'> Projects </a>
    <ul>
      <li> <a href='#s1q0'> How many Haskell projects do you contribute to? </a> </li>
      <li> <a href='#s1q1'> What is the total size of all the Haskell projects you contribute to? </a> </li>
      <li> <a href='#s1q2'> Which platforms do you develop Haskell on? </a> </li>
      <li> <a href='#s1q3'> Which platforms do you target? </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s2'> Compilers </a>
    <ul>
      <li> <a href='#s2q0'> Which Haskell compilers do you use? </a> </li>
      <li> <a href='#s2q1'> Which installation methods do you use for your Haskell compiler? </a> </li>
      <li> <a href='#s2q2'> Has upgrading your Haskell compiler broken your code in the last year? </a> </li>
      <li> <a href='#s2q3'> How has upgrading your Haskell compiler broken your code in the last year? </a> </li>
      <li> <a href='#s2q4'> Which versions of GHC do you use? </a> </li>
      <li> <a href='#s2q5'> Which language extensions would you like to be enabled by default? </a> </li>
      <li> <a href='#s2q6'> How important do you feel it would be to have a new version of the Haskell language standard? </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s3'> Tooling </a>
    <ul>
      <li> <a href='#s3q0'> Which build tools do you use for Haskell? </a> </li>
      <li> <a href='#s3q1'> Which editors do you use for Haskell? </a> </li>
      <li> <a href='#s3q2'> Which version control systems do you use for Haskell? </a> </li>
      <li> <a href='#s3q3'> Where do you get Haskell packages from? </a> </li>
      <li> <a href='#s3q4'> Which tools do you use to test Haskell code? </a> </li>
      <li> <a href='#s3q5'> Which tools do you use to benchmark Haskell code? </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s4'> Infrastructure </a>
    <ul>
      <li> <a href='#s4q0'> Which tools do you use to deploy Haskell applications? </a> </li>
      <li> <a href='#s4q1'> Where do you deploy Haskell applications? </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s5'> Community </a>
    <ul>
      <li> <a href='#s5q0'> Where do you interact with the Haskell community? </a> </li>
      <li> <a href='#s5q1'> Which of the following Haskell topics would you like to see more written about? </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s6'> Feelings </a>
    <ul>
      <li> <a href='#s6q19'> I would prefer to use Haskell for my next new project. </a> </li>
      <li> <a href='#s6q18'> I would recommend using Haskell to others. </a> </li>
      <li> <a href='#s6q14'> Once my Haskell program compiles, it generally does what I intended. </a> </li>
      <li> <a href='#s6q2'> I am satisfied with Haskell's compilers, such as GHC. </a> </li>
      <li> <a href='#s6q1'> I am satisfied with Haskell as a language. </a> </li>
      <li> <a href='#s6q13'> I think that software written in Haskell is easy to maintain. </a> </li>
      <li> <a href='#s6q16'> Haskell's performance meets my needs. </a> </li>
      <li> <a href='#s6q0'> I feel welcome in the Haskell community. </a> </li>
      <li> <a href='#s6q4'> I am satisfied with Haskell's package repositories, such as Hackage. </a> </li>
      <li> <a href='#s6q6'> I think Haskell libraries are high quality. </a> </li>
      <li> <a href='#s6q15'> I think that Haskell libraries perform well. </a> </li>
      <li> <a href='#s6q20'> Haskell is working well for my team. </a> </li>
      <li> <a href='#s6q5'> I can find Haskell libraries for the things that I need. </a> </li>
      <li> <a href='#s6q3'> I am satisfied with Haskell's build tools, such as Cabal. </a> </li>
      <li> <a href='#s6q12'> I think that Haskell libraries work well together. </a> </li>
      <li> <a href='#s6q11'> I think that Haskell libraries provide a stable API. </a> </li>
      <li> <a href='#s6q7'> I have a good understanding of Haskell best practices. </a> </li>
      <li> <a href='#s6q10'> I think that Haskell libraries are easy to use. </a> </li>
      <li> <a href='#s6q21'> Haskell is critical to my company's success. </a> </li>
      <li> <a href='#s6q23'> As a hiring manager, I can easily find qualified Haskell candidates. </a> </li>
      <li> <a href='#s6q8'> I think Haskell libraries are well documented. </a> </li>
      <li> <a href='#s6q17'> I can easily reason about the performance of my Haskell code. </a> </li>
      <li> <a href='#s6q9'> I can easily compare competing Haskell libraries to select the best one. </a> </li>
      <li> <a href='#s6q22'> As a candidate, I can easily find Haskell jobs. </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s7'> Demographics </a>
    <ul>
      <li> <a href='#s7q0'> Which country do you live in? </a> </li>
      <li> <a href='#s7q1'> How old are you? </a> </li>
      <li> <a href='#s7q2'> What is your gender? </a> </li>
      <li> <a href='#s7q3'> Do you identify as transgender? </a> </li>
      <li> <a href='#s7q4'> Are you a student? </a> </li>
      <li> <a href='#s7q5'> What is the highest level of education you have completed? </a> </li>
      <li> <a href='#s7q6'> What is your employment status? </a> </li>
      <li> <a href='#s7q7'> How large is the company you work for? </a> </li>
      <li> <a href='#s7q8'> How many years have you been coding? </a> </li>
      <li> <a href='#s7q9'> How many years have you been coding professionally? </a> </li>
      <li> <a href='#s7q10'> Do you code as a hobby? </a> </li>
      <li> <a href='#s7q11'> Have you contributed to any open source projects? </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s8'> Meta </a>
    <ul>
      <li> <a href='#s8q0'> Did you take any previous surveys? </a> </li>
      <li> <a href='#s8q1'> How did you hear about this survey? </a> </li>
    </ul>
  </li>
  <li>
    <a href='#s9'> Free response </a>
    <ul>
      <li> <a href='#s9q0'> If you wanted to convince someone to use Haskell, what would you say? </a> </li>
      <li> <a href='#s9q1'> If you could change one thing about Haskell, what would it be? </a> </li>
    </ul>
  </li>
</ul>

---

<h2 id='s0q0'> What is your email address? </h2>

Email addresses were collected but aren't included in the results.
This year I decided to make email addresses required as a way to counteract the bogus submissions I got last year.
As far as I can tell all of this year's submissions were legitimate.
However I received fewer submissions this year compared to previous years.
This year I received 1,211 submissions.
Last year (2018) I received 1,361.
The year before (2017) I received 1,335.

<h2 id='s0'> Haskell usage </h2>

<h3 id='s0q1'> Do you use Haskell? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 89%;'> </div>
    <div class='percent'> 89% </div>
    <div class='count'> 1075 </div>
    <div class='choice'> Yes </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 92 </div>
    <div class='choice'> No, but I used to </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 35 </div>
    <div class='choice'> No, I never have </div>
  </div>
</div>

As expected, most people filling out the survey are Haskell users.
This is consistent with previous years.

<h3 id='s0q2'> If you stopped using Haskell, how long did you use it before you stopped? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 14 </div>
    <div class='choice'> Less than 1 day </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 18 </div>
    <div class='choice'> 1 day to 1 week </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 25 </div>
    <div class='choice'> 1 week to 1 month </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 46 </div>
    <div class='choice'> 1 month to 1 year </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 80 </div>
    <div class='choice'> More than 1 year </div>
  </div>
</div>

Most people who stopped using Haskell used it for more than a year.
In both previous surveys most people had used Haskell for less than a year.
What changed?
Why are people using Haskell for longer before stopping?
It's hard to say based only on the survey data.

<h3 id='s0q3'> If you do not use Haskell, why not? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 117 </div>
    <div class='choice'> My company doesn't use Haskell </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 47 </div>
    <div class='choice'> Haskell's documentation is not good enough </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 48 </div>
    <div class='choice'> Haskell lacks critical libraries </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 41 </div>
    <div class='choice'> Haskell lacks critical tools </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 39 </div>
    <div class='choice'> Haskell is too hard to learn </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 22 </div>
    <div class='choice'> Haskell's performance is not good enough </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 23 </div>
    <div class='choice'> Haskell does not support the platforms I need </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 14 </div>
    <div class='choice'> Haskell lacks critical features </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 55 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Haskell the language comes across looking pretty good here.
Most of the complaints are about the ecosystem, including documentation, packages, and tools.
This is consistent with previous years, where documentation is a frequent complaint.
Notably many people that do use Haskell aren't thrilled with the documentation either.

<h3 id='s0q4'> How long have you been using Haskell? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 15 </div>
    <div class='choice'> Less than 1 day </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 13 </div>
    <div class='choice'> 1 day to 1 week </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 27 </div>
    <div class='choice'> 1 week to 1 month </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 14%;'> </div>
    <div class='percent'> 14% </div>
    <div class='count'> 167 </div>
    <div class='choice'> 1 month to 1 year </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 14%;'> </div>
    <div class='percent'> 14% </div>
    <div class='count'> 174 </div>
    <div class='choice'> 1 year to 2 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 15%;'> </div>
    <div class='percent'> 15% </div>
    <div class='count'> 185 </div>
    <div class='choice'> 2 years to 3 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 12%;'> </div>
    <div class='percent'> 12% </div>
    <div class='count'> 140 </div>
    <div class='choice'> 3 years to 4 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 103 </div>
    <div class='choice'> 4 years to 5 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 91 </div>
    <div class='choice'> 5 years to 6 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 44 </div>
    <div class='choice'> 6 years to 7 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 227 </div>
    <div class='choice'> More than 7 years </div>
  </div>
</div>

Similar to last year there's a peak around the two year mark.
Also there's a large number of people that have used Haskell for a long time.
I thought that having seven discrete year buckets would be plenty, but it looks like I'll need even more for next year's survey!

<h3 id='s0q5'> How frequently do you use Haskell? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 40%;'> </div>
    <div class='percent'> 40% </div>
    <div class='count'> 482 </div>
    <div class='choice'> Daily </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 32%;'> </div>
    <div class='percent'> 32% </div>
    <div class='count'> 383 </div>
    <div class='choice'> Weekly </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 189 </div>
    <div class='choice'> Monthly </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 39 </div>
    <div class='choice'> Yearly </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 93 </div>
    <div class='choice'> Rarely </div>
  </div>
</div>

Most people that use Haskell use it daily.
This is consistent with previous years.
To mean that suggests that it's a good choice for a wide variety of tasks, otherwise people may only sometimes reach for it.
That matches my own personal experience as well.

<h3 id='s0q6'> How would you rate your proficiency in Haskell? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 14%;'> </div>
    <div class='percent'> 14% </div>
    <div class='count'> 169 </div>
    <div class='choice'> Beginner </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 41%;'> </div>
    <div class='percent'> 41% </div>
    <div class='count'> 495 </div>
    <div class='choice'> Intermediate </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 30%;'> </div>
    <div class='percent'> 30% </div>
    <div class='count'> 361 </div>
    <div class='choice'> Advanced </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 137 </div>
    <div class='choice'> Expert </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 31 </div>
    <div class='choice'> Master </div>
  </div>
</div>

Haskell has a high skill ceiling, and most users consider themselves intermediate users.
It seems like there's always something more to know, but you don't need to know much in order to get things done.
This matches previous years.

I think a common complaint from the Haskell community is that there's a lot of content for beginners and experts, but not much in between.
That's especially frustrating since so much of the community is in that intermediate level of proficiency.

<h3 id='s0q7'> Where do you use Haskell? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 85%;'> </div>
    <div class='percent'> 85% </div>
    <div class='count'> 1026 </div>
    <div class='choice'> Home </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 54%;'> </div>
    <div class='percent'> 54% </div>
    <div class='count'> 653 </div>
    <div class='choice'> Work </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 139 </div>
    <div class='choice'> School </div>
  </div>
</div>

This is remarkably consistent with previous years.
Plenty of people use Haskell at home, but it's not breaking into the workplace.
It's hard to analyze this question more on its own.
Other questions in the survey, like the next two, give more insight.

<h3 id='s0q8'> Do you use Haskell at work? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 30%;'> </div>
    <div class='percent'> 30% </div>
    <div class='count'> 363 </div>
    <div class='choice'> Yes, most of the time </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 22%;'> </div>
    <div class='percent'> 22% </div>
    <div class='count'> 261 </div>
    <div class='choice'> Yes, some of the time </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 17 </div>
    <div class='choice'> No, but my company does </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 41%;'> </div>
    <div class='percent'> 41% </div>
    <div class='count'> 492 </div>
    <div class='choice'> No, but I'd like to </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 34 </div>
    <div class='choice'> No, and I don't want to </div>
  </div>
</div>

More people use Haskell at work than don't, which is great.
This is similar to previous years, but this year I asked about wanting to use Haskell if you're not already.
Most people that aren't using Haskell at work want to be.
So what's stopping them?
The next question sheds some light on that.

<h3 id='s0q9'> If you do not use Haskell at work, why not? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 33%;'> </div>
    <div class='percent'> 33% </div>
    <div class='count'> 398 </div>
    <div class='choice'> My company doesn't use Haskell </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 105 </div>
    <div class='choice'> It's too hard to hire Haskell developers </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 91 </div>
    <div class='choice'> Haskell lacks critical libraries </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 77 </div>
    <div class='choice'> Haskell is too hard to learn </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 72 </div>
    <div class='choice'> Haskell lacks critical tools </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 62 </div>
    <div class='choice'> Haskell's documentation is not good enough </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 40 </div>
    <div class='choice'> Haskell does not support the platforms I need </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 29 </div>
    <div class='choice'> Haskell's performance is not good enough </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 20 </div>
    <div class='choice'> Haskell lacks critical features </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 95 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Most people don't use Haskell at work simply because it's not one of the blessed languages.
And it's not one of the blessed languages because it's hard to hire Haskell developers.
Seems like a bit of a catch-22.
Regardless this is similar to previous years, and also similar to the "If you do not use Haskell, why not?" question.
Haskell's documentation, package ecosystem, and tooling all need to level up in order to be viable to use at work.

<h3 id='s0q10'> Which programming languages other than Haskell are you fluent in? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 52%;'> </div>
    <div class='percent'> 52% </div>
    <div class='count'> 630 </div>
    <div class='choice'> Python </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 50%;'> </div>
    <div class='percent'> 50% </div>
    <div class='count'> 601 </div>
    <div class='choice'> JavaScript </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 45%;'> </div>
    <div class='percent'> 45% </div>
    <div class='count'> 539 </div>
    <div class='choice'> C </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 44%;'> </div>
    <div class='percent'> 44% </div>
    <div class='count'> 532 </div>
    <div class='choice'> Java </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 29%;'> </div>
    <div class='percent'> 29% </div>
    <div class='count'> 348 </div>
    <div class='choice'> Shell </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 28%;'> </div>
    <div class='percent'> 28% </div>
    <div class='count'> 345 </div>
    <div class='choice'> C++ </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 235 </div>
    <div class='choice'> TypeScript </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 229 </div>
    <div class='choice'> C# </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 18%;'> </div>
    <div class='percent'> 18% </div>
    <div class='count'> 212 </div>
    <div class='choice'> Elm </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 17%;'> </div>
    <div class='percent'> 17% </div>
    <div class='count'> 209 </div>
    <div class='choice'> Rust </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 192 </div>
    <div class='choice'> Scala </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 15%;'> </div>
    <div class='percent'> 15% </div>
    <div class='count'> 176 </div>
    <div class='choice'> Ruby </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 12%;'> </div>
    <div class='percent'> 12% </div>
    <div class='count'> 140 </div>
    <div class='choice'> Go </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 139 </div>
    <div class='choice'> PHP </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 120 </div>
    <div class='choice'> PureScript </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 100 </div>
    <div class='choice'> Assembly </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 90 </div>
    <div class='choice'> Clojure </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 84 </div>
    <div class='choice'> Perl </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 82 </div>
    <div class='choice'> Ocaml </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 73 </div>
    <div class='choice'> Matlab </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 68 </div>
    <div class='choice'> F# </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 68 </div>
    <div class='choice'> Kotlin </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 64 </div>
    <div class='choice'> R </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 63 </div>
    <div class='choice'> Lua </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 57 </div>
    <div class='choice'> Erlang </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 55 </div>
    <div class='choice'> Objective-C </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 54 </div>
    <div class='choice'> Swift </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 29 </div>
    <div class='choice'> VB.NET </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 20 </div>
    <div class='choice'> Julia </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 20 </div>
    <div class='choice'> VBA </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 19 </div>
    <div class='choice'> Groovy </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 8 </div>
    <div class='choice'> Hack </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 12%;'> </div>
    <div class='percent'> 12% </div>
    <div class='count'> 149 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Once again the clear front runners are Python, JavaScript, C, Java, Shell, and C++.
I'm not sure what conclusions can be drawn from this, other than those are popular languages and they remain popular even with Haskell users.
Perhaps these results suggest that the Haskell community could see what those other language communities are doing well and try to copy them.
Also we could try to produce content that shows how to take something from, say, Python and translate it into Haskell.

<h3 id='s0q11'> Which types of software do you develop with Haskell? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 63%;'> </div>
    <div class='percent'> 63% </div>
    <div class='count'> 758 </div>
    <div class='choice'> Command-line programs (CLI) </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 47%;'> </div>
    <div class='percent'> 47% </div>
    <div class='count'> 572 </div>
    <div class='choice'> API services (returning non-HTML) </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 38%;'> </div>
    <div class='percent'> 38% </div>
    <div class='count'> 463 </div>
    <div class='choice'> Libraries or frameworks </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 37%;'> </div>
    <div class='percent'> 37% </div>
    <div class='count'> 448 </div>
    <div class='choice'> Data processing </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 35%;'> </div>
    <div class='percent'> 35% </div>
    <div class='count'> 425 </div>
    <div class='choice'> Automation or scripts </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 33%;'> </div>
    <div class='percent'> 33% </div>
    <div class='count'> 400 </div>
    <div class='choice'> Web services (returning HTML) </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 20%;'> </div>
    <div class='percent'> 20% </div>
    <div class='count'> 248 </div>
    <div class='choice'> Agents or daemons </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 123 </div>
    <div class='choice'> Desktop programs (GUI) </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 99 </div>
    <div class='choice'> Other </div>
  </div>
</div>

CLI programs and API services are even more popular this year than last, but the overall distribution is pretty similar.
For people trying to use Haskell at work, introducing a CLI or API program is probably the way to go.
Their popularity in the Haskell community means that they're likely to have the best packages available and good documentation.
Usually there are a wide range of approaches too, like optparse-applicative versus optparse-generic, or scotty versus servant-server.

<h3 id='s0q12'> Which industries do you use Haskell in? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 27%;'> </div>
    <div class='percent'> 27% </div>
    <div class='count'> 329 </div>
    <div class='choice'> Web </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 196 </div>
    <div class='choice'> Banking or finance </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 190 </div>
    <div class='choice'> Education </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 113 </div>
    <div class='choice'> Commerce or retail </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 60 </div>
    <div class='choice'> Gaming </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 57 </div>
    <div class='choice'> Healthcare or medical </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> Mobile </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 37 </div>
    <div class='choice'> Embedded </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 33 </div>
    <div class='choice'> Government </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 227 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Web continues to dominate, with finance and education more or less tied in second.
There's a long tail of things grouped into "Other".
The most popular are cryptocurrencies, academia/research/science, and security.
I will be sure to split out those industries next year.

<h2 id='s1'> Projects </h2>

<h3 id='s1q0'> How many Haskell projects do you contribute to? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 30%;'> </div>
    <div class='percent'> 30% </div>
    <div class='count'> 358 </div>
    <div class='choice'> 0 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 17%;'> </div>
    <div class='percent'> 17% </div>
    <div class='count'> 210 </div>
    <div class='choice'> 1 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 188 </div>
    <div class='choice'> 2 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 136 </div>
    <div class='choice'> 3 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 54 </div>
    <div class='choice'> 4 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 62 </div>
    <div class='choice'> 5 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 91 </div>
    <div class='choice'> 6 to 10 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 28 </div>
    <div class='choice'> 11 to 20 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 41 </div>
    <div class='choice'> More than 20 </div>
  </div>
</div>

Many people do not contribute to any Haskell projects.
Almost half of people contribute to between one and three projects.
This is similar to last year.

<h3 id='s1q1'> What is the total size of all the Haskell projects you contribute to? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 26%;'> </div>
    <div class='percent'> 26% </div>
    <div class='count'> 311 </div>
    <div class='choice'> Less than 1,000 lines of code </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 26%;'> </div>
    <div class='percent'> 26% </div>
    <div class='count'> 312 </div>
    <div class='choice'> Between 1,000 and 9,999 lines of code </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 22%;'> </div>
    <div class='percent'> 22% </div>
    <div class='count'> 265 </div>
    <div class='choice'> Between 10,000 and 99,999 lines of code </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 133 </div>
    <div class='choice'> More than 100,000 lines of code </div>
  </div>
</div>

The "Less than 1,000 lines of code" bucket saw an increase compared to previous years.
Not many people are working on very large projects.
Taken with the previous question, it seems like most people are working on a few small projects.
This is perhaps a consequence of most people not using Haskell at work, since personal projects take time to maintain and typically don't grow that large.

<h3 id='s1q2'> Which platforms do you develop Haskell on? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 74%;'> </div>
    <div class='percent'> 74% </div>
    <div class='count'> 901 </div>
    <div class='choice'> Linux </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 40%;'> </div>
    <div class='percent'> 40% </div>
    <div class='count'> 479 </div>
    <div class='choice'> MacOS </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 192 </div>
    <div class='choice'> Windows </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> BSD </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 8 </div>
    <div class='choice'> Other </div>
  </div>
</div>

This distribution looks basically identical to previous years.
My takeaway from this is that Haskell developer tools would do well to focus on Linux first, before adding support for MacOS and Windows.

<h3 id='s1q3'> Which platforms do you target? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 88%;'> </div>
    <div class='percent'> 88% </div>
    <div class='count'> 1060 </div>
    <div class='choice'> Linux </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 415 </div>
    <div class='choice'> MacOS </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 26%;'> </div>
    <div class='percent'> 26% </div>
    <div class='count'> 317 </div>
    <div class='choice'> Windows </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 63 </div>
    <div class='choice'> BSD </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 42 </div>
    <div class='choice'> Android </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> iOS </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 33 </div>
    <div class='choice'> Other </div>
  </div>
</div>

This also looks exactly like it did in previous years.
Linux is far and away the most popular deployment target.
Again my takeaway is for runtime tools like debuggers or profilers to focus on Linux first before supporting MacOS and Windows.

<h2 id='s2'> Compilers </h2>

<h3 id='s2q0'> Which Haskell compilers do you use? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 98%;'> </div>
    <div class='percent'> 98% </div>
    <div class='count'> 1181 </div>
    <div class='choice'> GHC </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 97 </div>
    <div class='choice'> GHCJS </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 13 </div>
    <div class='choice'> Eta </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 24 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Unsurprisingly GHC is far and away the most popular Haskell compiler.
GHCJS is holding steady.
Eta is less popular than last year.

<h3 id='s2q1'> Which installation methods do you use for your Haskell compiler? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 67%;'> </div>
    <div class='percent'> 67% </div>
    <div class='count'> 807 </div>
    <div class='choice'> Stack </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 28%;'> </div>
    <div class='percent'> 28% </div>
    <div class='count'> 344 </div>
    <div class='choice'> Nix </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 199 </div>
    <div class='choice'> Operating system package </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 13%;'> </div>
    <div class='percent'> 13% </div>
    <div class='count'> 154 </div>
    <div class='choice'> Haskell Platform </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 114 </div>
    <div class='choice'> ghcup </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 115 </div>
    <div class='choice'> Official binaries </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 80 </div>
    <div class='choice'> Source </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 26 </div>
    <div class='choice'> Minimal installer </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Both Nix and ghcup are more popular than previous years.
Everything else stayed more or less the same.

<h3 id='s2q2'> Has upgrading your Haskell compiler broken your code in the last year? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 66%;'> </div>
    <div class='percent'> 66% </div>
    <div class='count'> 805 </div>
    <div class='choice'> No </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 29%;'> </div>
    <div class='percent'> 29% </div>
    <div class='count'> 349 </div>
    <div class='choice'> Yes </div>
  </div>
</div>

This bounced back from last year with 54% "No", but it still hasn't returned to 2017's peak of 84% "No".
It's disappointing that nearly one-third of Haskell users had their code broken by a compiler upgrade in the past year.
The next question goes into more detail.

<h3 id='s2q3'> How has upgrading your Haskell compiler broken your code in the last year? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 233 </div>
    <div class='choice'> Expected changes, such as the MonadFail proposal </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 230 </div>
    <div class='choice'> Incompatible dependencies </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 103 </div>
    <div class='choice'> New warnings </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 49 </div>
    <div class='choice'> Compiler bugs </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 43 </div>
    <div class='choice'> Unexpected changes </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 25 </div>
    <div class='choice'> Other </div>
  </div>
</div>

This is similar to last year, except that the "New warnings" choice almost doubled.
Some new warnings were introduced this year that effectively fired on all code, like `-Wmissing-deriving-strategies`, so that probably had something to do with it.
The "Expected changes" and "Incompatible dependencies" choices are intertwined because it takes time for dependencies to upgrade in response to expected changes.
This year saw the move to the `MonadFail` type class for the `fail` method, which ended up breaking a lot of code that needed to be fixed.
Personally, while I'm in favor of moving `fail` out of the `Monad` type class, it had a very real cost in terms of time spent patching old dependencies.

<h3 id='s2q4'> Which versions of GHC do you use? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 96 </div>
    <div class='choice'> HEAD </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 35%;'> </div>
    <div class='percent'> 35% </div>
    <div class='count'> 425 </div>
    <div class='choice'> 8.8.x </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 67%;'> </div>
    <div class='percent'> 67% </div>
    <div class='count'> 810 </div>
    <div class='choice'> 8.6.x </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 24%;'> </div>
    <div class='percent'> 24% </div>
    <div class='count'> 285 </div>
    <div class='choice'> 8.4.x </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 126 </div>
    <div class='choice'> 8.2.x </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 70 </div>
    <div class='choice'> 8.0.x </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 47 </div>
    <div class='choice'> 7.x </div>
  </div>
</div>

Although the version numbers obviously change, the distribution remains remarkably consistent.
The three most recent major versions (8.8, 8.6, 8.4) cover the vast majority of users.
I've said this before and I'll say it again:
Don't spend too much time maintaining support for older versions of GHC.
Especially if it lets you simplify your package description by removing conditionals or simplify your code by removing `CPP`.

<h3 id='s2q5'> Which language extensions would you like to be enabled by default? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'> <div class='bar' style='width: 48%;'> </div> <div class='percent'> 48% </div> <div class='count'> 578 </div> <div class='choice'> OverloadedStrings </div> </div>
  <div class='row'> <div class='bar' style='width: 40%;'> </div> <div class='percent'> 40% </div> <div class='count'> 489 </div> <div class='choice'> LambdaCase </div> </div>
  <div class='row'> <div class='bar' style='width: 34%;'> </div> <div class='percent'> 34% </div> <div class='count'> 414 </div> <div class='choice'> DeriveGeneric </div> </div>
  <div class='row'> <div class='bar' style='width: 29%;'> </div> <div class='percent'> 29% </div> <div class='count'> 356 </div> <div class='choice'> DeriveFunctor </div> </div>
  <div class='row'> <div class='bar' style='width: 29%;'> </div> <div class='percent'> 29% </div> <div class='count'> 355 </div> <div class='choice'> GADTs </div> </div>
  <div class='row'> <div class='bar' style='width: 28%;'> </div> <div class='percent'> 28% </div> <div class='count'> 338 </div> <div class='choice'> BangPatterns </div> </div>
  <div class='row'> <div class='bar' style='width: 26%;'> </div> <div class='percent'> 26% </div> <div class='count'> 312 </div> <div class='choice'> FlexibleInstances </div> </div>
  <div class='row'> <div class='bar' style='width: 25%;'> </div> <div class='percent'> 25% </div> <div class='count'> 302 </div> <div class='choice'> FlexibleContexts </div> </div>
  <div class='row'> <div class='bar' style='width: 25%;'> </div> <div class='percent'> 25% </div> <div class='count'> 302 </div> <div class='choice'> ScopedTypeVariables </div> </div>
  <div class='row'> <div class='bar' style='width: 25%;'> </div> <div class='percent'> 25% </div> <div class='count'> 298 </div> <div class='choice'> RankNTypes </div> </div>
  <div class='row'> <div class='bar' style='width: 24%;'> </div> <div class='percent'> 24% </div> <div class='count'> 293 </div> <div class='choice'> DeriveFoldable </div> </div>
  <div class='row'> <div class='bar' style='width: 23%;'> </div> <div class='percent'> 23% </div> <div class='count'> 273 </div> <div class='choice'> GeneralizedNewtypeDeriving </div> </div>
  <div class='row'> <div class='bar' style='width: 22%;'> </div> <div class='percent'> 22% </div> <div class='count'> 269 </div> <div class='choice'> TypeApplications </div> </div>
  <div class='row'> <div class='bar' style='width: 21%;'> </div> <div class='percent'> 21% </div> <div class='count'> 251 </div> <div class='choice'> TypeFamilies </div> </div>
  <div class='row'> <div class='bar' style='width: 21%;'> </div> <div class='percent'> 21% </div> <div class='count'> 250 </div> <div class='choice'> DeriveTraversable </div> </div>
  <div class='row'> <div class='bar' style='width: 20%;'> </div> <div class='percent'> 20% </div> <div class='count'> 245 </div> <div class='choice'> DataKinds </div> </div>
  <div class='row'> <div class='bar' style='width: 19%;'> </div> <div class='percent'> 19% </div> <div class='count'> 235 </div> <div class='choice'> TupleSections </div> </div>
  <div class='row'> <div class='bar' style='width: 19%;'> </div> <div class='percent'> 19% </div> <div class='count'> 227 </div> <div class='choice'> MultiParamTypeClasses </div> </div>
  <div class='row'> <div class='bar' style='width: 18%;'> </div> <div class='percent'> 18% </div> <div class='count'> 213 </div> <div class='choice'> DerivingVia </div> </div>
  <div class='row'> <div class='bar' style='width: 18%;'> </div> <div class='percent'> 18% </div> <div class='count'> 212 </div> <div class='choice'> TypeOperators </div> </div>
  <div class='row'> <div class='bar' style='width: 17%;'> </div> <div class='percent'> 17% </div> <div class='count'> 207 </div> <div class='choice'> KindSignatures </div> </div>
  <div class='row'> <div class='bar' style='width: 15%;'> </div> <div class='percent'> 15% </div> <div class='count'> 178 </div> <div class='choice'> DerivingStrategies </div> </div>
  <div class='row'> <div class='bar' style='width: 15%;'> </div> <div class='percent'> 15% </div> <div class='count'> 176 </div> <div class='choice'> DeriveDataTypeable </div> </div>
  <div class='row'> <div class='bar' style='width: 14%;'> </div> <div class='percent'> 14% </div> <div class='count'> 172 </div> <div class='choice'> MultiWayIf </div> </div>
  <div class='row'> <div class='bar' style='width: 14%;'> </div> <div class='percent'> 14% </div> <div class='count'> 168 </div> <div class='choice'> ViewPatterns </div> </div>
  <div class='row'> <div class='bar' style='width: 14%;'> </div> <div class='percent'> 14% </div> <div class='count'> 164 </div> <div class='choice'> StandaloneDeriving </div> </div>
  <div class='row'> <div class='bar' style='width: 13%;'> </div> <div class='percent'> 13% </div> <div class='count'> 163 </div> <div class='choice'> ConstraintKinds </div> </div>
  <div class='row'> <div class='bar' style='width: 13%;'> </div> <div class='percent'> 13% </div> <div class='count'> 161 </div> <div class='choice'> DeriveAnyClass </div> </div>
  <div class='row'> <div class='bar' style='width: 13%;'> </div> <div class='percent'> 13% </div> <div class='count'> 160 </div> <div class='choice'> RecordWildCards </div> </div>
  <div class='row'> <div class='bar' style='width: 13%;'> </div> <div class='percent'> 13% </div> <div class='count'> 157 </div> <div class='choice'> EmptyCase </div> </div>
  <div class='row'> <div class='bar' style='width: 12%;'> </div> <div class='percent'> 12% </div> <div class='count'> 144 </div> <div class='choice'> ApplicativeDo </div> </div>
  <div class='row'> <div class='bar' style='width: 12%;'> </div> <div class='percent'> 12% </div> <div class='count'> 141 </div> <div class='choice'> FunctionalDependencies </div> </div>
  <div class='row'> <div class='bar' style='width: 11%;'> </div> <div class='percent'> 11% </div> <div class='count'> 139 </div> <div class='choice'> ExplicitForAll </div> </div>
  <div class='row'> <div class='bar' style='width: 11%;'> </div> <div class='percent'> 11% </div> <div class='count'> 135 </div> <div class='choice'> InstanceSigs </div> </div>
  <div class='row'> <div class='bar' style='width: 11%;'> </div> <div class='percent'> 11% </div> <div class='count'> 128 </div> <div class='choice'> GADTSyntax </div> </div>
  <div class='row'> <div class='bar' style='width: 10%;'> </div> <div class='percent'> 10% </div> <div class='count'> 125 </div> <div class='choice'> PatternSynonyms </div> </div>
  <div class='row'> <div class='bar' style='width: 10%;'> </div> <div class='percent'> 10% </div> <div class='count'> 122 </div> <div class='choice'> NamedFieldPuns </div> </div>
  <div class='row'> <div class='bar' style='width: 10%;'> </div> <div class='percent'> 10% </div> <div class='count'> 120 </div> <div class='choice'> NumericUnderscores </div> </div>
  <div class='row'> <div class='bar' style='width: 100%;'> </div> <div class='percent'> 212% </div> <div class='count'> 2563 </div> <div class='choice'> Other </div> </div>
</div>

This is everyone's favorite question.
As you can see, there's a long tail of extensions that people would like to be enabled by default.
I only included things above that got at least 10% of the vote.
`OverloadedStrings` is always the most popular, but it actually got more popular this year.
`LambdaCase` continues to hang out in second place, followed by some extensions related to deriving and `GADTs`.
My read of this is that people want some quality of life improvements without having to manually enable them, either in their Cabal file or in every source file.

<h3 id='s2q6'> How important do you feel it would be to have a new version of the Haskell language standard? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 12%;'> </div>
    <div class='percent'> 12% </div>
    <div class='count'> 148 </div>
    <div class='choice'> Extremely important </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 23%;'> </div>
    <div class='percent'> 23% </div>
    <div class='count'> 277 </div>
    <div class='choice'> Very important </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 36%;'> </div>
    <div class='percent'> 36% </div>
    <div class='count'> 442 </div>
    <div class='choice'> Moderately important </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 13%;'> </div>
    <div class='percent'> 13% </div>
    <div class='count'> 156 </div>
    <div class='choice'> Slightly important </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 95 </div>
    <div class='choice'> Not at all important </div>
  </div>
</div>

This is consistent with last year.
People want a new version of the Haskell language standard (perhaps to enable some of those extensions?) but it's not a showstopper.

<h2 id='s3'> Tooling </h2>

<h3 id='s3q0'> Which build tools do you use for Haskell? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 74%;'> </div>
    <div class='percent'> 74% </div>
    <div class='count'> 894 </div>
    <div class='choice'> Stack </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 51%;'> </div>
    <div class='percent'> 51% </div>
    <div class='count'> 619 </div>
    <div class='choice'> Cabal </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 30%;'> </div>
    <div class='percent'> 30% </div>
    <div class='count'> 360 </div>
    <div class='choice'> Nix </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 110 </div>
    <div class='choice'> Shake </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 102 </div>
    <div class='choice'> Make </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 56 </div>
    <div class='choice'> ghc-pkg </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 47 </div>
    <div class='choice'> Bazel </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 22 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Nix gained some ground, but otherwise this looks about the same as previous years.
Stack continues to be the most popular build tool for Haskell.

<h3 id='s3q1'> Which editors do you use for Haskell? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 41%;'> </div>
    <div class='percent'> 41% </div>
    <div class='count'> 493 </div>
    <div class='choice'> Vi </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 32%;'> </div>
    <div class='percent'> 32% </div>
    <div class='count'> 391 </div>
    <div class='choice'> Emacs </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 30%;'> </div>
    <div class='percent'> 30% </div>
    <div class='count'> 367 </div>
    <div class='choice'> Visual Studio Code </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 63 </div>
    <div class='choice'> Atom </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 60 </div>
    <div class='choice'> Sublime Text </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 59 </div>
    <div class='choice'> IntelliJ IDEA </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 136 </div>
    <div class='choice'> Other </div>
  </div>
</div>

I feel like there are two main "camps" here: The terminal-based editor users and the graphical ones.
Vi and Emacs remain as popular as ever.
However VSCode continues to get more popular year over year.
Notably Atom continues to drop in popularity, so perhaps Atom users are abandoning it for VSCode.
If things keep going at this rate, VSCode is going to overtake Vi/Emacs next year.
Popular "Other" choices include various Vi-family editors like Neovim, various Emacs-family editors like Spacemacs, and Kakoune.

<h3 id='s3q2'> Which version control systems do you use for Haskell? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 95%;'> </div>
    <div class='percent'> 95% </div>
    <div class='count'> 1152 </div>
    <div class='choice'> Git </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 34 </div>
    <div class='choice'> Mercurial </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 33 </div>
    <div class='choice'> Darcs </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 12 </div>
    <div class='choice'> Pijul </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 9 </div>
    <div class='choice'> Subversion </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 5 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Exactly the same as previous years.
Everyone uses Git.
Hardly anybody uses anything else.

<h3 id='s3q3'> Where do you get Haskell packages from? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 68%;'> </div>
    <div class='percent'> 68% </div>
    <div class='count'> 818 </div>
    <div class='choice'> Hackage </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 65%;'> </div>
    <div class='percent'> 65% </div>
    <div class='count'> 782 </div>
    <div class='choice'> Stackage </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 26%;'> </div>
    <div class='percent'> 26% </div>
    <div class='count'> 310 </div>
    <div class='choice'> Nix </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 21%;'> </div>
    <div class='percent'> 21% </div>
    <div class='count'> 254 </div>
    <div class='choice'> Source </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 24 </div>
    <div class='choice'> Other </div>
  </div>
</div>

This is similar to previous years, except that Nix is more popular than it was before.
Seems like Nix is gaining ground in a lot of different places.

<h3 id='s3q4'> Which tools do you use to test Haskell code? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 55%;'> </div>
    <div class='percent'> 55% </div>
    <div class='count'> 663 </div>
    <div class='choice'> QuickCheck </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 44%;'> </div>
    <div class='percent'> 44% </div>
    <div class='count'> 533 </div>
    <div class='choice'> Hspec </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 32%;'> </div>
    <div class='percent'> 32% </div>
    <div class='count'> 389 </div>
    <div class='choice'> HUnit </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 24%;'> </div>
    <div class='percent'> 24% </div>
    <div class='count'> 289 </div>
    <div class='choice'> Tasty </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 17%;'> </div>
    <div class='percent'> 17% </div>
    <div class='count'> 203 </div>
    <div class='choice'> Hedgehog </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 60 </div>
    <div class='choice'> SmallCheck </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 44 </div>
    <div class='choice'> Haskell Test Framework </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 30 </div>
    <div class='choice'> Other </div>
  </div>
</div>

This is more or less the same as last year, except that both HUnit and Hedgehog are more popular.
I'm still surprised that property-based testing with QuickCheck is more popular than typical unit testing libraries like Hspec and HUnit.
I'm happy about it, though!
Property-based tests are wonderful, in my opinion.

<h3 id='s3q5'> Which tools do you use to benchmark Haskell code? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 411 </div>
    <div class='choice'> Criterion </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 53 </div>
    <div class='choice'> Bench </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 46 </div>
    <div class='choice'> Gauge </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 28 </div>
    <div class='choice'> Other </div>
  </div>
</div>

This is the same as last year.
Criterion is the most popular tool for benchmarking Haskell code.

<h2 id='s4'> Infrastructure </h2>

<h3 id='s4q0'> Which tools do you use to deploy Haskell applications? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 40%;'> </div>
    <div class='percent'> 40% </div>
    <div class='count'> 487 </div>
    <div class='choice'> Static binaries </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 408 </div>
    <div class='choice'> Docker images </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 18%;'> </div>
    <div class='percent'> 18% </div>
    <div class='count'> 219 </div>
    <div class='choice'> Nix expressions </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 190 </div>
    <div class='choice'> Dynamic binaries </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> Other </div>
  </div>
</div>

This looks pretty much the same as last year.
I'm a little confused about the popularity of static binaries, since producing static Haskell executables with libraries like glibc and libpq is challening.
Perhaps people are deploying mostly static binaries.
Regardless, Nix is more popular this year than last, and Docker remains a popular alternative to binaries.

<h3 id='s4q1'> Where do you deploy Haskell applications? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 43%;'> </div>
    <div class='percent'> 43% </div>
    <div class='count'> 524 </div>
    <div class='choice'> Self or company owned servers </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 26%;'> </div>
    <div class='percent'> 26% </div>
    <div class='count'> 311 </div>
    <div class='choice'> Amazon Web Services </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 92 </div>
    <div class='choice'> Google Cloud </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 80 </div>
    <div class='choice'> Digital Ocean </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 53 </div>
    <div class='choice'> Heroku </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 40 </div>
    <div class='choice'> Microsoft Azure </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 67 </div>
    <div class='choice'> Other </div>
  </div>
</div>

This is about the same as last year.
Both Google's and Microsoft's clouds are little more popular.
That being said, the most popular options by a long shot are self-owned infrastructure or AWS.

<h2 id='s5'> Community </h2>

<h3 id='s5q0'> Where do you interact with the Haskell community? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 56%;'> </div>
    <div class='percent'> 56% </div>
    <div class='count'> 680 </div>
    <div class='choice'> Reddit </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 55%;'> </div>
    <div class='percent'> 55% </div>
    <div class='count'> 667 </div>
    <div class='choice'> GitHub </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 32%;'> </div>
    <div class='percent'> 32% </div>
    <div class='count'> 383 </div>
    <div class='choice'> Twitter </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 25%;'> </div>
    <div class='percent'> 25% </div>
    <div class='count'> 299 </div>
    <div class='choice'> Stack Overflow </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 23%;'> </div>
    <div class='percent'> 23% </div>
    <div class='count'> 275 </div>
    <div class='choice'> Slack </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 22%;'> </div>
    <div class='percent'> 22% </div>
    <div class='count'> 264 </div>
    <div class='choice'> IRC </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 22%;'> </div>
    <div class='percent'> 22% </div>
    <div class='count'> 261 </div>
    <div class='choice'> Meetups </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 21%;'> </div>
    <div class='percent'> 21% </div>
    <div class='count'> 250 </div>
    <div class='choice'> Conferences (commercial) </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 20%;'> </div>
    <div class='percent'> 20% </div>
    <div class='count'> 242 </div>
    <div class='choice'> Mailing lists </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 15%;'> </div>
    <div class='percent'> 15% </div>
    <div class='count'> 185 </div>
    <div class='choice'> Conferences (academic) </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 97 </div>
    <div class='choice'> Discord </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 89 </div>
    <div class='choice'> Lobsters </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 57 </div>
    <div class='choice'> Telegram </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 51 </div>
    <div class='choice'> Gitter </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 31 </div>
    <div class='choice'> Mastodon </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 30 </div>
    <div class='choice'> Discourse </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 21 </div>
    <div class='choice'> Matrix/Riot </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> Other </div>
  </div>
</div>

Yet another one that's basically the same as the previous years.
This is the first year that Slack overtook IRC though.

<h3 id='s5q1'> Which of the following Haskell topics would you like to see more written about? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 57%;'> </div>
    <div class='percent'> 57% </div>
    <div class='count'> 687 </div>
    <div class='choice'> Best practices </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 44%;'> </div>
    <div class='percent'> 44% </div>
    <div class='count'> 530 </div>
    <div class='choice'> Design patterns </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 42%;'> </div>
    <div class='percent'> 42% </div>
    <div class='count'> 504 </div>
    <div class='choice'> Application architectures </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 417 </div>
    <div class='choice'> Library walkthroughs </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 413 </div>
    <div class='choice'> Performance analysis </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 409 </div>
    <div class='choice'> Tooling choices </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 32%;'> </div>
    <div class='percent'> 32% </div>
    <div class='count'> 384 </div>
    <div class='choice'> Case studies </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 31%;'> </div>
    <div class='percent'> 31% </div>
    <div class='count'> 377 </div>
    <div class='choice'> Production infrastructure </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 28%;'> </div>
    <div class='percent'> 28% </div>
    <div class='count'> 343 </div>
    <div class='choice'> Debugging how-tos </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 25%;'> </div>
    <div class='percent'> 25% </div>
    <div class='count'> 307 </div>
    <div class='choice'> Web development </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 24%;'> </div>
    <div class='percent'> 24% </div>
    <div class='count'> 285 </div>
    <div class='choice'> Project maintenance </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 22%;'> </div>
    <div class='percent'> 22% </div>
    <div class='count'> 267 </div>
    <div class='choice'> Algorithm implementations </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 21%;'> </div>
    <div class='percent'> 21% </div>
    <div class='count'> 251 </div>
    <div class='choice'> Project setup </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 21%;'> </div>
    <div class='percent'> 21% </div>
    <div class='count'> 249 </div>
    <div class='choice'> GUIs </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 230 </div>
    <div class='choice'> Beginner fundamentals </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 226 </div>
    <div class='choice'> Machine learning </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 189 </div>
    <div class='choice'> Game development </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 14%;'> </div>
    <div class='percent'> 14% </div>
    <div class='count'> 164 </div>
    <div class='choice'> Mobile development </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 126 </div>
    <div class='choice'> Comparisons to other languages </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 31 </div>
    <div class='choice'> Other </div>
  </div>
</div>

The top six choices are exactly the same as last year.
Haskell users would like to see more written about best practices, design patterns, application architectures, library walkthroughs, performance analysis, and tooling choices.
I think this year has seen improvement in these intermediate-level topics, thanks in no small part to Type Classes, but clearly there's room for more.
For those of you that are using Haskell at work, I'll bet the community would appreciate you writing about some of the choices you've made along the way.

<h2 id='s6'> Feelings </h2>

Up until this point I have been keeping the order of the questions the same as they were in the survey.
For this section I will instead sort them from best to worst.
Since each question has the same answer choices, a score can be given to each question by assigning a number value to each choice and averaging them.
"Strongly agree" is worth 5 points, "Agree" 4, "Neutral" 3, "Disagree" 2, and "Strongly disagree" 1.

<h3 id='s6q19'> I would prefer to use Haskell for my next new project. </h3>
<p> Single select. Average: 4.45 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 58%;'> </div>
    <div class='percent'> 58% </div>
    <div class='count'> 702 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 27%;'> </div>
    <div class='percent'> 27% </div>
    <div class='count'> 330 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 87 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 32 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 10 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

Similar to last year, most people would prefer to use Haskell for their next project.

<h3 id='s6q18'> I would recommend using Haskell to others. </h3>
<p> Single select. Average: 4.32 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 47%;'> </div>
    <div class='percent'> 47% </div>
    <div class='count'> 575 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 36%;'> </div>
    <div class='percent'> 36% </div>
    <div class='count'> 434 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 106 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 9 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

Also similar to last year, most people would recommend Haskell to others.

<h3 id='s6q14'> Once my Haskell program compiles, it generally does what I intended. </h3>
<p> Single select. Average: 4.19 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 409 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 48%;'> </div>
    <div class='percent'> 48% </div>
    <div class='count'> 586 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 139 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 16 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 7 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This is a bit of a meme about Haskell.
Compared to last year people actually agree with this more.
So perhaps there's some truth to this meme.

<h3 id='s6q2'> I am satisfied with Haskell's compilers, such as GHC. </h3>
<p> Single select. Average: 4.18 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 413 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 49%;'> </div>
    <div class='percent'> 49% </div>
    <div class='count'> 593 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 117 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 30 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 9 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

Just like last year, people like their Haskell compiler, which is overwhelming GHC.

<h3 id='s6q1'> I am satisfied with Haskell as a language. </h3>
<p> Single select. Average: 4.18 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 35%;'> </div>
    <div class='percent'> 35% </div>
    <div class='count'> 419 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 50%;'> </div>
    <div class='percent'> 50% </div>
    <div class='count'> 609 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 89 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 45 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 11 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This is more or less the same as the past two years.
People like Haskell as a language.

<h3 id='s6q13'> I think that software written in Haskell is easy to maintain. </h3>
<p> Single select. Average: 4.17 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 37%;'> </div>
    <div class='percent'> 37% </div>
    <div class='count'> 447 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 41%;'> </div>
    <div class='percent'> 41% </div>
    <div class='count'> 494 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 15%;'> </div>
    <div class='percent'> 15% </div>
    <div class='count'> 179 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 22 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 9 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This is similar to last year, but not as many people feel strongly about it.
For me personally this is the most compelling reason to use Haskell, but I can see how the changes in GHC 8.8 (namely `MonadFail` and deriving strategies) could make it a harder sell in the short term due to churn.

<h3 id='s6q16'> Haskell's performance meets my needs. </h3>
<p> Single select. Average: 4.05 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 29%;'> </div>
    <div class='percent'> 29% </div>
    <div class='count'> 354 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 47%;'> </div>
    <div class='percent'> 47% </div>
    <div class='count'> 572 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 14%;'> </div>
    <div class='percent'> 14% </div>
    <div class='count'> 164 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 54 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 9 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This has improved a little bit since last year.
Haskell is generally fast enough to meet people's needs.

<h3 id='s6q0'> I feel welcome in the Haskell community. </h3>
<p> Single select. Average: 4.02 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 31%;'> </div>
    <div class='percent'> 31% </div>
    <div class='count'> 374 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 43%;'> </div>
    <div class='percent'> 43% </div>
    <div class='count'> 516 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 225 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 16 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This is basically the same as last year.
The Haskell community is a place where most everyone feels welcome.

<h3 id='s6q4'> I am satisfied with Haskell's package repositories, such as Hackage. </h3>
<p> Single select. Average: 3.85 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 20%;'> </div>
    <div class='percent'> 20% </div>
    <div class='count'> 247 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 50%;'> </div>
    <div class='percent'> 50% </div>
    <div class='count'> 602 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 18%;'> </div>
    <div class='percent'> 18% </div>
    <div class='count'> 219 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 71 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 19 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

Similar to the past two years, people generally like Haskell's package repositories, but they don't feel too strongly about it.

<h3 id='s6q6'> I think Haskell libraries are high quality. </h3>
<p> Single select. Average: 3.82 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 226 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 47%;'> </div>
    <div class='percent'> 47% </div>
    <div class='count'> 572 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 24%;'> </div>
    <div class='percent'> 24% </div>
    <div class='count'> 285 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 59 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 10 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

At this point we start getting into the questions where more people feel neutral than strongly agree, even though overall the sentiment is still positive.
The answers to this question have stayed remarkably consistent year after year.
Haskell libraries are high quality, but they're not blowing people away with how great they are.

<h3 id='s6q15'> I think that Haskell libraries perform well. </h3>
<p> Single select. Average: 3.74 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 139 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 51%;'> </div>
    <div class='percent'> 51% </div>
    <div class='count'> 620 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 28%;'> </div>
    <div class='percent'> 28% </div>
    <div class='count'> 345 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 30 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 9 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This looks about the same as last year.
Haskell libraries perform well, but not as well as Haskell itself.
To mean that means there's some room for improvement, but without more specific data it's hard to come up with an actionable takeaway.

<h3 id='s6q20'> Haskell is working well for my team. </h3>
<p> Single select. Average: 3.71 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 22%;'> </div>
    <div class='percent'> 22% </div>
    <div class='count'> 270 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 21%;'> </div>
    <div class='percent'> 21% </div>
    <div class='count'> 254 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 414 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 44 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 19 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

<h3 id='s6q5'> I can find Haskell libraries for the things that I need. </h3>
<p> Single select. Average: 3.58 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 123 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 49%;'> </div>
    <div class='percent'> 49% </div>
    <div class='count'> 590 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 25%;'> </div>
    <div class='percent'> 25% </div>
    <div class='count'> 299 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 127 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 20 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This is a little worse than the past two years.
That makes sense to me, since the lack of critical libraries is one of the main reasons why people aren't using Haskell at work.

<h3 id='s6q3'> I am satisfied with Haskell's build tools, such as Cabal. </h3>
<p> Single select. Average: 3.49 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 12%;'> </div>
    <div class='percent'> 12% </div>
    <div class='count'> 147 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 42%;'> </div>
    <div class='percent'> 42% </div>
    <div class='count'> 513 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 25%;'> </div>
    <div class='percent'> 25% </div>
    <div class='count'> 307 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 12%;'> </div>
    <div class='percent'> 12% </div>
    <div class='count'> 145 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 49 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This has improved a little over the previous years.
It could be that people are switching over to Nix and enjoying that.
Or it could be that people are enjoying the recent improvements to `cabal-install`.
Or maybe people are simply getting more comfortable with their existing workflows.

<h3 id='s6q12'> I think that Haskell libraries work well together. </h3>
<p> Single select. Average: 3.44 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 87 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 41%;'> </div>
    <div class='percent'> 41% </div>
    <div class='count'> 492 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 416 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 10%;'> </div>
    <div class='percent'> 10% </div>
    <div class='count'> 121 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 22 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This hasn't changed much compared to previous years.
I'd be curious to drill down into this one to see how Haskell libraries could work better together.
Is it a symptom of libraries that define data types also having to define type class instances?
Or is it related to libraries in the same domain not sharing a core set of data types?
I don't have enough information to say.

<h3 id='s6q11'> I think that Haskell libraries provide a stable API. </h3>
<p> Single select. Average: 3.44 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 67 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 42%;'> </div>
    <div class='percent'> 42% </div>
    <div class='count'> 506 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 37%;'> </div>
    <div class='percent'> 37% </div>
    <div class='count'> 444 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 98 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 21 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This is the same as the past two years.
Personally, I feel like most libraries are very stable, but there have been some high-profile instability in the past year, like `network` version 3.

<h3 id='s6q7'> I have a good understanding of Haskell best practices. </h3>
<p> Single select. Average: 3.20 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 94 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 31%;'> </div>
    <div class='percent'> 31% </div>
    <div class='count'> 381 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 412 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 18%;'> </div>
    <div class='percent'> 18% </div>
    <div class='count'> 220 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 56 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

Here's where a significant number of people start disagreeing.
For this question, the results look similar to last year.
Some people moved from "Disagree" to "Neutral", so maybe we're making some progress on this front.
Regardless, most Haskell users don't feel like they have a good understanding of best practices.

<h3 id='s6q10'> I think that Haskell libraries are easy to use. </h3>
<p> Single select. Average: 3.11 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 41 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 28%;'> </div>
    <div class='percent'> 28% </div>
    <div class='count'> 344 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 42%;'> </div>
    <div class='percent'> 42% </div>
    <div class='count'> 507 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 17%;'> </div>
    <div class='percent'> 17% </div>
    <div class='count'> 208 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 47 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This looks the same as previous years.
If I had to guess, I'd say that the lack of good documentation is what makes Haskell libraries hard to use.
Or at least better documentation could make libraries easier to use.

<h3 id='s6q21'> Haskell is critical to my company's success. </h3>
<p> Single select. Average: 3.08 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 15%;'> </div>
    <div class='percent'> 15% </div>
    <div class='count'> 177 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 13%;'> </div>
    <div class='percent'> 13% </div>
    <div class='count'> 154 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 31%;'> </div>
    <div class='percent'> 31% </div>
    <div class='count'> 375 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 131 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 12%;'> </div>
    <div class='percent'> 12% </div>
    <div class='count'> 150 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This distribution is similar to last year.
I'm a little confused by the results though.
When I wrote the question, I had assumed that strongly disagreeing with this would mean that Haskell is actively harmful to your company's success.
I think that's not how people interpreted this question.
Next year I will try to make this clearer.

<h3 id='s6q23'> As a hiring manager, I can easily find qualified Haskell candidates. </h3>
<p> Single select. Average: 2.91 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 43 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 93 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 42%;'> </div>
    <div class='percent'> 42% </div>
    <div class='count'> 509 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 95 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 80 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This is actually looking better than last year.
I think many people answered "Neutral" because they aren't a hiring manager, which kind of skews the results.
Regardless, things are looking up!
It's slightly easier this year to hire Haskell developers than last year.

<h3 id='s6q8'> I think Haskell libraries are well documented. </h3>
<p> Single select. Average: 2.86 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 39 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 23%;'> </div>
    <div class='percent'> 23% </div>
    <div class='count'> 280 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 36%;'> </div>
    <div class='percent'> 36% </div>
    <div class='count'> 432 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 24%;'> </div>
    <div class='percent'> 24% </div>
    <div class='count'> 292 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 115 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

Here's where we get into the questions where more people disagree than agree.
Broadly speaking this one looks the same as the last two years, but there has been a slight improvement.
This is frustrating to see because Haskell has a reputation for not having great documentation, but not much improvement is being made in the area.
I wonder if there's a way for us as a community to encourage people to write documentation?

<h3 id='s6q17'> I can easily reason about the performance of my Haskell code. </h3>
<p> Single select. Average: 2.75 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 41 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 18%;'> </div>
    <div class='percent'> 18% </div>
    <div class='count'> 217 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 34%;'> </div>
    <div class='percent'> 34% </div>
    <div class='count'> 408 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 30%;'> </div>
    <div class='percent'> 30% </div>
    <div class='count'> 369 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 109 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

Another evergreen Haskell problem with not much movement over the years.
Haskell's laziness by default means that any performance reasoning you've got from other languages probably doesn't apply, since those languages are most likely strict.
However I think it's important to note that in general people are happy with the performance of their Haskell programs, even if they may not easily be able to reason about it.

<h3 id='s6q9'> I can easily compare competing Haskell libraries to select the best one. </h3>
<p> Single select. Average: 2.69 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 30 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 19%;'> </div>
    <div class='percent'> 19% </div>
    <div class='count'> 232 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 32%;'> </div>
    <div class='percent'> 32% </div>
    <div class='count'> 390 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 30%;'> </div>
    <div class='percent'> 30% </div>
    <div class='count'> 360 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 12%;'> </div>
    <div class='percent'> 12% </div>
    <div class='count'> 143 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

This has improved a little, but it's still a sore spot.
It's hard to select between competing Haskell libraries, if you're even aware of the alternatives in the first place.
I'm encouraged by some new libraries explicitly comparing themselves to their competitors, but that doesn't seem like something that every library is going to do.

<h3 id='s6q22'> As a candidate, I can easily find Haskell jobs. </h3>
<p> Single select. Average: 2.48 </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 33 </div>
    <div class='choice'>  Strongly agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 130 </div>
    <div class='choice'> Agree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 30%;'> </div>
    <div class='percent'> 30% </div>
    <div class='count'> 366 </div>
    <div class='choice'> Neutral </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 25%;'> </div>
    <div class='percent'> 25% </div>
    <div class='count'> 298 </div>
    <div class='choice'> Disagree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 18%;'> </div>
    <div class='percent'> 18% </div>
    <div class='count'> 219 </div>
    <div class='choice'> Strongly disagree </div>
  </div>
</div>

And finally here we have the prompt that Haskell users feel the worst about:
Being able to find a job using Haskell.
I would love to be able to break this down even further.
My guess is that this is primarily influenced by two things:
A general lack of Haskell jobs, and the Haskell jobs that are available requiring a high level of expertise.
There aren't many entry-level Haskell jobs available.

<h2 id='s7'> Demographics </h2>

<h3 id='s7q0'> Which country do you live in? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'> <div class='bar' style='width: 26%;'> </div> <div class='percent'> 26% </div> <div class='count'> 316 </div> <div class='choice'> United States </div> </div>
  <div class='row'> <div class='bar' style='width: 10%;'> </div> <div class='percent'> 10% </div> <div class='count'> 126 </div> <div class='choice'> United Kingdom </div> </div>
  <div class='row'> <div class='bar' style='width: 7%;'> </div> <div class='percent'> 7% </div> <div class='count'> 88 </div> <div class='choice'> Germany </div> </div>
  <div class='row'> <div class='bar' style='width: 4%;'> </div> <div class='percent'> 4% </div> <div class='count'> 54 </div> <div class='choice'> France </div> </div>
  <div class='row'> <div class='bar' style='width: 4%;'> </div> <div class='percent'> 4% </div> <div class='count'> 48 </div> <div class='choice'> Canada </div> </div>
  <div class='row'> <div class='bar' style='width: 4%;'> </div> <div class='percent'> 4% </div> <div class='count'> 45 </div> <div class='choice'> Australia </div> </div>
  <div class='row'> <div class='bar' style='width: 4%;'> </div> <div class='percent'> 4% </div> <div class='count'> 44 </div> <div class='choice'> Russia </div> </div>
  <div class='row'> <div class='bar' style='width: 3%;'> </div> <div class='percent'> 3% </div> <div class='count'> 40 </div> <div class='choice'> Netherlands </div> </div>
  <div class='row'> <div class='bar' style='width: 2%;'> </div> <div class='percent'> 2% </div> <div class='count'> 28 </div> <div class='choice'> Poland </div> </div>
  <div class='row'> <div class='bar' style='width: 2%;'> </div> <div class='percent'> 2% </div> <div class='count'> 27 </div> <div class='choice'> Sweden </div> </div>
  <div class='row'> <div class='bar' style='width: 2%;'> </div> <div class='percent'> 2% </div> <div class='count'> 26 </div> <div class='choice'> Japan </div> </div>
  <div class='row'> <div class='bar' style='width: 2%;'> </div> <div class='percent'> 2% </div> <div class='count'> 20 </div> <div class='choice'> Switzerland </div> </div>
  <div class='row'> <div class='bar' style='width: 2%;'> </div> <div class='percent'> 2% </div> <div class='count'> 20 </div> <div class='choice'> Italy </div> </div>
  <div class='row'> <div class='bar' style='width: 2%;'> </div> <div class='percent'> 2% </div> <div class='count'> 19 </div> <div class='choice'> India </div> </div>
  <div class='row'> <div class='bar' style='width: 2%;'> </div> <div class='percent'> 2% </div> <div class='count'> 19 </div> <div class='choice'> Brazil </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 16 </div> <div class='choice'> Norway </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 15 </div> <div class='choice'> Finland </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 14 </div> <div class='choice'> Ukraine </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 14 </div> <div class='choice'> Spain </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 14 </div> <div class='choice'> Austria </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 11 </div> <div class='choice'> Belgium </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 10 </div> <div class='choice'> Denmark </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 10 </div> <div class='choice'> China </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 9 </div> <div class='choice'> Turkey </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 9 </div> <div class='choice'> Singapore </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 9 </div> <div class='choice'> New Zealand </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 9 </div> <div class='choice'> Ireland </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 9 </div> <div class='choice'> Czech Republic </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 8 </div> <div class='choice'> Romania </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 8 </div> <div class='choice'> Argentina </div> </div>
  <div class='row'> <div class='bar' style='width: 1%;'> </div> <div class='percent'> 1% </div> <div class='count'> 7 </div> <div class='choice'> Greece </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 6 </div> <div class='choice'> Israel </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 6 </div> <div class='choice'> Bulgaria </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 5 </div> <div class='choice'> South Africa </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 4 </div> <div class='choice'> South Korea </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 4 </div> <div class='choice'> Portugal </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 4 </div> <div class='choice'> Croatia </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 4 </div> <div class='choice'> Colombia </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 4 </div> <div class='choice'> Chile </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 3 </div> <div class='choice'> Mexico </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 3 </div> <div class='choice'> Ecuador </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 2 </div> <div class='choice'> Vietnam </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 2 </div> <div class='choice'> Philippines </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 2 </div> <div class='choice'> Paraguay </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 2 </div> <div class='choice'> Nigeria </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 2 </div> <div class='choice'> Malaysia </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 2 </div> <div class='choice'> Estonia </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 2 </div> <div class='choice'> Belarus </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Zimbabwe </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Yemen </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Uruguay </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Uganda </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Thailand </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Slovakia </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Saudi Arabia </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Puerto Rico </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Pakistan </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Other </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> North Korea </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Morocco </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Luxembourg </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Lithuania </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Latvia </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Kenya </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Iran </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Hungary </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Haiti </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Dominican Republic </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Costa Rica </div> </div>
  <div class='row'> <div class='bar' style='width: 0%;'> </div> <div class='percent'> 0% </div> <div class='count'> 1 </div> <div class='choice'> Cambodia </div> </div>
</div>

<h3 id='s7q1'> How old are you? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 6 </div>
    <div class='choice'> Under 18 years old </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 195 </div>
    <div class='choice'> 18 to 24 years old </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 46%;'> </div>
    <div class='percent'> 46% </div>
    <div class='count'> 552 </div>
    <div class='choice'> 25 to 34 years old </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 24%;'> </div>
    <div class='percent'> 24% </div>
    <div class='count'> 292 </div>
    <div class='choice'> 35 to 44 years old </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 85 </div>
    <div class='choice'> 45 to 54 years old </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 26 </div>
    <div class='choice'> 55 to 64 years old </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 4 </div>
    <div class='choice'> Over 65 years old </div>
  </div>
</div>

<h3 id='s7q2'> What is your gender? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 91%;'> </div>
    <div class='percent'> 91% </div>
    <div class='count'> 1100 </div>
    <div class='choice'> Male </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 32 </div>
    <div class='choice'> Female </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 28 </div>
    <div class='choice'> Non-binary </div>
  </div>
</div>

<h3 id='s7q3'> Do you identify as transgender? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 91%;'> </div>
    <div class='percent'> 91% </div>
    <div class='count'> 1106 </div>
    <div class='choice'> No </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 28 </div>
    <div class='choice'> Yes </div>
  </div>
</div>

<h3 id='s7q4'> Are you a student? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 81%;'> </div>
    <div class='percent'> 81% </div>
    <div class='count'> 986 </div>
    <div class='choice'> No </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 131 </div>
    <div class='choice'> Yes, full time </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 4%;'> </div>
    <div class='percent'> 4% </div>
    <div class='count'> 53 </div>
    <div class='choice'> Yes, part time </div>
  </div>
</div>

<h3 id='s7q5'> What is the highest level of education you have completed? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 11 </div>
    <div class='choice'> Less than high school diploma </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 87 </div>
    <div class='choice'> High school diploma </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 134 </div>
    <div class='choice'> Some college </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 20 </div>
    <div class='choice'> Associate degree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 35%;'> </div>
    <div class='percent'> 35% </div>
    <div class='count'> 424 </div>
    <div class='choice'> Bachelor's degree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 26%;'> </div>
    <div class='percent'> 26% </div>
    <div class='count'> 312 </div>
    <div class='choice'> Master's degree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 23 </div>
    <div class='choice'> Professional degree </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 13%;'> </div>
    <div class='percent'> 13% </div>
    <div class='count'> 155 </div>
    <div class='choice'> Doctoral degree </div>
  </div>
</div>

<h3 id='s7q6'> What is your employment status? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 72%;'> </div>
    <div class='percent'> 72% </div>
    <div class='count'> 867 </div>
    <div class='choice'> Employed full time </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 87 </div>
    <div class='choice'> Employed part time </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 94 </div>
    <div class='choice'> Self employed </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 70 </div>
    <div class='choice'> Not employed, but looking for work </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 39 </div>
    <div class='choice'> Not employed, and not looking for work </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 7 </div>
    <div class='choice'> Retired </div>
  </div>
</div>

<h3 id='s7q7'> How large is the company you work for? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 13%;'> </div>
    <div class='percent'> 13% </div>
    <div class='count'> 163 </div>
    <div class='choice'> Fewer than 10 employees </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 32%;'> </div>
    <div class='percent'> 32% </div>
    <div class='count'> 390 </div>
    <div class='choice'> 10 to 99 employees </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 16%;'> </div>
    <div class='percent'> 16% </div>
    <div class='count'> 196 </div>
    <div class='choice'> 100 to 999 employees </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 23%;'> </div>
    <div class='percent'> 23% </div>
    <div class='count'> 275 </div>
    <div class='choice'> More than 1,000 employees </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 78 </div>
    <div class='choice'> Other </div>
  </div>
</div>

<h3 id='s7q8'> How many years have you been coding? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 91 </div>
    <div class='choice'> 0 to 4 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 25%;'> </div>
    <div class='percent'> 25% </div>
    <div class='count'> 307 </div>
    <div class='choice'> 5 to 9 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 22%;'> </div>
    <div class='percent'> 22% </div>
    <div class='count'> 268 </div>
    <div class='choice'> 10 to 14 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 15%;'> </div>
    <div class='percent'> 15% </div>
    <div class='count'> 184 </div>
    <div class='choice'> 15 to 19 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 11%;'> </div>
    <div class='percent'> 11% </div>
    <div class='count'> 133 </div>
    <div class='choice'> 20 to 24 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 80 </div>
    <div class='choice'> 25 to 29 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 8%;'> </div>
    <div class='percent'> 8% </div>
    <div class='count'> 102 </div>
    <div class='choice'> 30 or more years </div>
  </div>
</div>

<h3 id='s7q9'> How many years have you been coding professionally? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 32%;'> </div>
    <div class='percent'> 32% </div>
    <div class='count'> 383 </div>
    <div class='choice'> 0 to 4 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 29%;'> </div>
    <div class='percent'> 29% </div>
    <div class='count'> 346 </div>
    <div class='choice'> 5 to 9 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 14%;'> </div>
    <div class='percent'> 14% </div>
    <div class='count'> 175 </div>
    <div class='choice'> 10 to 14 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 9%;'> </div>
    <div class='percent'> 9% </div>
    <div class='count'> 111 </div>
    <div class='choice'> 15 to 19 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 75 </div>
    <div class='choice'> 20 to 24 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 31 </div>
    <div class='choice'> 25 to 29 years </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 33 </div>
    <div class='choice'> 30 or more years </div>
  </div>
</div>

<h3 id='s7q10'> Do you code as a hobby? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 92%;'> </div>
    <div class='percent'> 92% </div>
    <div class='count'> 1110 </div>
    <div class='choice'> Yes </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 5%;'> </div>
    <div class='percent'> 5% </div>
    <div class='count'> 65 </div>
    <div class='choice'> No </div>
  </div>
</div>

<h3 id='s7q11'> Have you contributed to any open source projects? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 79%;'> </div>
    <div class='percent'> 79% </div>
    <div class='count'> 959 </div>
    <div class='choice'> Yes </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 18%;'> </div>
    <div class='percent'> 18% </div>
    <div class='count'> 216 </div>
    <div class='choice'> No </div>
  </div>
</div>

<h2 id='s8'> Meta </h2>

<h3 id='s8q0'> Did you take any previous surveys? </h3>
<p> Multiple select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 44%;'> </div>
    <div class='percent'> 44% </div>
    <div class='count'> 538 </div>
    <div class='choice'> 2018 </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 24%;'> </div>
    <div class='percent'> 24% </div>
    <div class='count'> 285 </div>
    <div class='choice'> 2017 </div>
  </div>
</div>

<h3 id='s8q1'> How did you hear about this survey? </h3>
<p> Single select. </p>
<div class='answer'>
  <div class='row'>
    <div class='bar' style='width: 31%;'> </div>
    <div class='percent'> 31% </div>
    <div class='count'> 376 </div>
    <div class='choice'> Reddit </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 20%;'> </div>
    <div class='percent'> 20% </div>
    <div class='count'> 248 </div>
    <div class='choice'> Twitter </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 17%;'> </div>
    <div class='percent'> 17% </div>
    <div class='count'> 203 </div>
    <div class='choice'> Haskell Weekly </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 79 </div>
    <div class='choice'> Lobsters </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 7%;'> </div>
    <div class='percent'> 7% </div>
    <div class='count'> 90 </div>
    <div class='choice'> Mailing lists </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 6%;'> </div>
    <div class='percent'> 6% </div>
    <div class='count'> 77 </div>
    <div class='choice'> Slack </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 2%;'> </div>
    <div class='percent'> 2% </div>
    <div class='count'> 28 </div>
    <div class='choice'> In person </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 1%;'> </div>
    <div class='percent'> 1% </div>
    <div class='count'> 11 </div>
    <div class='choice'> Telegram </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 4 </div>
    <div class='choice'> Discourse </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 4 </div>
    <div class='choice'> Mastodon </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 3 </div>
    <div class='choice'> Discord </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 3 </div>
    <div class='choice'> GitHub </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 3 </div>
    <div class='choice'> IRC </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 0%;'> </div>
    <div class='percent'> 0% </div>
    <div class='count'> 2 </div>
    <div class='choice'> Matrix/Riot </div>
  </div>
  <div class='row'>
    <div class='bar' style='width: 3%;'> </div>
    <div class='percent'> 3% </div>
    <div class='count'> 36 </div>
    <div class='choice'> Other </div>
  </div>
</div>

<h2 id='s9'> Free response </h2>

<h3 id='s9q0'> If you wanted to convince someone to use Haskell, what would you say? </h3>
<p> Optional. Free responses were collected but not analyzed. </p>

<h3 id='s9q1'> If you could change one thing about Haskell, what would it be? </h3>
<p> Optional. Free responses were collected but not analyzed. </p>
