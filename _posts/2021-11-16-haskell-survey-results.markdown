---
title: 2021 State of Haskell Survey Results
---

The fifth annual State of Haskell Survey closed this week.
This post will graph the responses, analyze them, and compare them to previous years.
You may be interested in similar posts from [2020][], [2019][], [2018][], and [2017][].

This year the survey received fewer responses than previous years.
Compared to last year, it's down by about 15%.

Year | Responses
---- | ---------
2021 | 1152
2020 | 1348
2019 | 1211
2018 | 1361
2017 | 1335

You can download the responses as a [JSON][] or [CSV][] file.
Both are licensed under the [ODbL][].

<style>
  .row { position: relative; }
  .row:hover { background: #cbc9e2; }
  .bar { height: 100%; left: 0; max-width: 100%; position: absolute; top: 0; }
  .purple { background: #9e9ac8; }
  .blue { background: #67a9cf; }
  .red { background: #ef8a62; }
  .percent, .count, .choice { display: inline-block; position: relative; }
  .percent, .count { text-align: right; width: 3em; }
  .choice { padding-left: 1em; }
</style>
<ol>
  <li>
    <a href="#s10">Survey</a>
    <ol>
      <li>
        <a href="#s10q0">When did you submit your survey response?</a>
      </li>
      <li>
        <a href="#s10q1">Did you provide an email address?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s0">Haskell usage</a>
    <ol>
      <li>
        <a href="#s0q1">Do you use Haskell?</a>
      </li>
      <li>
        <a href="#s0q2">If you stopped using Haskell, how long did you use it before you stopped?</a>
      </li>
      <li>
        <a href="#s0q3">If you do not use Haskell, why not?</a>
      </li>
      <li>
        <a href="#s0q4">How many years have you been using Haskell?</a>
      </li>
      <li>
        <a href="#s0q5">How frequently do you use Haskell?</a>
      </li>
      <li>
        <a href="#s0q6">How would you rate your proficiency in Haskell?</a>
      </li>
      <li>
        <a href="#s0q7">Where do you use Haskell?</a>
      </li>
      <li>
        <a href="#s0q8">Do you use Haskell at work?</a>
      </li>
      <li>
        <a href="#s0q9">If you do not use Haskell at work, why not?</a>
      </li>
      <li>
        <a href="#s0q10">Which programming languages other than Haskell are you fluent in?</a>
      </li>
      <li>
        <a href="#s0q11">Which types of software do you develop with Haskell?</a>
      </li>
      <li>
        <a href="#s0q12">Which industries do you use Haskell in?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s1">Projects</a>
    <ol>
      <li>
        <a href="#s1q0">How many Haskell projects do you contribute to?</a>
      </li>
      <li>
        <a href="#s1q1">What is the total size of all the Haskell projects you contribute to?</a>
      </li>
      <li>
        <a href="#s1q2">Which platforms do you develop Haskell on?</a>
      </li>
      <li>
        <a href="#s1q3">Which platforms do you target?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s2">Compilers</a>
    <ol>
      <li>
        <a href="#s2q0">Which Haskell compilers do you use?</a>
      </li>
      <li>
        <a href="#s2q1">Which installation methods do you use for your Haskell compiler?</a>
      </li>
      <li>
        <a href="#s2q2">Has upgrading your Haskell compiler broken your code in the last year?</a>
      </li>
      <li>
        <a href="#s2q3">How has upgrading your Haskell compiler broken your code in the last year?</a>
      </li>
      <li>
        <a href="#s2q4">Which versions of GHC do you use?</a>
      </li>
      <li>
        <a href="#s2q5">Which language extensions would you like to be enabled by default?</a>
      </li>
      <li>
        <a href="#s2q6">How important do you feel it would be to have a new version of the Haskell language standard?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s3">Tooling</a>
    <ol>
      <li>
        <a href="#s3q0">Which build tools do you use for Haskell?</a>
      </li>
      <li>
        <a href="#s3q1">Which editors do you use for Haskell?</a>
      </li>
      <li>
        <a href="#s3q2">Which IDEs do you use for Haskell?</a>
      </li>
      <li>
        <a href="#s3q3">Which version control systems do you use for Haskell?</a>
      </li>
      <li>
        <a href="#s3q4">Where do you get Haskell packages from?</a>
      </li>
      <li>
        <a href="#s3q5">Which tools do you use to test Haskell code?</a>
      </li>
      <li>
        <a href="#s3q6">Which tools do you use to benchmark Haskell code?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s4">Infrastructure</a>
    <ol>
      <li>
        <a href="#s4q0">Which tools do you use to deploy Haskell applications?</a>
      </li>
      <li>
        <a href="#s4q1">Where do you deploy Haskell applications?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s5">Community</a>
    <ol>
      <li>
        <a href="#s5q0">Where do you interact with the Haskell community?</a>
      </li>
      <li>
        <a href="#s5q1">Which of the following Haskell topics would you like to see more written about?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s6">Feelings</a>
    <ol>
      <li>
        <a href="#s6q19">I would prefer to use Haskell for my next new project.</a>
      </li>
      <li>
        <a href="#s6q18">I would recommend using Haskell to others.</a>
      </li>
      <li>
        <a href="#s6q1">I am satisfied with Haskell as a language.</a>
      </li>
      <li>
        <a href="#s6q14">Once my Haskell program compiles, it generally does what I intended.</a>
      </li>
      <li>
        <a href="#s6q13">I think that software written in Haskell is easy to maintain.</a>
      </li>
      <li>
        <a href="#s6q2">I am satisfied with Haskell's compilers, such as GHC.</a>
      </li>
      <li>
        <a href="#s6q0">I feel welcome in the Haskell community.</a>
      </li>
      <li>
        <a href="#s6q16">Haskell's performance meets my needs.</a>
      </li>
      <li>
        <a href="#s6q4">I am satisfied with Haskell's package repositories, such as Hackage.</a>
      </li>
      <li>
        <a href="#s6q6">I think Haskell libraries are high quality.</a>
      </li>
      <li>
        <a href="#s6q15">I think that Haskell libraries perform well.</a>
      </li>
      <li>
        <a href="#s6q20">Haskell is working well for my team.</a>
      </li>
      <li>
        <a href="#s6q5">I can find Haskell libraries for the things that I need.</a>
      </li>
      <li>
        <a href="#s6q12">I think that Haskell libraries work well together.</a>
      </li>
      <li>
        <a href="#s6q11">I think that Haskell libraries provide a stable API.</a>
      </li>
      <li>
        <a href="#s6q3">I am satisfied with Haskell's build tools, such as Cabal.</a>
      </li>
      <li>
        <a href="#s6q21">Haskell is critical to my company's success.</a>
      </li>
      <li>
        <a href="#s6q7">I have a good understanding of Haskell best practices.</a>
      </li>
      <li>
        <a href="#s6q10">I think that Haskell libraries are easy to use.</a>
      </li>
      <li>
        <a href="#s6q23">As a hiring manager, I can easily find qualified Haskell candidates.</a>
      </li>
      <li>
        <a href="#s6q8">I think Haskell libraries are well documented.</a>
      </li>
      <li>
        <a href="#s6q9">I can easily compare competing Haskell libraries to select the best one.</a>
      </li>
      <li>
        <a href="#s6q22">As a candidate, I can easily find Haskell jobs.</a>
      </li>
      <li>
        <a href="#s6q17">I can easily reason about the performance of my Haskell code.</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s7">Demographics</a>
    <ol>
      <li>
        <a href="#s7q0">Which country do you live in?</a>
      </li>
      <li>
        <a href="#s7q1">Do you consider yourself a member of an underrepresented or marginalized group in technology?</a>
      </li>
      <li>
        <a href="#s7q2">Do you feel your belonging to an underrepresented or marginalized group in technology makes it difficult for you to participate in the Haskell community?</a>
      </li>
      <li>
        <a href="#s7q3">Are you a student?</a>
      </li>
      <li>
        <a href="#s7q4">What is the highest level of education you have completed?</a>
      </li>
      <li>
        <a href="#s7q5">What is your employment status?</a>
      </li>
      <li>
        <a href="#s7q6">How large is the company you work for?</a>
      </li>
      <li>
        <a href="#s7q7">How many years have you been coding?</a>
      </li>
      <li>
        <a href="#s7q8">How many years have you been coding professionally?</a>
      </li>
      <li>
        <a href="#s7q9">Do you code as a hobby?</a>
      </li>
      <li>
        <a href="#s7q10">Have you contributed to any open source projects?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s8">Meta</a>
    <ol>
      <li>
        <a href="#s8q0">Did you take any previous surveys?</a>
      </li>
      <li>
        <a href="#s8q1">How did you hear about this survey?</a>
      </li>
    </ol>
  </li>
  <li>
    <a href="#s9">Free response</a>
    <ol>
      <li>
        <a href="#s9q0">If you wanted to convince someone to use Haskell, what would you say?</a>
      </li>
      <li>
        <a href="#s9q1">If you could change one thing about Haskell, what would it be?</a>
      </li>
    </ol>
  </li>
</ol>
<h2 id="s10">Survey</h2>
<h3 id="s10q0">When did you respond to the survey?</h3>
<div class="answer">
  <div class="row">
    <div style="width: 28.30%;" class="bar purple"/>
    <div class="percent">28%</div>
    <div class="count">326</div>
    <div class="choice">2021-11-01</div>
  </div>
  <div class="row">
    <div style="width: 30.38%;" class="bar purple"/>
    <div class="percent">30%</div>
    <div class="count">350</div>
    <div class="choice">2021-11-02</div>
  </div>
  <div class="row">
    <div style="width: 6.08%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">70</div>
    <div class="choice">2021-11-03</div>
  </div>
  <div class="row">
    <div style="width: 6.16%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">71</div>
    <div class="choice">2021-11-04</div>
  </div>
  <div class="row">
    <div style="width: 3.56%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">41</div>
    <div class="choice">2021-11-05</div>
  </div>
  <div class="row">
    <div style="width: 2.17%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">25</div>
    <div class="choice">2021-11-06</div>
  </div>
  <div class="row">
    <div style="width: 1.56%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">18</div>
    <div class="choice">2021-11-07</div>
  </div>
  <div class="row">
    <div style="width: 4.69%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">54</div>
    <div class="choice">2021-11-08</div>
  </div>
  <div class="row">
    <div style="width: 2.78%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">32</div>
    <div class="choice">2021-11-09</div>
  </div>
  <div class="row">
    <div style="width: 3.21%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">37</div>
    <div class="choice">2021-11-10</div>
  </div>
  <div class="row">
    <div style="width: 2.60%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">30</div>
    <div class="choice">2021-11-11</div>
  </div>
  <div class="row">
    <div style="width: 2.34%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">27</div>
    <div class="choice">2021-11-12</div>
  </div>
  <div class="row">
    <div style="width: 1.39%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">16</div>
    <div class="choice">2021-11-13</div>
  </div>
  <div class="row">
    <div style="width: 3.04%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">35</div>
    <div class="choice">2021-11-14</div>
  </div>
  <div class="row">
    <div style="width: 1.74%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">20</div>
    <div class="choice">2021-11-15</div>
  </div>
</div>
<h3 id="s10q1">Did you provide an email address?</h3>
<div class="answer">
  <div class="row">
    <div style="width: 54.08%;" class="bar purple"/>
    <div class="percent">54%</div>
    <div class="count">623</div>
    <div class="choice">No</div>
  </div>
  <div class="row">
    <div style="width: 45.92%;" class="bar purple"/>
    <div class="percent">46%</div>
    <div class="count">529</div>
    <div class="choice">Yes</div>
  </div>
</div>
<h2 id="s0">Haskell usage</h2>
<h3 id="s0q1">Do you use Haskell?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 85.59%;" class="bar purple"/>
    <div class="percent">86%</div>
    <div class="count">986</div>
    <div class="choice">Yes</div>
  </div>
  <div class="row">
    <div style="width: 10.59%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">122</div>
    <div class="choice">No, but I used to</div>
  </div>
  <div class="row">
    <div style="width: 2.86%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">33</div>
    <div class="choice">No, I never have</div>
  </div>
</div>
<h3 id="s0q2">If you stopped using Haskell, how long did you use it before you stopped?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 0.43%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">5</div>
    <div class="choice">Less than 1 day</div>
  </div>
  <div class="row">
    <div style="width: 0.78%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">9</div>
    <div class="choice">1 day to 1 week</div>
  </div>
  <div class="row">
    <div style="width: 2.52%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">29</div>
    <div class="choice">1 week to 1 month</div>
  </div>
  <div class="row">
    <div style="width: 4.60%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">53</div>
    <div class="choice">1 month to 1 year</div>
  </div>
  <div class="row">
    <div style="width: 6.25%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">72</div>
    <div class="choice">More than 1 year</div>
  </div>
</div>
<h3 id="s0q3">If you do not use Haskell, why not?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 11.37%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">131</div>
    <div class="choice">My company doesn't use Haskell</div>
  </div>
  <div class="row">
    <div style="width: 4.43%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">51</div>
    <div class="choice">Haskell's documentation is not good enough</div>
  </div>
  <div class="row">
    <div style="width: 3.99%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">46</div>
    <div class="choice">Haskell is too hard to learn</div>
  </div>
  <div class="row">
    <div style="width: 3.82%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">44</div>
    <div class="choice">Haskell lacks critical libraries</div>
  </div>
  <div class="row">
    <div style="width: 3.04%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">35</div>
    <div class="choice">Haskell lacks critical tools</div>
  </div>
  <div class="row">
    <div style="width: 2.00%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">23</div>
    <div class="choice">Haskell's performance is not good enough</div>
  </div>
  <div class="row">
    <div style="width: 1.30%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">15</div>
    <div class="choice">Haskell lacks critical features</div>
  </div>
  <div class="row">
    <div style="width: 1.22%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">14</div>
    <div class="choice">Haskell does not support the platforms I need</div>
  </div>
  <div class="row">
    <div style="width: 3.91%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">45</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s0q4">How many years have you been using Haskell?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 13.63%;" class="bar purple"/>
    <div class="percent">14%</div>
    <div class="count">157</div>
    <div class="choice">Less than 1</div>
  </div>
  <div class="row">
    <div style="width: 13.37%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">154</div>
    <div class="choice">1 to 2</div>
  </div>
  <div class="row">
    <div style="width: 11.02%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">127</div>
    <div class="choice">2 to 3</div>
  </div>
  <div class="row">
    <div style="width: 10.50%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">121</div>
    <div class="choice">3 to 4</div>
  </div>
  <div class="row">
    <div style="width: 9.38%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">108</div>
    <div class="choice">4 to 5</div>
  </div>
  <div class="row">
    <div style="width: 8.33%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">96</div>
    <div class="choice">5 to 6</div>
  </div>
  <div class="row">
    <div style="width: 4.86%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">56</div>
    <div class="choice">6 to 7</div>
  </div>
  <div class="row">
    <div style="width: 4.69%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">54</div>
    <div class="choice">7 to 8</div>
  </div>
  <div class="row">
    <div style="width: 2.69%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">31</div>
    <div class="choice">8 to 9</div>
  </div>
  <div class="row">
    <div style="width: 4.60%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">53</div>
    <div class="choice">9 to 10</div>
  </div>
  <div class="row">
    <div style="width: 4.25%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">49</div>
    <div class="choice">10 to 11</div>
  </div>
  <div class="row">
    <div style="width: 1.56%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">18</div>
    <div class="choice">11 to 12</div>
  </div>
  <div class="row">
    <div style="width: 2.26%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">26</div>
    <div class="choice">12 to 13</div>
  </div>
  <div class="row">
    <div style="width: 0.95%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">11</div>
    <div class="choice">13 to 14</div>
  </div>
  <div class="row">
    <div style="width: 0.87%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">10</div>
    <div class="choice">14 to 15</div>
  </div>
  <div class="row">
    <div style="width: 3.73%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">43</div>
    <div class="choice">More than 15</div>
  </div>
</div>
<h3 id="s0q5">How frequently do you use Haskell?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 40.80%;" class="bar purple"/>
    <div class="percent">41%</div>
    <div class="count">470</div>
    <div class="choice">Daily</div>
  </div>
  <div class="row">
    <div style="width: 25.78%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">297</div>
    <div class="choice">Weekly</div>
  </div>
  <div class="row">
    <div style="width: 15.62%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">180</div>
    <div class="choice">Monthly</div>
  </div>
  <div class="row">
    <div style="width: 4.17%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">48</div>
    <div class="choice">Yearly</div>
  </div>
  <div class="row">
    <div style="width: 8.42%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">97</div>
    <div class="choice">Rarely</div>
  </div>
</div>
<h3 id="s0q6">How would you rate your proficiency in Haskell?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 1.04%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">12</div>
    <div class="choice">I can't write or read Haskell</div>
  </div>
  <div class="row">
    <div style="width: 18.58%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">214</div>
    <div class="choice">I can write simple programs in Haskell</div>
  </div>
  <div class="row">
    <div style="width: 23.52%;" class="bar purple"/>
    <div class="percent">24%</div>
    <div class="count">271</div>
    <div class="choice">I can write useful, production-ready code but it is a struggle</div>
  </div>
  <div class="row">
    <div style="width: 44.01%;" class="bar purple"/>
    <div class="percent">44%</div>
    <div class="count">507</div>
    <div class="choice">I am productive writing Haskell</div>
  </div>
  <div class="row">
    <div style="width: 11.72%;" class="bar purple"/>
    <div class="percent">12%</div>
    <div class="count">135</div>
    <div class="choice">I'm an expert</div>
  </div>
</div>
<h3 id="s0q7">Where do you use Haskell?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 78.65%;" class="bar purple"/>
    <div class="percent">79%</div>
    <div class="count">906</div>
    <div class="choice">Home</div>
  </div>
  <div class="row">
    <div style="width: 49.83%;" class="bar purple"/>
    <div class="percent">50%</div>
    <div class="count">574</div>
    <div class="choice">Industry</div>
  </div>
  <div class="row">
    <div style="width: 17.27%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">199</div>
    <div class="choice">Academia</div>
  </div>
  <div class="row">
    <div style="width: 7.20%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">83</div>
    <div class="choice">School</div>
  </div>
</div>
<h3 id="s0q8">Do you use Haskell at work?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 31.51%;" class="bar purple"/>
    <div class="percent">32%</div>
    <div class="count">363</div>
    <div class="choice">Yes, most of the time</div>
  </div>
  <div class="row">
    <div style="width: 18.40%;" class="bar purple"/>
    <div class="percent">18%</div>
    <div class="count">212</div>
    <div class="choice">Yes, some of the time</div>
  </div>
  <div class="row">
    <div style="width: 0.95%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">11</div>
    <div class="choice">No, but my company does</div>
  </div>
  <div class="row">
    <div style="width: 36.11%;" class="bar purple"/>
    <div class="percent">36%</div>
    <div class="count">416</div>
    <div class="choice">No, but I'd like to</div>
  </div>
  <div class="row">
    <div style="width: 3.21%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">37</div>
    <div class="choice">No, and I don't want to</div>
  </div>
</div>
<h3 id="s0q9">If you do not use Haskell at work, why not?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 31.86%;" class="bar purple"/>
    <div class="percent">32%</div>
    <div class="count">367</div>
    <div class="choice">My company doesn't use Haskell</div>
  </div>
  <div class="row">
    <div style="width: 10.42%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">120</div>
    <div class="choice">It's too hard to hire Haskell developers</div>
  </div>
  <div class="row">
    <div style="width: 7.03%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">81</div>
    <div class="choice">Haskell is too hard to learn</div>
  </div>
  <div class="row">
    <div style="width: 6.60%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">76</div>
    <div class="choice">Haskell lacks critical libraries</div>
  </div>
  <div class="row">
    <div style="width: 5.99%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">69</div>
    <div class="choice">Haskell's documentation is not good enough</div>
  </div>
  <div class="row">
    <div style="width: 4.69%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">54</div>
    <div class="choice">Haskell lacks critical tools</div>
  </div>
  <div class="row">
    <div style="width: 3.73%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">43</div>
    <div class="choice">Haskell does not support the platforms I need</div>
  </div>
  <div class="row">
    <div style="width: 2.43%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">28</div>
    <div class="choice">Haskell's performance is not good enough</div>
  </div>
  <div class="row">
    <div style="width: 1.82%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">21</div>
    <div class="choice">Haskell lacks critical features</div>
  </div>
  <div class="row">
    <div style="width: 5.12%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">59</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s0q10">Which programming languages other than Haskell are you fluent in?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 52.86%;" class="bar purple"/>
    <div class="percent">53%</div>
    <div class="count">609</div>
    <div class="choice">Python</div>
  </div>
  <div class="row">
    <div style="width: 47.48%;" class="bar purple"/>
    <div class="percent">47%</div>
    <div class="count">547</div>
    <div class="choice">JavaScript</div>
  </div>
  <div class="row">
    <div style="width: 44.10%;" class="bar purple"/>
    <div class="percent">44%</div>
    <div class="count">508</div>
    <div class="choice">C</div>
  </div>
  <div class="row">
    <div style="width: 41.23%;" class="bar purple"/>
    <div class="percent">41%</div>
    <div class="count">475</div>
    <div class="choice">Java</div>
  </div>
  <div class="row">
    <div style="width: 29.25%;" class="bar purple"/>
    <div class="percent">29%</div>
    <div class="count">337</div>
    <div class="choice">C++</div>
  </div>
  <div class="row">
    <div style="width: 26.30%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">303</div>
    <div class="choice">Shell</div>
  </div>
  <div class="row">
    <div style="width: 26.30%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">303</div>
    <div class="choice">TypeScript</div>
  </div>
  <div class="row">
    <div style="width: 20.49%;" class="bar purple"/>
    <div class="percent">20%</div>
    <div class="count">236</div>
    <div class="choice">Rust</div>
  </div>
  <div class="row">
    <div style="width: 16.67%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">192</div>
    <div class="choice">Elm</div>
  </div>
  <div class="row">
    <div style="width: 16.32%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">188</div>
    <div class="choice">C#</div>
  </div>
  <div class="row">
    <div style="width: 14.76%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">170</div>
    <div class="choice">Scala</div>
  </div>
  <div class="row">
    <div style="width: 12.76%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">147</div>
    <div class="choice">PureScript</div>
  </div>
  <div class="row">
    <div style="width: 12.59%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">145</div>
    <div class="choice">Ruby</div>
  </div>
  <div class="row">
    <div style="width: 11.89%;" class="bar purple"/>
    <div class="percent">12%</div>
    <div class="count">137</div>
    <div class="choice">Go</div>
  </div>
  <div class="row">
    <div style="width: 10.50%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">121</div>
    <div class="choice">PHP</div>
  </div>
  <div class="row">
    <div style="width: 7.29%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">84</div>
    <div class="choice">Clojure</div>
  </div>
  <div class="row">
    <div style="width: 7.12%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">82</div>
    <div class="choice">Lua</div>
  </div>
  <div class="row">
    <div style="width: 7.12%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">82</div>
    <div class="choice">Ocaml</div>
  </div>
  <div class="row">
    <div style="width: 6.77%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">78</div>
    <div class="choice">Assembly</div>
  </div>
  <div class="row">
    <div style="width: 6.77%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">78</div>
    <div class="choice">Kotlin</div>
  </div>
  <div class="row">
    <div style="width: 5.64%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">65</div>
    <div class="choice">Perl</div>
  </div>
  <div class="row">
    <div style="width: 5.47%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">63</div>
    <div class="choice">F#</div>
  </div>
  <div class="row">
    <div style="width: 4.60%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">53</div>
    <div class="choice">Erlang</div>
  </div>
  <div class="row">
    <div style="width: 4.25%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">49</div>
    <div class="choice">R</div>
  </div>
  <div class="row">
    <div style="width: 4.17%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">48</div>
    <div class="choice">Matlab</div>
  </div>
  <div class="row">
    <div style="width: 4.08%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">47</div>
    <div class="choice">Swift</div>
  </div>
  <div class="row">
    <div style="width: 10.33%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">119</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s0q11">Which types of software do you develop with Haskell?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 62.50%;" class="bar purple"/>
    <div class="percent">62%</div>
    <div class="count">720</div>
    <div class="choice">Command-line programs (CLI)</div>
  </div>
  <div class="row">
    <div style="width: 46.61%;" class="bar purple"/>
    <div class="percent">47%</div>
    <div class="count">537</div>
    <div class="choice">API services (returning non-HTML)</div>
  </div>
  <div class="row">
    <div style="width: 35.16%;" class="bar purple"/>
    <div class="percent">35%</div>
    <div class="count">405</div>
    <div class="choice">Libraries or frameworks</div>
  </div>
  <div class="row">
    <div style="width: 33.33%;" class="bar purple"/>
    <div class="percent">33%</div>
    <div class="count">384</div>
    <div class="choice">Data processing</div>
  </div>
  <div class="row">
    <div style="width: 27.95%;" class="bar purple"/>
    <div class="percent">28%</div>
    <div class="count">322</div>
    <div class="choice">Automation or scripts</div>
  </div>
  <div class="row">
    <div style="width: 27.60%;" class="bar purple"/>
    <div class="percent">28%</div>
    <div class="count">318</div>
    <div class="choice">Web services (returning HTML)</div>
  </div>
  <div class="row">
    <div style="width: 18.66%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">215</div>
    <div class="choice">Agents or daemons</div>
  </div>
  <div class="row">
    <div style="width: 8.59%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">99</div>
    <div class="choice">Desktop programs (GUI)</div>
  </div>
  <div class="row">
    <div style="width: 8.42%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">97</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s0q12">Which industries do you use Haskell in?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 23.18%;" class="bar purple"/>
    <div class="percent">23%</div>
    <div class="count">267</div>
    <div class="choice">Web</div>
  </div>
  <div class="row">
    <div style="width: 14.24%;" class="bar purple"/>
    <div class="percent">14%</div>
    <div class="count">164</div>
    <div class="choice">Academia</div>
  </div>
  <div class="row">
    <div style="width: 13.02%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">150</div>
    <div class="choice">Banking or finance</div>
  </div>
  <div class="row">
    <div style="width: 9.81%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">113</div>
    <div class="choice">Education</div>
  </div>
  <div class="row">
    <div style="width: 8.77%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">101</div>
    <div class="choice">Science</div>
  </div>
  <div class="row">
    <div style="width: 7.12%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">82</div>
    <div class="choice">Commerce or retail</div>
  </div>
  <div class="row">
    <div style="width: 6.42%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">74</div>
    <div class="choice">Cryptocurrency</div>
  </div>
  <div class="row">
    <div style="width: 4.51%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">52</div>
    <div class="choice">Gaming</div>
  </div>
  <div class="row">
    <div style="width: 3.21%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">37</div>
    <div class="choice">Healthcare or medical</div>
  </div>
  <div class="row">
    <div style="width: 2.43%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">28</div>
    <div class="choice">Embedded</div>
  </div>
  <div class="row">
    <div style="width: 2.00%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">23</div>
    <div class="choice">Mobile</div>
  </div>
  <div class="row">
    <div style="width: 1.82%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">21</div>
    <div class="choice">Government</div>
  </div>
  <div class="row">
    <div style="width: 11.28%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">130</div>
    <div class="choice">Other</div>
  </div>
</div>
<h2 id="s1">Projects</h2>
<h3 id="s1q0">How many Haskell projects do you contribute to?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 25.95%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">299</div>
    <div class="choice">0</div>
  </div>
  <div class="row">
    <div style="width: 15.28%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">176</div>
    <div class="choice">1</div>
  </div>
  <div class="row">
    <div style="width: 14.06%;" class="bar purple"/>
    <div class="percent">14%</div>
    <div class="count">162</div>
    <div class="choice">2</div>
  </div>
  <div class="row">
    <div style="width: 9.11%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">105</div>
    <div class="choice">3</div>
  </div>
  <div class="row">
    <div style="width: 3.21%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">37</div>
    <div class="choice">4</div>
  </div>
  <div class="row">
    <div style="width: 3.82%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">44</div>
    <div class="choice">5</div>
  </div>
  <div class="row">
    <div style="width: 8.07%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">93</div>
    <div class="choice">6 to 10</div>
  </div>
  <div class="row">
    <div style="width: 2.34%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">27</div>
    <div class="choice">11 to 20</div>
  </div>
  <div class="row">
    <div style="width: 2.86%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">33</div>
    <div class="choice">More than 20</div>
  </div>
</div>
<h3 id="s1q1">What is the total size of all the Haskell projects you contribute to?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 11.81%;" class="bar purple"/>
    <div class="percent">12%</div>
    <div class="count">136</div>
    <div class="choice">Less than 1,000 lines of code</div>
  </div>
  <div class="row">
    <div style="width: 19.62%;" class="bar purple"/>
    <div class="percent">20%</div>
    <div class="count">226</div>
    <div class="choice">Between 1,000 and 9,999 lines of code</div>
  </div>
  <div class="row">
    <div style="width: 20.40%;" class="bar purple"/>
    <div class="percent">20%</div>
    <div class="count">235</div>
    <div class="choice">Between 10,000 and 99,999 lines of code</div>
  </div>
  <div class="row">
    <div style="width: 12.59%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">145</div>
    <div class="choice">More than 100,000 lines of code</div>
  </div>
</div>
<h3 id="s1q2">Which platforms do you develop Haskell on?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 77.95%;" class="bar purple"/>
    <div class="percent">78%</div>
    <div class="count">898</div>
    <div class="choice">Linux</div>
  </div>
  <div class="row">
    <div style="width: 33.77%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">389</div>
    <div class="choice">MacOS</div>
  </div>
  <div class="row">
    <div style="width: 14.93%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">172</div>
    <div class="choice">Windows</div>
  </div>
  <div class="row">
    <div style="width: 2.69%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">31</div>
    <div class="choice">BSD</div>
  </div>
  <div class="row">
    <div style="width: 0.78%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">9</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s1q3">Which platforms do you target?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 86.28%;" class="bar purple"/>
    <div class="percent">86%</div>
    <div class="count">994</div>
    <div class="choice">Linux</div>
  </div>
  <div class="row">
    <div style="width: 33.59%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">387</div>
    <div class="choice">MacOS</div>
  </div>
  <div class="row">
    <div style="width: 25.95%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">299</div>
    <div class="choice">Windows</div>
  </div>
  <div class="row">
    <div style="width: 5.03%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">58</div>
    <div class="choice">BSD</div>
  </div>
  <div class="row">
    <div style="width: 3.21%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">37</div>
    <div class="choice">Android</div>
  </div>
  <div class="row">
    <div style="width: 3.04%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">35</div>
    <div class="choice">iOS</div>
  </div>
  <div class="row">
    <div style="width: 1.91%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">22</div>
    <div class="choice">Other</div>
  </div>
</div>
<h2 id="s2">Compilers</h2>
<h3 id="s2q0">Which Haskell compilers do you use?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 97.14%;" class="bar purple"/>
    <div class="percent">97%</div>
    <div class="count">1119</div>
    <div class="choice">GHC</div>
  </div>
  <div class="row">
    <div style="width: 8.16%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">94</div>
    <div class="choice">GHCJS</div>
  </div>
  <div class="row">
    <div style="width: 1.39%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">16</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s2q1">Which installation methods do you use for your Haskell compiler?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 50.26%;" class="bar purple"/>
    <div class="percent">50%</div>
    <div class="count">579</div>
    <div class="choice">Stack</div>
  </div>
  <div class="row">
    <div style="width: 33.77%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">389</div>
    <div class="choice">ghcup</div>
  </div>
  <div class="row">
    <div style="width: 33.68%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">388</div>
    <div class="choice">Nix</div>
  </div>
  <div class="row">
    <div style="width: 14.32%;" class="bar purple"/>
    <div class="percent">14%</div>
    <div class="count">165</div>
    <div class="choice">Operating system package</div>
  </div>
  <div class="row">
    <div style="width: 8.51%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">98</div>
    <div class="choice">Haskell Platform</div>
  </div>
  <div class="row">
    <div style="width: 7.20%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">83</div>
    <div class="choice">Official binaries</div>
  </div>
  <div class="row">
    <div style="width: 5.64%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">65</div>
    <div class="choice">Homebrew</div>
  </div>
  <div class="row">
    <div style="width: 5.56%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">64</div>
    <div class="choice">Source</div>
  </div>
  <div class="row">
    <div style="width: 3.04%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">35</div>
    <div class="choice">Chocolatey</div>
  </div>
  <div class="row">
    <div style="width: 1.74%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">20</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s2q2">Has upgrading your Haskell compiler broken your code in the last year?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 50.00%;" class="bar purple"/>
    <div class="percent">50%</div>
    <div class="count">576</div>
    <div class="choice">No</div>
  </div>
  <div class="row">
    <div style="width: 28.39%;" class="bar purple"/>
    <div class="percent">28%</div>
    <div class="count">327</div>
    <div class="choice">Yes</div>
  </div>
</div>
<h3 id="s2q3">How has upgrading your Haskell compiler broken your code in the last year?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 18.40%;" class="bar purple"/>
    <div class="percent">18%</div>
    <div class="count">212</div>
    <div class="choice">Incompatible dependencies</div>
  </div>
  <div class="row">
    <div style="width: 15.97%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">184</div>
    <div class="choice">Expected changes, such as the MonadFail proposal</div>
  </div>
  <div class="row">
    <div style="width: 6.42%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">74</div>
    <div class="choice">New warnings</div>
  </div>
  <div class="row">
    <div style="width: 3.73%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">43</div>
    <div class="choice">Compiler bugs</div>
  </div>
  <div class="row">
    <div style="width: 3.39%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">39</div>
    <div class="choice">Unexpected changes</div>
  </div>
  <div class="row">
    <div style="width: 1.82%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">21</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s2q4">Which versions of GHC do you use?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 10.33%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">119</div>
    <div class="choice">&gt; 9.2</div>
  </div>
  <div class="row">
    <div style="width: 16.41%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">189</div>
    <div class="choice">9.2</div>
  </div>
  <div class="row">
    <div style="width: 19.36%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">223</div>
    <div class="choice">9.0</div>
  </div>
  <div class="row">
    <div style="width: 64.67%;" class="bar purple"/>
    <div class="percent">65%</div>
    <div class="count">745</div>
    <div class="choice">8.10.x</div>
  </div>
  <div class="row">
    <div style="width: 18.84%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">217</div>
    <div class="choice">8.8.x</div>
  </div>
  <div class="row">
    <div style="width: 13.45%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">155</div>
    <div class="choice">8.6.x</div>
  </div>
  <div class="row">
    <div style="width: 4.08%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">47</div>
    <div class="choice">8.4.x</div>
  </div>
  <div class="row">
    <div style="width: 1.48%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">17</div>
    <div class="choice">8.2.x</div>
  </div>
  <div class="row">
    <div style="width: 1.22%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">14</div>
    <div class="choice">&lt; 8.2</div>
  </div>
</div>
<h3 id="s2q5">Which language extensions would you like to be enabled by default?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 45.92%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 1.65%;" class="bar red"/>
    <div class="percent">+46%</div>
    <div class="percent">-2%</div>
    <div class="count">+529</div>
    <div class="count">-19</div>
    <div class="choice">LambdaCase</div>
  </div>
  <div class="row">
    <div style="width: 41.23%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.86%;" class="bar red"/>
    <div class="percent">+41%</div>
    <div class="percent">-5%</div>
    <div class="count">+475</div>
    <div class="count">-56</div>
    <div class="choice">OverloadedStrings</div>
  </div>
  <div class="row">
    <div style="width: 40.19%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 1.82%;" class="bar red"/>
    <div class="percent">+40%</div>
    <div class="percent">-2%</div>
    <div class="count">+463</div>
    <div class="count">-21</div>
    <div class="choice">DeriveGeneric</div>
  </div>
  <div class="row">
    <div style="width: 37.07%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 1.91%;" class="bar red"/>
    <div class="percent">+37%</div>
    <div class="percent">-2%</div>
    <div class="count">+427</div>
    <div class="count">-22</div>
    <div class="choice">DeriveFunctor</div>
  </div>
  <div class="row">
    <div style="width: 35.94%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.39%;" class="bar red"/>
    <div class="percent">+36%</div>
    <div class="percent">-3%</div>
    <div class="count">+414</div>
    <div class="count">-39</div>
    <div class="choice">GADTs</div>
  </div>
  <div class="row">
    <div style="width: 35.76%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.26%;" class="bar red"/>
    <div class="percent">+36%</div>
    <div class="percent">-2%</div>
    <div class="count">+412</div>
    <div class="count">-26</div>
    <div class="choice">BangPatterns</div>
  </div>
  <div class="row">
    <div style="width: 33.94%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.08%;" class="bar red"/>
    <div class="percent">+34%</div>
    <div class="percent">-2%</div>
    <div class="count">+391</div>
    <div class="count">-24</div>
    <div class="choice">DerivingVia</div>
  </div>
  <div class="row">
    <div style="width: 32.64%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.26%;" class="bar red"/>
    <div class="percent">+33%</div>
    <div class="percent">-2%</div>
    <div class="count">+376</div>
    <div class="count">-26</div>
    <div class="choice">DeriveFoldable</div>
  </div>
  <div class="row">
    <div style="width: 31.08%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.04%;" class="bar red"/>
    <div class="percent">+31%</div>
    <div class="percent">-3%</div>
    <div class="count">+358</div>
    <div class="count">-35</div>
    <div class="choice">ScopedTypeVariables</div>
  </div>
  <div class="row">
    <div style="width: 31.08%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.00%;" class="bar red"/>
    <div class="percent">+31%</div>
    <div class="percent">-2%</div>
    <div class="count">+358</div>
    <div class="count">-23</div>
    <div class="choice">TypeApplications</div>
  </div>
  <div class="row">
    <div style="width: 30.90%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.12%;" class="bar red"/>
    <div class="percent">+31%</div>
    <div class="percent">-3%</div>
    <div class="count">+356</div>
    <div class="count">-36</div>
    <div class="choice">FlexibleInstances</div>
  </div>
  <div class="row">
    <div style="width: 30.56%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.86%;" class="bar red"/>
    <div class="percent">+31%</div>
    <div class="percent">-3%</div>
    <div class="count">+352</div>
    <div class="count">-33</div>
    <div class="choice">FlexibleContexts</div>
  </div>
  <div class="row">
    <div style="width: 29.77%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.34%;" class="bar red"/>
    <div class="percent">+30%</div>
    <div class="percent">-2%</div>
    <div class="count">+343</div>
    <div class="count">-27</div>
    <div class="choice">DeriveTraversable</div>
  </div>
  <div class="row">
    <div style="width: 29.77%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.86%;" class="bar red"/>
    <div class="percent">+30%</div>
    <div class="percent">-3%</div>
    <div class="count">+343</div>
    <div class="count">-33</div>
    <div class="choice">GeneralizedNewtypeDeriving</div>
  </div>
  <div class="row">
    <div style="width: 29.69%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.08%;" class="bar red"/>
    <div class="percent">+30%</div>
    <div class="percent">-2%</div>
    <div class="count">+342</div>
    <div class="count">-24</div>
    <div class="choice">DerivingStrategies</div>
  </div>
  <div class="row">
    <div style="width: 27.52%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.95%;" class="bar red"/>
    <div class="percent">+28%</div>
    <div class="percent">-3%</div>
    <div class="count">+317</div>
    <div class="count">-34</div>
    <div class="choice">RankNTypes</div>
  </div>
  <div class="row">
    <div style="width: 27.26%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.43%;" class="bar red"/>
    <div class="percent">+27%</div>
    <div class="percent">-4%</div>
    <div class="count">+314</div>
    <div class="count">-51</div>
    <div class="choice">DataKinds</div>
  </div>
  <div class="row">
    <div style="width: 26.65%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.60%;" class="bar red"/>
    <div class="percent">+27%</div>
    <div class="percent">-3%</div>
    <div class="count">+307</div>
    <div class="count">-30</div>
    <div class="choice">MultiParamTypeClasses</div>
  </div>
  <div class="row">
    <div style="width: 25.61%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.30%;" class="bar red"/>
    <div class="percent">+26%</div>
    <div class="percent">-3%</div>
    <div class="count">+295</div>
    <div class="count">-38</div>
    <div class="choice">TupleSections</div>
  </div>
  <div class="row">
    <div style="width: 24.83%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.73%;" class="bar red"/>
    <div class="percent">+25%</div>
    <div class="percent">-4%</div>
    <div class="count">+286</div>
    <div class="count">-43</div>
    <div class="choice">TypeFamilies</div>
  </div>
  <div class="row">
    <div style="width: 24.05%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.43%;" class="bar red"/>
    <div class="percent">+24%</div>
    <div class="percent">-2%</div>
    <div class="count">+277</div>
    <div class="count">-28</div>
    <div class="choice">KindSignatures</div>
  </div>
  <div class="row">
    <div style="width: 23.70%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.60%;" class="bar red"/>
    <div class="percent">+24%</div>
    <div class="percent">-3%</div>
    <div class="count">+273</div>
    <div class="count">-30</div>
    <div class="choice">StandaloneDeriving</div>
  </div>
  <div class="row">
    <div style="width: 22.66%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.17%;" class="bar red"/>
    <div class="percent">+23%</div>
    <div class="percent">-4%</div>
    <div class="count">+261</div>
    <div class="count">-48</div>
    <div class="choice">MultiWayIf</div>
  </div>
  <div class="row">
    <div style="width: 21.61%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.69%;" class="bar red"/>
    <div class="percent">+22%</div>
    <div class="percent">-3%</div>
    <div class="count">+249</div>
    <div class="count">-31</div>
    <div class="choice">TypeOperators</div>
  </div>
  <div class="row">
    <div style="width: 21.18%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.39%;" class="bar red"/>
    <div class="percent">+21%</div>
    <div class="percent">-3%</div>
    <div class="count">+244</div>
    <div class="count">-39</div>
    <div class="choice">DeriveDataTypeable</div>
  </div>
  <div class="row">
    <div style="width: 20.83%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.39%;" class="bar red"/>
    <div class="percent">+21%</div>
    <div class="percent">-3%</div>
    <div class="count">+240</div>
    <div class="count">-39</div>
    <div class="choice">EmptyCase</div>
  </div>
  <div class="row">
    <div style="width: 20.66%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.65%;" class="bar red"/>
    <div class="percent">+21%</div>
    <div class="percent">-4%</div>
    <div class="count">+238</div>
    <div class="count">-42</div>
    <div class="choice">ViewPatterns</div>
  </div>
  <div class="row">
    <div style="width: 19.53%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.47%;" class="bar red"/>
    <div class="percent">+20%</div>
    <div class="percent">-3%</div>
    <div class="count">+225</div>
    <div class="count">-40</div>
    <div class="choice">DeriveLift</div>
  </div>
  <div class="row">
    <div style="width: 19.44%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.12%;" class="bar red"/>
    <div class="percent">+19%</div>
    <div class="percent">-3%</div>
    <div class="count">+224</div>
    <div class="count">-36</div>
    <div class="choice">GADTSyntax</div>
  </div>
  <div class="row">
    <div style="width: 19.27%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.99%;" class="bar red"/>
    <div class="percent">+19%</div>
    <div class="percent">-4%</div>
    <div class="count">+222</div>
    <div class="count">-46</div>
    <div class="choice">ConstraintKinds</div>
  </div>
  <div class="row">
    <div style="width: 19.10%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.60%;" class="bar red"/>
    <div class="percent">+19%</div>
    <div class="percent">-3%</div>
    <div class="count">+220</div>
    <div class="count">-30</div>
    <div class="choice">NumericUnderscores</div>
  </div>
  <div class="row">
    <div style="width: 18.92%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.12%;" class="bar red"/>
    <div class="percent">+19%</div>
    <div class="percent">-5%</div>
    <div class="count">+218</div>
    <div class="count">-59</div>
    <div class="choice">NamedFieldPuns</div>
  </div>
  <div class="row">
    <div style="width: 18.66%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.99%;" class="bar red"/>
    <div class="percent">+19%</div>
    <div class="percent">-4%</div>
    <div class="count">+215</div>
    <div class="count">-46</div>
    <div class="choice">ExplicitForAll</div>
  </div>
  <div class="row">
    <div style="width: 18.49%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.08%;" class="bar red"/>
    <div class="percent">+18%</div>
    <div class="percent">-4%</div>
    <div class="count">+213</div>
    <div class="count">-47</div>
    <div class="choice">FunctionalDependencies</div>
  </div>
  <div class="row">
    <div style="width: 18.32%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.03%;" class="bar red"/>
    <div class="percent">+18%</div>
    <div class="percent">-7%</div>
    <div class="count">+211</div>
    <div class="count">-81</div>
    <div class="choice">DeriveAnyClass</div>
  </div>
  <div class="row">
    <div style="width: 18.14%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.30%;" class="bar red"/>
    <div class="percent">+18%</div>
    <div class="percent">-3%</div>
    <div class="count">+209</div>
    <div class="count">-38</div>
    <div class="choice">InstanceSigs</div>
  </div>
  <div class="row">
    <div style="width: 17.71%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.17%;" class="bar red"/>
    <div class="percent">+18%</div>
    <div class="percent">-4%</div>
    <div class="count">+204</div>
    <div class="count">-48</div>
    <div class="choice">BinaryLiterals</div>
  </div>
  <div class="row">
    <div style="width: 17.36%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.42%;" class="bar red"/>
    <div class="percent">+17%</div>
    <div class="percent">-6%</div>
    <div class="count">+200</div>
    <div class="count">-74</div>
    <div class="choice">RecordWildCards</div>
  </div>
  <div class="row">
    <div style="width: 17.27%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 9.72%;" class="bar red"/>
    <div class="percent">+17%</div>
    <div class="percent">-10%</div>
    <div class="count">+199</div>
    <div class="count">-112</div>
    <div class="choice">ApplicativeDo</div>
  </div>
  <div class="row">
    <div style="width: 16.06%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.39%;" class="bar red"/>
    <div class="percent">+16%</div>
    <div class="percent">-3%</div>
    <div class="count">+185</div>
    <div class="count">-39</div>
    <div class="choice">PatternSynonyms</div>
  </div>
  <div class="row">
    <div style="width: 15.80%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 3.91%;" class="bar red"/>
    <div class="percent">+16%</div>
    <div class="percent">-4%</div>
    <div class="count">+182</div>
    <div class="count">-45</div>
    <div class="choice">ExistentialQuantification</div>
  </div>
  <div class="row">
    <div style="width: 15.71%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 2.95%;" class="bar red"/>
    <div class="percent">+16%</div>
    <div class="percent">-3%</div>
    <div class="count">+181</div>
    <div class="count">-34</div>
    <div class="choice">StandaloneKindSignatures</div>
  </div>
  <div class="row">
    <div style="width: 14.50%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.51%;" class="bar red"/>
    <div class="percent">+14%</div>
    <div class="percent">-7%</div>
    <div class="count">+167</div>
    <div class="count">-75</div>
    <div class="choice">DuplicateRecordFields</div>
  </div>
  <div class="row">
    <div style="width: 14.41%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.94%;" class="bar red"/>
    <div class="percent">+14%</div>
    <div class="percent">-7%</div>
    <div class="count">+166</div>
    <div class="count">-80</div>
    <div class="choice">BlockArguments</div>
  </div>
  <div class="row">
    <div style="width: 13.89%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.34%;" class="bar red"/>
    <div class="percent">+14%</div>
    <div class="percent">-4%</div>
    <div class="count">+160</div>
    <div class="count">-50</div>
    <div class="choice">DefaultSignatures</div>
  </div>
  <div class="row">
    <div style="width: 13.72%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.34%;" class="bar red"/>
    <div class="percent">+14%</div>
    <div class="percent">-4%</div>
    <div class="count">+158</div>
    <div class="count">-50</div>
    <div class="choice">PolyKinds</div>
  </div>
  <div class="row">
    <div style="width: 11.55%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.51%;" class="bar red"/>
    <div class="percent">+12%</div>
    <div class="percent">-5%</div>
    <div class="count">+133</div>
    <div class="count">-52</div>
    <div class="choice">ImportQualifiedPost</div>
  </div>
  <div class="row">
    <div style="width: 11.28%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.95%;" class="bar red"/>
    <div class="percent">+11%</div>
    <div class="percent">-5%</div>
    <div class="count">+130</div>
    <div class="count">-57</div>
    <div class="choice">Rank2Types</div>
  </div>
  <div class="row">
    <div style="width: 10.24%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.08%;" class="bar red"/>
    <div class="percent">+10%</div>
    <div class="percent">-6%</div>
    <div class="count">+118</div>
    <div class="count">-70</div>
    <div class="choice">OverloadedRecordDot</div>
  </div>
  <div class="row">
    <div style="width: 10.07%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 8.42%;" class="bar red"/>
    <div class="percent">+10%</div>
    <div class="percent">-8%</div>
    <div class="count">+116</div>
    <div class="count">-97</div>
    <div class="choice">OverloadedLists</div>
  </div>
  <div class="row">
    <div style="width: 9.90%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.73%;" class="bar red"/>
    <div class="percent">+10%</div>
    <div class="percent">-6%</div>
    <div class="count">+114</div>
    <div class="count">-66</div>
    <div class="choice">DisambiguateRecordFields</div>
  </div>
  <div class="row">
    <div style="width: 9.72%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.25%;" class="bar red"/>
    <div class="percent">+10%</div>
    <div class="percent">-6%</div>
    <div class="count">+112</div>
    <div class="count">-72</div>
    <div class="choice">OverloadedLabels</div>
  </div>
  <div class="row">
    <div style="width: 9.72%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.69%;" class="bar red"/>
    <div class="percent">+10%</div>
    <div class="percent">-5%</div>
    <div class="count">+112</div>
    <div class="count">-54</div>
    <div class="choice">TypeFamilyDependencies</div>
  </div>
  <div class="row">
    <div style="width: 9.29%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.60%;" class="bar red"/>
    <div class="percent">+9%</div>
    <div class="percent">-5%</div>
    <div class="count">+107</div>
    <div class="count">-53</div>
    <div class="choice">HexFloatLiterals</div>
  </div>
  <div class="row">
    <div style="width: 9.03%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.51%;" class="bar red"/>
    <div class="percent">+9%</div>
    <div class="percent">-5%</div>
    <div class="count">+104</div>
    <div class="count">-52</div>
    <div class="choice">NegativeLiterals</div>
  </div>
  <div class="row">
    <div style="width: 8.94%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.82%;" class="bar red"/>
    <div class="percent">+9%</div>
    <div class="percent">-6%</div>
    <div class="count">+103</div>
    <div class="count">-67</div>
    <div class="choice">NamedWildCards</div>
  </div>
  <div class="row">
    <div style="width: 8.77%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.20%;" class="bar red"/>
    <div class="percent">+9%</div>
    <div class="percent">-7%</div>
    <div class="count">+101</div>
    <div class="count">-83</div>
    <div class="choice">UnicodeSyntax</div>
  </div>
  <div class="row">
    <div style="width: 8.51%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.73%;" class="bar red"/>
    <div class="percent">+9%</div>
    <div class="percent">-6%</div>
    <div class="count">+98</div>
    <div class="count">-66</div>
    <div class="choice">PartialTypeSignatures</div>
  </div>
  <div class="row">
    <div style="width: 8.25%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.25%;" class="bar red"/>
    <div class="percent">+8%</div>
    <div class="percent">-4%</div>
    <div class="count">+95</div>
    <div class="count">-49</div>
    <div class="choice">QuantifiedConstraints</div>
  </div>
  <div class="row">
    <div style="width: 8.16%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 10.76%;" class="bar red"/>
    <div class="percent">+8%</div>
    <div class="percent">-11%</div>
    <div class="count">+94</div>
    <div class="count">-124</div>
    <div class="choice">TemplateHaskell</div>
  </div>
  <div class="row">
    <div style="width: 8.07%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.16%;" class="bar red"/>
    <div class="percent">+8%</div>
    <div class="percent">-6%</div>
    <div class="count">+93</div>
    <div class="count">-71</div>
    <div class="choice">OverloadedRecordUpdate</div>
  </div>
  <div class="row">
    <div style="width: 8.07%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.60%;" class="bar red"/>
    <div class="percent">+8%</div>
    <div class="percent">-5%</div>
    <div class="count">+93</div>
    <div class="count">-53</div>
    <div class="choice">TypeSynonymInstances</div>
  </div>
  <div class="row">
    <div style="width: 7.64%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.68%;" class="bar red"/>
    <div class="percent">+8%</div>
    <div class="percent">-7%</div>
    <div class="count">+88</div>
    <div class="count">-77</div>
    <div class="choice">QuasiQuotes</div>
  </div>
  <div class="row">
    <div style="width: 7.12%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.73%;" class="bar red"/>
    <div class="percent">+7%</div>
    <div class="percent">-6%</div>
    <div class="count">+82</div>
    <div class="count">-66</div>
    <div class="choice">TypeInType</div>
  </div>
  <div class="row">
    <div style="width: 6.94%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.69%;" class="bar red"/>
    <div class="percent">+7%</div>
    <div class="percent">-5%</div>
    <div class="count">+80</div>
    <div class="count">-54</div>
    <div class="choice">PackageImports</div>
  </div>
  <div class="row">
    <div style="width: 6.42%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.08%;" class="bar red"/>
    <div class="percent">+6%</div>
    <div class="percent">-6%</div>
    <div class="count">+74</div>
    <div class="count">-70</div>
    <div class="choice">NoStarIsType</div>
  </div>
  <div class="row">
    <div style="width: 6.42%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.08%;" class="bar red"/>
    <div class="percent">+6%</div>
    <div class="percent">-4%</div>
    <div class="count">+74</div>
    <div class="count">-47</div>
    <div class="choice">RoleAnnotations</div>
  </div>
  <div class="row">
    <div style="width: 5.90%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.69%;" class="bar red"/>
    <div class="percent">+6%</div>
    <div class="percent">-5%</div>
    <div class="count">+68</div>
    <div class="count">-54</div>
    <div class="choice">NullaryTypeClasses</div>
  </div>
  <div class="row">
    <div style="width: 5.82%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 10.85%;" class="bar red"/>
    <div class="percent">+6%</div>
    <div class="percent">-11%</div>
    <div class="count">+67</div>
    <div class="count">-125</div>
    <div class="choice">NoImplicitPrelude</div>
  </div>
  <div class="row">
    <div style="width: 5.21%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.29%;" class="bar red"/>
    <div class="percent">+5%</div>
    <div class="percent">-7%</div>
    <div class="count">+60</div>
    <div class="count">-84</div>
    <div class="choice">RecursiveDo</div>
  </div>
  <div class="row">
    <div style="width: 4.95%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.86%;" class="bar red"/>
    <div class="percent">+5%</div>
    <div class="percent">-5%</div>
    <div class="count">+57</div>
    <div class="count">-56</div>
    <div class="choice">ConstrainedClassMethods</div>
  </div>
  <div class="row">
    <div style="width: 4.86%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.08%;" class="bar red"/>
    <div class="percent">+5%</div>
    <div class="percent">-6%</div>
    <div class="count">+56</div>
    <div class="count">-70</div>
    <div class="choice">UnboxedTuples</div>
  </div>
  <div class="row">
    <div style="width: 4.77%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 13.98%;" class="bar red"/>
    <div class="percent">+5%</div>
    <div class="percent">-14%</div>
    <div class="count">+55</div>
    <div class="count">-161</div>
    <div class="choice">AllowAmbiguousTypes</div>
  </div>
  <div class="row">
    <div style="width: 4.77%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.47%;" class="bar red"/>
    <div class="percent">+5%</div>
    <div class="percent">-5%</div>
    <div class="count">+55</div>
    <div class="count">-63</div>
    <div class="choice">ExplicitNamespaces</div>
  </div>
  <div class="row">
    <div style="width: 4.69%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 4.95%;" class="bar red"/>
    <div class="percent">+5%</div>
    <div class="percent">-5%</div>
    <div class="count">+54</div>
    <div class="count">-57</div>
    <div class="choice">NumDecimals</div>
  </div>
  <div class="row">
    <div style="width: 4.69%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 11.63%;" class="bar red"/>
    <div class="percent">+5%</div>
    <div class="percent">-12%</div>
    <div class="count">+54</div>
    <div class="count">-134</div>
    <div class="choice">StrictData</div>
  </div>
  <div class="row">
    <div style="width: 4.60%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.08%;" class="bar red"/>
    <div class="percent">+5%</div>
    <div class="percent">-6%</div>
    <div class="count">+53</div>
    <div class="count">-70</div>
    <div class="choice">UnboxedSums</div>
  </div>
  <div class="row">
    <div style="width: 4.34%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.64%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-8%</div>
    <div class="count">+50</div>
    <div class="count">-88</div>
    <div class="choice">MonadComprehensions</div>
  </div>
  <div class="row">
    <div style="width: 4.25%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.81%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-8%</div>
    <div class="count">+49</div>
    <div class="count">-90</div>
    <div class="choice">ForeignFunctionInterface</div>
  </div>
  <div class="row">
    <div style="width: 4.25%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 9.20%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-9%</div>
    <div class="count">+49</div>
    <div class="count">-106</div>
    <div class="choice">LinearTypes</div>
  </div>
  <div class="row">
    <div style="width: 4.25%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 8.07%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-8%</div>
    <div class="count">+49</div>
    <div class="count">-93</div>
    <div class="choice">MagicHash</div>
  </div>
  <div class="row">
    <div style="width: 4.08%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 11.98%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-12%</div>
    <div class="count">+47</div>
    <div class="count">-138</div>
    <div class="choice">Arrows</div>
  </div>
  <div class="row">
    <div style="width: 4.08%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.90%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-6%</div>
    <div class="count">+47</div>
    <div class="count">-68</div>
    <div class="choice">ParallelListComp</div>
  </div>
  <div class="row">
    <div style="width: 3.91%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.30%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-5%</div>
    <div class="count">+45</div>
    <div class="count">-61</div>
    <div class="choice">LiberalTypeSynonyms</div>
  </div>
  <div class="row">
    <div style="width: 3.91%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.12%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-5%</div>
    <div class="count">+45</div>
    <div class="count">-59</div>
    <div class="choice">UnliftedDatatypes</div>
  </div>
  <div class="row">
    <div style="width: 3.91%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 5.03%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-5%</div>
    <div class="count">+45</div>
    <div class="count">-58</div>
    <div class="choice">UnliftedNewtypes</div>
  </div>
  <div class="row">
    <div style="width: 3.73%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.77%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-7%</div>
    <div class="count">+43</div>
    <div class="count">-78</div>
    <div class="choice">NoMonomorphismRestriction</div>
  </div>
  <div class="row">
    <div style="width: 3.56%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 14.50%;" class="bar red"/>
    <div class="percent">+4%</div>
    <div class="percent">-14%</div>
    <div class="count">+41</div>
    <div class="count">-167</div>
    <div class="choice">Cpp</div>
  </div>
  <div class="row">
    <div style="width: 3.39%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 11.98%;" class="bar red"/>
    <div class="percent">+3%</div>
    <div class="percent">-12%</div>
    <div class="count">+39</div>
    <div class="count">-138</div>
    <div class="choice">UndecidableInstances</div>
  </div>
  <div class="row">
    <div style="width: 3.30%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.73%;" class="bar red"/>
    <div class="percent">+3%</div>
    <div class="percent">-8%</div>
    <div class="count">+38</div>
    <div class="count">-89</div>
    <div class="choice">ImpredicativeTypes</div>
  </div>
  <div class="row">
    <div style="width: 3.12%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 9.03%;" class="bar red"/>
    <div class="percent">+3%</div>
    <div class="percent">-9%</div>
    <div class="count">+36</div>
    <div class="count">-104</div>
    <div class="choice">CApiFFI</div>
  </div>
  <div class="row">
    <div style="width: 3.12%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.25%;" class="bar red"/>
    <div class="percent">+3%</div>
    <div class="percent">-6%</div>
    <div class="count">+36</div>
    <div class="count">-72</div>
    <div class="choice">PostfixOperators</div>
  </div>
  <div class="row">
    <div style="width: 2.95%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 9.20%;" class="bar red"/>
    <div class="percent">+3%</div>
    <div class="percent">-9%</div>
    <div class="count">+34</div>
    <div class="count">-106</div>
    <div class="choice">TemplateHaskellQuotes</div>
  </div>
  <div class="row">
    <div style="width: 2.86%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 11.72%;" class="bar red"/>
    <div class="percent">+3%</div>
    <div class="percent">-12%</div>
    <div class="count">+33</div>
    <div class="count">-135</div>
    <div class="choice">OverlappingInstances</div>
  </div>
  <div class="row">
    <div style="width: 2.78%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.55%;" class="bar red"/>
    <div class="percent">+3%</div>
    <div class="percent">-8%</div>
    <div class="count">+32</div>
    <div class="count">-87</div>
    <div class="choice">NoFieldSelectors</div>
  </div>
  <div class="row">
    <div style="width: 2.60%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 13.11%;" class="bar red"/>
    <div class="percent">+3%</div>
    <div class="percent">-13%</div>
    <div class="count">+30</div>
    <div class="count">-151</div>
    <div class="choice">Strict</div>
  </div>
  <div class="row">
    <div style="width: 2.43%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 9.72%;" class="bar red"/>
    <div class="percent">+2%</div>
    <div class="percent">-10%</div>
    <div class="count">+28</div>
    <div class="count">-112</div>
    <div class="choice">DatatypeContexts</div>
  </div>
  <div class="row">
    <div style="width: 1.91%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.86%;" class="bar red"/>
    <div class="percent">+2%</div>
    <div class="percent">-7%</div>
    <div class="count">+22</div>
    <div class="count">-79</div>
    <div class="choice">ExtendedDefaultRules</div>
  </div>
  <div class="row">
    <div style="width: 1.91%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.51%;" class="bar red"/>
    <div class="percent">+2%</div>
    <div class="percent">-7%</div>
    <div class="count">+22</div>
    <div class="count">-75</div>
    <div class="choice">MonoLocalBinds</div>
  </div>
  <div class="row">
    <div style="width: 1.74%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 11.28%;" class="bar red"/>
    <div class="percent">+2%</div>
    <div class="percent">-11%</div>
    <div class="count">+20</div>
    <div class="count">-130</div>
    <div class="choice">ImplicitParams</div>
  </div>
  <div class="row">
    <div style="width: 1.48%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.77%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-7%</div>
    <div class="count">+17</div>
    <div class="count">-78</div>
    <div class="choice">TransformListComp</div>
  </div>
  <div class="row">
    <div style="width: 1.39%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 6.16%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-6%</div>
    <div class="count">+16</div>
    <div class="count">-71</div>
    <div class="choice">StaticPointers</div>
  </div>
  <div class="row">
    <div style="width: 1.04%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.03%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-7%</div>
    <div class="count">+12</div>
    <div class="count">-81</div>
    <div class="choice">InterruptibleFFI</div>
  </div>
  <div class="row">
    <div style="width: 1.04%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 10.33%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-10%</div>
    <div class="count">+12</div>
    <div class="count">-119</div>
    <div class="choice">RebindableSyntax</div>
  </div>
  <div class="row">
    <div style="width: 0.95%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 10.59%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-11%</div>
    <div class="count">+11</div>
    <div class="count">-122</div>
    <div class="choice">UndecidableSuperClasses</div>
  </div>
  <div class="row">
    <div style="width: 0.87%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 11.46%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-11%</div>
    <div class="count">+10</div>
    <div class="count">-132</div>
    <div class="choice">NPlusKPatterns</div>
  </div>
  <div class="row">
    <div style="width: 0.69%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 13.89%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-14%</div>
    <div class="count">+8</div>
    <div class="count">-160</div>
    <div class="choice">IncoherentInstances</div>
  </div>
  <div class="row">
    <div style="width: 0.69%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.99%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-8%</div>
    <div class="count">+8</div>
    <div class="count">-92</div>
    <div class="choice">NoTraditionalRecordSyntax</div>
  </div>
  <div class="row">
    <div style="width: 0.61%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 9.98%;" class="bar red"/>
    <div class="percent">+1%</div>
    <div class="percent">-10%</div>
    <div class="count">+7</div>
    <div class="count">-115</div>
    <div class="choice">Unsafe</div>
  </div>
  <div class="row">
    <div style="width: 0.43%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 7.99%;" class="bar red"/>
    <div class="percent">+0%</div>
    <div class="percent">-8%</div>
    <div class="count">+5</div>
    <div class="count">-92</div>
    <div class="choice">NoMonadFailDesugaring</div>
  </div>
  <div class="row">
    <div style="width: 0.43%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 8.94%;" class="bar red"/>
    <div class="percent">+0%</div>
    <div class="percent">-9%</div>
    <div class="count">+5</div>
    <div class="count">-103</div>
    <div class="choice">Trustworthy</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 10.07%;" class="bar red"/>
    <div class="percent">+0%</div>
    <div class="percent">-10%</div>
    <div class="count">+2</div>
    <div class="count">-116</div>
    <div class="choice">NoEmptyDataDecls</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar blue"/>
    <div style="left: auto; right: 0; width: 8.77%;" class="bar red"/>
    <div class="percent">+0%</div>
    <div class="percent">-9%</div>
    <div class="count">+2</div>
    <div class="count">-101</div>
    <div class="choice">NoPatternGuards</div>
  </div>
</div>
<h3 id="s2q6">How important do you feel it would be to have a new version of the Haskell language standard?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 11.81%;" class="bar purple"/>
    <div class="percent">12%</div>
    <div class="count">136</div>
    <div class="choice">Extremely important</div>
  </div>
  <div class="row">
    <div style="width: 16.06%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">185</div>
    <div class="choice">Very important</div>
  </div>
  <div class="row">
    <div style="width: 30.30%;" class="bar purple"/>
    <div class="percent">30%</div>
    <div class="count">349</div>
    <div class="choice">Moderately important</div>
  </div>
  <div class="row">
    <div style="width: 15.45%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">178</div>
    <div class="choice">Slightly important</div>
  </div>
  <div class="row">
    <div style="width: 8.25%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">95</div>
    <div class="choice">Not at all important</div>
  </div>
</div>
<h2 id="s3">Tooling</h2>
<h3 id="s3q0">Which build tools do you use for Haskell?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 61.55%;" class="bar purple"/>
    <div class="percent">62%</div>
    <div class="count">709</div>
    <div class="choice">Cabal</div>
  </div>
  <div class="row">
    <div style="width: 60.16%;" class="bar purple"/>
    <div class="percent">60%</div>
    <div class="count">693</div>
    <div class="choice">Stack</div>
  </div>
  <div class="row">
    <div style="width: 33.77%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">389</div>
    <div class="choice">Nix</div>
  </div>
  <div class="row">
    <div style="width: 10.07%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">116</div>
    <div class="choice">haskell.nix</div>
  </div>
  <div class="row">
    <div style="width: 9.64%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">111</div>
    <div class="choice">Make</div>
  </div>
  <div class="row">
    <div style="width: 5.38%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">62</div>
    <div class="choice">Shake</div>
  </div>
  <div class="row">
    <div style="width: 3.21%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">37</div>
    <div class="choice">ghc-pkg</div>
  </div>
  <div class="row">
    <div style="width: 2.26%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">26</div>
    <div class="choice">Bazel</div>
  </div>
  <div class="row">
    <div style="width: 1.22%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">14</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s3q1">Which editors do you use for Haskell?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 45.31%;" class="bar purple"/>
    <div class="percent">45%</div>
    <div class="count">522</div>
    <div class="choice">Visual Studio Code</div>
  </div>
  <div class="row">
    <div style="width: 39.93%;" class="bar purple"/>
    <div class="percent">40%</div>
    <div class="count">460</div>
    <div class="choice">Vi family</div>
  </div>
  <div class="row">
    <div style="width: 28.73%;" class="bar purple"/>
    <div class="percent">29%</div>
    <div class="count">331</div>
    <div class="choice">Emacs family</div>
  </div>
  <div class="row">
    <div style="width: 3.82%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">44</div>
    <div class="choice">Sublime Text</div>
  </div>
  <div class="row">
    <div style="width: 3.56%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">41</div>
    <div class="choice">IntelliJ IDEA</div>
  </div>
  <div class="row">
    <div style="width: 2.00%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">23</div>
    <div class="choice">Atom</div>
  </div>
  <div class="row">
    <div style="width: 2.86%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">33</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s3q2">Which IDEs do you use for Haskell?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 64.32%;" class="bar purple"/>
    <div class="percent">64%</div>
    <div class="count">741</div>
    <div class="choice">Haskell Language Server (HLS)</div>
  </div>
  <div class="row">
    <div style="width: 19.18%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">221</div>
    <div class="choice">ghcid</div>
  </div>
  <div class="row">
    <div style="width: 4.25%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">49</div>
    <div class="choice">IntelliJ</div>
  </div>
  <div class="row">
    <div style="width: 3.91%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">45</div>
    <div class="choice">ghcide</div>
  </div>
  <div class="row">
    <div style="width: 1.48%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">17</div>
    <div class="choice">Intero</div>
  </div>
  <div class="row">
    <div style="width: 2.08%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">24</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s3q3">Which version control systems do you use for Haskell?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 95.31%;" class="bar purple"/>
    <div class="percent">95%</div>
    <div class="count">1098</div>
    <div class="choice">Git</div>
  </div>
  <div class="row">
    <div style="width: 1.82%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">21</div>
    <div class="choice">Darcs</div>
  </div>
  <div class="row">
    <div style="width: 0.87%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">10</div>
    <div class="choice">Mercurial</div>
  </div>
  <div class="row">
    <div style="width: 0.87%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">10</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s3q4">Where do you get Haskell packages from?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 69.44%;" class="bar purple"/>
    <div class="percent">69%</div>
    <div class="count">800</div>
    <div class="choice">Hackage</div>
  </div>
  <div class="row">
    <div style="width: 53.82%;" class="bar purple"/>
    <div class="percent">54%</div>
    <div class="count">620</div>
    <div class="choice">Stackage</div>
  </div>
  <div class="row">
    <div style="width: 29.95%;" class="bar purple"/>
    <div class="percent">30%</div>
    <div class="count">345</div>
    <div class="choice">Nix</div>
  </div>
  <div class="row">
    <div style="width: 20.66%;" class="bar purple"/>
    <div class="percent">21%</div>
    <div class="count">238</div>
    <div class="choice">Source</div>
  </div>
  <div class="row">
    <div style="width: 0.95%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">11</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s3q5">Which tools do you use to test Haskell code?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 52.52%;" class="bar purple"/>
    <div class="percent">53%</div>
    <div class="count">605</div>
    <div class="choice">QuickCheck</div>
  </div>
  <div class="row">
    <div style="width: 44.70%;" class="bar purple"/>
    <div class="percent">45%</div>
    <div class="count">515</div>
    <div class="choice">Hspec</div>
  </div>
  <div class="row">
    <div style="width: 25.95%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">299</div>
    <div class="choice">HUnit</div>
  </div>
  <div class="row">
    <div style="width: 24.74%;" class="bar purple"/>
    <div class="percent">25%</div>
    <div class="count">285</div>
    <div class="choice">Tasty</div>
  </div>
  <div class="row">
    <div style="width: 17.71%;" class="bar purple"/>
    <div class="percent">18%</div>
    <div class="count">204</div>
    <div class="choice">Hedgehog</div>
  </div>
  <div class="row">
    <div style="width: 3.56%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">41</div>
    <div class="choice">SmallCheck</div>
  </div>
  <div class="row">
    <div style="width: 3.39%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">39</div>
    <div class="choice">Haskell Test Framework</div>
  </div>
  <div class="row">
    <div style="width: 1.82%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">21</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s3q6">Which tools do you use to benchmark Haskell code?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 27.95%;" class="bar purple"/>
    <div class="percent">28%</div>
    <div class="count">322</div>
    <div class="choice">Criterion</div>
  </div>
  <div class="row">
    <div style="width: 4.51%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">52</div>
    <div class="choice">Bench</div>
  </div>
  <div class="row">
    <div style="width: 3.30%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">38</div>
    <div class="choice">Gauge</div>
  </div>
  <div class="row">
    <div style="width: 2.95%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">34</div>
    <div class="choice">Other</div>
  </div>
</div>
<h2 id="s4">Infrastructure</h2>
<h3 id="s4q0">Which tools do you use to deploy Haskell applications?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 36.55%;" class="bar purple"/>
    <div class="percent">37%</div>
    <div class="count">421</div>
    <div class="choice">Static binaries</div>
  </div>
  <div class="row">
    <div style="width: 34.46%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">397</div>
    <div class="choice">Docker images</div>
  </div>
  <div class="row">
    <div style="width: 22.31%;" class="bar purple"/>
    <div class="percent">22%</div>
    <div class="count">257</div>
    <div class="choice">Nix expressions</div>
  </div>
  <div class="row">
    <div style="width: 14.32%;" class="bar purple"/>
    <div class="percent">14%</div>
    <div class="count">165</div>
    <div class="choice">Dynamic binaries</div>
  </div>
  <div class="row">
    <div style="width: 0.95%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">11</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s4q1">Where do you deploy Haskell applications?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 37.76%;" class="bar purple"/>
    <div class="percent">38%</div>
    <div class="count">435</div>
    <div class="choice">Self or company owned servers</div>
  </div>
  <div class="row">
    <div style="width: 23.26%;" class="bar purple"/>
    <div class="percent">23%</div>
    <div class="count">268</div>
    <div class="choice">Amazon Web Services</div>
  </div>
  <div class="row">
    <div style="width: 7.20%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">83</div>
    <div class="choice">Google Cloud</div>
  </div>
  <div class="row">
    <div style="width: 5.99%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">69</div>
    <div class="choice">Digital Ocean</div>
  </div>
  <div class="row">
    <div style="width: 3.65%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">42</div>
    <div class="choice">Heroku</div>
  </div>
  <div class="row">
    <div style="width: 2.78%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">32</div>
    <div class="choice">Microsoft Azure</div>
  </div>
  <div class="row">
    <div style="width: 4.60%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">53</div>
    <div class="choice">Other</div>
  </div>
</div>
<h2 id="s5">Community</h2>
<h3 id="s5q0">Where do you interact with the Haskell community?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 54.17%;" class="bar purple"/>
    <div class="percent">54%</div>
    <div class="count">624</div>
    <div class="choice">Reddit</div>
  </div>
  <div class="row">
    <div style="width: 51.56%;" class="bar purple"/>
    <div class="percent">52%</div>
    <div class="count">594</div>
    <div class="choice">GitHub</div>
  </div>
  <div class="row">
    <div style="width: 27.34%;" class="bar purple"/>
    <div class="percent">27%</div>
    <div class="count">315</div>
    <div class="choice">Twitter</div>
  </div>
  <div class="row">
    <div style="width: 20.23%;" class="bar purple"/>
    <div class="percent">20%</div>
    <div class="count">233</div>
    <div class="choice">Stack Overflow</div>
  </div>
  <div class="row">
    <div style="width: 19.27%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">222</div>
    <div class="choice">Discord</div>
  </div>
  <div class="row">
    <div style="width: 19.27%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">222</div>
    <div class="choice">Slack</div>
  </div>
  <div class="row">
    <div style="width: 17.19%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">198</div>
    <div class="choice">IRC</div>
  </div>
  <div class="row">
    <div style="width: 16.15%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">186</div>
    <div class="choice">Conferences (commercial)</div>
  </div>
  <div class="row">
    <div style="width: 14.58%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">168</div>
    <div class="choice">Discourse</div>
  </div>
  <div class="row">
    <div style="width: 13.28%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">153</div>
    <div class="choice">Mailing lists</div>
  </div>
  <div class="row">
    <div style="width: 12.93%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">149</div>
    <div class="choice">Conferences (academic)</div>
  </div>
  <div class="row">
    <div style="width: 10.42%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">120</div>
    <div class="choice">Meetups</div>
  </div>
  <div class="row">
    <div style="width: 7.90%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">91</div>
    <div class="choice">Telegram</div>
  </div>
  <div class="row">
    <div style="width: 6.51%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">75</div>
    <div class="choice">Matrix/Riot</div>
  </div>
  <div class="row">
    <div style="width: 5.82%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">67</div>
    <div class="choice">Lobsters</div>
  </div>
  <div class="row">
    <div style="width: 2.34%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">27</div>
    <div class="choice">Mastodon</div>
  </div>
  <div class="row">
    <div style="width: 2.43%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">28</div>
    <div class="choice">Zulip</div>
  </div>
  <div class="row">
    <div style="width: 1.74%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">20</div>
    <div class="choice">Gitter</div>
  </div>
  <div class="row">
    <div style="width: 1.91%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">22</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s5q1">Which of the following Haskell topics would you like to see more written about?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 56.34%;" class="bar purple"/>
    <div class="percent">56%</div>
    <div class="count">649</div>
    <div class="choice">Best practices</div>
  </div>
  <div class="row">
    <div style="width: 42.10%;" class="bar purple"/>
    <div class="percent">42%</div>
    <div class="count">485</div>
    <div class="choice">Design patterns</div>
  </div>
  <div class="row">
    <div style="width: 41.67%;" class="bar purple"/>
    <div class="percent">42%</div>
    <div class="count">480</div>
    <div class="choice">Application architectures</div>
  </div>
  <div class="row">
    <div style="width: 37.33%;" class="bar purple"/>
    <div class="percent">37%</div>
    <div class="count">430</div>
    <div class="choice">Performance analysis</div>
  </div>
  <div class="row">
    <div style="width: 33.42%;" class="bar purple"/>
    <div class="percent">33%</div>
    <div class="count">385</div>
    <div class="choice">Debugging how-tos</div>
  </div>
  <div class="row">
    <div style="width: 32.12%;" class="bar purple"/>
    <div class="percent">32%</div>
    <div class="count">370</div>
    <div class="choice">Library walkthroughs</div>
  </div>
  <div class="row">
    <div style="width: 29.51%;" class="bar purple"/>
    <div class="percent">30%</div>
    <div class="count">340</div>
    <div class="choice">Production infrastructure</div>
  </div>
  <div class="row">
    <div style="width: 27.60%;" class="bar purple"/>
    <div class="percent">28%</div>
    <div class="count">318</div>
    <div class="choice">Tooling choices</div>
  </div>
  <div class="row">
    <div style="width: 25.69%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">296</div>
    <div class="choice">Case studies</div>
  </div>
  <div class="row">
    <div style="width: 24.57%;" class="bar purple"/>
    <div class="percent">25%</div>
    <div class="count">283</div>
    <div class="choice">Algorithm implementations</div>
  </div>
  <div class="row">
    <div style="width: 23.61%;" class="bar purple"/>
    <div class="percent">24%</div>
    <div class="count">272</div>
    <div class="choice">Project maintenance</div>
  </div>
  <div class="row">
    <div style="width: 23.35%;" class="bar purple"/>
    <div class="percent">23%</div>
    <div class="count">269</div>
    <div class="choice">Testing</div>
  </div>
  <div class="row">
    <div style="width: 22.14%;" class="bar purple"/>
    <div class="percent">22%</div>
    <div class="count">255</div>
    <div class="choice">GUIs</div>
  </div>
  <div class="row">
    <div style="width: 19.70%;" class="bar purple"/>
    <div class="percent">20%</div>
    <div class="count">227</div>
    <div class="choice">Web development</div>
  </div>
  <div class="row">
    <div style="width: 19.44%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">224</div>
    <div class="choice">Project setup</div>
  </div>
  <div class="row">
    <div style="width: 17.80%;" class="bar purple"/>
    <div class="percent">18%</div>
    <div class="count">205</div>
    <div class="choice">Beginner fundamentals</div>
  </div>
  <div class="row">
    <div style="width: 15.80%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">182</div>
    <div class="choice">Machine learning</div>
  </div>
  <div class="row">
    <div style="width: 14.76%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">170</div>
    <div class="choice">Game development</div>
  </div>
  <div class="row">
    <div style="width: 11.28%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">130</div>
    <div class="choice">Mobile development</div>
  </div>
  <div class="row">
    <div style="width: 8.51%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">98</div>
    <div class="choice">Comparisons to other languages</div>
  </div>
  <div class="row">
    <div style="width: 3.73%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">43</div>
    <div class="choice">Other</div>
  </div>
</div>
<h2 id="s6">Feelings</h2>
<h3 id="s6q19">I would prefer to use Haskell for my next new project.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 53.99%;" class="bar purple"/>
    <div class="percent">54%</div>
    <div class="count">622</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 25.61%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">295</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 8.33%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">96</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 3.30%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">38</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.82%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">21</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q18">I would recommend using Haskell to others.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 44.10%;" class="bar purple"/>
    <div class="percent">44%</div>
    <div class="count">508</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 36.28%;" class="bar purple"/>
    <div class="percent">36%</div>
    <div class="count">418</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 9.11%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">105</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 3.12%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">36</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 0.95%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">11</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q1">I am satisfied with Haskell as a language.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 33.07%;" class="bar purple"/>
    <div class="percent">33%</div>
    <div class="count">381</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 48.26%;" class="bar purple"/>
    <div class="percent">48%</div>
    <div class="count">556</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 8.25%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">95</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 4.69%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">54</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 0.78%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">9</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q14">Once my Haskell program compiles, it generally does what I intended.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 33.42%;" class="bar purple"/>
    <div class="percent">33%</div>
    <div class="count">385</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 45.23%;" class="bar purple"/>
    <div class="percent">45%</div>
    <div class="count">521</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 10.24%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">118</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 4.08%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">47</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 0.52%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">6</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q13">I think that software written in Haskell is easy to maintain.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 34.11%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">393</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 40.02%;" class="bar purple"/>
    <div class="percent">40%</div>
    <div class="count">461</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 11.20%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">129</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 1.65%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">19</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.04%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">12</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q2">I am satisfied with Haskell's compilers, such as GHC.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 30.64%;" class="bar purple"/>
    <div class="percent">31%</div>
    <div class="count">353</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 48.09%;" class="bar purple"/>
    <div class="percent">48%</div>
    <div class="count">554</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 8.94%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">103</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 4.51%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">52</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 0.78%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">9</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q0">I feel welcome in the Haskell community.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 32.47%;" class="bar purple"/>
    <div class="percent">32%</div>
    <div class="count">374</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 37.41%;" class="bar purple"/>
    <div class="percent">37%</div>
    <div class="count">431</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 15.89%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">183</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 2.69%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">31</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.30%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">15</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q16">Haskell's performance meets my needs.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 26.91%;" class="bar purple"/>
    <div class="percent">27%</div>
    <div class="count">310</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 46.96%;" class="bar purple"/>
    <div class="percent">47%</div>
    <div class="count">541</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 10.76%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">124</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 3.82%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">44</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.04%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">12</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q4">I am satisfied with Haskell's package repositories, such as Hackage.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 17.36%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">200</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 49.13%;" class="bar purple"/>
    <div class="percent">49%</div>
    <div class="count">566</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 17.27%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">199</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 6.34%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">73</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.30%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">15</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q6">I think Haskell libraries are high quality.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 16.49%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">190</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 46.61%;" class="bar purple"/>
    <div class="percent">47%</div>
    <div class="count">537</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 21.70%;" class="bar purple"/>
    <div class="percent">22%</div>
    <div class="count">250</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 4.86%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">56</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.65%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">19</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q15">I think that Haskell libraries perform well.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 10.85%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">125</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 46.53%;" class="bar purple"/>
    <div class="percent">47%</div>
    <div class="count">536</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 22.40%;" class="bar purple"/>
    <div class="percent">22%</div>
    <div class="count">258</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 2.43%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">28</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 0.87%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">10</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q20">Haskell is working well for my team.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 21.44%;" class="bar purple"/>
    <div class="percent">21%</div>
    <div class="count">247</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 17.27%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">199</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 11.20%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">129</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 2.08%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">24</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.22%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">14</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q5">I can find Haskell libraries for the things that I need.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 11.02%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">127</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 46.53%;" class="bar purple"/>
    <div class="percent">47%</div>
    <div class="count">536</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 23.09%;" class="bar purple"/>
    <div class="percent">23%</div>
    <div class="count">266</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 9.55%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">110</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 2.17%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">25</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q12">I think that Haskell libraries work well together.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 8.07%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">93</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 38.80%;" class="bar purple"/>
    <div class="percent">39%</div>
    <div class="count">447</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 29.25%;" class="bar purple"/>
    <div class="percent">29%</div>
    <div class="count">337</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 8.07%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">93</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.39%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">16</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q11">I think that Haskell libraries provide a stable API.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 6.60%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">76</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 37.93%;" class="bar purple"/>
    <div class="percent">38%</div>
    <div class="count">437</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 30.21%;" class="bar purple"/>
    <div class="percent">30%</div>
    <div class="count">348</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 8.59%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">99</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 1.74%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">20</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q3">I am satisfied with Haskell's build tools, such as Cabal.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 10.07%;" class="bar purple"/>
    <div class="percent">10%</div>
    <div class="count">116</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 38.98%;" class="bar purple"/>
    <div class="percent">39%</div>
    <div class="count">449</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 23.44%;" class="bar purple"/>
    <div class="percent">23%</div>
    <div class="count">270</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 15.02%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">173</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 4.08%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">47</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q21">Haskell is critical to my company's success.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 15.28%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">176</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 12.50%;" class="bar purple"/>
    <div class="percent">12%</div>
    <div class="count">144</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 12.59%;" class="bar purple"/>
    <div class="percent">13%</div>
    <div class="count">145</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 6.77%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">78</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 5.90%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">68</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q7">I have a good understanding of Haskell best practices.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 7.38%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">85</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 34.29%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">395</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 27.43%;" class="bar purple"/>
    <div class="percent">27%</div>
    <div class="count">316</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 19.01%;" class="bar purple"/>
    <div class="percent">19%</div>
    <div class="count">219</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 5.21%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">60</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q10">I think that Haskell libraries are easy to use.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 3.82%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">44</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 27.78%;" class="bar purple"/>
    <div class="percent">28%</div>
    <div class="count">320</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 38.72%;" class="bar purple"/>
    <div class="percent">39%</div>
    <div class="count">446</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 15.80%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">182</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 3.73%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">43</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q23">As a hiring manager, I can easily find qualified Haskell candidates.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 2.17%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">25</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 5.03%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">58</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 10.76%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">124</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 5.90%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">68</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 3.99%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">46</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q8">I think Haskell libraries are well documented.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 3.73%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">43</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 25.61%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">295</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 32.20%;" class="bar purple"/>
    <div class="percent">32%</div>
    <div class="count">371</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 21.96%;" class="bar purple"/>
    <div class="percent">22%</div>
    <div class="count">253</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 8.42%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">97</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q9">I can easily compare competing Haskell libraries to select the best one.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 3.21%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">37</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 16.93%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">195</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 29.95%;" class="bar purple"/>
    <div class="percent">30%</div>
    <div class="count">345</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 30.99%;" class="bar purple"/>
    <div class="percent">31%</div>
    <div class="count">357</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 8.16%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">94</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q22">As a candidate, I can easily find Haskell jobs.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 3.12%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">36</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 10.76%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">124</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 15.80%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">182</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 16.93%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">195</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 15.71%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">181</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h3 id="s6q17">I can easily reason about the performance of my Haskell code.</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 2.43%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">28</div>
    <div class="choice"> Strongly agree</div>
  </div>
  <div class="row">
    <div style="width: 14.24%;" class="bar purple"/>
    <div class="percent">14%</div>
    <div class="count">164</div>
    <div class="choice">Agree</div>
  </div>
  <div class="row">
    <div style="width: 29.77%;" class="bar purple"/>
    <div class="percent">30%</div>
    <div class="count">343</div>
    <div class="choice">Neutral</div>
  </div>
  <div class="row">
    <div style="width: 30.56%;" class="bar purple"/>
    <div class="percent">31%</div>
    <div class="count">352</div>
    <div class="choice">Disagree</div>
  </div>
  <div class="row">
    <div style="width: 10.94%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">126</div>
    <div class="choice">Strongly disagree</div>
  </div>
</div>
<h2 id="s7">Demographics</h2>
<h3 id="s7q0">Which country do you live in?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 23.70%;" class="bar purple"/>
    <div class="percent">24%</div>
    <div class="count">273</div>
    <div class="choice">United States</div>
  </div>
  <div class="row">
    <div style="width: 8.77%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">101</div>
    <div class="choice">Germany</div>
  </div>
  <div class="row">
    <div style="width: 6.42%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">74</div>
    <div class="choice">United Kingdom</div>
  </div>
  <div class="row">
    <div style="width: 4.95%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">57</div>
    <div class="choice">Russia</div>
  </div>
  <div class="row">
    <div style="width: 3.99%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">46</div>
    <div class="choice">Canada</div>
  </div>
  <div class="row">
    <div style="width: 3.73%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">43</div>
    <div class="choice">Australia</div>
  </div>
  <div class="row">
    <div style="width: 3.56%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">41</div>
    <div class="choice">France</div>
  </div>
  <div class="row">
    <div style="width: 3.39%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">39</div>
    <div class="choice">Netherlands</div>
  </div>
  <div class="row">
    <div style="width: 3.12%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">36</div>
    <div class="choice">Sweden</div>
  </div>
  <div class="row">
    <div style="width: 2.60%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">30</div>
    <div class="choice">Japan</div>
  </div>
  <div class="row">
    <div style="width: 2.34%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">27</div>
    <div class="choice">Poland</div>
  </div>
  <div class="row">
    <div style="width: 2.26%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">26</div>
    <div class="choice">India</div>
  </div>
  <div class="row">
    <div style="width: 1.91%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">22</div>
    <div class="choice">Finland</div>
  </div>
  <div class="row">
    <div style="width: 1.65%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">19</div>
    <div class="choice">Norway</div>
  </div>
  <div class="row">
    <div style="width: 1.48%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">17</div>
    <div class="choice">Italy</div>
  </div>
  <div class="row">
    <div style="width: 1.39%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">16</div>
    <div class="choice">Austria</div>
  </div>
  <div class="row">
    <div style="width: 1.39%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">16</div>
    <div class="choice">Brazil</div>
  </div>
  <div class="row">
    <div style="width: 1.30%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">15</div>
    <div class="choice">Czech Republic</div>
  </div>
  <div class="row">
    <div style="width: 1.30%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">15</div>
    <div class="choice">Spain</div>
  </div>
  <div class="row">
    <div style="width: 1.30%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">15</div>
    <div class="choice">Ukraine</div>
  </div>
  <div class="row">
    <div style="width: 1.22%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">14</div>
    <div class="choice">China</div>
  </div>
  <div class="row">
    <div style="width: 1.13%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">13</div>
    <div class="choice">Belgium</div>
  </div>
  <div class="row">
    <div style="width: 1.04%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">12</div>
    <div class="choice">Denmark</div>
  </div>
  <div class="row">
    <div style="width: 1.04%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">12</div>
    <div class="choice">Switzerland</div>
  </div>
  <div class="row">
    <div style="width: 0.69%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">8</div>
    <div class="choice">Turkey</div>
  </div>
  <div class="row">
    <div style="width: 0.69%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">8</div>
    <div class="choice">Singapore</div>
  </div>
  <div class="row">
    <div style="width: 0.52%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">6</div>
    <div class="choice">Bulgaria</div>
  </div>
  <div class="row">
    <div style="width: 0.52%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">6</div>
    <div class="choice">Romania</div>
  </div>
  <div class="row">
    <div style="width: 0.43%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">5</div>
    <div class="choice">Israel</div>
  </div>
  <div class="row">
    <div style="width: 0.43%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">5</div>
    <div class="choice">South Korea</div>
  </div>
  <div class="row">
    <div style="width: 0.43%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">5</div>
    <div class="choice">New Zealand</div>
  </div>
  <div class="row">
    <div style="width: 0.35%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">4</div>
    <div class="choice">Argentina</div>
  </div>
  <div class="row">
    <div style="width: 0.35%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">4</div>
    <div class="choice">Belarus</div>
  </div>
  <div class="row">
    <div style="width: 0.35%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">4</div>
    <div class="choice">Croatia</div>
  </div>
  <div class="row">
    <div style="width: 0.35%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">4</div>
    <div class="choice">Ecuador</div>
  </div>
  <div class="row">
    <div style="width: 0.35%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">4</div>
    <div class="choice">Indonesia</div>
  </div>
  <div class="row">
    <div style="width: 0.35%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">4</div>
    <div class="choice">Ireland</div>
  </div>
  <div class="row">
    <div style="width: 0.26%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">3</div>
    <div class="choice">Hungary</div>
  </div>
  <div class="row">
    <div style="width: 0.26%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">3</div>
    <div class="choice">Iran</div>
  </div>
  <div class="row">
    <div style="width: 0.26%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">3</div>
    <div class="choice">South Africa</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Colombia</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Egypt</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Estonia</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Greece</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Kazakhstan</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Kenya</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Mexico</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Pakistan</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Peru</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Serbia and Montenegro</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Slovakia</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Thailand</div>
  </div>
  <div class="row">
    <div style="width: 0.17%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">2</div>
    <div class="choice">Vietnam</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Chile</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Honduras</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Hong Kong</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Iraq</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Lithuania</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Luxembourg</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Malaysia</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Nigeria</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Philippines</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Portugal</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Puerto Rico</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Taiwan</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Uganda</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Venezuela</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Western Sahara</div>
  </div>
  <div class="row">
    <div style="width: 0.09%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">1</div>
    <div class="choice">Zimbabwe</div>
  </div>
</div>
<h3 id="s7q1">Do you consider yourself a member of an underrepresented or marginalized group in technology?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 9.46%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">109</div>
    <div class="choice">Lesbian, gay, bisexual, queer or otherwise non-heterosexual</div>
  </div>
  <div class="row">
    <div style="width: 5.56%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">64</div>
    <div class="choice">Older or younger than the average developers I know</div>
  </div>
  <div class="row">
    <div style="width: 4.60%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">53</div>
    <div class="choice">Political beliefs</div>
  </div>
  <div class="row">
    <div style="width: 3.82%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">44</div>
    <div class="choice">Trans</div>
  </div>
  <div class="row">
    <div style="width: 3.47%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">40</div>
    <div class="choice">Woman or perceived as a woman</div>
  </div>
  <div class="row">
    <div style="width: 3.73%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">43</div>
    <div class="choice">Disabled or person with disability (including physical, mental, and other)</div>
  </div>
  <div class="row">
    <div style="width: 3.30%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">38</div>
    <div class="choice">Educational background</div>
  </div>
  <div class="row">
    <div style="width: 3.30%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">38</div>
    <div class="choice">Language</div>
  </div>
  <div class="row">
    <div style="width: 2.95%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">34</div>
    <div class="choice">Non-binary gender</div>
  </div>
  <div class="row">
    <div style="width: 2.95%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">34</div>
    <div class="choice">Racial or ethnic minority</div>
  </div>
  <div class="row">
    <div style="width: 2.60%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">30</div>
    <div class="choice">Religious beliefs</div>
  </div>
  <div class="row">
    <div style="width: 2.26%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">26</div>
    <div class="choice">Yes, but I prefer not to say which</div>
  </div>
  <div class="row">
    <div style="width: 2.08%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">24</div>
    <div class="choice">Cultural beliefs</div>
  </div>
  <div class="row">
    <div style="width: 1.91%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">22</div>
    <div class="choice">Other</div>
  </div>
</div>
<h3 id="s7q2">Do you feel your belonging to an underrepresented or marginalized group in technology makes it difficult for you to participate in the Haskell community?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 20.23%;" class="bar purple"/>
    <div class="percent">20%</div>
    <div class="count">233</div>
    <div class="choice">Never</div>
  </div>
  <div class="row">
    <div style="width: 6.34%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">73</div>
    <div class="choice">Sometimes</div>
  </div>
  <div class="row">
    <div style="width: 1.65%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">19</div>
    <div class="choice">Often</div>
  </div>
</div>
<h3 id="s7q3">Are you a student?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 71.01%;" class="bar purple"/>
    <div class="percent">71%</div>
    <div class="count">818</div>
    <div class="choice">No</div>
  </div>
  <div class="row">
    <div style="width: 13.80%;" class="bar purple"/>
    <div class="percent">14%</div>
    <div class="count">159</div>
    <div class="choice">Yes, full time</div>
  </div>
  <div class="row">
    <div style="width: 5.03%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">58</div>
    <div class="choice">Yes, part time</div>
  </div>
</div>
<h3 id="s7q4">What is the highest level of education you have completed?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 1.82%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">21</div>
    <div class="choice">Less than high school diploma</div>
  </div>
  <div class="row">
    <div style="width: 7.47%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">86</div>
    <div class="choice">High school diploma</div>
  </div>
  <div class="row">
    <div style="width: 9.20%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">106</div>
    <div class="choice">Some college</div>
  </div>
  <div class="row">
    <div style="width: 1.22%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">14</div>
    <div class="choice">Associate degree</div>
  </div>
  <div class="row">
    <div style="width: 34.29%;" class="bar purple"/>
    <div class="percent">34%</div>
    <div class="count">395</div>
    <div class="choice">Bachelor's degree</div>
  </div>
  <div class="row">
    <div style="width: 26.91%;" class="bar purple"/>
    <div class="percent">27%</div>
    <div class="count">310</div>
    <div class="choice">Master's degree</div>
  </div>
  <div class="row">
    <div style="width: 2.17%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">25</div>
    <div class="choice">Professional degree</div>
  </div>
  <div class="row">
    <div style="width: 11.28%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">130</div>
    <div class="choice">Doctoral degree</div>
  </div>
</div>
<h3 id="s7q5">What is your employment status?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 66.84%;" class="bar purple"/>
    <div class="percent">67%</div>
    <div class="count">770</div>
    <div class="choice">Employed full time</div>
  </div>
  <div class="row">
    <div style="width: 8.77%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">101</div>
    <div class="choice">Employed part time</div>
  </div>
  <div class="row">
    <div style="width: 7.38%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">85</div>
    <div class="choice">Self employed</div>
  </div>
  <div class="row">
    <div style="width: 4.17%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">48</div>
    <div class="choice">Not employed, and not looking for work</div>
  </div>
  <div class="row">
    <div style="width: 4.08%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">47</div>
    <div class="choice">Not employed, but looking for work</div>
  </div>
  <div class="row">
    <div style="width: 0.69%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">8</div>
    <div class="choice">Retired</div>
  </div>
</div>
<h3 id="s7q6">How large is the company you work for?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 10.85%;" class="bar purple"/>
    <div class="percent">11%</div>
    <div class="count">125</div>
    <div class="choice">Fewer than 10 employees</div>
  </div>
  <div class="row">
    <div style="width: 26.91%;" class="bar purple"/>
    <div class="percent">27%</div>
    <div class="count">310</div>
    <div class="choice">10 to 99 employees</div>
  </div>
  <div class="row">
    <div style="width: 19.53%;" class="bar purple"/>
    <div class="percent">20%</div>
    <div class="count">225</div>
    <div class="choice">100 to 999 employees</div>
  </div>
  <div class="row">
    <div style="width: 23.00%;" class="bar purple"/>
    <div class="percent">23%</div>
    <div class="count">265</div>
    <div class="choice">More than 1,000 employees</div>
  </div>
</div>
<h3 id="s7q7">How many years have you been coding?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 6.34%;" class="bar purple"/>
    <div class="percent">6%</div>
    <div class="count">73</div>
    <div class="choice">0 to 4 years</div>
  </div>
  <div class="row">
    <div style="width: 23.35%;" class="bar purple"/>
    <div class="percent">23%</div>
    <div class="count">269</div>
    <div class="choice">5 to 9 years</div>
  </div>
  <div class="row">
    <div style="width: 22.31%;" class="bar purple"/>
    <div class="percent">22%</div>
    <div class="count">257</div>
    <div class="choice">10 to 14 years</div>
  </div>
  <div class="row">
    <div style="width: 15.54%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">179</div>
    <div class="choice">15 to 19 years</div>
  </div>
  <div class="row">
    <div style="width: 11.63%;" class="bar purple"/>
    <div class="percent">12%</div>
    <div class="count">134</div>
    <div class="choice">20 to 24 years</div>
  </div>
  <div class="row">
    <div style="width: 6.68%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">77</div>
    <div class="choice">25 to 29 years</div>
  </div>
  <div class="row">
    <div style="width: 8.94%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">103</div>
    <div class="choice">30 or more years</div>
  </div>
</div>
<h3 id="s7q8">How many years have you been coding professionally?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 28.47%;" class="bar purple"/>
    <div class="percent">28%</div>
    <div class="count">328</div>
    <div class="choice">0 to 4 years</div>
  </div>
  <div class="row">
    <div style="width: 25.52%;" class="bar purple"/>
    <div class="percent">26%</div>
    <div class="count">294</div>
    <div class="choice">5 to 9 years</div>
  </div>
  <div class="row">
    <div style="width: 15.45%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">178</div>
    <div class="choice">10 to 14 years</div>
  </div>
  <div class="row">
    <div style="width: 8.59%;" class="bar purple"/>
    <div class="percent">9%</div>
    <div class="count">99</div>
    <div class="choice">15 to 19 years</div>
  </div>
  <div class="row">
    <div style="width: 7.03%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">81</div>
    <div class="choice">20 to 24 years</div>
  </div>
  <div class="row">
    <div style="width: 2.69%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">31</div>
    <div class="choice">25 to 29 years</div>
  </div>
  <div class="row">
    <div style="width: 2.17%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">25</div>
    <div class="choice">30 or more years</div>
  </div>
</div>
<h3 id="s7q9">Do you code as a hobby?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 88.54%;" class="bar purple"/>
    <div class="percent">89%</div>
    <div class="count">1020</div>
    <div class="choice">Yes</div>
  </div>
  <div class="row">
    <div style="width: 5.38%;" class="bar purple"/>
    <div class="percent">5%</div>
    <div class="count">62</div>
    <div class="choice">No</div>
  </div>
</div>
<h3 id="s7q10">Have you contributed to any open source projects?</h3>
<p>Optional. Single select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 76.91%;" class="bar purple"/>
    <div class="percent">77%</div>
    <div class="count">886</div>
    <div class="choice">Yes</div>
  </div>
  <div class="row">
    <div style="width: 16.84%;" class="bar purple"/>
    <div class="percent">17%</div>
    <div class="count">194</div>
    <div class="choice">No</div>
  </div>
</div>
<h2 id="s8">Meta</h2>
<h3 id="s8q0">Did you take any previous surveys?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 40.54%;" class="bar purple"/>
    <div class="percent">41%</div>
    <div class="count">467</div>
    <div class="choice">2020</div>
  </div>
  <div class="row">
    <div style="width: 32.47%;" class="bar purple"/>
    <div class="percent">32%</div>
    <div class="count">374</div>
    <div class="choice">2019</div>
  </div>
  <div class="row">
    <div style="width: 17.97%;" class="bar purple"/>
    <div class="percent">18%</div>
    <div class="count">207</div>
    <div class="choice">2018</div>
  </div>
  <div class="row">
    <div style="width: 11.72%;" class="bar purple"/>
    <div class="percent">12%</div>
    <div class="count">135</div>
    <div class="choice">2017</div>
  </div>
</div>
<h3 id="s8q1">How did you hear about this survey?</h3>
<p>Optional. Multi select.</p>
<div class="answer">
  <div class="row">
    <div style="width: 31.68%;" class="bar purple"/>
    <div class="percent">32%</div>
    <div class="count">365</div>
    <div class="choice">Reddit</div>
  </div>
  <div class="row">
    <div style="width: 15.89%;" class="bar purple"/>
    <div class="percent">16%</div>
    <div class="count">183</div>
    <div class="choice">Twitter</div>
  </div>
  <div class="row">
    <div style="width: 15.45%;" class="bar purple"/>
    <div class="percent">15%</div>
    <div class="count">178</div>
    <div class="choice">Haskell Weekly</div>
  </div>
  <div class="row">
    <div style="width: 7.64%;" class="bar purple"/>
    <div class="percent">8%</div>
    <div class="count">88</div>
    <div class="choice">Slack</div>
  </div>
  <div class="row">
    <div style="width: 7.20%;" class="bar purple"/>
    <div class="percent">7%</div>
    <div class="count">83</div>
    <div class="choice">Lobsters</div>
  </div>
  <div class="row">
    <div style="width: 4.34%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">50</div>
    <div class="choice">Discord</div>
  </div>
  <div class="row">
    <div style="width: 4.17%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">48</div>
    <div class="choice">Telegram</div>
  </div>
  <div class="row">
    <div style="width: 3.12%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">36</div>
    <div class="choice">Discourse</div>
  </div>
  <div class="row">
    <div style="width: 2.60%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">30</div>
    <div class="choice">In person</div>
  </div>
  <div class="row">
    <div style="width: 2.52%;" class="bar purple"/>
    <div class="percent">3%</div>
    <div class="count">29</div>
    <div class="choice">Mailing lists</div>
  </div>
  <div class="row">
    <div style="width: 1.65%;" class="bar purple"/>
    <div class="percent">2%</div>
    <div class="count">19</div>
    <div class="choice">IRC</div>
  </div>
  <div class="row">
    <div style="width: 0.78%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">9</div>
    <div class="choice">Matrix/Riot</div>
  </div>
  <div class="row">
    <div style="width: 0.69%;" class="bar purple"/>
    <div class="percent">1%</div>
    <div class="count">8</div>
    <div class="choice">Mastodon</div>
  </div>
  <div class="row">
    <div style="width: 0.35%;" class="bar purple"/>
    <div class="percent">0%</div>
    <div class="count">4</div>
    <div class="choice">GitHub</div>
  </div>
  <div class="row">
    <div style="width: 3.91%;" class="bar purple"/>
    <div class="percent">4%</div>
    <div class="count">45</div>
    <div class="choice">Other</div>
  </div>
</div>
<h2 id="s9">Free response</h2>
<h3 id="s9q0">If you wanted to convince someone to use Haskell, what would you say?</h3>
<p>Optional. Free response answers were collected but not analyzed.</p>
<h3 id="s9q1">If you could change one thing about Haskell, what would it be?</h3>
<p>Optional. Free response answers were collected but not analyzed.</p>

[2020]: {% post_url 2020-11-22-haskell-survey-results %}
[2019]: {% post_url 2019-11-16-haskell-survey-results %}
[2018]: {% post_url 2018-11-18-2018-state-of-haskell-survey-results %}
[2017]: {% post_url 2017-11-15-2017-state-of-haskell-survey-results %}
[JSON]: /static/pages/2021-11-16-haskell-survey-results.json.zip
[CSV]: /static/pages/2021-11-16-haskell-survey-results.csv.zip
[ODbL]: https://opendatacommons.org/licenses/odbl/
