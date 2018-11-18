---
title: 2018 state of Haskell survey results
---

The [2018 state of Haskell survey][1] opened at the start of November and closed a few days ago.
It was the second annual state of Haskell survey.
This year the survey was co-sponsored by Haskell Weekly and Haskell.org.

Last year the [2017 state of Haskell survey][2] received 1,335 responses.
This year we received almost four times as many.
However a large number of the responses appeared to be submitted by a script.
Of the 5,096 responses, 3,735 were identified as problematic and removed from the result set.
That left 1,361 legitimate responses, which compares favorably to last year, especially when you consider that this year's survey was much longer.
To everyone that participated in the survey:
Thank you!
You feedback is hugely valuable.

I am extremely disappointed that someone decided to submit thousands of bogus survey responses.
Apparently they wanted to tip the scales on a few divisive issues in the community.
Fortunately their attack was not very sophisticated, so it was easy to identify and ignore their responses.
([This function][5] shows you exactly how the problematic responses were identified.)
Even so, it has two profoundly negative impacts:
One, it casts doubt on the survey responses as a whole;
and two, it means that I'll have to devote time to countering attacks like this in future surveys.
That being said, I have confidence in the filtered survey results and I hope you do too.

This post will graph the responses to the multiple-choice questions.
I will not attempt to summarize the responses free-response questions.
I encourage any interested parties to download the raw results, do their own analysis, and share the results.
You can download the anonymized responses as a [CSV][3], which is licensed under the [ODbL 1.0 license][4].

- [Submission date](#question-001)
- [Haskell usage](#question-002)
  - [Do you use Haskell?](#question-003)
  - [If you stopped using Haskell, how long did you use it before you stopped?](#question-004)
  - [If you do not use Haskell, why not?](#question-005)
  - [How long have you been using Haskell?](#question-007)
  - [How frequently do you use Haskell?](#question-008)
  - [How would you rate your proficiency in Haskell?](#question-009)
  - [Where do you use Haskell?](#question-010)
  - [Do you use Haskell at work?](#question-011)
  - [If you do not use Haskell at work, why not?](#question-012)
  - [Which programming languages other than Haskell are you fluent in?](#question-014)
  - [Which types of software do you develop with Haskell?](#question-016)
  - [Which industries do you use Haskell in?](#question-018)
- [Projects](#question-020)
  - [How many Haskell projects do you contribute to?](#question-021)
  - [What is the total size of all the Haskell projects you contribute to?](#question-022)
  - [Which platforms do you develop Haskell on?](#question-023)
  - [Which platforms do you target?](#question-025)
- [Compilers](#question-027)
  - [Which Haskell compilers do you use?](#question-028)
  - [Which installation methods do you use for your Haskell compiler?](#question-030)
  - [Has upgrading your Haskell compiler broken your code in the last year?](#question-032)
  - [How has upgrading your Haskell compiler broken your code in the past year?](#question-033)
  - [Which versions of GHC do you use?](#question-035)
  - [How do you feel about the new GHC release schedule?](#question-036)
  - [Which GHC language extensions would you like to be enabled by default?](#question-038)
  - [How important do you feel it would be to have a new version of the Haskell standard?](#question-039)
- [Tooling](#question-040)
  - [Which build tools do you use for Haskell?](#question-041)
  - [Which editors do you use for Haskell?](#question-043)
  - [Which version control systems do you use for Haskell?](#question-045)
  - [Where do you get Haskell packages from?](#question-047)
  - [Which libraries do you use to test Haskell code?](#question-049)
  - [Which libraries do you use to benchmark Haskell code?](#question-051)
- [Infrastructure](#question-053)
  - [Which tools do you use to deploy Haskell applications?](#question-054)
  - [Where do you deploy Haskell applications?](#question-056)
- [Community](#question-058)
  - [Where do you interact with the Haskell community?](#question-059)
  - [Which of the following Haskell topics would you like to see more written about?](#question-061)
- [Feelings](#question-063)
  - [I feel welcome in the Haskell community.](#question-064)
  - [I am satisfied with Haskell as a language.](#question-065)
  - [I am satisfied with Haskell's compilers, such as GHC.](#question-067)
  - [I am satisfied with Haskell's build tools, such as Cabal.](#question-069)
  - [I am satisfied with Haskell's package repositories, such as Hackage.](#question-071)
  - [I can find Haskell libraries for the things that I need.](#question-073)
  - [I think Haskell libraries are high quality.](#question-074)
  - [I have a good understanding of Haskell best practices.](#question-075)
  - [I think Haskell libraries are well documented.](#question-076)
  - [I can easily compare competing Haskell libraries to select the best one.](#question-077)
  - [I think that Haskell libraries are easy to use.](#question-078)
  - [I think that Haskell libraries provide a stable API.](#question-079)
  - [I think that Haskell libraries work well together.](#question-080)
  - [I think that software written in Haskell is easy to maintain.](#question-081)
  - [Once my Haskell program compiles, it generally does what I intended.](#question-082)
  - [I think that Haskell libraries perform well.](#question-083)
  - [Haskell's performance meets my needs.](#question-084)
  - [I can easily reason about the performance of my Haskell code.](#question-085)
  - [I would recommend using Haskell to others.](#question-086)
  - [I would prefer to use Haskell for my next new project.](#question-087)
  - [Haskell is working well for my team.](#question-088)
  - [Haskell is critical to my company's success.](#question-089)
  - [As a candidate, I can easily find Haskell jobs.](#question-090)
  - [As a hiring manager, I can easily find qualified Haskell candidates.](#question-091)
- [Demographics](#question-094)
  - [Which country do you live in?](#question-095)
  - [How old are you?](#question-096)
  - [What is your gender?](#question-097)
  - [Do you identify as transgender?](#question-098)
  - [Are you a student?](#question-099)
  - [What is the highest level of education you have completed?](#question-100)
  - [What is your employment status?](#question-101)
  - [How large is the company you work for?](#question-102)
  - [How many years have you been coding?](#question-103)
  - [How many years have you been coding professionally?](#question-104)
  - [Do you code as a hobby?](#question-105)
  - [Have you contributed to any open source projects?](#question-106)
- [Meta survey](#question-107)
  - [Did you take last year's survey?](#question-108)
  - [How did you hear about this survey?](#question-109)

---

<h2 id='question-001'><a href='#question-001'>#</a> Submission date <a href='#'>^</a></h2>
<a href='question-001.svg'><img src='/static/images/2018/11/18/question-001.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>2018-11-01</td><td>513</td><td>37.7%</td></tr>
<tr><td>2018-11-02</td><td>310</td><td>22.8%</td></tr>
<tr><td>2018-11-03</td><td>118</td><td>8.7%</td></tr>
<tr><td>2018-11-04</td><td>75</td><td>5.5%</td></tr>
<tr><td>2018-11-05</td><td>63</td><td>4.6%</td></tr>
<tr><td>2018-11-06</td><td>56</td><td>4.1%</td></tr>
<tr><td>2018-11-07</td><td>32</td><td>2.4%</td></tr>
<tr><td>2018-11-08</td><td>44</td><td>3.2%</td></tr>
<tr><td>2018-11-09</td><td>22</td><td>1.6%</td></tr>
<tr><td>2018-11-10</td><td>12</td><td>0.9%</td></tr>
<tr><td>2018-11-11</td><td>16</td><td>1.2%</td></tr>
<tr><td>2018-11-12</td><td>28</td><td>2.1%</td></tr>
<tr><td>2018-11-13</td><td>35</td><td>2.6%</td></tr>
<tr><td>2018-11-14</td><td>37</td><td>2.7%</td></tr>
</tbody>
</table>

<h2 id='question-002'><a href='#question-002'>#</a> Haskell usage <a href='#'>^</a></h2>
<a href='question-002.svg'><img src='/static/images/2018/11/18/question-002.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>4</td><td>0.3%</td></tr>
<tr><td>2 hearts</td><td>1</td><td>0.1%</td></tr>
<tr><td>3 hearts</td><td>4</td><td>0.3%</td></tr>
<tr><td>4 hearts</td><td>4</td><td>0.3%</td></tr>
<tr><td>5 hearts</td><td>35</td><td>2.6%</td></tr>
<tr><td>6 hearts</td><td>16</td><td>1.2%</td></tr>
<tr><td>7 hearts</td><td>40</td><td>2.9%</td></tr>
<tr><td>8 hearts</td><td>84</td><td>6.2%</td></tr>
<tr><td>9 hearts</td><td>55</td><td>4.0%</td></tr>
<tr><td>10 hearts</td><td>501</td><td>36.8%</td></tr>
</tbody>
</table>

<h2 id='question-003'><a href='#question-003'>#</a> Do you use Haskell? (single select) <a href='#'>^</a></h2>
<a href='question-003.svg'><img src='/static/images/2018/11/18/question-003.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Yes</td><td>1202</td><td>88.3%</td></tr>
<tr><td>No, but I used to</td><td>103</td><td>7.6%</td></tr>
<tr><td>No, I never have</td><td>45</td><td>3.3%</td></tr>
</tbody>
</table>

<h2 id='question-004'><a href='#question-004'>#</a> If you stopped using Haskell, how long did you use it before you stopped? (single select) <a href='#'>^</a></h2>
<a href='question-004.svg'><img src='/static/images/2018/11/18/question-004.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Less than 1 day</td><td>6</td><td>0.4%</td></tr>
<tr><td>1 day to 1 week</td><td>11</td><td>0.8%</td></tr>
<tr><td>1 week to 1 month</td><td>20</td><td>1.5%</td></tr>
<tr><td>1 month to 1 year</td><td>66</td><td>4.8%</td></tr>
<tr><td>More than 1 year</td><td>53</td><td>3.9%</td></tr>
</tbody>
</table>

<h2 id='question-005'><a href='#question-005'>#</a> If you do not use Haskell, why not? (multiple select) <a href='#'>^</a></h2>
<a href='question-005.svg'><img src='/static/images/2018/11/18/question-005.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>My company doesn&apos;t use Haskell</td><td>89</td><td>6.5%</td></tr>
<tr><td>Haskell is too hard to learn</td><td>44</td><td>3.2%</td></tr>
<tr><td>Haskell&apos;s documentation is not good enough</td><td>35</td><td>2.6%</td></tr>
<tr><td>Haskell lacks critical tools</td><td>31</td><td>2.3%</td></tr>
<tr><td>Haskell lacks critical libraries</td><td>30</td><td>2.2%</td></tr>
<tr><td>Haskell does not support the platforms I need</td><td>12</td><td>0.9%</td></tr>
<tr><td>Haskell&apos;s performance is not good enough</td><td>12</td><td>0.9%</td></tr>
<tr><td>Haskell lacks critical features</td><td>8</td><td>0.6%</td></tr>
</tbody>
</table>

<h2 id='question-007'><a href='#question-007'>#</a> How long have you been using Haskell? (single select) <a href='#'>^</a></h2>
<a href='question-007.svg'><img src='/static/images/2018/11/18/question-007.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Less than 1 day</td><td>3</td><td>0.2%</td></tr>
<tr><td>1 day to 1 week</td><td>7</td><td>0.5%</td></tr>
<tr><td>1 week to 1 month</td><td>35</td><td>2.6%</td></tr>
<tr><td>1 month to 1 year</td><td>219</td><td>16.1%</td></tr>
<tr><td>1 year to 2 years</td><td>234</td><td>17.2%</td></tr>
<tr><td>2 years to 3 years</td><td>213</td><td>15.7%</td></tr>
<tr><td>3 years to 4 years</td><td>147</td><td>10.8%</td></tr>
<tr><td>4 years to 5 years</td><td>112</td><td>8.2%</td></tr>
<tr><td>More than 5 years</td><td>330</td><td>24.2%</td></tr>
</tbody>
</table>

<h2 id='question-008'><a href='#question-008'>#</a> How frequently do you use Haskell? (single select) <a href='#'>^</a></h2>
<a href='question-008.svg'><img src='/static/images/2018/11/18/question-008.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Daily</td><td>490</td><td>36.0%</td></tr>
<tr><td>Weekly</td><td>422</td><td>31.0%</td></tr>
<tr><td>Monthly</td><td>249</td><td>18.3%</td></tr>
<tr><td>Yearly</td><td>58</td><td>4.3%</td></tr>
<tr><td>Rarely</td><td>80</td><td>5.9%</td></tr>
</tbody>
</table>

<h2 id='question-009'><a href='#question-009'>#</a> How would you rate your proficiency in Haskell? (single select) <a href='#'>^</a></h2>
<a href='question-009.svg'><img src='/static/images/2018/11/18/question-009.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Beginner</td><td>247</td><td>18.1%</td></tr>
<tr><td>Intermediate</td><td>533</td><td>39.2%</td></tr>
<tr><td>Advanced</td><td>354</td><td>26.0%</td></tr>
<tr><td>Expert</td><td>167</td><td>12.3%</td></tr>
<tr><td>Master</td><td>28</td><td>2.1%</td></tr>
</tbody>
</table>

<h2 id='question-010'><a href='#question-010'>#</a> Where do you use Haskell? (multiple select) <a href='#'>^</a></h2>
<a href='question-010.svg'><img src='/static/images/2018/11/18/question-010.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Home</td><td>1077</td><td>79.1%</td></tr>
<tr><td>Work</td><td>666</td><td>48.9%</td></tr>
<tr><td>School</td><td>153</td><td>11.2%</td></tr>
</tbody>
</table>

<h2 id='question-011'><a href='#question-011'>#</a> Do you use Haskell at work? (single select) <a href='#'>^</a></h2>
<a href='question-011.svg'><img src='/static/images/2018/11/18/question-011.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Yes, most of the time</td><td>389</td><td>28.6%</td></tr>
<tr><td>Yes, some of the time</td><td>318</td><td>23.4%</td></tr>
<tr><td>No, but my company does</td><td>20</td><td>1.5%</td></tr>
<tr><td>No</td><td>558</td><td>41.0%</td></tr>
</tbody>
</table>

<h2 id='question-012'><a href='#question-012'>#</a> If you do not use Haskell at work, why not? (multiple select) <a href='#'>^</a></h2>
<a href='question-012.svg'><img src='/static/images/2018/11/18/question-012.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>My company doesn&apos;t use Haskell</td><td>438</td><td>32.2%</td></tr>
<tr><td>Haskell is too hard to learn</td><td>76</td><td>5.6%</td></tr>
<tr><td>It is too hard to hire Haskell developers</td><td>72</td><td>5.3%</td></tr>
<tr><td>Haskell lacks critical libraries</td><td>48</td><td>3.5%</td></tr>
<tr><td>Haskell lacks critical tools</td><td>41</td><td>3.0%</td></tr>
<tr><td>Haskell does not support the platforms I need</td><td>33</td><td>2.4%</td></tr>
<tr><td>Haskell&apos;s documentation is not good enough</td><td>31</td><td>2.3%</td></tr>
<tr><td>Haskell&apos;s performance is not good enough</td><td>12</td><td>0.9%</td></tr>
<tr><td>Haskell lacks critical features</td><td>8</td><td>0.6%</td></tr>
</tbody>
</table>

<h2 id='question-014'><a href='#question-014'>#</a> Which programming languages other than Haskell are you fluent in? (multiple select) <a href='#'>^</a></h2>
<a href='question-014.svg'><img src='/static/images/2018/11/18/question-014.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Python</td><td>659</td><td>48.4%</td></tr>
<tr><td>JavaScript</td><td>599</td><td>44.0%</td></tr>
<tr><td>Java</td><td>550</td><td>40.4%</td></tr>
<tr><td>C</td><td>512</td><td>37.6%</td></tr>
<tr><td>C++</td><td>394</td><td>28.9%</td></tr>
<tr><td>Shell</td><td>320</td><td>23.5%</td></tr>
<tr><td>C#</td><td>203</td><td>14.9%</td></tr>
<tr><td>Scala</td><td>198</td><td>14.5%</td></tr>
<tr><td>Ruby</td><td>174</td><td>12.8%</td></tr>
<tr><td>Rust</td><td>158</td><td>11.6%</td></tr>
<tr><td>TypeScript</td><td>146</td><td>10.7%</td></tr>
<tr><td>PHP</td><td>142</td><td>10.4%</td></tr>
<tr><td>Go</td><td>110</td><td>8.1%</td></tr>
<tr><td>Clojure</td><td>93</td><td>6.8%</td></tr>
<tr><td>Assembly</td><td>90</td><td>6.6%</td></tr>
<tr><td>Ocaml</td><td>87</td><td>6.4%</td></tr>
<tr><td>Perl</td><td>83</td><td>6.1%</td></tr>
<tr><td>R</td><td>65</td><td>4.8%</td></tr>
<tr><td>Lua</td><td>64</td><td>4.7%</td></tr>
<tr><td>F#</td><td>64</td><td>4.7%</td></tr>
<tr><td>Erlang</td><td>63</td><td>4.6%</td></tr>
<tr><td>Matlab</td><td>50</td><td>3.7%</td></tr>
<tr><td>Objective-C</td><td>47</td><td>3.5%</td></tr>
<tr><td>Swift</td><td>46</td><td>3.4%</td></tr>
<tr><td>Kotlin</td><td>46</td><td>3.4%</td></tr>
<tr><td>Groovy</td><td>21</td><td>1.5%</td></tr>
<tr><td>VB.NET</td><td>20</td><td>1.5%</td></tr>
<tr><td>VBA</td><td>15</td><td>1.1%</td></tr>
<tr><td>Julia</td><td>9</td><td>0.7%</td></tr>
<tr><td>Hack</td><td>5</td><td>0.4%</td></tr>
</tbody>
</table>

<h2 id='question-016'><a href='#question-016'>#</a> Which types of software do you develop with Haskell? (multiple select) <a href='#'>^</a></h2>
<a href='question-016.svg'><img src='/static/images/2018/11/18/question-016.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Command-line programs (CLI)</td><td>684</td><td>50.3%</td></tr>
<tr><td>API services (returning non-HTML)</td><td>532</td><td>39.1%</td></tr>
<tr><td>Data processing</td><td>437</td><td>32.1%</td></tr>
<tr><td>Libraries or frameworks</td><td>433</td><td>31.8%</td></tr>
<tr><td>Web services (returning HTML)</td><td>371</td><td>27.3%</td></tr>
<tr><td>Automation or scripts</td><td>358</td><td>26.3%</td></tr>
<tr><td>Agents or daemons</td><td>219</td><td>16.1%</td></tr>
<tr><td>Desktop programs (GUI)</td><td>108</td><td>7.9%</td></tr>
</tbody>
</table>

<h2 id='question-018'><a href='#question-018'>#</a> Which industries do you use Haskell in? (multiple select) <a href='#'>^</a></h2>
<a href='question-018.svg'><img src='/static/images/2018/11/18/question-018.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Web</td><td>329</td><td>24.2%</td></tr>
<tr><td>Banking or finance</td><td>185</td><td>13.6%</td></tr>
<tr><td>Education</td><td>184</td><td>13.5%</td></tr>
<tr><td>Commerce or retail</td><td>127</td><td>9.3%</td></tr>
<tr><td>Gaming</td><td>59</td><td>4.3%</td></tr>
<tr><td>Healthcare or medical</td><td>43</td><td>3.2%</td></tr>
<tr><td>Government</td><td>35</td><td>2.6%</td></tr>
<tr><td>Embedded</td><td>33</td><td>2.4%</td></tr>
<tr><td>Mobile</td><td>32</td><td>2.4%</td></tr>
</tbody>
</table>

<h2 id='question-020'><a href='#question-020'>#</a> Projects <a href='#'>^</a></h2>
<a href='question-020.svg'><img src='/static/images/2018/11/18/question-020.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>5</td><td>0.4%</td></tr>
<tr><td>2 hearts</td><td>0</td><td>0.0%</td></tr>
<tr><td>3 hearts</td><td>4</td><td>0.3%</td></tr>
<tr><td>4 hearts</td><td>13</td><td>1.0%</td></tr>
<tr><td>5 hearts</td><td>28</td><td>2.1%</td></tr>
<tr><td>6 hearts</td><td>40</td><td>2.9%</td></tr>
<tr><td>7 hearts</td><td>78</td><td>5.7%</td></tr>
<tr><td>8 hearts</td><td>115</td><td>8.4%</td></tr>
<tr><td>9 hearts</td><td>76</td><td>5.6%</td></tr>
<tr><td>10 hearts</td><td>356</td><td>26.2%</td></tr>
</tbody>
</table>

<h2 id='question-021'><a href='#question-021'>#</a> How many Haskell projects do you contribute to? (single select) <a href='#'>^</a></h2>
<a href='question-021.svg'><img src='/static/images/2018/11/18/question-021.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>0</td><td>432</td><td>31.7%</td></tr>
<tr><td>1</td><td>168</td><td>12.3%</td></tr>
<tr><td>2 to 5</td><td>484</td><td>35.6%</td></tr>
<tr><td>6 to 10</td><td>94</td><td>6.9%</td></tr>
<tr><td>11 to 20</td><td>34</td><td>2.5%</td></tr>
<tr><td>More than 20</td><td>32</td><td>2.4%</td></tr>
</tbody>
</table>

<h2 id='question-022'><a href='#question-022'>#</a> What is the total size of all the Haskell projects you contribute to? (single select) <a href='#'>^</a></h2>
<a href='question-022.svg'><img src='/static/images/2018/11/18/question-022.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Less than 1,000 lines of code</td><td>207</td><td>15.2%</td></tr>
<tr><td>1,000 lines of code to 9,999 lines of code</td><td>311</td><td>22.9%</td></tr>
<tr><td>10,000 lines of code to 99,999 lines of code</td><td>252</td><td>18.5%</td></tr>
<tr><td>100,000 or more lines of code</td><td>115</td><td>8.4%</td></tr>
</tbody>
</table>

<h2 id='question-023'><a href='#question-023'>#</a> Which platforms do you develop Haskell on? (multiple select) <a href='#'>^</a></h2>
<a href='question-023.svg'><img src='/static/images/2018/11/18/question-023.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Linux</td><td>1003</td><td>73.7%</td></tr>
<tr><td>MacOS</td><td>496</td><td>36.4%</td></tr>
<tr><td>Windows</td><td>209</td><td>15.4%</td></tr>
<tr><td>BSD</td><td>44</td><td>3.2%</td></tr>
</tbody>
</table>

<h2 id='question-025'><a href='#question-025'>#</a> Which platforms do you target? (multiple select) <a href='#'>^</a></h2>
<a href='question-025.svg'><img src='/static/images/2018/11/18/question-025.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Linux</td><td>1075</td><td>79.0%</td></tr>
<tr><td>MacOS</td><td>399</td><td>29.3%</td></tr>
<tr><td>Windows</td><td>314</td><td>23.1%</td></tr>
<tr><td>BSD</td><td>80</td><td>5.9%</td></tr>
<tr><td>Android</td><td>38</td><td>2.8%</td></tr>
<tr><td>iOS</td><td>33</td><td>2.4%</td></tr>
</tbody>
</table>

<h2 id='question-027'><a href='#question-027'>#</a> Compilers <a href='#'>^</a></h2>
<a href='question-027.svg'><img src='/static/images/2018/11/18/question-027.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>4</td><td>0.3%</td></tr>
<tr><td>2 hearts</td><td>1</td><td>0.1%</td></tr>
<tr><td>3 hearts</td><td>3</td><td>0.2%</td></tr>
<tr><td>4 hearts</td><td>9</td><td>0.7%</td></tr>
<tr><td>5 hearts</td><td>25</td><td>1.8%</td></tr>
<tr><td>6 hearts</td><td>41</td><td>3.0%</td></tr>
<tr><td>7 hearts</td><td>50</td><td>3.7%</td></tr>
<tr><td>8 hearts</td><td>114</td><td>8.4%</td></tr>
<tr><td>9 hearts</td><td>80</td><td>5.9%</td></tr>
<tr><td>10 hearts</td><td>368</td><td>27.0%</td></tr>
</tbody>
</table>

<h2 id='question-028'><a href='#question-028'>#</a> Which Haskell compilers do you use? (multiple select) <a href='#'>^</a></h2>
<a href='question-028.svg'><img src='/static/images/2018/11/18/question-028.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>GHC</td><td>1296</td><td>95.2%</td></tr>
<tr><td>GHCJS</td><td>131</td><td>9.6%</td></tr>
<tr><td>Eta</td><td>37</td><td>2.7%</td></tr>
</tbody>
</table>

<h2 id='question-030'><a href='#question-030'>#</a> Which installation methods do you use for your Haskell compiler? (multiple select) <a href='#'>^</a></h2>
<a href='question-030.svg'><img src='/static/images/2018/11/18/question-030.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Stack</td><td>916</td><td>67.3%</td></tr>
<tr><td>Nix</td><td>299</td><td>22.0%</td></tr>
<tr><td>Operating system package</td><td>277</td><td>20.4%</td></tr>
<tr><td>Haskell Platform</td><td>153</td><td>11.2%</td></tr>
<tr><td>Official binaries</td><td>149</td><td>10.9%</td></tr>
<tr><td>Source</td><td>93</td><td>6.8%</td></tr>
<tr><td>Minimal installer</td><td>35</td><td>2.6%</td></tr>
</tbody>
</table>

<h2 id='question-032'><a href='#question-032'>#</a> Has upgrading your Haskell compiler broken your code in the last year? (single select) <a href='#'>^</a></h2>
<a href='question-032.svg'><img src='/static/images/2018/11/18/question-032.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>No</td><td>736</td><td>54.1%</td></tr>
<tr><td>Yes</td><td>437</td><td>32.1%</td></tr>
</tbody>
</table>

<h2 id='question-033'><a href='#question-033'>#</a> How has upgrading your Haskell compiler broken your code in the past year? (multiple select) <a href='#'>^</a></h2>
<a href='question-033.svg'><img src='/static/images/2018/11/18/question-033.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Incompatible dependencies</td><td>247</td><td>18.1%</td></tr>
<tr><td>Expected changes, such as the MonadFail proposal</td><td>225</td><td>16.5%</td></tr>
<tr><td>New warnings</td><td>76</td><td>5.6%</td></tr>
<tr><td>Compiler bugs</td><td>58</td><td>4.3%</td></tr>
<tr><td>Unexpected changes</td><td>54</td><td>4.0%</td></tr>
</tbody>
</table>

<h2 id='question-035'><a href='#question-035'>#</a> Which versions of GHC do you use? <a href='#'>^</a></h2>
<a href='question-035.svg'><img src='/static/images/2018/11/18/question-035.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>HEAD</td><td>72</td><td>5.3%</td></tr>
<tr><td>8.6.x</td><td>329</td><td>24.2%</td></tr>
<tr><td>8.4.x</td><td>825</td><td>60.6%</td></tr>
<tr><td>8.2.x</td><td>318</td><td>23.4%</td></tr>
<tr><td>8.0.x</td><td>235</td><td>17.3%</td></tr>
<tr><td>7.10.x</td><td>78</td><td>5.7%</td></tr>
<tr><td>7.8.x</td><td>25</td><td>1.8%</td></tr>
<tr><td>7.6.x</td><td>7</td><td>0.5%</td></tr>
<tr><td>7.4.x</td><td>6</td><td>0.4%</td></tr>
<tr><td>7.2.x</td><td>2</td><td>0.1%</td></tr>
<tr><td>7.0.x</td><td>3</td><td>0.2%</td></tr>
</tbody>
</table>

<h2 id='question-036'><a href='#question-036'>#</a> How do you feel about the new GHC release schedule? (single select) <a href='#'>^</a></h2>
<a href='question-036.svg'><img src='/static/images/2018/11/18/question-036.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>I like it</td><td>533</td><td>39.2%</td></tr>
<tr><td>I am indifferent</td><td>361</td><td>26.5%</td></tr>
<tr><td>I dislike it</td><td>50</td><td>3.7%</td></tr>
<tr><td>I was not aware of it</td><td>210</td><td>15.4%</td></tr>
</tbody>
</table>

<h2 id='question-038'><a href='#question-038'>#</a> Which GHC language extensions would you like to be enabled by default? (multiple select) <a href='#'>^</a></h2>
<a href='question-038.svg'><img src='/static/images/2018/11/18/question-038.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>OverloadedStrings</td><td>486</td><td>35.7%</td></tr>
<tr><td>LambdaCase</td><td>327</td><td>24.0%</td></tr>
<tr><td>DeriveGeneric</td><td>263</td><td>19.3%</td></tr>
<tr><td>DeriveFunctor</td><td>260</td><td>19.1%</td></tr>
<tr><td>BangPatterns</td><td>254</td><td>18.7%</td></tr>
<tr><td>ScopedTypeVariables</td><td>254</td><td>18.7%</td></tr>
<tr><td>GADTs</td><td>251</td><td>18.4%</td></tr>
<tr><td>FlexibleInstances</td><td>223</td><td>16.4%</td></tr>
<tr><td>FlexibleContexts</td><td>217</td><td>15.9%</td></tr>
<tr><td>DeriveFoldable</td><td>192</td><td>14.1%</td></tr>
<tr><td>RankNTypes</td><td>187</td><td>13.7%</td></tr>
<tr><td>TupleSections</td><td>183</td><td>13.4%</td></tr>
<tr><td>MultiParamTypeClasses</td><td>178</td><td>13.1%</td></tr>
<tr><td>GeneralisedNewtypeDeriving</td><td>178</td><td>13.1%</td></tr>
<tr><td>DeriveTraversable</td><td>171</td><td>12.6%</td></tr>
<tr><td>DataKinds</td><td>144</td><td>10.6%</td></tr>
<tr><td>TypeApplications</td><td>138</td><td>10.1%</td></tr>
<tr><td>TypeOperators</td><td>136</td><td>10.0%</td></tr>
<tr><td>KindSignatures</td><td>135</td><td>9.9%</td></tr>
<tr><td>TypeFamilies</td><td>127</td><td>9.3%</td></tr>
<tr><td>ApplicativeDo</td><td>122</td><td>9.0%</td></tr>
<tr><td>StandaloneDeriving</td><td>117</td><td>8.6%</td></tr>
<tr><td>ConstraintKinds</td><td>116</td><td>8.5%</td></tr>
<tr><td>DerivingVia</td><td>116</td><td>8.5%</td></tr>
<tr><td>ViewPatterns</td><td>113</td><td>8.3%</td></tr>
<tr><td>InstanceSigs</td><td>113</td><td>8.3%</td></tr>
<tr><td>MultiWayIf</td><td>111</td><td>8.2%</td></tr>
<tr><td>RecordWildCards</td><td>110</td><td>8.1%</td></tr>
<tr><td>FunctionalDependencies</td><td>101</td><td>7.4%</td></tr>
<tr><td>DeriveDataTypeable</td><td>100</td><td>7.3%</td></tr>
<tr><td>EmptyCase</td><td>92</td><td>6.8%</td></tr>
<tr><td>NamedFieldPuns</td><td>87</td><td>6.4%</td></tr>
<tr><td>EmptyDataDecls</td><td>83</td><td>6.1%</td></tr>
<tr><td>ExplicitForAll</td><td>83</td><td>6.1%</td></tr>
<tr><td>DerivingStrategies</td><td>76</td><td>5.6%</td></tr>
<tr><td>DeriveAnyClass</td><td>74</td><td>5.4%</td></tr>
<tr><td>BinaryLiterals</td><td>74</td><td>5.4%</td></tr>
<tr><td>DefaultSignatures</td><td>69</td><td>5.1%</td></tr>
<tr><td>NumericUnderscores</td><td>67</td><td>4.9%</td></tr>
<tr><td>PatternSynonyms</td><td>64</td><td>4.7%</td></tr>
<tr><td>DuplicateRecordFields</td><td>62</td><td>4.6%</td></tr>
<tr><td>ExistentialQuantification</td><td>57</td><td>4.2%</td></tr>
<tr><td>OverloadedLists</td><td>55</td><td>4.0%</td></tr>
<tr><td>PolyKinds</td><td>54</td><td>4.0%</td></tr>
<tr><td>GADTSyntax</td><td>54</td><td>4.0%</td></tr>
<tr><td>BlockArguments</td><td>53</td><td>3.9%</td></tr>
<tr><td>NoImplicitPrelude</td><td>53</td><td>3.9%</td></tr>
<tr><td>DeriveLift</td><td>47</td><td>3.5%</td></tr>
<tr><td>PatternGuards</td><td>42</td><td>3.1%</td></tr>
<tr><td>AutoDeriveTypeable</td><td>39</td><td>2.9%</td></tr>
<tr><td>UnicodeSyntax</td><td>38</td><td>2.8%</td></tr>
<tr><td>TypeSynonymInstances</td><td>37</td><td>2.7%</td></tr>
<tr><td>PartialTypeSignatures</td><td>36</td><td>2.6%</td></tr>
<tr><td>NoMonomorphismRestriction</td><td>34</td><td>2.5%</td></tr>
<tr><td>TypeInType</td><td>33</td><td>2.4%</td></tr>
<tr><td>TemplateHaskell</td><td>31</td><td>2.3%</td></tr>
<tr><td>DisambiguateRecordFields</td><td>30</td><td>2.2%</td></tr>
<tr><td>TypeFamilyDependencies</td><td>29</td><td>2.1%</td></tr>
<tr><td>QuasiQuotes</td><td>28</td><td>2.1%</td></tr>
<tr><td>RecordPuns</td><td>25</td><td>1.8%</td></tr>
<tr><td>PackageImports</td><td>24</td><td>1.8%</td></tr>
<tr><td>QuantifiedConstraints</td><td>22</td><td>1.6%</td></tr>
<tr><td>AllowAmbiguousTypes</td><td>22</td><td>1.6%</td></tr>
<tr><td>Arrows</td><td>21</td><td>1.5%</td></tr>
<tr><td>EmptyDataDeriving</td><td>21</td><td>1.5%</td></tr>
<tr><td>NamedWildCards</td><td>20</td><td>1.5%</td></tr>
<tr><td>NegativeLiterals</td><td>19</td><td>1.4%</td></tr>
<tr><td>ParallelListComp</td><td>19</td><td>1.4%</td></tr>
<tr><td>DoAndIfThenElse</td><td>19</td><td>1.4%</td></tr>
<tr><td>MonadComprehensions</td><td>18</td><td>1.3%</td></tr>
<tr><td>OverloadedLabels</td><td>18</td><td>1.3%</td></tr>
<tr><td>RecursiveDo</td><td>17</td><td>1.2%</td></tr>
<tr><td>NumDecimals</td><td>17</td><td>1.2%</td></tr>
<tr><td>NullaryTypeClasses</td><td>17</td><td>1.2%</td></tr>
<tr><td>PatternSignatures</td><td>16</td><td>1.2%</td></tr>
<tr><td>Rank2Types</td><td>16</td><td>1.2%</td></tr>
<tr><td>ForeignFunctionInterface</td><td>15</td><td>1.1%</td></tr>
<tr><td>UnboxedTuples</td><td>15</td><td>1.1%</td></tr>
<tr><td>StarIsType</td><td>14</td><td>1.0%</td></tr>
<tr><td>RoleAnnotations</td><td>14</td><td>1.0%</td></tr>
<tr><td>MagicHash</td><td>13</td><td>1.0%</td></tr>
<tr><td>LiberalTypeSynonyms</td><td>13</td><td>1.0%</td></tr>
<tr><td>CPP</td><td>12</td><td>0.9%</td></tr>
<tr><td>HexFloatLiterals</td><td>12</td><td>0.9%</td></tr>
<tr><td>MonadFailDesugaring</td><td>11</td><td>0.8%</td></tr>
<tr><td>StrictData</td><td>11</td><td>0.8%</td></tr>
<tr><td>ExplicitNamespaces</td><td>11</td><td>0.8%</td></tr>
<tr><td>UnboxedSums</td><td>11</td><td>0.8%</td></tr>
<tr><td>UndecidableInstances</td><td>10</td><td>0.7%</td></tr>
<tr><td>ImplicitParams</td><td>9</td><td>0.7%</td></tr>
<tr><td>ConstrainedClassMethods</td><td>9</td><td>0.7%</td></tr>
<tr><td>DoRec</td><td>9</td><td>0.7%</td></tr>
<tr><td>OverlappingInstances</td><td>8</td><td>0.6%</td></tr>
<tr><td>Strict</td><td>7</td><td>0.5%</td></tr>
<tr><td>DatatypeContexts</td><td>6</td><td>0.4%</td></tr>
<tr><td>CApiFFI</td><td>6</td><td>0.4%</td></tr>
<tr><td>TransformListComp</td><td>5</td><td>0.4%</td></tr>
<tr><td>TemplateHaskellQuotes</td><td>5</td><td>0.4%</td></tr>
<tr><td>PolymorphicComponents</td><td>5</td><td>0.4%</td></tr>
<tr><td>RelaxedPolyRec</td><td>5</td><td>0.4%</td></tr>
<tr><td>ExtendedDefaultRules</td><td>4</td><td>0.3%</td></tr>
<tr><td>PostfixOperators</td><td>4</td><td>0.3%</td></tr>
<tr><td>NPlusKPatterns</td><td>4</td><td>0.3%</td></tr>
<tr><td>ParallelArrays</td><td>3</td><td>0.2%</td></tr>
<tr><td>ImpredicativeTypes</td><td>3</td><td>0.2%</td></tr>
<tr><td>InterruptibleFFI</td><td>3</td><td>0.2%</td></tr>
<tr><td>AlternativeLayoutRule</td><td>2</td><td>0.1%</td></tr>
<tr><td>NoTraditionalRecordSyntax</td><td>2</td><td>0.1%</td></tr>
<tr><td>RebindableSyntax</td><td>2</td><td>0.1%</td></tr>
<tr><td>UnliftedFFITypes</td><td>2</td><td>0.1%</td></tr>
<tr><td>UndecidableSuperClasses</td><td>2</td><td>0.1%</td></tr>
<tr><td>RelaxedLayout</td><td>2</td><td>0.1%</td></tr>
<tr><td>AlternativeLayoutRuleTransitional</td><td>1</td><td>0.1%</td></tr>
<tr><td>MonoLocalBinds</td><td>1</td><td>0.1%</td></tr>
<tr><td>JavaScriptFFI</td><td>1</td><td>0.1%</td></tr>
<tr><td>GHCForeignImportPrim</td><td>1</td><td>0.1%</td></tr>
</tbody>
</table>

<h2 id='question-039'><a href='#question-039'>#</a> How important do you feel it would be to have a new version of the Haskell standard? (single select) <a href='#'>^</a></h2>
<a href='question-039.svg'><img src='/static/images/2018/11/18/question-039.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Not at all important</td><td>68</td><td>5.0%</td></tr>
<tr><td>Slightly important</td><td>135</td><td>9.9%</td></tr>
<tr><td>Moderately important</td><td>370</td><td>27.2%</td></tr>
<tr><td>Very important</td><td>302</td><td>22.2%</td></tr>
<tr><td>Extremely important</td><td>148</td><td>10.9%</td></tr>
</tbody>
</table>

<h2 id='question-040'><a href='#question-040'>#</a> Tooling <a href='#'>^</a></h2>
<a href='question-040.svg'><img src='/static/images/2018/11/18/question-040.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>4</td><td>0.3%</td></tr>
<tr><td>2 hearts</td><td>8</td><td>0.6%</td></tr>
<tr><td>3 hearts</td><td>1</td><td>0.1%</td></tr>
<tr><td>4 hearts</td><td>17</td><td>1.2%</td></tr>
<tr><td>5 hearts</td><td>28</td><td>2.1%</td></tr>
<tr><td>6 hearts</td><td>35</td><td>2.6%</td></tr>
<tr><td>7 hearts</td><td>70</td><td>5.1%</td></tr>
<tr><td>8 hearts</td><td>97</td><td>7.1%</td></tr>
<tr><td>9 hearts</td><td>81</td><td>6.0%</td></tr>
<tr><td>10 hearts</td><td>321</td><td>23.6%</td></tr>
</tbody>
</table>

<h2 id='question-041'><a href='#question-041'>#</a> Which build tools do you use for Haskell? (multiple select) <a href='#'>^</a></h2>
<a href='question-041.svg'><img src='/static/images/2018/11/18/question-041.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Stack</td><td>995</td><td>73.1%</td></tr>
<tr><td>Cabal</td><td>654</td><td>48.1%</td></tr>
<tr><td>Nix</td><td>311</td><td>22.9%</td></tr>
<tr><td>Make</td><td>106</td><td>7.8%</td></tr>
<tr><td>Shake</td><td>75</td><td>5.5%</td></tr>
<tr><td>ghc-pkg</td><td>69</td><td>5.1%</td></tr>
<tr><td>Bazel</td><td>24</td><td>1.8%</td></tr>
<tr><td>Mafia</td><td>13</td><td>1.0%</td></tr>
</tbody>
</table>

<h2 id='question-043'><a href='#question-043'>#</a> Which editors do you use for Haskell? (multiple select) <a href='#'>^</a></h2>
<a href='question-043.svg'><img src='/static/images/2018/11/18/question-043.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Emacs</td><td>523</td><td>38.4%</td></tr>
<tr><td>Vi</td><td>471</td><td>34.6%</td></tr>
<tr><td>Visual Studio Code</td><td>329</td><td>24.2%</td></tr>
<tr><td>Atom</td><td>112</td><td>8.2%</td></tr>
<tr><td>Sublime Text</td><td>83</td><td>6.1%</td></tr>
<tr><td>IntelliJ IDEA</td><td>62</td><td>4.6%</td></tr>
<tr><td>Notepad++</td><td>19</td><td>1.4%</td></tr>
</tbody>
</table>

<h2 id='question-045'><a href='#question-045'>#</a> Which version control systems do you use for Haskell? (multiple select) <a href='#'>^</a></h2>
<a href='question-045.svg'><img src='/static/images/2018/11/18/question-045.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Git</td><td>1236</td><td>90.8%</td></tr>
<tr><td>Darcs</td><td>42</td><td>3.1%</td></tr>
<tr><td>Mercurial</td><td>29</td><td>2.1%</td></tr>
<tr><td>Subversion</td><td>11</td><td>0.8%</td></tr>
<tr><td>Pijul</td><td>10</td><td>0.7%</td></tr>
<tr><td>Fossil</td><td>8</td><td>0.6%</td></tr>
</tbody>
</table>

<h2 id='question-047'><a href='#question-047'>#</a> Where do you get Haskell packages from? (multiple select) <a href='#'>^</a></h2>
<a href='question-047.svg'><img src='/static/images/2018/11/18/question-047.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Stackage</td><td>867</td><td>63.7%</td></tr>
<tr><td>Hackage</td><td>831</td><td>61.1%</td></tr>
<tr><td>Source</td><td>305</td><td>22.4%</td></tr>
<tr><td>Nix</td><td>273</td><td>20.1%</td></tr>
</tbody>
</table>

<h2 id='question-049'><a href='#question-049'>#</a> Which libraries do you use to test Haskell code? (multiple select) <a href='#'>^</a></h2>
<a href='question-049.svg'><img src='/static/images/2018/11/18/question-049.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>QuickCheck</td><td>696</td><td>51.1%</td></tr>
<tr><td>Hspec</td><td>537</td><td>39.5%</td></tr>
<tr><td>HUnit</td><td>333</td><td>24.5%</td></tr>
<tr><td>Tasty</td><td>323</td><td>23.7%</td></tr>
<tr><td>Hedgehog</td><td>162</td><td>11.9%</td></tr>
<tr><td>SmallCheck</td><td>58</td><td>4.3%</td></tr>
<tr><td>Haskell Test Framework</td><td>53</td><td>3.9%</td></tr>
<tr><td>LeanCheck</td><td>4</td><td>0.3%</td></tr>
</tbody>
</table>

<h2 id='question-051'><a href='#question-051'>#</a> Which libraries do you use to benchmark Haskell code? (multiple select) <a href='#'>^</a></h2>
<a href='question-051.svg'><img src='/static/images/2018/11/18/question-051.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Criterion</td><td>446</td><td>32.8%</td></tr>
<tr><td>Bench</td><td>54</td><td>4.0%</td></tr>
<tr><td>Gauge</td><td>34</td><td>2.5%</td></tr>
</tbody>
</table>

<h2 id='question-053'><a href='#question-053'>#</a> Infrastructure <a href='#'>^</a></h2>
<a href='question-053.svg'><img src='/static/images/2018/11/18/question-053.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>6</td><td>0.4%</td></tr>
<tr><td>2 hearts</td><td>5</td><td>0.4%</td></tr>
<tr><td>3 hearts</td><td>4</td><td>0.3%</td></tr>
<tr><td>4 hearts</td><td>7</td><td>0.5%</td></tr>
<tr><td>5 hearts</td><td>26</td><td>1.9%</td></tr>
<tr><td>6 hearts</td><td>33</td><td>2.4%</td></tr>
<tr><td>7 hearts</td><td>69</td><td>5.1%</td></tr>
<tr><td>8 hearts</td><td>107</td><td>7.9%</td></tr>
<tr><td>9 hearts</td><td>62</td><td>4.6%</td></tr>
<tr><td>10 hearts</td><td>309</td><td>22.7%</td></tr>
</tbody>
</table>

<h2 id='question-054'><a href='#question-054'>#</a> Which tools do you use to deploy Haskell applications? (multiple select) <a href='#'>^</a></h2>
<a href='question-054.svg'><img src='/static/images/2018/11/18/question-054.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Static binaries</td><td>510</td><td>37.5%</td></tr>
<tr><td>Docker images</td><td>402</td><td>29.5%</td></tr>
<tr><td>Dynamic binaries</td><td>189</td><td>13.9%</td></tr>
<tr><td>Nix expressions</td><td>183</td><td>13.4%</td></tr>
</tbody>
</table>

<h2 id='question-056'><a href='#question-056'>#</a> Where do you deploy Haskell applications? (multiple select) <a href='#'>^</a></h2>
<a href='question-056.svg'><img src='/static/images/2018/11/18/question-056.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Self or company owned servers</td><td>504</td><td>37.0%</td></tr>
<tr><td>Amazon Web Services (AWS)</td><td>328</td><td>24.1%</td></tr>
<tr><td>Google Cloud</td><td>81</td><td>6.0%</td></tr>
<tr><td>Digital Ocean</td><td>81</td><td>6.0%</td></tr>
<tr><td>Heroku</td><td>53</td><td>3.9%</td></tr>
<tr><td>Microsoft Azure</td><td>15</td><td>1.1%</td></tr>
</tbody>
</table>

<h2 id='question-058'><a href='#question-058'>#</a> Community <a href='#'>^</a></h2>
<a href='question-058.svg'><img src='/static/images/2018/11/18/question-058.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>2</td><td>0.1%</td></tr>
<tr><td>2 hearts</td><td>3</td><td>0.2%</td></tr>
<tr><td>3 hearts</td><td>5</td><td>0.4%</td></tr>
<tr><td>4 hearts</td><td>8</td><td>0.6%</td></tr>
<tr><td>5 hearts</td><td>22</td><td>1.6%</td></tr>
<tr><td>6 hearts</td><td>30</td><td>2.2%</td></tr>
<tr><td>7 hearts</td><td>52</td><td>3.8%</td></tr>
<tr><td>8 hearts</td><td>93</td><td>6.8%</td></tr>
<tr><td>9 hearts</td><td>68</td><td>5.0%</td></tr>
<tr><td>10 hearts</td><td>327</td><td>24.0%</td></tr>
</tbody>
</table>

<h2 id='question-059'><a href='#question-059'>#</a> Where do you interact with the Haskell community? (multiple select) <a href='#'>^</a></h2>
<a href='question-059.svg'><img src='/static/images/2018/11/18/question-059.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Reddit</td><td>699</td><td>51.4%</td></tr>
<tr><td>GitHub</td><td>590</td><td>43.4%</td></tr>
<tr><td>Twitter</td><td>392</td><td>28.8%</td></tr>
<tr><td>IRC</td><td>348</td><td>25.6%</td></tr>
<tr><td>Stack Overflow</td><td>266</td><td>19.5%</td></tr>
<tr><td>Meetups</td><td>250</td><td>18.4%</td></tr>
<tr><td>Mailing lists</td><td>248</td><td>18.2%</td></tr>
<tr><td>Slack</td><td>225</td><td>16.5%</td></tr>
<tr><td>Conferences (commercial)</td><td>206</td><td>15.1%</td></tr>
<tr><td>Conferences (academic)</td><td>157</td><td>11.5%</td></tr>
<tr><td>Discord</td><td>110</td><td>8.1%</td></tr>
<tr><td>Gitter</td><td>65</td><td>4.8%</td></tr>
<tr><td>Lobsters</td><td>65</td><td>4.8%</td></tr>
<tr><td>Telegram</td><td>42</td><td>3.1%</td></tr>
<tr><td>Mastodon</td><td>27</td><td>2.0%</td></tr>
<tr><td>Matrix/Riot</td><td>13</td><td>1.0%</td></tr>
</tbody>
</table>

<h2 id='question-061'><a href='#question-061'>#</a> Which of the following Haskell topics would you like to see more written about? (multiple select) <a href='#'>^</a></h2>
<a href='question-061.svg'><img src='/static/images/2018/11/18/question-061.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Best practices</td><td>563</td><td>41.4%</td></tr>
<tr><td>Design patterns</td><td>375</td><td>27.6%</td></tr>
<tr><td>Application architectures</td><td>375</td><td>27.6%</td></tr>
<tr><td>Performance analysis</td><td>312</td><td>22.9%</td></tr>
<tr><td>Library walkthroughs</td><td>296</td><td>21.7%</td></tr>
<tr><td>Tooling choices</td><td>271</td><td>19.9%</td></tr>
<tr><td>Debugging how-tos</td><td>245</td><td>18.0%</td></tr>
<tr><td>Production infrastructure</td><td>233</td><td>17.1%</td></tr>
<tr><td>Case studies</td><td>213</td><td>15.7%</td></tr>
<tr><td>Web development</td><td>189</td><td>13.9%</td></tr>
<tr><td>Algorithm implementations</td><td>174</td><td>12.8%</td></tr>
<tr><td>GUIs</td><td>163</td><td>12.0%</td></tr>
<tr><td>Beginner fundamentals</td><td>156</td><td>11.5%</td></tr>
<tr><td>Machine learning</td><td>156</td><td>11.5%</td></tr>
<tr><td>Project setup</td><td>134</td><td>9.8%</td></tr>
<tr><td>Project maintenance</td><td>125</td><td>9.2%</td></tr>
<tr><td>Game development</td><td>123</td><td>9.0%</td></tr>
<tr><td>Mobile development</td><td>93</td><td>6.8%</td></tr>
<tr><td>Comparisons to other languages</td><td>60</td><td>4.4%</td></tr>
</tbody>
</table>

<h2 id='question-063'><a href='#question-063'>#</a> Feelings <a href='#'>^</a></h2>
<a href='question-063.svg'><img src='/static/images/2018/11/18/question-063.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>3</td><td>0.2%</td></tr>
<tr><td>2 hearts</td><td>1</td><td>0.1%</td></tr>
<tr><td>3 hearts</td><td>5</td><td>0.4%</td></tr>
<tr><td>4 hearts</td><td>9</td><td>0.7%</td></tr>
<tr><td>5 hearts</td><td>17</td><td>1.2%</td></tr>
<tr><td>6 hearts</td><td>36</td><td>2.6%</td></tr>
<tr><td>7 hearts</td><td>58</td><td>4.3%</td></tr>
<tr><td>8 hearts</td><td>96</td><td>7.1%</td></tr>
<tr><td>9 hearts</td><td>74</td><td>5.4%</td></tr>
<tr><td>10 hearts</td><td>342</td><td>25.1%</td></tr>
</tbody>
</table>

<h2 id='question-064'><a href='#question-064'>#</a> I feel welcome in the Haskell community. <a href='#'>^</a></h2>
<a href='question-064.svg'><img src='/static/images/2018/11/18/question-064.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>11</td><td>0.8%</td></tr>
<tr><td>Disagree</td><td>42</td><td>3.1%</td></tr>
<tr><td>Neutral</td><td>226</td><td>16.6%</td></tr>
<tr><td>Agree</td><td>502</td><td>36.9%</td></tr>
<tr><td>Strongly agree</td><td>449</td><td>33.0%</td></tr>
</tbody>
</table>

<h2 id='question-065'><a href='#question-065'>#</a> I am satisfied with Haskell as a language. <a href='#'>^</a></h2>
<a href='question-065.svg'><img src='/static/images/2018/11/18/question-065.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>9</td><td>0.7%</td></tr>
<tr><td>Disagree</td><td>43</td><td>3.2%</td></tr>
<tr><td>Neutral</td><td>109</td><td>8.0%</td></tr>
<tr><td>Agree</td><td>587</td><td>43.1%</td></tr>
<tr><td>Strongly agree</td><td>516</td><td>37.9%</td></tr>
</tbody>
</table>

<h2 id='question-067'><a href='#question-067'>#</a> I am satisfied with Haskell&apos;s compilers, such as GHC. <a href='#'>^</a></h2>
<a href='question-067.svg'><img src='/static/images/2018/11/18/question-067.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>5</td><td>0.4%</td></tr>
<tr><td>Disagree</td><td>36</td><td>2.6%</td></tr>
<tr><td>Neutral</td><td>96</td><td>7.1%</td></tr>
<tr><td>Agree</td><td>626</td><td>46.0%</td></tr>
<tr><td>Strongly agree</td><td>471</td><td>34.6%</td></tr>
</tbody>
</table>

<h2 id='question-069'><a href='#question-069'>#</a> I am satisfied with Haskell&apos;s build tools, such as Cabal. <a href='#'>^</a></h2>
<a href='question-069.svg'><img src='/static/images/2018/11/18/question-069.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>34</td><td>2.5%</td></tr>
<tr><td>Disagree</td><td>144</td><td>10.6%</td></tr>
<tr><td>Neutral</td><td>323</td><td>23.7%</td></tr>
<tr><td>Agree</td><td>539</td><td>39.6%</td></tr>
<tr><td>Strongly agree</td><td>162</td><td>11.9%</td></tr>
</tbody>
</table>

<h2 id='question-071'><a href='#question-071'>#</a> I am satisfied with Haskell&apos;s package repositories, such as Hackage. <a href='#'>^</a></h2>
<a href='question-071.svg'><img src='/static/images/2018/11/18/question-071.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>9</td><td>0.7%</td></tr>
<tr><td>Disagree</td><td>67</td><td>4.9%</td></tr>
<tr><td>Neutral</td><td>204</td><td>15.0%</td></tr>
<tr><td>Agree</td><td>653</td><td>48.0%</td></tr>
<tr><td>Strongly agree</td><td>256</td><td>18.8%</td></tr>
</tbody>
</table>

<h2 id='question-073'><a href='#question-073'>#</a> I can find Haskell libraries for the things that I need. <a href='#'>^</a></h2>
<a href='question-073.svg'><img src='/static/images/2018/11/18/question-073.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>12</td><td>0.9%</td></tr>
<tr><td>Disagree</td><td>92</td><td>6.8%</td></tr>
<tr><td>Neutral</td><td>256</td><td>18.8%</td></tr>
<tr><td>Agree</td><td>687</td><td>50.5%</td></tr>
<tr><td>Strongly agree</td><td>159</td><td>11.7%</td></tr>
</tbody>
</table>

<h2 id='question-074'><a href='#question-074'>#</a> I think Haskell libraries are high quality. <a href='#'>^</a></h2>
<a href='question-074.svg'><img src='/static/images/2018/11/18/question-074.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>5</td><td>0.4%</td></tr>
<tr><td>Disagree</td><td>49</td><td>3.6%</td></tr>
<tr><td>Neutral</td><td>300</td><td>22.0%</td></tr>
<tr><td>Agree</td><td>588</td><td>43.2%</td></tr>
<tr><td>Strongly agree</td><td>241</td><td>17.7%</td></tr>
</tbody>
</table>

<h2 id='question-075'><a href='#question-075'>#</a> I have a good understanding of Haskell best practices. <a href='#'>^</a></h2>
<a href='question-075.svg'><img src='/static/images/2018/11/18/question-075.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>53</td><td>3.9%</td></tr>
<tr><td>Disagree</td><td>269</td><td>19.8%</td></tr>
<tr><td>Neutral</td><td>376</td><td>27.6%</td></tr>
<tr><td>Agree</td><td>419</td><td>30.8%</td></tr>
<tr><td>Strongly agree</td><td>106</td><td>7.8%</td></tr>
</tbody>
</table>

<h2 id='question-076'><a href='#question-076'>#</a> I think Haskell libraries are well documented. <a href='#'>^</a></h2>
<a href='question-076.svg'><img src='/static/images/2018/11/18/question-076.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>127</td><td>9.3%</td></tr>
<tr><td>Disagree</td><td>353</td><td>25.9%</td></tr>
<tr><td>Neutral</td><td>421</td><td>30.9%</td></tr>
<tr><td>Agree</td><td>265</td><td>19.5%</td></tr>
<tr><td>Strongly agree</td><td>39</td><td>2.9%</td></tr>
</tbody>
</table>

<h2 id='question-077'><a href='#question-077'>#</a> I can easily compare competing Haskell libraries to select the best one. <a href='#'>^</a></h2>
<a href='question-077.svg'><img src='/static/images/2018/11/18/question-077.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>131</td><td>9.6%</td></tr>
<tr><td>Disagree</td><td>441</td><td>32.4%</td></tr>
<tr><td>Neutral</td><td>368</td><td>27.0%</td></tr>
<tr><td>Agree</td><td>217</td><td>15.9%</td></tr>
<tr><td>Strongly agree</td><td>34</td><td>2.5%</td></tr>
</tbody>
</table>

<h2 id='question-078'><a href='#question-078'>#</a> I think that Haskell libraries are easy to use. <a href='#'>^</a></h2>
<a href='question-078.svg'><img src='/static/images/2018/11/18/question-078.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>43</td><td>3.2%</td></tr>
<tr><td>Disagree</td><td>217</td><td>15.9%</td></tr>
<tr><td>Neutral</td><td>546</td><td>40.1%</td></tr>
<tr><td>Agree</td><td>340</td><td>25.0%</td></tr>
<tr><td>Strongly agree</td><td>45</td><td>3.3%</td></tr>
</tbody>
</table>

<h2 id='question-079'><a href='#question-079'>#</a> I think that Haskell libraries provide a stable API. <a href='#'>^</a></h2>
<a href='question-079.svg'><img src='/static/images/2018/11/18/question-079.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>11</td><td>0.8%</td></tr>
<tr><td>Disagree</td><td>93</td><td>6.8%</td></tr>
<tr><td>Neutral</td><td>405</td><td>29.8%</td></tr>
<tr><td>Agree</td><td>556</td><td>40.9%</td></tr>
<tr><td>Strongly agree</td><td>82</td><td>6.0%</td></tr>
</tbody>
</table>

<h2 id='question-080'><a href='#question-080'>#</a> I think that Haskell libraries work well together. <a href='#'>^</a></h2>
<a href='question-080.svg'><img src='/static/images/2018/11/18/question-080.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>18</td><td>1.3%</td></tr>
<tr><td>Disagree</td><td>105</td><td>7.7%</td></tr>
<tr><td>Neutral</td><td>396</td><td>29.1%</td></tr>
<tr><td>Agree</td><td>522</td><td>38.4%</td></tr>
<tr><td>Strongly agree</td><td>117</td><td>8.6%</td></tr>
</tbody>
</table>

<h2 id='question-081'><a href='#question-081'>#</a> I think that software written in Haskell is easy to maintain. <a href='#'>^</a></h2>
<a href='question-081.svg'><img src='/static/images/2018/11/18/question-081.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>7</td><td>0.5%</td></tr>
<tr><td>Disagree</td><td>31</td><td>2.3%</td></tr>
<tr><td>Neutral</td><td>132</td><td>9.7%</td></tr>
<tr><td>Agree</td><td>485</td><td>35.6%</td></tr>
<tr><td>Strongly agree</td><td>523</td><td>38.4%</td></tr>
</tbody>
</table>

<h2 id='question-082'><a href='#question-082'>#</a> Once my Haskell program compiles, it generally does what I intended. <a href='#'>^</a></h2>
<a href='question-082.svg'><img src='/static/images/2018/11/18/question-082.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>7</td><td>0.5%</td></tr>
<tr><td>Disagree</td><td>57</td><td>4.2%</td></tr>
<tr><td>Neutral</td><td>123</td><td>9.0%</td></tr>
<tr><td>Agree</td><td>649</td><td>47.7%</td></tr>
<tr><td>Strongly agree</td><td>384</td><td>28.2%</td></tr>
</tbody>
</table>

<h2 id='question-083'><a href='#question-083'>#</a> I think that Haskell libraries perform well. <a href='#'>^</a></h2>
<a href='question-083.svg'><img src='/static/images/2018/11/18/question-083.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>5</td><td>0.4%</td></tr>
<tr><td>Disagree</td><td>47</td><td>3.5%</td></tr>
<tr><td>Neutral</td><td>320</td><td>23.5%</td></tr>
<tr><td>Agree</td><td>638</td><td>46.9%</td></tr>
<tr><td>Strongly agree</td><td>142</td><td>10.4%</td></tr>
</tbody>
</table>

<h2 id='question-084'><a href='#question-084'>#</a> Haskell&apos;s performance meets my needs. <a href='#'>^</a></h2>
<a href='question-084.svg'><img src='/static/images/2018/11/18/question-084.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>14</td><td>1.0%</td></tr>
<tr><td>Disagree</td><td>60</td><td>4.4%</td></tr>
<tr><td>Neutral</td><td>159</td><td>11.7%</td></tr>
<tr><td>Agree</td><td>585</td><td>43.0%</td></tr>
<tr><td>Strongly agree</td><td>367</td><td>27.0%</td></tr>
</tbody>
</table>

<h2 id='question-085'><a href='#question-085'>#</a> I can easily reason about the performance of my Haskell code. <a href='#'>^</a></h2>
<a href='question-085.svg'><img src='/static/images/2018/11/18/question-085.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>114</td><td>8.4%</td></tr>
<tr><td>Disagree</td><td>374</td><td>27.5%</td></tr>
<tr><td>Neutral</td><td>418</td><td>30.7%</td></tr>
<tr><td>Agree</td><td>240</td><td>17.6%</td></tr>
<tr><td>Strongly agree</td><td>39</td><td>2.9%</td></tr>
</tbody>
</table>

<h2 id='question-086'><a href='#question-086'>#</a> I would recommend using Haskell to others. <a href='#'>^</a></h2>
<a href='question-086.svg'><img src='/static/images/2018/11/18/question-086.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>5</td><td>0.4%</td></tr>
<tr><td>Disagree</td><td>32</td><td>2.4%</td></tr>
<tr><td>Neutral</td><td>95</td><td>7.0%</td></tr>
<tr><td>Agree</td><td>412</td><td>30.3%</td></tr>
<tr><td>Strongly agree</td><td>682</td><td>50.1%</td></tr>
</tbody>
</table>

<h2 id='question-087'><a href='#question-087'>#</a> I would prefer to use Haskell for my next new project. <a href='#'>^</a></h2>
<a href='question-087.svg'><img src='/static/images/2018/11/18/question-087.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>15</td><td>1.1%</td></tr>
<tr><td>Disagree</td><td>42</td><td>3.1%</td></tr>
<tr><td>Neutral</td><td>88</td><td>6.5%</td></tr>
<tr><td>Agree</td><td>315</td><td>23.1%</td></tr>
<tr><td>Strongly agree</td><td>765</td><td>56.2%</td></tr>
</tbody>
</table>

<h2 id='question-088'><a href='#question-088'>#</a> Haskell is working well for my team. <a href='#'>^</a></h2>
<a href='question-088.svg'><img src='/static/images/2018/11/18/question-088.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>26</td><td>1.9%</td></tr>
<tr><td>Disagree</td><td>47</td><td>3.5%</td></tr>
<tr><td>Neutral</td><td>261</td><td>19.2%</td></tr>
<tr><td>Agree</td><td>222</td><td>16.3%</td></tr>
<tr><td>Strongly agree</td><td>276</td><td>20.3%</td></tr>
</tbody>
</table>

<h2 id='question-089'><a href='#question-089'>#</a> Haskell is critical to my company&apos;s success. <a href='#'>^</a></h2>
<a href='question-089.svg'><img src='/static/images/2018/11/18/question-089.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>132</td><td>9.7%</td></tr>
<tr><td>Disagree</td><td>113</td><td>8.3%</td></tr>
<tr><td>Neutral</td><td>245</td><td>18.0%</td></tr>
<tr><td>Agree</td><td>153</td><td>11.2%</td></tr>
<tr><td>Strongly agree</td><td>186</td><td>13.7%</td></tr>
</tbody>
</table>

<h2 id='question-090'><a href='#question-090'>#</a> As a candidate, I can easily find Haskell jobs. <a href='#'>^</a></h2>
<a href='question-090.svg'><img src='/static/images/2018/11/18/question-090.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>219</td><td>16.1%</td></tr>
<tr><td>Disagree</td><td>305</td><td>22.4%</td></tr>
<tr><td>Neutral</td><td>283</td><td>20.8%</td></tr>
<tr><td>Agree</td><td>103</td><td>7.6%</td></tr>
<tr><td>Strongly agree</td><td>45</td><td>3.3%</td></tr>
</tbody>
</table>

<h2 id='question-091'><a href='#question-091'>#</a> As a hiring manager, I can easily find qualified Haskell candidates. <a href='#'>^</a></h2>
<a href='question-091.svg'><img src='/static/images/2018/11/18/question-091.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Strongly disagree</td><td>69</td><td>5.1%</td></tr>
<tr><td>Disagree</td><td>113</td><td>8.3%</td></tr>
<tr><td>Neutral</td><td>274</td><td>20.1%</td></tr>
<tr><td>Agree</td><td>70</td><td>5.1%</td></tr>
<tr><td>Strongly agree</td><td>33</td><td>2.4%</td></tr>
</tbody>
</table>

<h2 id='question-094'><a href='#question-094'>#</a> Demographics <a href='#'>^</a></h2>
<a href='question-094.svg'><img src='/static/images/2018/11/18/question-094.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>8</td><td>0.6%</td></tr>
<tr><td>2 hearts</td><td>0</td><td>0.0%</td></tr>
<tr><td>3 hearts</td><td>5</td><td>0.4%</td></tr>
<tr><td>4 hearts</td><td>6</td><td>0.4%</td></tr>
<tr><td>5 hearts</td><td>22</td><td>1.6%</td></tr>
<tr><td>6 hearts</td><td>29</td><td>2.1%</td></tr>
<tr><td>7 hearts</td><td>54</td><td>4.0%</td></tr>
<tr><td>8 hearts</td><td>94</td><td>6.9%</td></tr>
<tr><td>9 hearts</td><td>74</td><td>5.4%</td></tr>
<tr><td>10 hearts</td><td>310</td><td>22.8%</td></tr>
</tbody>
</table>

<h2 id='question-095'><a href='#question-095'>#</a> Which country do you live in? (single select) <a href='#'>^</a></h2>
<a href='question-095.svg'><img src='/static/images/2018/11/18/question-095.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>United States</td><td>364</td><td>26.7%</td></tr>
<tr><td>United Kingdom</td><td>107</td><td>7.9%</td></tr>
<tr><td>Germany</td><td>107</td><td>7.9%</td></tr>
<tr><td>Australia</td><td>70</td><td>5.1%</td></tr>
<tr><td>France</td><td>55</td><td>4.0%</td></tr>
<tr><td>Russia</td><td>49</td><td>3.6%</td></tr>
<tr><td>Canada</td><td>44</td><td>3.2%</td></tr>
<tr><td>Sweden</td><td>36</td><td>2.6%</td></tr>
<tr><td>Netherlands</td><td>34</td><td>2.5%</td></tr>
<tr><td>India</td><td>24</td><td>1.8%</td></tr>
<tr><td>Japan</td><td>21</td><td>1.5%</td></tr>
<tr><td>Italy</td><td>20</td><td>1.5%</td></tr>
<tr><td>Poland</td><td>20</td><td>1.5%</td></tr>
<tr><td>Finland</td><td>19</td><td>1.4%</td></tr>
<tr><td>Switzerland</td><td>19</td><td>1.4%</td></tr>
<tr><td>Austria</td><td>17</td><td>1.2%</td></tr>
<tr><td>Norway</td><td>16</td><td>1.2%</td></tr>
<tr><td>Czechia</td><td>15</td><td>1.1%</td></tr>
<tr><td>Denmark</td><td>14</td><td>1.0%</td></tr>
<tr><td>Belgium</td><td>13</td><td>1.0%</td></tr>
<tr><td>Spain</td><td>13</td><td>1.0%</td></tr>
<tr><td>New Zealand</td><td>11</td><td>0.8%</td></tr>
<tr><td>Brazil</td><td>10</td><td>0.7%</td></tr>
<tr><td>China</td><td>9</td><td>0.7%</td></tr>
<tr><td>Ireland</td><td>8</td><td>0.6%</td></tr>
<tr><td>Israel</td><td>8</td><td>0.6%</td></tr>
<tr><td>Ukraine</td><td>8</td><td>0.6%</td></tr>
<tr><td>Portugal</td><td>7</td><td>0.5%</td></tr>
<tr><td>Croatia</td><td>6</td><td>0.4%</td></tr>
<tr><td>Belarus</td><td>5</td><td>0.4%</td></tr>
<tr><td>Romania</td><td>5</td><td>0.4%</td></tr>
<tr><td>Argentina</td><td>4</td><td>0.3%</td></tr>
<tr><td>Bulgaria</td><td>4</td><td>0.3%</td></tr>
<tr><td>Greece</td><td>4</td><td>0.3%</td></tr>
<tr><td>Indonesia</td><td>4</td><td>0.3%</td></tr>
<tr><td>Latvia</td><td>4</td><td>0.3%</td></tr>
<tr><td>South Africa</td><td>4</td><td>0.3%</td></tr>
<tr><td>Turkey</td><td>4</td><td>0.3%</td></tr>
<tr><td>Ecuador</td><td>3</td><td>0.2%</td></tr>
<tr><td>Estonia</td><td>3</td><td>0.2%</td></tr>
<tr><td>Hungary</td><td>3</td><td>0.2%</td></tr>
<tr><td>Kenya</td><td>3</td><td>0.2%</td></tr>
<tr><td>Singapore</td><td>3</td><td>0.2%</td></tr>
<tr><td>Mexico</td><td>2</td><td>0.1%</td></tr>
<tr><td>Thailand</td><td>2</td><td>0.1%</td></tr>
<tr><td>Algeria</td><td>1</td><td>0.1%</td></tr>
<tr><td>Armenia</td><td>1</td><td>0.1%</td></tr>
<tr><td>Azerbaijan</td><td>1</td><td>0.1%</td></tr>
<tr><td>Bolivia</td><td>1</td><td>0.1%</td></tr>
<tr><td>Cameroon</td><td>1</td><td>0.1%</td></tr>
<tr><td>Chile</td><td>1</td><td>0.1%</td></tr>
<tr><td>Colombia</td><td>1</td><td>0.1%</td></tr>
<tr><td>Egypt</td><td>1</td><td>0.1%</td></tr>
<tr><td>Guatemala</td><td>1</td><td>0.1%</td></tr>
<tr><td>Iceland</td><td>1</td><td>0.1%</td></tr>
<tr><td>Iran</td><td>1</td><td>0.1%</td></tr>
<tr><td>Kazakhstan</td><td>1</td><td>0.1%</td></tr>
<tr><td>Lebanon</td><td>1</td><td>0.1%</td></tr>
<tr><td>Malta</td><td>1</td><td>0.1%</td></tr>
<tr><td>Nepal</td><td>1</td><td>0.1%</td></tr>
<tr><td>Nigeria</td><td>1</td><td>0.1%</td></tr>
<tr><td>Pakistan</td><td>1</td><td>0.1%</td></tr>
<tr><td>Paraguay</td><td>1</td><td>0.1%</td></tr>
<tr><td>Peru</td><td>1</td><td>0.1%</td></tr>
<tr><td>Philippines</td><td>1</td><td>0.1%</td></tr>
<tr><td>Serbia</td><td>1</td><td>0.1%</td></tr>
<tr><td>Slovakia</td><td>1</td><td>0.1%</td></tr>
<tr><td>Slovenia</td><td>1</td><td>0.1%</td></tr>
<tr><td>South Korea</td><td>1</td><td>0.1%</td></tr>
<tr><td>Syria</td><td>1</td><td>0.1%</td></tr>
<tr><td>Uganda</td><td>1</td><td>0.1%</td></tr>
<tr><td>Vietnam</td><td>1</td><td>0.1%</td></tr>
</tbody>
</table>

<h2 id='question-096'><a href='#question-096'>#</a> How old are you? (single select) <a href='#'>^</a></h2>
<a href='question-096.svg'><img src='/static/images/2018/11/18/question-096.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Under 18 years old</td><td>13</td><td>1.0%</td></tr>
<tr><td>18 to 24 years old</td><td>200</td><td>14.7%</td></tr>
<tr><td>25 to 34 years old</td><td>608</td><td>44.7%</td></tr>
<tr><td>35 to 44 years old</td><td>283</td><td>20.8%</td></tr>
<tr><td>45 to 54 years old</td><td>80</td><td>5.9%</td></tr>
<tr><td>55 to 64 years old</td><td>26</td><td>1.9%</td></tr>
<tr><td>65 years or older</td><td>8</td><td>0.6%</td></tr>
</tbody>
</table>

<h2 id='question-097'><a href='#question-097'>#</a> What is your gender? (single select) <a href='#'>^</a></h2>
<a href='question-097.svg'><img src='/static/images/2018/11/18/question-097.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Male</td><td>1119</td><td>82.2%</td></tr>
<tr><td>Female</td><td>49</td><td>3.6%</td></tr>
<tr><td>Non-binary</td><td>29</td><td>2.1%</td></tr>
</tbody>
</table>

<h2 id='question-098'><a href='#question-098'>#</a> Do you identify as transgender? (single select) <a href='#'>^</a></h2>
<a href='question-098.svg'><img src='/static/images/2018/11/18/question-098.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>No</td><td>1079</td><td>79.3%</td></tr>
<tr><td>Yes</td><td>35</td><td>2.6%</td></tr>
</tbody>
</table>

<h2 id='question-099'><a href='#question-099'>#</a> Are you a student? (single select) <a href='#'>^</a></h2>
<a href='question-099.svg'><img src='/static/images/2018/11/18/question-099.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>No</td><td>987</td><td>72.5%</td></tr>
<tr><td>Yes, full time</td><td>158</td><td>11.6%</td></tr>
<tr><td>Yes, part time</td><td>68</td><td>5.0%</td></tr>
</tbody>
</table>

<h2 id='question-100'><a href='#question-100'>#</a> What is the highest level of education you have completed? (single select) <a href='#'>^</a></h2>
<a href='question-100.svg'><img src='/static/images/2018/11/18/question-100.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Less than high school diploma</td><td>22</td><td>1.6%</td></tr>
<tr><td>High school diploma</td><td>101</td><td>7.4%</td></tr>
<tr><td>Some college</td><td>131</td><td>9.6%</td></tr>
<tr><td>Associate degree</td><td>24</td><td>1.8%</td></tr>
<tr><td>Bachelor&apos;s degree</td><td>432</td><td>31.7%</td></tr>
<tr><td>Master&apos;s degree</td><td>338</td><td>24.8%</td></tr>
<tr><td>Professional degree</td><td>23</td><td>1.7%</td></tr>
<tr><td>Doctoral degree</td><td>145</td><td>10.7%</td></tr>
</tbody>
</table>

<h2 id='question-101'><a href='#question-101'>#</a> What is your employment status? (single select) <a href='#'>^</a></h2>
<a href='question-101.svg'><img src='/static/images/2018/11/18/question-101.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Employed full time</td><td>876</td><td>64.4%</td></tr>
<tr><td>Self employed</td><td>124</td><td>9.1%</td></tr>
<tr><td>Employed part time</td><td>89</td><td>6.5%</td></tr>
<tr><td>Not employed, but looking for work</td><td>67</td><td>4.9%</td></tr>
<tr><td>Not employed, and not looking for work</td><td>52</td><td>3.8%</td></tr>
<tr><td>Retired</td><td>4</td><td>0.3%</td></tr>
</tbody>
</table>

<h2 id='question-102'><a href='#question-102'>#</a> How large is the company you work for? (single select) <a href='#'>^</a></h2>
<a href='question-102.svg'><img src='/static/images/2018/11/18/question-102.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Fewer than 10 employees</td><td>205</td><td>15.1%</td></tr>
<tr><td>10 to 99 employees</td><td>363</td><td>26.7%</td></tr>
<tr><td>100 to 999 employees</td><td>225</td><td>16.5%</td></tr>
<tr><td>1,000 or more employees</td><td>276</td><td>20.3%</td></tr>
</tbody>
</table>

<h2 id='question-103'><a href='#question-103'>#</a> How many years have you been coding? (single select) <a href='#'>^</a></h2>
<a href='question-103.svg'><img src='/static/images/2018/11/18/question-103.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>0 to 4 years</td><td>95</td><td>7.0%</td></tr>
<tr><td>5 to 9 years</td><td>315</td><td>23.1%</td></tr>
<tr><td>10 to 14 years</td><td>313</td><td>23.0%</td></tr>
<tr><td>15 to 19 years</td><td>208</td><td>15.3%</td></tr>
<tr><td>20 to 24 years</td><td>133</td><td>9.8%</td></tr>
<tr><td>25 to 29 years</td><td>69</td><td>5.1%</td></tr>
<tr><td>30 or more years</td><td>94</td><td>6.9%</td></tr>
</tbody>
</table>

<h2 id='question-104'><a href='#question-104'>#</a> How many years have you been coding professionally? (single select) <a href='#'>^</a></h2>
<a href='question-104.svg'><img src='/static/images/2018/11/18/question-104.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>0 to 4 years</td><td>397</td><td>29.2%</td></tr>
<tr><td>5 to 9 years</td><td>353</td><td>25.9%</td></tr>
<tr><td>10 to 19 years</td><td>305</td><td>22.4%</td></tr>
<tr><td>20 to 29 years</td><td>102</td><td>7.5%</td></tr>
<tr><td>30 or more years</td><td>26</td><td>1.9%</td></tr>
</tbody>
</table>

<h2 id='question-105'><a href='#question-105'>#</a> Do you code as a hobby? (single select) <a href='#'>^</a></h2>
<a href='question-105.svg'><img src='/static/images/2018/11/18/question-105.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Yes</td><td>1183</td><td>86.9%</td></tr>
<tr><td>No</td><td>59</td><td>4.3%</td></tr>
</tbody>
</table>

<h2 id='question-106'><a href='#question-106'>#</a> Have you contributed to any open source projects? (single select) <a href='#'>^</a></h2>
<a href='question-106.svg'><img src='/static/images/2018/11/18/question-106.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Yes</td><td>1031</td><td>75.8%</td></tr>
<tr><td>No</td><td>206</td><td>15.1%</td></tr>
</tbody>
</table>

<h2 id='question-107'><a href='#question-107'>#</a> Meta survey <a href='#'>^</a></h2>
<a href='question-107.svg'><img src='/static/images/2018/11/18/question-107.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>1 heart</td><td>5</td><td>0.4%</td></tr>
<tr><td>2 hearts</td><td>3</td><td>0.2%</td></tr>
<tr><td>3 hearts</td><td>3</td><td>0.2%</td></tr>
<tr><td>4 hearts</td><td>5</td><td>0.4%</td></tr>
<tr><td>5 hearts</td><td>27</td><td>2.0%</td></tr>
<tr><td>6 hearts</td><td>26</td><td>1.9%</td></tr>
<tr><td>7 hearts</td><td>45</td><td>3.3%</td></tr>
<tr><td>8 hearts</td><td>95</td><td>7.0%</td></tr>
<tr><td>9 hearts</td><td>67</td><td>4.9%</td></tr>
<tr><td>10 hearts</td><td>321</td><td>23.6%</td></tr>
</tbody>
</table>

<h2 id='question-108'><a href='#question-108'>#</a> Did you take last year&apos;s survey? (single select) <a href='#'>^</a></h2>
<a href='question-108.svg'><img src='/static/images/2018/11/18/question-108.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>No</td><td>616</td><td>45.3%</td></tr>
<tr><td>Yes</td><td>322</td><td>23.7%</td></tr>
<tr><td>I don&apos;t remember</td><td>294</td><td>21.6%</td></tr>
</tbody>
</table>

<h2 id='question-109'><a href='#question-109'>#</a> How did you hear about this survey? (single select) <a href='#'>^</a></h2>
<a href='question-109.svg'><img src='/static/images/2018/11/18/question-109.svg' alt='' width='800.0' height='600.0'></a>
<table>
<thead>
<tr><th>Answer</th><th>Count</th><th>Percent</th></tr></thead>
<tbody>
<tr><td>Reddit</td><td>394</td><td>28.9%</td></tr>
<tr><td>Twitter</td><td>296</td><td>21.7%</td></tr>
<tr><td>Mailing list</td><td>121</td><td>8.9%</td></tr>
<tr><td>Haskell Weekly</td><td>113</td><td>8.3%</td></tr>
<tr><td>Lobsters</td><td>78</td><td>5.7%</td></tr>
<tr><td>Slack</td><td>65</td><td>4.8%</td></tr>
<tr><td>Discord</td><td>49</td><td>3.6%</td></tr>
<tr><td>Other</td><td>48</td><td>3.5%</td></tr>
<tr><td>In person</td><td>30</td><td>2.2%</td></tr>
<tr><td>IRC</td><td>12</td><td>0.9%</td></tr>
<tr><td>Telegram</td><td>12</td><td>0.9%</td></tr>
<tr><td>Gitter</td><td>6</td><td>0.4%</td></tr>
<tr><td>Matrix/Riot</td><td>2</td><td>0.1%</td></tr>
<tr><td>Mastodon</td><td>2</td><td>0.1%</td></tr>
</tbody>
</table>

[1]: {% post_url 2018-11-01-2018-state-of-haskell-survey %}
[2]: {% post_url 2017-11-15-2017-state-of-haskell-survey-results %}
[3]: /static/pages/2018-11-16-2018-state-of-haskell-survey-results.csv.zip
[4]: https://opendatacommons.org/licenses/odbl/
[5]: https://gist.github.com/tfausak/cc381ddd0ddd8304cc3eae6250966f55/782825c9e30034eff20954c7545d5fcdd0928a89#file-main-hs-L29
