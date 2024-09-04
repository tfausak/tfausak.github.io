---
title: Haskell Weekly switching from Mailchimp to listmonk
---

I have been running the [Haskell Weekly][1] newsletter for more than eight years.
For that entire time, [Mailchimp][2] has been responsible for sending the emails.
Starting this week, that is going to change.
The emails will instead be sent by [listmonk][3] (via [Amazon Simple Email Service][4]).
If you're a subscriber, you may notice that some things are slightly different, like formatting and styling.
But most of the changes are happening behind the scenes.

My main motivation is cost.
I pay around $140 per month for Mailchimp, and that only goes up as more people subscribe.
listmonk is free and open source.
I don't know exactly how much Amazon SES will cost me, but it will be significantly cheaper than Mailchimp.

In short, Haskell Weekly is moving from Mailchimp to listmonk.
It should be low impact --- hopefully most subscribers won't really notice the change happening.
If you're curious about the technical details, please read [the PR][5] that makes the switch.

[1]: https://haskellweekly.news
[2]: https://mailchimp.com
[3]: https://listmonk.app
[4]: https://aws.amazon.com/ses/
[5]: https://github.com/haskellweekly/haskellweekly/pull/336
