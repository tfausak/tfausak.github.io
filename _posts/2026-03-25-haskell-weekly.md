---
title: Haskell Weekly lost its subscriber list
---

> _Every_ day is [International Backup Awareness Day](https://blog.codinghorror.com/international-backup-awareness-day/).

If you haven't received an email from Haskell Weekly in a while, you're not the only one!
The last successful newsletter email went out on February 12th, which was a little more than a month ago.
Unfortunately something caused the subscriber list to be reset.
The problem is fixed now, but you will have to [re-subscribe](https://haskellweekly.news/newsletter.html).

I've been running Haskell Weekly for nearly ten years.
This is the first major operational problem that I can remember.
And it's pretty major!
The list of subscribers is the most important thing to an email newsletter.
So how did I lose them?

To be honest, I'm not entirely sure.
Haskell Weekly is hosted on [Fly](https://fly.io).
It also used to use an unmanaged Fly Postgres instance.
And as of about two years ago it uses [listmonk](https://listmonk.app) for managing the subscriber list.

I'm providing these details for context --- none of them appear to be at fault.
And also this doesn't appear to be malicious.
I did not make any code changes around this time, nor did I make any configuration changes.
As best as I can tell, a service was automatically re-deployed and that somehow caused the database to be deleted or truncated.

This wouldn't have been a big deal if I had backups, but sadly I did not.
Fly's unmanaged Postgres keeps snapshots for five days.
Even if I had noticed this as soon as possible (on February 19th), it would've been too late to recover anything from those snapshots.
This is not Fly's fault!
It's clearly mine.

To prevent this from happening again, I've switched to Fly's _managed_ Postgres, which has backups.
Also I will periodically export the subscriber list and store it somewhere else.

I'm sorry for the mishap.
If you'd like to continue receiving the Haskell Weekly newsletter as an email, please re-subscribe here: <https://haskellweekly.news/newsletter.html>.
