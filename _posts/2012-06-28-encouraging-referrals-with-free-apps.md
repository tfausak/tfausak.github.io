---
layout: post
title: Encouraging Referrals with Free Apps
published: false
---

At [Famigo][], we're always making our [Sandbox][] better. But one of the biggest problems we face is letting people know about our app. Sure, we can (and do) run ads and work on app store SEO, but word-of-mouth is much more powerful. Dropbox noticed this and implemented [a referral program][] to give out free space.

We started a similar program. Instead of space, we give out apps. Here's the idea: refer a friend to the Sandbox and you both get a free app. We're all very excited about this and we're working hard to get it in your hands as soon as possible.

## Front End

This feature is a piece of cake to use. There are no signup codes and nothing to remember or hang on to. Here's what you, as a user of the Sandbox, have to do:

1.  Tap the glowing share button in manage mode.
2.  Select the people you want to share the Sandbox with.
3.  Send them an email.

That's it! We even prepopulate the email's recipients, subject, and body. All your have to do is hit send. You can customize the email if you want to, though. We just use the default email client.

On the other side of things, this is what the recipients of your email have to do:

1.  Open your email, read it and click on the link.
2.  Download, install, and run the Sandbox.

What could be easier? Once they finish setting up their sandbox, you'll both get a push notification with your free app.

## Back End

In order to make things easy for the end-user, we have to do some magic on the back end. It's not terribly complicated, but I think it's a pretty novel referral program. Here's how it works:

After the user selects the contacts they want to share with, their email addresses are uploaded to our server. We hash them, store them for later, and send back a shareable URL.

When any of the recipients go to the URL their friend sent them, we note that they opened it and send them along to the Play Store. Nothing interesting happens at this step, but it allows us to effectively track users throughout the process.

Then they download and install the Sandbox. Running it logs them in, at which point we hash their email and check for any referrals to redeem. If there is one, we mark it as used and pick an app they don't have already. (If they already have all the apps we've got, we apologize in an email.)

Now we know a referral has been completed and we've picked a new app for both people to get. We need to get it to them! So we pass the baton to [Urban Airship][], who sends a push notification to both devies.

## Privacy

Because of this feature, the Sandbox now requests the [`READ_CONTACTS`][] permission. We were hesitant to request this permission. We don't want to betray our users' trust. Also, our users' privacy is very important to us.

To that end, this is exactly what happens:

1.  You, as a user of the Sandbox, decide to share it with your friends and family. From manage mode, you tap the glowing share button. It tells you what's going to happen and asks you to select some contacts.
2.  We read your contacts and show you an easy way to select emails. This is the only point in the Sandbox where your contacts are accessed.
3.  After selecting some contacts, their emails are uploaded to Famigo, where they are hashed and stored. They are uploaded through our API, which logs the call for about six hours. After that, the email addresses are unrecoverable.
4.  You are presented with the default email app, pre-populated with the contacts you selected and a shareable URL.
5.  Some time later, one of the people you referred opens the link you sent them, downloads the Sandbox and runs it. When they do that, we hash the email and check to see if anyone referred them.
6.  Thirty days after you sent the email, we remove the email addresses from our database and invalidate the referral.

[]: http://www.famigo.com/
[]: https://play.google.com/store/apps/details?id=com.famigo.sandbox
[]: https://www.dropbox.com/help/54
[]: http://urbanairship.com/
[]: http://developer.android.com/reference/android/Manifest.permission.html#READ_CONTACTS
