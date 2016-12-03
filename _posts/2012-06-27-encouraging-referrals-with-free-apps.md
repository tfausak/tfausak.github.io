---
title: Encouraging referrals with free apps
---

At Famigo, we're always making our [Sandbox][2] better. But
one of the biggest problems we face is letting people know about
our app. Sure, we can (and do) run ads and work on app store SEO,
but word-of-mouth is much more powerful. For instance, Dropbox
noticed this and implemented [a referral program][3] to give out
free space.

We started a similar program. Except instead of space, we give out
apps. Here's the idea: refer a friend to the Sandbox and you both
get a free app! We're all very excited about this and we're working
hard to get it in your hands as soon as possible.

## Front End

This feature is a piece of cake to use. There are no signup codes
and nothing to remember or hang on to. Here's what you, as a user
of the Sandbox, have to do:

1.  Tap the glowing share button in manage mode.
2.  Select the people you want to share the Sandbox with.
3.  Send them an email.

That's it! We even pre-populate the email's recipients, subject, and
body. All your have to do is hit send. You can customize the email
if you want to, though. We just use the default email client.

On the other side of things, this is what the recipients of your
email have to do:

1.  Open your email, read it and click on the link.
2.  Download, install, and run the Sandbox.

What could be easier? Once they finish setting up their sandbox,
you'll both get a push notification with your free app.

## Back End

In order to make things easy for the end-user, we have to do some
magic on the back end. It's not terribly complicated, but I think
it's a pretty novel referral program. Here's how it works:

After the user selects some email addresses, they're uploaded to
our server. We hash them, store them for later, and send back a
shareable URL.

When any of the recipients go to the URL their friend sent them,
we note that they opened it and send them along to the Play Store.
Nothing interesting happens at this step, but it allows us to track
the effectiveness of our referral campaign.

Then they download and install the Sandbox. Running it logs them
in, at which point we hash their email and check for any referrals
to redeem. If there is one, we mark it as used and pick an app they
don't have already. (If they already have all the apps we've got,
we can't do anything so we send an apology email.)

Now we know a referral has been completed and we've picked a new
app for both people to get. We need to get it to them, so we send
a push notification through [Urban Airship][4] to both devices.

## Privacy

As a result of implementing this referral program, the Sandbox now
requests the [`READ_CONTACTS`][5] permission. We didn't make this
decision lightly. Our users' privacy is very important to us.

I think the best way to address privacy concerns is with transparency.
To that end, this is exactly what happens during the referral
process:

1.  After deciding to share the Sandbox, we read your contacts and
    show you an email selection dialog. This is the only point in
    the Sandbox where we access your contacts.
2.  The email addresses you selected are uploaded to our server and
    hashed. We log the API call in plain text for about six hours.
3.  When one of the people you referred runs the Sandbox for the
    first time, we hash their email address to see if anyone referred
    them.
4.  Thirty days after you sent the referral email, we remove the
    hashed email addresses from our database and invalidate the
    referral.

[2]: https://play.google.com/store/apps/details?id=com.famigo.sandbox
[3]: https://www.dropbox.com/help/54/en
[4]: http://urbanairship.com
[5]: http://developer.android.com/reference/android/Manifest.permission.html#READ_CONTACTS
