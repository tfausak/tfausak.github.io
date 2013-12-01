---
layout: post
title: Replace Google Analytics with Piwik on OpenShift
---

I have used Google Analytics for years. Recently I've been interested
in moving off of Google products. I researched lots of other analytics
providers, but didn't find any that I liked. Then I found [Piwik][1].

Piwik is a free-as-in-speech web analytics platform. Its mission
statement is to "liberate web analytics". I wanted to set it up as
a free-as-in-beer replacement for Google Analytics. Thanks to
[OpenShift][2], that's possible.

To get started, clone my [piwik-openshift][3] repository.

{% highlight sh %}
git clone https://github.com/tfausak/piwik-openshift.git
cd piwik-openshift
{% endhighlight %}

I like sandboxing my apps with [Vagrant][4], but this step is optional.
If you skip it, just install [`rhc`][5].

{% highlight sh %}
vagrant up
vagrant ssh
cd /vagrant
{% endhighlight %}

If you don't have one already, [create an OpenShift account][6].
Then configure `rhc` to use it.

{% highlight sh %}
rhc setup
{% endhighlight %}

Next you need to create a PHP app. Piwik also requires MySQL, so
add that cartridge to your app.

{% highlight sh %}
rhc app create piwik php-5.3
rhc cartridge add mysql-5.1 --app piwik
{% endhighlight %}

Creating a new app clones the initial repository for you. You don't
need it, so you can just delete it.

{% highlight sh %}
rm --force --recursive piwik
{% endhighlight %}

We're going to use the current directory as the source for the app
we just created. That means we need to add a Git remote for it.
Then we need to push it to OpenShift, which will also deploy the
app.

{% highlight sh %}
url=$(rhc app show piwik | awk '/Git URL/ { print $3 }')
git remote add rhc $url
git push --force rhc master
{% endhighlight %}

All that's left to do is set up Piwik. Go to
`piwik-<namespace>.rhcloud.com` and follow the instructions. When
you get to the database setup, you'll have to ask `rhc` for the
values.

{% highlight sh %}
hostname=$(rhc app show piwik | awk '/SSH/ { print $2 }')
command='env | grep OPENSHIFT_MYSQL_DB_'
ssh $hostname $command
{% endhighlight %}

Here's the environment variables you should be looking for, along
with which field they map to.

-   `OPENSHIFT_MYSQL_DB_HOST`: database server
-   `OPENSHIFT_MYSQL_DB_USERNAME`: login
-   `OPENSHIFT_MYSQL_DB_PASSWORD`: password

Finish the rest of the setup and you should be good to go! Enjoy
your free analytics.

[1]: https://piwik.org
[2]: https://www.openshift.com
[3]: https://github.com/tfausak/piwik-openshift
[4]: http://www.vagrantup.com
[5]: https://www.openshift.com/developers/rhc-client-tools-install
[6]: https://openshift.redhat.com/app/account/new
