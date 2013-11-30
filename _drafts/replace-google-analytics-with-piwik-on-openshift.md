---
layout: post
title: Replace Google Analytics with Piwik on OpenShift
---

-   used GA for years
-   interested in moving off google properties
-   heard of piwik but didnt want to set up my own server
-   turns out it's not that hard
-   found a blog post that helped
-   http://blog.freedomsponsors.org/goodbye-google-analytics-hello-piwik/

-   i made a github repository with the essentials
-   https://github.com/tfausak/piwik-openshift
-   fork it if you want
-   but it shouldnt be necessary

{% highlight sh %}
git clone https://github.com/tfausak/piwik-openshift.git
cd piwik-openshift
{% endhighlight %}

-   i like vagrant
-   but these steps are optional
-   if you dont want to do them, you just need rhc (and therefore git)
-   https://www.openshift.com/developers/rhc-client-tools-install
-   http://git-scm.com

{% highlight sh %}
vagrant up
vagrant ssh
cd /vagrant
{% endhighlight %}

-   youll need to create an openshift account if you dont have one already
-   https://openshift.redhat.com/app/account/new
-   then youll have to configure rhc to use your account

{% highlight sh %}
rhc setup
{% endhighlight %}

-   create an app
-   need php and mysql

{% highlight sh %}
rhc app create piwik php-5.3
rhc cartridge add mysql-5.1 --app piwik
{% endhighlight %}

-   this clones the repo for you
-   you dont need it though
-   just delete it

{% highlight sh %}
rm --force --recursive piwik
{% endhighlight %}

-   add an rhc remote to the current directory
-   push it up to rhc

{% highlight sh %}
url=$(rhc app show piwik | awk '/Git URL/ { print $3 }')
git remote add rhc $url
git push --force rhc master
{% endhighlight %}

-   go to `http://piwik-<namespace>.rhcloud.com`
-   get mysql config from rhc

{% highlight sh %}
hostname=$(rhc app show piwik | awk '/SSH/ { print $2 }')
command='env | grep OPENSHIFT_MYSQL_DB_'
ssh $hostname $command
{% endhighlight %}

-   OPENSHIFT_MYSQL_DB_HOST     => database server
-   OPENSHIFT_MYSQL_DB_USERNAME => login
-   OPENSHIFT_MYSQL_DB_PASSWORD => password
-   "piwik"                     => database name

-   finish the setup and you're good to go
