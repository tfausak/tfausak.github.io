---
layout: post
title: The Famigo Tech Stack
published: false
---

The easiest way to explain most of our tech stack is to follow a
request to our web site or API.

1.  The first thing the request hits is our load balancer
    (`lb.famigo.com`), which is powered by Amazon's [Elastic Load
    Balancing][].
2.  Then it's routed to one of our servers (`buster.fam.io` or
    `dev1.fam.io`). For all intents and purposes, they are identical.
    They run [Ubuntu][] 11.04 "Natty Narwhal" on Amazon's [Elastic
    Compute Cloud][] (EC2).
3.  First thing it hits on the machine is [lighttpd][], our web
    server. This serves static files like stylesheets and images.
    (Actually, most of our static files are served from Amazon's [Simple
    Storage Service][] (S3) through their [CloudFront][] content delivery
    network.)
4.  For dynamic (i.e., not static) requests, lighttpd passes the
    baton to [flup][], which hands it off to [Python][] 2.7. Python's
    dependencies are managed with [virtualenv][].
5.  Our Python code uses [Django][] 1.3, a web framework.
6.  Most responses can be cached with [Memcached][] and served
    straight from memory.
7.  Requests that miss the cache probably hit the database. We use
    [MongoDB][] with [MongoEngine][] as our document-object mapper.

That covers almost everything required to return a web page back
to the user. There are still a couple more things, though.

-   We use [Rackspace][] to manage DNS for our servers (`lb`, `fido`,
    `buster`, and `dev1`) as well as forwarding old domains like
    `famigogames.com`.
-   All of our data is stored in Amazon's [Elastic Block Store][]
    (EBS). That handles the servers themselves, but the actual database
    is in ephemeral storage.
-   Search on the site is powered with [Solr][]. We interface with
    it through [pysolr][].
-   For long-running tasks, we use [Celery][], a distributed task
    queue. It stores its tasks in [Redis][].
-   All of our code is managed with [Git][] and hosted on [GitHub][]
    in a private repository. [Jenkins][] handles continuous integration
    whenever we push to GitHub.

In addition to all that, we use a few things to make sure everything
else is running smoothly.

-   [Monit][] runs on each of our servers and makes sure everything
    behaves.
-   `fido` keeps an eye on `buster` and `dev` with [Nagios][].
-   [CopperEgg][] and 10gen's [MMS][] handle external monitoring,
    in case everything goes belly-up.

  [elastic load balancing]: http://aws.amazon.com/elasticloadbalancing/
  [ubuntu]: http://www.ubuntu.com
  [elastic compute cloud]: http://aws.amazon.com/ec2/
  [lighttpd]: http://www.lighttpd.net
  [simple storage service]: http://aws.amazon.com/s3/
  [cloudfront]: http://aws.amazon.com/cloudfront/
  [flup]: http://trac.saddi.com/flup
  [python]: http://www.python.org
  [virtualenv]: http://www.virtualenv.org
  [django]: https://www.djangoproject.com
  [memcached]: http://memcached.org
  [mongodb]: http://www.mongodb.org
  [mongoengine]: http://mongoengine.org
  [rackspace]: http://www.rackspace.com
  [elastic block store]: http://aws.amazon.com/ebs/
  [solr]: http://lucene.apache.org/solr/
  [pysolr]: https://github.com/toastdriven/pysolr
  [celery]: http://celeryproject.org
  [redis]: http://redis.io
  [git]: http://git-scm.com
  [github]: https://github.com
  [jenkins]: http://jenkins-ci.org
  [monit]: http://mmonit.com/monit/
  [nagios]: http://www.nagios.org
  [copperegg]: http://copperegg.com
  [mms]: http://www.10gen.com/mongodb-monitoring-service
