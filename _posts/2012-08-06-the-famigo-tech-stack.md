---
layout: post
title: The Famigo Tech Stack
published: false
---

Ever wonder what happens when you point your browser to [Famigo][]?
An array of no fewer than 20 technologies work together to bring
you the requested page. The best way to explain it is to follow the
request from start to finish.

1.  The first thing the request hits is our load balancer, which
    is powered by Amazon's [Elastic Load Balancing][] service. This
    decides which server should handle the request by picking the
    one that's least busy.

2.  We have two identical servers for the load balancer to pick
    from. They both run the 64-bit version of [Ubuntu][] Server
    11.04 hosted on Amazon's [Elastic Compute Cloud][], also known
    as EC2. That means we don't have any physical servers in the
    Famigo offices; everything's in the cloud.

3.  After arriving on the server, the first program the request
    runs into is [lighttpd][], our web server. It serves static
    files like images and stylesheets. (Actually, most of our static
    files are served from Amazon's [Simple Storage Service][] (S3)
    through their [CloudFront][] content delivery network. It's a
    lot faster.)

4.  For requests that aren't static, lighttpd hands things over to
    [flup][], which in turn gives it to [Python][] 2.7.1. All of
    our server-side code is written in Python and uses [Django][]
    1.3, a web framework.

5.  Most dynamic requests can be saved and returned later without
    doing any extra work. This saves us from computing a page twice
    if we know the results are going to be the same. [Memcached][]
    handles caching the pages and getting them from memory.

6.  Pages that aren't in the cache probably require reading or
    writing from the database. We store everything in [MongoDB][]
    and retrieve it with [MongoEngine][], which maps database
    documents to Python objects.

That covers almost everything required to process a request and
return a web page. There are still a couple stones left to turn,
though.

## Back End

We use a lot of things on the back end that aren't directly involved
with processing requests but are nonetheless essential.

-   [Rackspace][] manages DNS for all our servers as well as
    forwarding old domains like `famigogames.com`.

-   We store our data in Amazon's [Elastic Block Store][] (EBS).
    That handles the server images; the actual databases are in
    ephemeral storage.

-   Our Python environment requires more than 40 dependencies, which
    are managed by [virtualenv][]. It's hard to overstate the
    importance and ease of using virtualenv.

-   [Solr][] powers site search. We speak to Solr through [pysolr][].

-   For long-running or periodic tasks, we use [Celery][]. It's a
    distributed task queue backed by [Redis][].

-   All of our code is managed with [Git][] and hosted on [GitHub][]
    in a private repository. [Jenkins][] handles continuous integration
    whenever we push to GitHub.

## Monitoring

In addition to all that, we use a few things to make sure everything
else is running smoothly.

-   Every one of our servers runs [Monit][] to make sure everything
    behaves. As soon as anything out of the ordinary happens, we
    get an email with a description of the offense.

-   We have one sentinel server that keeps an eye on the others
    using [Nagios][]. It's important for us not to put all our eggs
    in one basket, especially when it comes to monitoring.

-   Just in case everything goes belly-up, we use [CopperEgg][] and
    10get's [MMS][] for external monitoring.

  [famigo]: http://www.famigo.com
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
