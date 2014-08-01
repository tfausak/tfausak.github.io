---
title: The Famigo Tech Stack
---

Ever wonder what happens when you point your browser to [Famigo][1]?
An array of no fewer than 20 technologies work together to bring
you the requested page. The best way to explain it is to follow the
request from start to finish.

1.  The first thing the request hits is our load balancer, which
    is powered by Amazon's [Elastic Load Balancing][2] service. This
    decides which server should handle the request by picking the
    one that's least busy.

2.  We have two identical servers for the load balancer to pick
    from. They both run the 64-bit version of [Ubuntu][3] Server
    11.04 hosted on Amazon's [Elastic Compute Cloud][4], also known
    as EC2. That means we don't have any physical servers in the
    Famigo offices; everything's in the cloud.

3.  After arriving on the server, the first program the request
    runs into is [lighttpd][5], our web server. It serves static
    files like images and stylesheets. (Actually, most of our static
    files are served from Amazon's [Simple Storage Service][6] (S3)
    through their [CloudFront][7] content delivery network. It's a
    lot faster.)

4.  For requests that aren't static, lighttpd hands things over to
    [flup][8], which in turn gives it to [Python][9] 2.7.1. All of
    our server-side code is written in Python and uses [Django][10]
    1.3, a web framework.

5.  Most dynamic requests can be saved and returned later without
    doing any extra work. This saves us from computing a page twice
    if we know the results are going to be the same. [Memcached][11]
    handles caching the pages and getting them from memory.

6.  Pages that aren't in the cache probably require reading or
    writing from the database. We store everything in [MongoDB][12]
    and retrieve it with [MongoEngine][13], which maps database
    documents to Python objects.

That covers almost everything required to process a request and
return a web page. There are still a couple stones left to turn,
though.

## Back End

We use a lot of things on the back end that aren't directly involved
with processing requests but are nonetheless essential.

-   [Rackspace][14] manages DNS for all our servers as well as
    forwarding old domains like `famigogames.com`.
-   We store our data in Amazon's [Elastic Block Store][15] (EBS).
    That handles the server images; the actual databases are in
    ephemeral storage.
-   Our Python environment requires more than 40 dependencies, which
    are managed by [virtualenv][16]. It's hard to overstate the
    importance and ease of using virtualenv.
-   [Solr][17] powers site search. We speak to Solr through [pysolr][18].
-   For long-running or periodic tasks, we use [Celery][19]. It's a
    distributed task queue backed by [Redis][20].
-   All of our code is managed with [Git][21] and hosted on [GitHub][22]
    in a private repository. [Jenkins][23] handles continuous integration
    whenever we push to GitHub.

## Monitoring

In addition to all that, we use a few things to make sure everything
else is running smoothly.

-   Every one of our servers runs [Monit][24] to make sure everything
    behaves. As soon as anything out of the ordinary happens, we
    get an email with a description of the offense.
-   We have one sentinel server that keeps an eye on the others
    using [Nagios][25]. It's important for us not to put all our eggs
    in one basket, especially when it comes to monitoring.
-   Just in case everything goes belly-up, we use [CopperEgg][26] and
    10get's [MMS][27] for external monitoring.

[1]: http://www.famigo.com
[2]: http://aws.amazon.com/elasticloadbalancing/
[3]: http://www.ubuntu.com
[4]: http://aws.amazon.com/ec2/
[5]: http://www.lighttpd.net
[6]: http://aws.amazon.com/s3/
[7]: http://aws.amazon.com/cloudfront/
[8]: http://pypi.python.org/pypi/flup
[9]: http://www.python.org
[10]: https://www.djangoproject.com
[11]: http://memcached.org
[12]: http://www.mongodb.org
[13]: http://mongoengine.org
[14]: http://www.rackspace.com
[15]: http://aws.amazon.com/ebs/
[16]: http://www.virtualenv.org/en/latest/
[17]: http://lucene.apache.org/solr/
[18]: https://github.com/toastdriven/pysolr
[19]: http://celeryproject.org
[20]: http://redis.io
[21]: http://git-scm.com
[22]: https://github.com
[23]: http://jenkins-ci.org
[24]: http://mmonit.com/monit/
[25]: http://www.nagios.org
[26]: http://copperegg.com
[27]: http://www.10gen.com/products/mongodb-monitoring-service
