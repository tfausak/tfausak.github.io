---
title: Testing a Node.js HTTP server with Mocha
---

I spent the better part of the past week updating a [Node.js][1]
HTTP server from version 0.4.8 of Node to the most recent version,
0.8.20. I started looking over the change logs but they quickly
overwhelmed me. Two years of development on Node, V8, npm, and
dependent packages generated a lot of changes.

Instead of trying to reason about what might break, I decided to
write tests. That way any breaking changes would break the build.
Plus writing tests would allow me to refactor the code in the future
and be confident that I didn't break anything.

## Server

The server isn't too complicated. It returns an HTTP 200 OK response
to every request. It also includes the text "Hello, world!" in the
response.

``` javascript
var http = require('http');

this.server = http.createServer(function (req, res) {
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end('Hello, world!\n');
});

exports.listen = function () {
  this.server.listen.apply(this.server, arguments);
};

exports.close = function (callback) {
  this.server.close(callback);
};
```

You may notice that the server doesn't start automatically. You
have to call `listen` for anything to happen. Setting up the server
like this makes `require` idempotent, which is a good thing.

## Tests

I happen to use [Mocha][2] for testing, but the concepts described
here should be applicable to other JavaScript test frameworks.

Before getting to the actual tests, there's a little bit of boilerplate
to get out of the way. The test suite should fire up the server
when it starts and kill the server when it finishes.

``` javascript
var server = require('../lib/server');

describe('server', function () {
  before(function () {
    server.listen(8000);
  });

  after(function () {
    server.close();
  });
});
```

With that out of the way, it's on to the actual tests. Since the
server does so little, it stands to reason that there won't be much
to the tests. All they do is check the status code and response
body. Exactly what you'd expect.

``` javascript
var assert = require('assert'),
    http = require('http');

describe('/', function () {
  it('should return 200', function (done) {
    http.get('http://localhost:8000', function (res) {
      assert.equal(200, res.statusCode);
      done();
    });
  });

  it('should say "Hello, world!"', function (done) {
    http.get('http://localhost:8000', function (res) {
      var data = '';

      res.on('data', function (chunk) {
        data += chunk;
      });

      res.on('end', function () {
        assert.equal('Hello, world!\n', data);
        done();
      });
    });
  });
});
```

That's all there is to it! Running the test suite is a piece of
cake with `npm test`.

[1]: http://nodejs.org
[2]: http://visionmedia.github.com/mocha/
