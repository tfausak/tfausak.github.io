# [taylor.fausak.me][1]

Taylor Fausak's blog.

## Setup

Install [Vagrant][2] and [VirtualBox][3].

```sh
git clone https://github.com/tfausak/tfausak.github.io.git
cd tfausak.github.io
vagrant up
vagrant ssh
cd /vagrant
bundle exec rake serve
# http://localhost:4000
```

[1]: http://taylor.fausak.me
[2]: http://www.vagrantup.com
[3]: https://www.virtualbox.org
