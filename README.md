# [taylor.fausak.me][1]

This is my blog. There are many like it, but this one is mine.

## Setup

[Vagrant][2] makes setup a breeze.

```sh
git clone https://github.com/tfausak/tfausak.github.io.git
cd tfausak.github.io
vagrant up
vagrant ssh
cd /vagrant
rake
# http://localhost:4000
```

[1]: http://taylor.fausak.me
[2]: http://www.vagrantup.com
