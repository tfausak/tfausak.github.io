Vagrant.configure('2') do |config|
  config.vm.box = 'precise64'
  config.vm.box_url = 'http://files.vagrantup.com/precise64.box'
  config.vm.network :forwarded_port, guest: 4000, host: 4000

  config.vm.provision 'shell', inline: <<-SH
    set -e
    set -x

    update-locale LC_ALL=en_US.UTF-8

    aptitude -y update
    aptitude -y install build-essential imagemagick yui-compressor

    if test ! -f ruby-2.0.0-p247.tgz
    then
      wget --output-document=ruby-2.0.0-p247.tgz http://cache.ruby-lang.org/pub/ruby/2.0/ruby-2.0.0-p247.tar.gz
    fi

    if test ! -f ruby-2.0.0-p247.tar
    then
      zcat ruby-2.0.0-p247.tgz > ruby-2.0.0-p247.tar
    fi

    if test ! -d ruby-2.0.0-p247
    then
      tar --extract --file ruby-2.0.0-p247.tar
    fi

    cd ruby-2.0.0-p247
    if test ! -f Makefile
    then
      ./configure --disable-install-doc
    fi
    make
    make install
    cd ..

    echo 'gem: --no-document' > .gemrc
    gem update --system
    gem install bundler

    cd /vagrant
    bundle install
  SH
end
