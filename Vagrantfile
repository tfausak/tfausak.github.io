# Vagrant 1.6.3 <http://www.vagrantup.com/downloads.html>
# VirtualBox 4.3.12 <https://www.virtualbox.org/wiki/Downloads>

Vagrant.require_version '~> 1.6.3'

Vagrant.configure('2') do |config|
  config.vm.box = 'chef/ubuntu-13.10'
  config.vm.box_version = '~> 1.0'
  config.vm.network :forwarded_port, guest: 4000, host: 4000

  config.vm.provision :shell, inline: <<-'SHELL'
    set -e -x
    update-locale LC_ALL=en_US.UTF-8
    wget -qO- https://deb.nodesource.com/setup | bash -
    apt-get install --assume-yes imagemagick make nodejs yui-compressor
    if ! ruby --version | grep --fixed-strings 2.1.2; then
      test -f ruby-2.1.2.tar.bz2 ||
        wget cache.ruby-lang.org/pub/ruby/2.1/ruby-2.1.2.tar.bz2
      test -f ruby-2.1.2.tar ||
        bunzip2 -k ruby-2.1.2.tar.bz2
      test -d ruby-2.1.2 ||
        tar --extract --file ruby-2.1.2.tar
      cd ruby-2.1.2
      ./configure --disable-install-doc
      make
      make install
      cd ..
    fi
    gem update --no-document --system
  SHELL

  config.vm.provision :shell, inline: <<-'SHELL', privileged: false
    set -e -x
    echo 'PATH="$(ruby -e puts\(Gem.user_dir\))/bin:$PATH"' > .bash_profile
    source .bash_profile
    echo '{ gem: --no-document, install: --user-install }' > .gemrc
    gem install bundler
    cd /vagrant
    bundle install
  SHELL
end
