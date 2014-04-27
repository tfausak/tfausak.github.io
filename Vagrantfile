# Vagrant 1.5.4 <http://www.vagrantup.com/downloads.html>
# VirtualBox 4.3.10 <https://www.virtualbox.org/wiki/Downloads>

Vagrant.require_version '~> 1.5'

Vagrant.configure('2') do |config|
  config.vm.box = 'chef/ubuntu-13.10'
  config.vm.box_version = '~> 1.0'
  config.vm.network :forwarded_port, guest: 4000, host: 4000

  config.vm.provision :shell, inline: <<-'SHELL'
    set -e -x
    update-locale LC_ALL=en_US.UTF-8
    apt-get update
    apt-get install --assume-yes imagemagick make yui-compressor
    if ! ruby -v | grep -F 2.1.1p76; then
      test -f ruby-2.1.1.tar.bz2 ||
        wget cache.ruby-lang.org/pub/ruby/2.1/ruby-2.1.1.tar.bz2
      test -f ruby-2.1.1.tar ||
        bunzip2 -k ruby-2.1.1.tar.bz2
      test -d ruby-2.1.1 ||
        tar --extract --file ruby-2.1.1.tar
      cd ruby-2.1.1
      ./configure --disable-install-doc
      make
      make install
      cd ..
    fi
  SHELL

  config.vm.provision :shell, inline: <<-'SHELL', privileged: false
    set -e -x
    echo 'PATH="$(ruby -e puts\(Gem.user_dir\))/bin:$PATH"' > .bash_profile
    source .bash_profile
    echo '{ gem: --no-document, install: --user-install }' > .gemrc
    sudo gem update --system
    gem install bundler
    cd /vagrant
    bundle install
  SHELL
end
