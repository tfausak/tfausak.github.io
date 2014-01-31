# Vagrant 1.4.3 <http://www.vagrantup.com/downloads.html>
# VirtualBox 4.3.6 <https://www.virtualbox.org/wiki/Downloads>

Vagrant.require_version '>= 1.4.3'

Vagrant.configure('2') do |config|
  config.vm.box = 'precise64'
  config.vm.box_download_checksum = '5803ee2fa7c5ded51a59f7928a2fead0'
  config.vm.box_download_checksum_type = 'md5'
  config.vm.box_url = 'http://files.vagrantup.com/precise64.box'
  config.vm.network :forwarded_port, guest: 4000, host: 4000

  config.vm.provision :shell, inline: <<-'SHELL'
    set -e -x
    update-locale LC_ALL=en_US.UTF-8
    aptitude -q -y update
    aptitude -y install make imagemagick yui-compressor
    if ! ruby -v | grep -F -q 2.1.0p0; then
      test -f ruby-2.1.0.tar.bz2 ||
        wget -q cache.ruby-lang.org/pub/ruby/2.1/ruby-2.1.0.tar.bz2
      test -f ruby-2.1.0.tar ||
        bunzip2 -k -q ruby-2.1.0.tar.bz2
      test -d ruby-2.1.0 ||
        tar -x -f ruby-2.1.0.tar
      cd ruby-2.1.0
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
    cd /vagrant
    rm Gemfile.lock
    gem install --file Gemfile
  SHELL
end
