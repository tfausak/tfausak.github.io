.PHONY: all clean server setup

all: clean setup server

clean:
	rm -d -f -R ./_site/

# On a Vagrant box, you'll probably want this command instead:
#   env LC_ALL=en_US.UTF-8 bundle exec jekyll serve --drafts --future --host=0.0.0.0 --port=8080 --watch
server:
	bundle exec jekyll serve --drafts --future --host=127.0.0.1 --port=4000 --watch

setup:
	which -s brew || ruby -e "`curl -fsSL https://raw.github.com/mxcl/homebrew/go`"
	brew list rbenv >/dev/null || brew install rbenv
	rbenv versions | grep --fixed-strings --quiet 2.0.0-p247 || rbenv install 2.0.0-p247
	rbenv local | grep --fixed-strings --quiet 2.0.0-p247 || rbenv local 2.0.0-p247
	gem update --system >/dev/null
	rbenv which bundle >/dev/null || gem install bundler
	bundle install --quiet
