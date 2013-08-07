.PHONY: all clean server setup

all: clean setup server

clean:
	rm -fr ./_site/

server:
	bundle exec jekyll serve --drafts --future --watch

setup:
	which brew || ruby -e "`curl -fsSL https://raw.github.com/mxcl/homebrew/go`"
	brew list rbenv || brew install rbenv
	rbenv versions | grep -F 1.9.3-p448 || rbenv install 1.9.3-p448
	rbenv local | grep -F 1.9.3-p448 || rbenv local 1.9.3-p448
	gem update --system
	rbenv which bundle || gem install bundler
	bundle install
