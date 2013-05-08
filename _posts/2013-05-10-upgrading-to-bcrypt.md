---
layout: post
title: Upgrading to bcrypt
---

[![Crypt][1]][2]

Usually when a database leaks, it contains a bunch of email addresses
and passwords. Any user with a row in the database should be aware
that someone could discover their credentials. Depending on how
their data is stored, it can be either simple or impossible to
extract the passwords. From least to most secure, these six formats
cover the majority of password storage techniques:

1.  Plain text
2.  Obfuscated (Caesar cipher)
3.  Encrypted (Triple DES)
4.  Hashed (MD5)
5.  Salted and hashed
6.  Computed from a key derivation function (bcrypt)

It's unequivocally better to store passwords more securely. There
are no downsides and brute-force attacks become orders of magnitude
harder. Any greenfield project should use bcrypt (or another key
derivation function like PBKDF2 or scrypt).

But what about existing projects? Upgrading to a new scheme isn't
trivial because current passwords need to be migrated. In fact, the
user shouldn't be able to tell that anything changed. The old system
should be silently deprecated and replaced with the new one.

Here's how to do exactly that with Rails 3.2.13 and bcrypt-ruby
3.0.1 on Ruby 1.9.3-p392.

## Before

Let's say you're in the worst possible scenario: you store passwords
in plain text. Hopefully you don't actually do this, but it makes
this example a lot simpler. The same principles work regardless of
how you store your passwords.

Assuming a straightforward user model, you might authenticate users
with a class method. All it does is try to find the user, then
compare the passwords. If everything checks out, it returns the
user. In all other cases, it returns `nil`.

{% highlight ruby %}
class User < ActiveRecord::Base
  def self.authenticate(username, password)
    user = find_by_username(username)
    user if user && password == user.password
  end
end
{% endhighlight %}

## During

We want to jump straight to the best case scenario and start using
bcrypt. Three things are necessary to get that done: add another
field to the user model; add a handful of new methods; and modify
the authenticate model.

Up first is adding a new field to the user model. We need to store
the derived key bcrypt generates. A simple migration takes care of
this step:

{% highlight ruby %}
class AddBcryptHashToUser < ActiveRecord::Migration
  def change
    add_column :users, :bcrypt_hash, :string
  end
end
{% endhighlight %}

Now we need a couple utility functions. They'll allow us to see
which users use bcrypt, set the password, and compare strings against
it. These all require the [bcrypt-ruby][3] gem, so add `gem
'bcrypt-ruby'` to you Gemfile.

{% highlight ruby %}
require 'bcrypt'
class User < ActiveRecord::Base
  include BCrypt
  def bcrypt?
    bcrypt_hash.present?
  end
  def bcrypt
    @bcrypt ||= Password.new(bcrypt_hash) if bcrypt?
  end
  def bcrypt=(new_password)
    @bcrypt = Password.create(new_password)
    self.bcrypt_hash = @bcrypt
  end
end
{% endhighlight %}

Lastly the authenticate function needs to modified to do a couple
things. It should compare using bcrypt if the user has been updated.
If it hasn't, it should compare using the old method, then generate
a new hash using bcrypt and delete the old one.

{% highlight ruby %}
def self.authenticate(username, password)
  user = find_by_username(username)
  return unless user
  if user.bcrypt?
    user if user.bcrypt == password
  elsif password == user.password
    user.bcrypt = password
    user.password = nil
    user.save!
    user
  end
end
{% endhighlight %}

## After

At some point you'll want to remove everything that's still stored
in the old format. For users that haven't updated yet, a new password
must be generated. You can either email it to them or they can rely
on your password recovery service.

{% highlight ruby %}
require 'bcrypt'
class RemovePasswordFromUser < ActiveRecord::Migration
  def up
    user_ids = ActiveRecord::Base.connection.
      select_all('SELECT id FROM users WHERE bcrypt_hash IS NULL').
      map { |e| e['id'] }
    remove_column :users, :password
    return if user_ids.blank?

    passwords = user_ids.length.times.
      map { SecureRandom.hex }
    bcrypt_hashes = passwords.
      map { |e| BCrypt::Password.create(e) }
    cases = user_ids.zip(bcrypt_hashes).
      map { |a, b| "WHEN #{a} THEN '#{b}'" }
    update_sql <<-SQL
      UPDATE users
      SET bcrypt_hash = CASE id #{cases.join(' ')} END
      WHERE id IN (#{user_ids.join(', ')})
    SQL
  end
end
{% endhighlight %}

## Testing

Depending on how your tests are set up, switching to bcrypt could
slow them down. Changing the work factor is the easiest way to avoid
this slowdown. In the next version, bcrypt-ruby will support setting
the cost with `BCrypt::Engine.cost = x`. For the time being, monkey
patching is the way to go. Drop this into `spec/support/bcrypt.rb`:

{% highlight ruby %}
require 'bcrypt'
module BCrypt
  class Engine
    Kernel.silence_warnings do
      DEFAULT_COST = 1
    end
  end
end
{% endhighlight %}

[1]: /static/images/2013-05-10-crypt.jpg
[2]: http://commons.wikimedia.org/wiki/File:Wola_Gułowska-trumna.jpg
[3]: https://github.com/codahale/bcrypt-ruby
