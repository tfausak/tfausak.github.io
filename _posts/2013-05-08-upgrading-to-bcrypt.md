---
layout: post
title: Upgrading to bcrypt
---

<aside>Update: <a href="{% post_url 2013-05-21-immediately-migrating-existing-passwords-to-bcrypt %}">Immediately Migrating Existing Passwords to bcrypt</a></aside>

[![Crypt][1]][2]

Every so often, someone hacks a company and steals their database.
Usually the database contains a bunch of email addresses and
passwords. Two weeks ago, [LivingSocial][3] was hacked, leaking 50
million users' data. Even companies as big as [Sony][4] aren't
immune; they were hacked in 2011 and had 77 million users' data
stolen.

As a developer, it's your responsibility to protect your users'
data should this happen to you. Depending on how you store passwords,
it can either be trivial or impossible for attackers to compromise
your users' accounts. From least to most secure, these six formats
cover the majority of password storage techniques:

1.  Plain text
2.  Obfuscated (Caesar cipher)
3.  Encrypted (Triple DES)
4.  Hashed (MD5)
5.  Salted and hashed
6.  Computed from a key derivation function (bcrypt)

It's unequivocally better to store passwords securely. There are
no downsides and brute-force attacks become orders of magnitude
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
the authenticate method.

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
it. These all require the [bcrypt-ruby][5] gem, so add `gem
'bcrypt-ruby'` to your Gemfile.

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

Lastly, the authenticate function needs to be modified. It should
compare using bcrypt if the user has been updated. If they haven't,
it should compare using the old method.

Once a user authenticates using the old method, it should generate
a bcrypt hash for them so it'll use that next time. In addition,
it needs to delete data stored by the old method. If it doesn't,
an attacker could just focus their efforts on the legacy data.

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
    user_ids = ActiveRecord::Base.connection.select_all(
        'SELECT id FROM users WHERE bcrypt_hash IS NULL').
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

    user_ids.zip(passwords).each do |user_id, password|
      # Send an email, generate a notification, ...
    end
  end
end
{% endhighlight %}

## Testing

Depending on how your tests are set up, switching to bcrypt could
slow them down. Changing the work factor is the easiest way to avoid
this slowdown. The next version of bcrypt-ruby will support setting
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

## Conclusion

Upgrading a legacy system to use bcrypt isn't that hard. You should
do it sooner rather than later. In the unlikely (but entirely
possible) event of a database leak, your users' passwords will
be protected.

[1]: /static/images/2013/05/10/crypt.jpg
[2]: http://commons.wikimedia.org/wiki/File%3AWola_Gu%C5%82owska-trumna.jpg
[3]: https://www.livingsocial.com/createpassword
[4]: http://blog.us.playstation.com/2011/04/26/update-on-playstation-network-and-qriocity/
[5]: https://github.com/codahale/bcrypt-ruby
