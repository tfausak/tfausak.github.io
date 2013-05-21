---
layout: post
title: Immediately Migrating Existing Passwords to bcrypt
---

[Redditor kcen commented][1] on my last post about [upgrading to bcrypt][2], pointing out a problem and proposing a solution:

> The transition from the old style password store to the bcrypted store leaves far too much in limbo and essentially leaves you with a large set of known insecure passwords in your database. \[...\]
>
> I solved this problem at a company I joined a couple years ago. \[...\] I created a new database column, bcrypted all of those hashes, then dropped the original password column from the database.

At first, and for no good reason, I disliked the idea of bcrypting a password hash.
It just felt weird.
I asked some of my colleagues about it and they agreed.
However, none of us could come up with any real objections.

I decided to do a little research.
After all, combining cryptographic primitives in the wrong way is an easy way to do cryptography wrong.
I eventually found [a question on the cryptography Stack Exchange][3] that assuaged my fears.
It said that "the overall idea is a sound migration strategy", which was good enough for me.

Motivated by that, and [Geoffrey Couprie's Tweet][4] that "security cannot afford to be eventually consistent", here's how to immediately migrate passwords to bcrypt.

## Setup

Assume that everything's the same as before, except that the passwords
are hashed. It doesn't matter how they're hashed or if a salt is
used. Let's keep it simple by calling the hash function `digest`
and storing the result in `password_hash`.

{% highlight ruby %}
def digest(password)
  password.hash.to_s
end
# account.password_hash = digest(password)
{% endhighlight %}

Setting the password is now slightly more complicated than before.
Instead of simply using the plain text password as the input to
bcrypt, we have to use the password hash. This adds a layer of
indirection but allows us to migrate without knowing the original
passwords.

{% highlight ruby %}
def bcrypt=(new_password)
  @bcrypt = self.bcrypt_hash =
    Password.create(digest(new_password))
end
{% endhighlight %}

Similarly, checking passwords now requires comparing against the
hashed password.

{% highlight ruby %}
def self.authenticate(username, password)
  return unless user = find_by_username(username)
  password_hash = digest(password)
  if user.bcrypt?
    user if user.bcrypt == password_hash
  elsif user.password_hash == password_hash
    user.bcrypt = password_hash
    user.save!
    user
  end
end
{% endhighlight %}

## Migrate

Once that's done, all that's left is the actual migration.
It'll take a long time to run, but that's a good thing.
It's by design.

There are three main things going on with this migration:

1.  Grab unmigrated users in chunks until there are no more left.
    This allows the migration to pick up from where it left off if it gets interrupted.

2.  Calculate each user's bcrypt hash using the password hash as input.
    Since bcrypt is designed to be slow, this will take a while.

3.  Save the bcrypt hash.
    Using `update_column` avoids triggering callbacks or running validators.

{% highlight ruby %}
class BcryptMigration < ActiveRecord::Migration
  class User < ActiveRecord::Base; end
  def up
    loop do
      users = User.select([:id, :password_hash]).
        where(:bcrypt_hash => nil).order(:id).limit(100)
      break if users.empty?
      users.each do |user|
        bcrypt_hash =
          BCrypt::Password.create(user['password_hash'])
        user.update_column(:bcrypt_hash, bcrypt_hash)
      end
    end
  end
end
{% endhighlight %}

[1]: http://www.reddit.com/r/rails/comments/1e049z/upgrading_to_bcrypt/c9vws08
[2]: {% post_url 2013-05-08-upgrading-to-bcrypt %}
[3]: http://crypto.stackexchange.com/questions/2945/is-this-password-migration-strategy-secure
[4]: https://twitter.com/gcouprie/status/335888084170338304
