---
title: Immediately Migrating Existing Passwords to bcrypt
---

> Security cannot afford to be "eventually consistent".

That's [Geoffroy Couprie's response][1] to my last post about
[upgrading to bcrypt][2]. He's right, of course. The solution he
proposed is the same one [kcen suggested][3] on Reddit:

> I solved this problem at a company I joined a couple years ago.
> \[...\] I created a new database column, bcrypted all of those
> hashes, then dropped the original password column from the database.

At first, and for no good reason, I disliked the idea of bcrypting
a password hash. It just felt weird. I asked around and everyone
agreed: weird, but no real objections.

So I decided to do a little research. After all, combining cryptographic
primitives in the wrong way is an easy way to do cryptography wrong.
I eventually found [a question on the cryptography Stack Exchange][4]
that assuaged my fears. It said that "the overall idea is a sound
migration strategy", which was good enough for me.

## Setup

Assume that everything's the same as before, except that the passwords
are hashed. It doesn't matter how they're hashed or if a salt is
used. Let's keep it simple by calling the hash function `digest`
and storing the result in `password_hash`.

{% highlight ruby %}
def digest(password)
  password.hash.to_s
end
# user.password_hash = digest(password)
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
    user.bcrypt = password
    user.save!
    user
  end
end
{% endhighlight %}

## Migrate

After doing that, the only thing left to do is the actual migration.
Be warned: this will take a long time. Although the exact time
depends on your machine, you can get an estimate using the
[benchmark][5] module.

{% highlight ruby %}
Benchmark.measure do
  100.times do
    BCrypt::Password.create('secret')
  end
end.total
# => 7.45
{% endhighlight %}

The migration itself is pretty straightforward. It has three moving
parts:

1.  Grab unmigrated users in chunks until none are left. This allows
    the migration to pick up from where it left off it it gets
    interrupted. In addition, users migrated through the authenticate
    method won't throw a wrench in the works.

2.  Calculate a bcrypt hash for each user using the password hash
    as input. This part will take a while, since bcrypt is designed
    to be slow.

3.  Save the bcrypt hash to the database. Using `update_column`
    avoids triggering callbacks or running validators.

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

Although you could remove `password_hash` entirely in this migration,
it's better to do that as a separate migration after this one
finishes. That way if anything goes wrong with the switch to bcrypt
you can fall back to the old method.

[1]: https://twitter.com/gcouprie/status/335888084170338304
[2]: {% post_url 2013-05-08-upgrading-to-bcrypt %}
[3]: http://www.reddit.com/r/rails/comments/1e049z/upgrading_to_bcrypt/c9vws08
[4]: http://crypto.stackexchange.com/questions/2945/is-this-password-migration-strategy-secure
[5]: http://www.ruby-doc.org/stdlib-2.0/libdoc/benchmark/rdoc/Benchmark.html
