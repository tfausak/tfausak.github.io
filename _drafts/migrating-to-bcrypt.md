---
layout: post
title: Migrating to bcrypt
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

Assume everything is the same as before, with one notable exception: passwords are hashed.
The method of hashing is unimportant, as is the presence of a salt.
For simplicity's sake, let's say they're hashed with a function called `digest`.
And instead of saving the password itself, save the hash into a field called `password_hash`.

{% highlight ruby %}
def digest(password)
  password.hash.to_s
end
# account.password_hash = digest(password)
{% endhighlight %}

Now instead of generating bcrypt hashes using the plain text, use the hash.
This goes for comparing, too.
We'll need to make two changes to the user model:
update the bcrypt assignment to use the hash,
and modify the authenticate method to also use the hash.

{% highlight ruby %}
class User < ActiveRecord::Base
  def bcrypt=(new_password)
    @bcrypt = Password.create(digest(new_password))
    self.bcrypt_hash = @bcrypt
  end
  def self.authenticate(username, password)
    user = find_by_username(username)
    return unless user
    if user.bcrypt?
      user if user.bcrypt == password
    elsif digest(password) == user.password_hash
      user.bcrypt = digest(password)
      user.password = nil
      user.save!
      user
    end
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
