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
It said that "the overall idea is a sound migration strategy," which was good enough for me.

Motivated by that, and [Geoffrey Couprie's Tweet][4] that "security cannot afford to be eventually consistent", here's how to immediately migrate passwords to bcrypt.

* * *

- everything in the old post stands
- just add this migration
- it will take a long time to run
- that's a good thing, though

- unlike the last post, let's assume you hash your passwords
- it doesn't matter how (md5, sha1, etc.)
- or if you use a salt
- for simplicity, assume `hash(password)` is what you do
- and that you store the result in a column called `password_hash`

- now instead of calculating bcrypt hashes using the plain text, you'll use the hash
- `bcrypt(hash(password))`
- this goes for both comparing and storing
- so we need to update the user model and the authenticate method

{% highlight ruby %}
def bcrypt=(new_password)
  @bcrypt = Password.create(hash(new_password))
  self.bcrypt_hash = @bcrypt
end
{% endhighlight %}

{% highlight ruby %}
def self.authenticate(username, password)
  user = find_by_username(username)
  return unless user
  if user.bcrypt?
    user if user.bcrypt == password
  elsif hash(password) == user.password_hash
    user.bcrypt = hash(password)
    user.password = nil
    user.save!
    user
  end
end
{% endhighlight %}

{% highlight ruby %}
class CalculateBcryptHashes < ActiveRecord::Migration
  class User < ActiveRecord::Base; end
  def up
    loop do
      #
      users = User.select([:id, :password_hash]).
        where(:bcrypt_hash => nil).order(:id).limit(100)
      break if users.empty?
      #
      users.each do |user|
        user['bcrypt_hash'] =
          BCrypt::Password.create(user['password_hash'])
      end
      #
      users.each do |user|
        user.update_column(:bcrypt_hash, user['bcrypt_hash'])
      end
    end
  end
end
{% endhighlight %}

[1]: http://www.reddit.com/r/rails/comments/1e049z/upgrading_to_bcrypt/c9vws08
[2]: {% post_url 2013-05-08-upgrading-to-bcrypt %}
[3]: http://crypto.stackexchange.com/questions/2945/is-this-password-migration-strategy-secure
[4]: https://twitter.com/gcouprie/status/335888084170338304
