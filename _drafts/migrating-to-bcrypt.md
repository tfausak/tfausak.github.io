---
layout: post
title: Migrating to bcrypt
---

> The transition from the old style pw store to the bcrypted store leaves far too much in limbo and essentially leaves you with a large set of known insecure passwords in your database.
http://www.reddit.com/r/rails/comments/1e049z/upgrading_to_bcrypt/c9vws08

> @jjarmoc @taylorfausak security cannot afford to be "eventually consistent" ;)
https://twitter.com/gcouprie/status/335888084170338304

> The overall idea is a sound migration strategy.
http://crypto.stackexchange.com/questions/2945/is-this-password-migration-strategy-secure

/2013/05/08/upgrading-to-bcrypt/

{% highlight ruby %}
class CalculateBcryptHashes < ActiveRecord::Migration
  class User < ActiveRecord::Base; end
  def up
    loop do
      users = User.select([:id, :password_hash]).
        where(:bcrypt_hash => nil).order(:id).limit(100)
      break if users.empty?
      users.each do |user|
        user['bcrypt_hash'] =
          BCrypt::Password.create(user['password_hash'])
      end
      users.each do |user|
        user.update_column(:bcrypt_hash, user['bcrypt_hash'])
      end
    end
  end
end
{% endhighlight %}
