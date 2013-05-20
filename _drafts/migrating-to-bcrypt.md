---
layout: post
title: Migrating to bcrypt
---

/2013/05/08/upgrading-to-bcrypt/
http://www.reddit.com/r/rails/comments/1e049z/upgrading_to_bcrypt/c9vws08
http://crypto.stackexchange.com/questions/2945/is-this-password-migration-strategy-secure
https://twitter.com/gcouprie/status/335888084170338304

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
