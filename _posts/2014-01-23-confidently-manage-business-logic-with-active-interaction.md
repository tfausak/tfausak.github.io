---
layout: post
title: Confidently Manage Business Logic with Active­Interaction
---

*([Aaron Lasseigne][1] co-authored this post for the [OrgSync
Developer Blog][2].)*

We are proud to announce the release of [ActiveInteraction][3]
version 1.0. ActiveInteraction is a gem for managing application
specific business logic. Instead of living in controllers or models,
business logic can find a home in interactions. They are designed
to integrate seamlessly with Rails by behaving like ActiveModels.
Use ActiveInteraction to shrink your controllers, slim your models,
and DRY your code.

## Case Study

We built ActiveInteraction to solve a particular kind of problem.
It's one we think lots of Rails developers grapple with. Let's walk
through an example to see how ActiveInteraction can help.

### The Fat Controller

Back in 2007, OrgSync started on Rails one point something. It
looked like most Rails projects. Had it started on Rails 4, it would
probably look like this:

{% highlight ruby %}
class UserController < ActionController::Base
  def create
    unless params[:user].key?(:password)
      params[:user][:password] = SecureRandom.hex
    end

    @user = User.new(user_params)

    if @user.save
      Notifications.welcome(@user).deliver

      redirect_to @user
    else
      render 'new'
    end
  end

  private

  def user_params
    params.require(:user).permit(:email, :password)
  end
end
{% endhighlight %}

This approach is littered with problems:

- It blows up if `params[:user]` isn't a hash.
- It lacks contextual information about why it does what it does.
- It has too many responsibilities.
- Controller specific setup is required in order to test the logic.

### The Fat Model

If the business logic doesn't belong in the controller, where does
it belong? The model is a natural fit since all this logic deals
with it.

{% highlight ruby %}
class User < ActiveRecord::Base
  before_save :ensure_password
  after_create :send_welcome_email

  private

  def ensure_password
    if password.nil?
      self.password = SecureRandom.hex
    end
  end

  def send_welcome_email
    Notifications.welcome(self).deliver
  end
end
{% endhighlight %}

The controller slims down and stays focused on request logic.

{% highlight ruby %}
class UserController < ActionController::Base
  def create
    @user = User.new(user_params)

    if @user.save
      redirect_to @user
    else
      render 'new'
    end
  end

  private

  def user_params
    params.require(:user).permit(:email, :password)
  end
end
{% endhighlight %}

Something still isn't right. The controller has to know which
parameters the model cares about. And what if you don't want to
send a welcome email? Skipping callbacks is possible, but it's a
pain.

In addition, we moved the fat from controllers to models. Although
that put the logic closer to where it belongs, we managed to pollute
our model with email delivery and password generation. Models should
be concerned with validation, storage, and retrieval.

### Enter Mutations

We couldn't help but feel like something was missing, but we didn't
know what the next step of our journey would be. That's when we
stumbled upon [Architecture the Lost Years][4], a presentation by
Robert Martin. It introduced us to the Interactor pattern, which
we loved.

We searched for a Ruby interactor library and found [Mutations][5].
It seemed to fit our needs, so we began moving our business logic
into interactions.

{% highlight ruby %}
class CreateUser < Mutations::Command
  required do
    string :email, matches: /^.+@.+$/
    boolean :send_welcome_email, default: true
  end

  optional do
    string :password
  end

  def execute
    ensure_password

    user = User.create!(inputs.slice(:email, :password))

    if send_welcome_email
      Notifications.welcome(user).deliver
    end

    user
  end

  private

  def ensure_password
    unless password_present?
      self.password = SecureRandom.hex
    end
  end
end
{% endhighlight %}

It resulted in more lines of code, but they were *better* lines of
code. We quickly saw the benefits of this approach. We were able
to easily share code between the web and API controllers. And thanks
to its declarative nature, generating documentation was a piece of
cake.

Models no longer contained conceptually distinct but practically
tangled business logic. Instead, each piece of logic got its own
easily understandable file. Models slimmed way down.

{% highlight ruby %}
class User < ActiveRecord::Base
  # Down to a size 0!
end
{% endhighlight %}

The controller grew by a few lines but it still only dealt with
what it had to.

{% highlight ruby %}
class UserController < ActionController::Base
  def create
    outcome = CreateUser.run(params)

    if outcome.success?
      redirect_to outcome.result
    else
      @user = User.new

      outcome.errors.message.each do |attribute, message|
        unless @user.has_attribute?(attribute)
          attribute = :base
        end

        @user.errors.add(attribute, message)
      end

      render 'new'
    end
  end
end
{% endhighlight %}

This direction looked promising, but had a few problems. Notice how
the controller creates a model solely for attaching errors. Mutation
results don't quack like ActiveModels, so using them with forms
feels like a square peg in a round hole.

Mutations purposefully separates itself from Rails, which is a
perfectly reasonable design decision. It comes at a cost though.
Custom validators are rendered useless and Rails specific classes,
like UploadedFile and TimeWithZone, aren't supported.

### Introducing ActiveInteraction

We wanted our custom validators and times with zones. We wanted
interoperability with gems like Formtastic. We wanted a library
built with Rails in mind.

We took what we loved from Mutations and built the gem we wanted.

{% highlight ruby %}
class CreateUser < ActiveInteraction::Base
  string :email
  string :password, default: nil
  boolean :send_welcome_email, default: true

  validate :ensure_password

  validates :email, email: true
  validates :password, presence: true

  def execute
    user = User.create!(inputs.slice(:email, :password))

    if send_welcome_email
      Notifications.welcome(user).deliver
    end

    user
  end

  private

  def ensure_password
    unless password?
      self.password = SecureRandom.hex
    end
  end
end
{% endhighlight %}

Similar interfaces made the transition from Mutations to ActiveInteraction
quick and painless. And just like before, the model ends up empty.

{% highlight ruby %}
class User < ActiveRecord::Base; end
{% endhighlight %}

Unlike before, the controller changes very little.

{% highlight ruby %}
class UserController < ActionController::Base
  def create
    outcome = CreateUser.run(params[:user])

    if outcome.valid?
      redirect_to outcome.result
    else
      @user = outcome
      render 'new'
    end
  end
end
{% endhighlight %}

Notice how the invalid outcome is assigned straight to `@user`.
That's because the outcome of running an interaction quacks like
an ActiveModel. It can be dropped right into a form without having
to jump through any hoops.

## Conclusion

We're so thrilled with this direction that we've been using
interactions in production since July. Our development team has
provided valuable feedback and a variety of use cases. Thanks to
them, we've settled on a solid interface and a compelling feature
set. It's been a great addition to our code base and we hope it
helps yours.

Check out the full documentation and more about [ActiveInteraction
on GitHub][3].

[1]: http://aaronlasseigne.com
[2]: http://devblog.orgsync.com/confidently-manage-business-logic-activeinteraction/
[3]: https://github.com/orgsync/active_interaction
[4]: http://www.confreaks.com/videos/759-rubymidwest2011-keynote-architecture-the-lost-years
[5]: https://github.com/cypriss/mutations
