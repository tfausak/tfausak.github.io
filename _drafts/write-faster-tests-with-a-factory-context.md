---
layout: post
title: Write Faster Tests with a Factory Context
---

At [OrgSync][1], we test our Ruby code with [RSpec][2] and
[factory_girl][3]. A while back, I noticed our tests were slowing
down for no apparent reason. It turns out our factories were creating
a bunch of unnecessary objects behind the scenes. For example, the
event factory ultimately creates 19 objects. Of those, 12 are
redundant.

It's possible to combat this by specifying the associations, which
is what I did. I got fed up with manually doing that all the time
and made a shared context. It ended up being much faster and a lot
easier to use.

Here's how I did it, with Ruby 2.1.0p0, rspec 2.14.1, and factory_girl
4.3.0.

Let's get started by writing some simple classes. We're going to
model a Reddit-style site with users, posts, and votes. Posts are
submitted by users and users cast votes on posts.

In real life these would be ActiveRecord objects. They'd have
full-blown associations using `has_many` and `belongs_to`. But
that's not necessary for this simple example.

{% highlight ruby %}
class User
  attr_accessor :id
end

class Post
  attr_accessor :id, :user
end

class Vote
  attr_accessor :id, :post, :user
end
{% endhighlight %}

Next we're going to create factories for these classes. They're
pretty simple, but we have to use the `:build` strategy on associations.
If we didn't, factory_girl would try to save them. They won't respond
to `save!` because they aren't ActiveRecord objects.

{% highlight ruby %}
factory :user do
  sequence(:id)
end

factory :post do
  sequence(:id)
  association :user,
    strategy: :build
end

factory :vote do
  sequence(:id)
  association :post,
    strategy: :build
  association :user,
    strategy: :build
end
{% endhighlight %}

Now we can use those factories in some tests. This particular test
doesn't do much, but it does show that two posts will be created.

{% highlight ruby %}
let(:post) { build(:post) }
let(:vote) { build(:vote) }

it do
  expect(vote.post).to_not eq(post)
end
{% endhighlight %}

Sometimes this is what you want, but usually it isn't. To avoid
creating extra objects, you need to specify all of the associations.
That's tedious and error-prone, especially as the number of objects
increases.

So to keep from repeating yourself, put all the definitions in a
shared context.

{% highlight ruby %}
shared_context 'factories' do
  let(:user) do
    build(:user)
  end

  let(:post) do
    build(:post, user: user)
  end

  let(:vote) do
    build(:vote, post: post, user: user)
  end
end
{% endhighlight %}

Then you can include the context and just start talking about the
objects you want. You don't have to build anything, and fewer objects
will be created behind the scenes.

For example, this test creates half as many objects as the last one.

{% highlight ruby %}
include_context 'factories'

it do
  expect(vote.post).to eq(post)
end
{% endhighlight %}

Let me repeat that: In this contrived example with three simple
models, using the context created *half* as many objects. In a real
test with real objects, that would result in a significant speedup.

But what if we wanted to use `let!` to eagerly load some objects?
It looks like the factory context won't let us do that. But it does
--- just talk about the objects that need to be loaded.

{% highlight ruby %}
before do
  user
  post
  vote
end
{% endhighlight %}

That's not great, though. It's not immediately obvious why those
statements are there. We can do better by adding a helper method
to the context.

{% highlight ruby %}
def preload(*factories)
  factories.each do |factory|
    send(factory)
  end
end
{% endhighlight %}

Now you can use `preload` when you want to eagerly load an object.

{% highlight ruby %}
before do
  preload(:user, :post, :vote)
end
{% endhighlight %}

After switching to a factory context, writing tests got easier and
running tests got faster. Plus we didn't lose any expressiveness
compared to the old way. What's not to like?

[1]: http://www.orgsync.com
[2]: http://rspec.info
[3]: https://github.com/thoughtbot/factory_girl
