---
title: Announcing ActiveInteraction 2
---

*([Aaron Lasseigne][] co-authored this post for the [OrgSync Dev Blog][].)*

![ActiveInteraction logo][]

We are proud to announce the release of [ActiveInteraction][] 2.
This is the first major version change since we [released ActiveInteraction][] more than a year ago.
We made some backwards-incompatible changes that make working with interactions easier and faster.

To simplify transitioning from 1.5.1 to 2.0.0,
we are also releasing 1.6.0.
It backports some features from 2.0.0
and adds deprecations warnings for features that will be removed.

This blog post explains the changes we made and why we made them.
It also shows you how to update interactions from 1.5.1 to 2.0.0.

## Transactions

We removed support for ActiveRecord transactions ([issue 205][]).
This means that interactions are not wrapped in a transaction by default.
To retain the old behavior,
wrap your `execute` method in an `ActiveRecord::Base.transaction` block.

We also removed the `transaction` method,
since it does not do anything anymore.

We decided to remove transactions because we saw that most interactions did not need them.
The added cost and chance of deadlocks was not worth it in general.

``` rb
# v1.6
class Example < ActiveInteraction::Base
  # This is the default.
  transaction true

  def execute
    # ...
  end
end

# v2.0
class Example < ActiveInteraction::Base
  def execute
    ActiveInteraction::Base.transaction do
      # ...
    end
  end
end
```

You will get a deprecation warning if you use `transaction` with 1.6.

## Errors

We replaced symbolic errors with detailed errors ([issue 250][]).
Detailed errors will be part of Rails 5.
We started using symbolic errors in version 0.6.0
and are happy to see something similar make its way into Rails 5.
Unfortunately their APIs differ slightly.
See the example below for details.

If you want to use detailed errors in your own code,
check out the [active_model-errors_details][] gem.

``` rb
# v1.6
class Example < ActiveInteraction::Base
  def execute
    errors.add_sym :base, :invalid
    errors.add_sym :base, :custom, '...'
  end
end
Example.run.errors.symbolic
# => {:base=>[:invalid,:custom]}

# v2.0
class Example < ActiveInteraction::Base
  def execute
    errors.add :base, :invalid
    errors.add :base, :custom, message: '...'
  end
end
Example.run.errors.details
# => {:base=>[{:error=>:invalid},{:error=>:custom,:message=>'...'}]}
```

You will get a deprecation warning if you use either `add_sym` or `symbolic` with 1.6.

## Objects

We renamed the `model` filter to `object` ([issue 264][]).
This more accurately reflects what the filter can be used for.
We initially used the `model` filter for ActiveModel objects.
But it works with any object,
so the name was misleading.

``` rb
# v1.6
class Example < ActiveInteraction::Base
  model :regexp
end
Example.run(regexp: /.../)

# v2.0
class Example < ActiveInteraction::Base
  object :regexp
end
Example.run(regexp: /.../)
```

You will get a deprecation warning if you use `model` with 1.6.

## Hashes

We switched the `hash` filter to use indifferent access ([issue 164][]).
This prevents a possible denial of service attack.
As a result,
hash keys will be strings instead of symbols.

``` rb
class Example < ActiveInteraction::Base
  hash :options,
    strip: false

  def execute
    options.keys
  end
end

# v1.6
Example.run!(options: { enable: true })
# => [:enable]

# v2.0
Example.run!(options: { enable: true })
# => ["enable"]
```

## Files

We changed the `file` filter to accept anything that responds to `eof?` ([pull 236][]).
It used to accept only `File`s and `Tempfile`s.
Now it accepts a wider range of IO objects.

``` rb
class Example < ActiveInteraction::Base
  file :io

  def execute
    io.read
  end
end

# v1.6
Example.run!(io: StringIO.new('Hello, world!'))
# ActiveInteraction::InvalidInteractionError: Io is not a valid file

# v2.0
Example.run!(io: StringIO.new('Hello, world!'))
# => "Hello, world!"
```

## Results

We added the ability to return results from invalid interactions ([issue 168][]).
Setting the result to `nil` was unnecessary.
The right way to check for validity is to use `valid?`.
This change allows you to return something from an interaction even if there are errors.
This can be very useful when updating an existing record.

``` rb
class Example < ActiveInteraction::Base
  def execute
    errors.add(:base)
    'something'
  end
end

# v1.6
outcome = Example.run
outcome.valid?
# => false
outcome.result
# => nil

# v2.0
outcome = Example.run
outcome.valid?
# => false
outcome.result
# => "something"
```

## Defaults

We made it so `Proc` defaults are not eagerly evaluated ([issue 269][]).
They never should have been in the first place.
This was an oversight when we introduced this feature.

``` rb
class Example < ActiveInteraction::Base
  boolean :flag,
    default: -> {
      puts 'defaulting...'
      true
    }

  def execute
    puts 'executing...'
  end
end

# v1.6
# defaulting...
Example.run
# executing...

# v2.0
Example.run
# defaulting...
# executing...
```

## Contributors

A big thanks to everyone who contributed to ActiveInteraction!
We could not have done it without you.

- [Aaron Lasseigne][]
- [Alexey Blinov][]
- [Alexey Shein][]
- [Andrea Longhi][]
- [Cameron Desautels][]
- [Casey Foster][]
- [Daniel Hollands][]
- [Matt Buck][]
- [Taylor Fausak][]

[aaron lasseigne]: http://aaronlasseigne.com
[orgsync dev blog]: http://devblog.orgsync.com/2015/05/07/TODO/
[activeinteraction logo]: /static/images/2015/05/07/active-interaction.svg
[activeinteraction]: http://devblog.orgsync.com/active_interaction/
[released ActiveInteraction]: {% post_url 2014-01-23-confidently-manage-business-logic-with-active-interaction %}
[issue 205]: https://github.com/orgsync/active_interaction/issues/205
[issue 250]: https://github.com/orgsync/active_interaction/issues/250
[active_model-errors_details]: https://rubygems.org/gems/active_model-errors_details
[issue 264]: https://github.com/orgsync/active_interaction/issues/264
[issue 164]: https://github.com/orgsync/active_interaction/issues/164
[pull 236]: https://github.com/orgsync/active_interaction/pull/236
[issue 168]: https://github.com/orgsync/active_interaction/issues/168
[issue 269]: https://github.com/orgsync/active_interaction/issues/269
[aaron lasseigne]: https://github.com/AaronLasseigne
[alexey blinov]: https://github.com/nilcolor
[alexey shein]: https://github.com/conf
[andrea longhi]: https://github.com/spaghetticode
[cameron desautels]: https://github.com/camdez
[casey foster]: https://github.com/caseywebdev
[daniel hollands]: https://github.com/LimeBlast
[matt buck]: https://github.com/techpeace
[taylor fausak]: https://github.com/tfausak
