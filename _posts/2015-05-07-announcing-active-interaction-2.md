---
title: Announcing ActiveInteraction 2
---

*([Aaron Lasseigne][] co-authored this post for the [OrgSync Dev Blog][].)*

![ActiveInteraction logo][]

## Transactions

We removed support for ActiveRecord transactions ([issue 205][]).
Interactions are no longer wrapped in a transaction by default.
To retain the old behavior,
wrap your `execute` method in an `ActiveRecord::Base.transaction` block.

We also removed the `transaction` method,
since it does not do anything anymore.

You will get a deprecation warning if you use `transaction` with 1.6.

## Errors

We replaced symbolic errors with detailed errors ([issue 250][]).
We love symbolic errors,
but Rails 5 will use detailed errors.
They accomplish the same thing in slightly different ways.

Instead of adding symbolic errors with `add_sym`,
add detailed errors with `add`.
So instead of `errors.add_sym(:foo, :bar)`,
just do `errors.add(:foo, :bar)`.
(`add_sym` also took an optional third parameter to use as the message.
Pass a `message` option to `add` for that behavior.
`errors.add_sym(:foo, :bar, 'bar')` becomes `errors.add(:foo, :bar, message: 'bar')`.)

And instead of reading symbolic errors with `symbolic`,
get detailed errors with `details`.
So instead of `errors.symbolic[:foo]`,
do `errors.details[:foo]`.
(`symbolic` returned an array of symbolic.
`details` returns an array of hashes.
The symbol is available at the `:error` key.
`[:bar]` becomes `[{ error: :bar }]`.)

You will get a deprecation warning if you use either `add_sym` or `symbolic` with 1.6.

## Objects

We renamed the `model` filter to `object` ([issue 264][]).
This more accurately reflects what the filter can be used for.
We initially used the `model` filter for ActiveModel objects.
But it works with any object,
so the name was misleading.

You will get a deprecation warning if you use `model` with 1.6.

## Hashes

We switched the `hash` filter to use indifferent access ([issue 164][]).
This prevents a possible denial of service attack.
Hash keys will be strings instead of symbols.

## Files

We changed the `file` filter to accept anything that respond to `eof?` ([pull 236][]).
We used to accept either a `File` or a `Tempfile`.
This change accepts a wider range of IO objects.

## Results

We added the ability to return results from invalid interactions ([issue 168][]).
Setting the result to `nil` was unnecessary since the right way to check for validity is to use `valid?`.
This change allows you to return something from an interaction even if something goes wrong.
This can be very useful when updating an existing record.

## Defaults

We made it so `Proc` defaults are not eagerly evaluated ([issue 269][]).
They never should have been eagerly evaluated.
This was an oversight when we introduced this feature.

## Contributors

A big thanks to everyone who contributed to ActiveInteraction!

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
[issue 205]: https://github.com/orgsync/active_interaction/issues/205
[issue 250]: https://github.com/orgsync/active_interaction/issues/250
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
