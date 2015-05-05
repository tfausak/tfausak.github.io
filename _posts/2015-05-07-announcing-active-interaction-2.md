---
title: Announcing ActiveInteraction 2
---

*([Aaron Lasseigne][] co-authored this post for the [OrgSync Dev Blog][].)*

![ActiveInteraction logo][]

## Transactions

- https://github.com/orgsync/active_interaction/issues/205
- removed transaction support
- proved not to be a good default
- without the default, `transaction` didn't add much
- warning for `transaction` in 1.6
- use `ActiveRecord::Base.transaction` instead

## Errors

- https://github.com/orgsync/active_interaction/issues/250
- replaced symbolic errors with detailed errors
- this is what rails 5 will use
- replaces `symbolic` with `details` (differnt return type)
- replaces `add_sym` with `add` (different arity)
- warning in 1.6

## Objects

- https://github.com/orgsync/active_interaction/issues/264
- renamed model to object
- initially designed for ActiveRecord models
- useful for any type of object
- warning in 1.6

## Hashes

- https://github.com/orgsync/active_interaction/issues/164
- use hashes with indifferent access throughout
- prevent DoS attack based on symbol keys
- no warning in 1.6

## Files

- https://github.com/orgsync/active_interaction/pull/236
- file filter accepts anything that respond to `#eof?`
- used to special case uploaded files for rails
- this allows for more IO-type objects
- no warning in 1.6

## Results

- https://github.com/orgsync/active_interaction/issues/168
- added results to invalid outcomes
- this allows returning a common object from an interaction
- this is mostly useful when updating an existing record
- no warning in 1.6

## Defaults

- https://github.com/orgsync/active_interaction/issues/269
- prevented proc defaults from being eagerly evaluated
- they were evaluated due to an oversight when adding this feature
- should never have been eagerly evaluated in the first place
- no warning in 1.6

## Stats

- https://github.com/orgsync/active_interaction/compare/v1.0.0...v2.0.0
- from 1.0.0 to 2.0.0
- 587 commits
- 4,051 additions
- 1,051 deletions

## Contributors

- @AaronLasseigne Aaron Lasseigne
- @camdez         Cameron Desautels
- @caseywebdev    Casey Foster
- @conf           Alexey Shein
- @LimeBlast      Daniel Hollands
- @nilcolor       Alexey Blinov
- @spaghetticode  Andrea Longhi
- @techpeace      Matt Buck
- @tfausak        Taylor Fausak

[aaron lasseigne]: http://aaronlasseigne.com
[orgsync dev blog]: http://devblog.orgsync.com/2015/05/07/TODO/
[activeinteraction logo]: /static/images/2015/05/07/active-interaction.svg
