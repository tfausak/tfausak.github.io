---
title: Announcing ActiveInteraction 2
---

*([Aaron Lasseigne][] co-authored this post for the [OrgSync Dev Blog][].)*

![ActiveInteraction logo][]

## Transactions

- removed transaction support
- proved not to be a good default
- without the default, `transaction` didn't add much
- warning for `transaction` in 1.6
- use `ActiveRecord::Base.transaction` instead

## Errors

- replaced symbolic errors with detailed errors
- this is what rails 5 will use
- replaces `symbolic` with `details` (differnt return type)
- replaces `add_sym` with `add` (different arity)
- warning in 1.6

## Objects

- renamed model to object
- initially designed for ActiveRecord models
- useful for any type of object
- warning in 1.6

## Results

- added results to invalid outcomes
- this allows returning a common object from an interaction
- this is mostly useful when updating an existing record
- no warning in 1.6

## Hashes

- use hashes with indifferent access throughout
- prevent DoS attack based on symbol keys
- no warning in 1.6

## Files

- file filter accepts anything that respond to `#eof?`
- used to special case uploaded files for rails
- this allows for more IO-type objects
- no warning in 1.6

## Defaults

- prevented proc defaults from being eagerly evaluated
- they were evaluated due to an oversight when adding this feature
- should never have been eagerly evaluated in the first place
- no warning in 1.6

## Stats

- from 1.6.0 to 2.0.0
  - 111 commits
  - 234 additions
  - 614 deletions
- from 1.0.0 to 2.0.0
  - 587 commits
  - 4,051 additions
  - 1,051 deletions
- contributions from
  - orgsync
    @AaronLasseigne
    @camdez
    @caseywebdev
    @tfausak
  - not orgsync
    @LimeBlast
    @nilcolor
    @spaghetticode
    @techpeace
    TODO others

[aaron lasseigne]: http://aaronlasseigne.com
[orgsync dev blog]: http://devblog.orgsync.com/2015/05/07/TODO/
[activeinteraction logo]: /static/images/2015/05/07/active-interaction.svg
