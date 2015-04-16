---
title: On the reaction to Flow
---

My last post [introduced Flow][1], a library for writing more
understandable Haskell. The response to it [on Reddit][2] surprised
me. It was more negative than I expected.

Many people who commented assumed that I didn't understand idiomatic
Haskell. This caught me off guard. I explicitly talked about and
showed idiomatic Haskell in my post. Flow provides alternatives to
some of the current idioms.

> I would suggest that you learn the community's syntax so you can
> interact with them more effectively. --- [mightybyte][3]

> Modify your understanding to be more compatible with idiomatic
> Haskell. --- [ReinH][4]

> I think you'll find that after becoming a little more experienced
> with Haskell it's perfectly possible to ignore most of these
> [operators like `.` and `$`] when reading code. --- [katieandjohn][5]

Other people assumed that I had a poor understanding of math. I can
understand that because math was completely absent from my post. I
did that intentionally; I don't think it's helpful to pretend that
programming and math are the same. That being said, I understand
how function composition works. I've taken ten years of math classes.

> I remember the composition operator felt like it was backwards
> when I learned it in a math class. But then I had another ten years
> of math classes. --- [chreekat][6]

> If and when mathematics notation is reinvented then we can start
> composing things the "right" way round. --- [tomejaguar][7]

> I'm much more prone to trust mathematical precedent than the
> vagaries of syntax in programming languages. --- [amyers127][8]

So far the comments I've shown have been personal attacks. They
seem to say that I would be okay with `.` and `$` if only I understood
Haskell (or math) better. I think these types of comments are shitty,
but I guess they're to be expected on the internet.

These next comments actually critisize Flow, which I welcome. Because
Flow allows you to express operations as a pipeline, it can look
imperative. Some people don't like that, which confused me. Avoiding
imperative-looking code in functional programming doesn't help
anyone. It's like saying you shouldn't use the imperative-looking
[`for`][9] because the functional-looking [`traverse`][10] is better
somehow. Sometimes it makes more sense to think about things
imperatively, regardless of how they actually get evaluated. That's
the whole idea behind `do` notation!

> I want to express things as to what they are in terms of value
> and not in terms of how to get to these values, which is what
> Imperative Programming does. --- [evohunz][11]

> I don't see function application as English prose (read left to
> right), but rather a mathematical construct read from the argument.
> --- [amyers127][12]

This argument reminds me of the [useless use of `cat`][13]. For the
uninitiated, `cat` ends up in a lot of places where it is "useless"
because the program you pipe it into can do the same thing. For
example, these commands are all identical:

{% highlight sh %}
$ cat a-file | grep a-pattern
$ grep a-pattern < a-file
$ grep a-pattern a-file
{% endhighlight %}

Depending on the circumstances, any one of those could be the best
way to express yourself. Flow tries to give you that freedom with
Haskell. Consider the following equivalent expressions:

{% highlight hs %}
aValue |> aFunction |> anotherFunction
anotherFunction . aFunction $ aValue
anotherFunction (aFunction aValue)
{% endhighlight %}

I didn't mean to imply that you should use Flow all the time. I
think Flow allows you to write more understandable Haskell in certain
circumstances. I don't anticipate it completely replacing the
existing idioms. Perhaps I should have expressed that in my last
blog post.

[1]: {% post_url 2015-04-09-write-more-understandable-haskell-with-flow %}
[2]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow
[3]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow/cq7tx3k
[4]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow/cq847a1
[5]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow/cq7serc
[6]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow/cq7u12s
[7]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow/cq7x7jm
[8]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow/cq7s59v
[9]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Traversable.html#v:for
[10]: http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Traversable.html#v:traverse
[11]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow/cq7waya
[12]: http://www.reddit.com/r/haskell/comments/324415/write_more_understandable_haskell_with_flow/cq7qkgm
[13]: http://en.wikipedia.org/wiki/Cat_%28Unix%29#Useless_use_of_cat
