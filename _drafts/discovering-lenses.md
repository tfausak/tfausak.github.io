---
layout: post
title: Discovering Lenses
--

But working on strive my Haskell library for the Strawe API thing kind and interesting problem. I had several types with overlapping field names. Some of the fields have the same type but not all of them dead. The problem was how can I distinctly get and set those fields.

Going to walk you through my thought process because when I started I was afraid of lenses. I didn't want to use them because that would mean and be afraid of my code. So I tried some alternative solutions that didn't end up working. I hope to explain lenses away that evening on Hassler can understand.

Let's get started by the finding a type. Will represent an athlete and it will have one field their name.

    data Athlete = Athlete String

    athlete :: Athlete
    athlete = Athlete "Taylor"

You'll need to get the name back out of this type is Teasbend matching.

    getName :: Athlete -> String
    getName (Athlete name) = name

    print (getName athlete)
    -- "Taylor"

Similarly the only way to set the name is also Teasbend matching.

    setName :: Athlete -> String -> Athlete
    (Athlete _) name = Athlete name

    print (getName (setName athlete "Douglas"))
    -- "Douglas"

Writing all the boilerplate is tedious. Fortunately has classes covered. By using Records and text we can avoid writing most of that.

    data Athlete = Athlete { name :: String }

    athlete :: Athlete
    athlete = Athlete { name = "Taylor Fausak" }

Just by using the records index we get add get her for free.

    -- name :: Athlete -> String
    print (name athlete)
    -- "Taylor"

Unfortunately we don't get a function or a setter but there is some text for it.

    print (name (athlete { name = "Douglas" }))
    -- "Douglas"

This is the same as before except it's clear and we robust code.

Now what happens we try to add another type with the same field name.For instance what if we wanted to represent a club Which has a name. Haskell's type system falls down.

    data Athlete = Athlete { name :: String }
    data Club    = Club    { name :: String }
    — Multiple declarations of `name'

Even though both names have the same type the compiler won't allow this. we can use the Detonte simple option to tell GHC to output the D sugared code. Looking at that it becomes obvious why this is a problem.

    name :: Athlete -> String
    name (Athlete a) = a

    name :: Club :: String
    name (Club c) = c

We tried to define the same function twice with different signatures. so I can we get around this problem.

One way is to define each type in its own module. That way there field names won't collide with each other. so let's try that.

    -- Athlete.hs
    data Athlete = Athlete { name :: String }

    -- Club.hs
    data Club = Club { name :: String }

    club :: Club
    club = Club { name = "Fixed Touring" }

    -- Main.hs
    import Athlete
    import Club

    print (name anAthlete)
    -- Ambiguous occurrence `name'
    -- It could refer to either `Athlete.name`, ...
    --                       or `Club.name`, ...

Why did this happen? Beechfield is uniquely defined in the module but when you import both Haskell doesn't know which one you're talking about. fortunately this is easy to fix. all you have to do is use the fully qualified name.

    print (Athlete.name athlete)
    -- "Taylor"

    print (Club.name club)
    -- "Fixed Touring"

Is pretty ridiculous though. In this example the type names are pretty short but it's easy to imagine that they may be very long. In that case typing up up only qualified typing every time you want to access the field is ridiculous. But again Haskell saves us. We can do qualified imports of modules to give them shorter names.

    import qualified Athlete as A
    import qualified Club    as C

    print (A.name athlete)
    -- "Taylor Fausak"

    print (C.name club)
    -- "Fixed Touring"

That's a great solution and works really well for a lot of cases. However not perfect. Princetons what if your short names collide.? or in my case what if you want your library to be useful with a single import? There's no way to export modules from a module in Haskell. So I can't bre-X for my objects to the end-user.

So I needed a better solution. One thing us learn from izadi was prefixing field names with the typed name. It's not as the sink is what we just tried but it's better in other ways.

    data Athlete = Athlete { athleteName :: String }
    data Club    = Club    { clubName    :: String }

    athlete :: Athlete
    athlete = Athlete { athleteName = "Taylor Fausak" }

    club :: Club
    club = Club { clubName = "Fixed Touring" }

    print (athleteName athlete)
    -- "Taylor Fausak"

    print (clubName club)
    -- "Fixed Touring"

This is pretty verbose. However they can all be exported together. And there's no ambiguity about which field you're talking about. This is especially important if you Greybil names aren't descriptive. Another benefit of this approach is that you can use keywords is field names. Brenstance if you had a field called type it would not be prefixed with the typed name is something like object type.

Still even with all those positives this isn't a very appealing solution. The long names make dealing with records annoying. Using type classes we can do better.

    class HasName a where
        name :: a -> String

    instance HasName Athlete where
        name athlete = athleteName athlete

    instance HasName Club where
        name club = clubName club

    print (name athlete)
    -- "Taylor Fausak"

    print (name club)
    -- "Fixed Touring"

In fact it looks like a perfect solution. We can define all the types in a single module, weekend access the fields with a short name, And we don't get any compilers.

So what's the catch? Well let's try to change the type of one of the fields. For instance what if we decide club names Suneera presented with strings they should be represented with text.

    import Data.Text (Text)

    data Club = Club { clubName :: Text }

When we try to define the instance Will get in there.

    Couldn't match type `Text' with `[Char]'
    Expected type: String
      Actual type: Text
    In the return type of a call of `clubName'
    In the expression: clubName club
    In an equation for `name': name club = clubName club

GHC is telling us that it expects the name function to return a string and clubs implementation of the name function is actually returning text. What we need is a way to define the same function in a typeclass with multiple input and output types.

We can do just that with two language extensions: FlexibleInstances and MultiParamTypeClasses. Basically we're saying that to implement this type class, you have to provide two types: the input and the output. Previously we were only supplying the input and the output was required to be a string.

    {-# LANGUAGE FlexibleInstances     #-}
    {-# LANGUAGE MultiParamTypeClasses #-}

    import Data.Text (Text, pack)

    class HasName a b where
        name :: a -> b

    instance HasName Athlete String where
        name athlete = athleteName athlete

    instance HasName Club Text where
        name club = clubName club

    print (name athlete :: String)
    -- "Taylor Fausak"

    print (name club :: Text)
    -- "Fixed Touring"

This looks promising, but it has a few problems. If we leave off the type hinting, we get an error.

    No instance for (HasName Athlete a0)
      arising from a use of `getName'
    The type variable `a0' is ambiguous
    Possible fix: add a type signature that fixes these type variable(s)
    Note: there is a potential instance available:
      instance HasName Athlete String -- Defined at lens-09.hs:17:10
    Possible fix: add an instance declaration for (HasName Athlete a0)

GHC is telling us that even though it knows the input is an athlete, the output could be anything. But that's ridiculous, right? We just told it that name returns a string from athletes. Except we didn't, really. We could add another instance like this:

    instance HasName Athlete Text where
        name athlete = pack (athleteName athlete)

This is the second problem: we can have many instances for the same type. What we want is a way to say that for any given input type, all instances must have the same output type. That effectively means there can only be one instance per type.

That sounds hard. Fortunately we can use another language extension to solve it: FunctionalDependencies. All that does is allow us to say this:

    {-# LANGUAGE FunctionalDependencies #-}

    class HasName a b | a -> b where
        name :: a -> b

Everything else is the same as before, but now the type class says that any instances with the same `a` must have the same `b`. So once we define an instance, we don't have to hint at the types.

    instance HasName Athlete String where
        name athlete = athleteName athlete

    print (name athlete)
    -- "Taylor Fausak"

Indeed, if we try to define conflicting instances like before, we'll get an error

    Functional dependencies conflict between instance declarations:
      instance HasName Athlete String -- Defined at lens-10.hs:17:10
      instance HasName Athlete Text -- Defined at lens-10.hs:20:10
