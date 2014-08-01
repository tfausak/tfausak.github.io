---
title: Lenses from the ground up
---

# TODO

- make sure code compiles
- host gist with snapshots for each snippet
- try to figure out the functor thing
- over/update whatever instead of set?

---

start with a basic data type
define getters and setters for the fields

``` hs
data Athlete = Athlete String

getName :: Athlete -> String
getName (Athlete name) = name

setName :: Athlete -> String -> Athlete
setName (Athlete _) name = Athlete name

blankAthlete = Athlete ""
athlete = setName blankAthlete "Taylor Fausak"
getName athlete
```

that works
but the getters and setters are tedious
its easy to imagine it getting out of control
as the number of fields goes up
lets make the compiler do it for us

``` hs
data Athlete = Athlete { name :: String }

blankAthlete = Athlete { name = "" }
athlete = blankAthlete { name = "Taylor Fausak" }
name athlete
```

pretty amazing
but what happens when we add another data type to the mix
it has a field with the same name

``` hs
data Club = Club { name :: String }
```

since both name functions exist at the top level
haskell doesnt know which one you want
youll get an error like this one

> Multiple declarations of `name'

one way around this is to put each type in a separate file
and then import them

``` hs
-- Athlete.hs
data Athlete = Athlete { name :: String }
```

``` hs
-- Club.hs
data Club = Club { name :: String }
```

``` hs
-- Main.hs
import Athlete
import Club

blankAthlete = Athlete { name = "" }
```

even though this looks completely unambiguous
haskell cant deal with it
youll get another error

> Ambiguous occurrence `name'
> It could refer to either `Athlete.name'
> or `Club.name'

you can get out of this mess
by using the fully qualified names

``` hs
blankAthlete = Athlete { Athlete.name = "" }
athlete = blankAthlete { Athlete.name = "Taylor Fausak" }
Athlete.name athlete

blankClub = Club { Club.name = "" }
club = blankClub { Club.name = "Fixed Touring" }
Club.name club
```

this works
but its annoyingly verbose
for once
haskell has us covered
you can change the name of an import

``` hs
import Athlete as A
import Club as C

blankAthlete = Athlete { A.name = "" }
athlete = blankAthlete { A.name = "Taylor Fausak" }
A.name athlete

blankClub = Club { C.name = "" }
club = blankClub { C.name = "Fixed Touring" }
C.name club
```

this works
and its pretty clean
but what happens when we want to import lots of modules
maybe the names will conflict
another problem is that you cant reexport modules like this
so if someone wants to use your library
theyll have to import a bunch of stuff
those problems can be fixed
by introducing verbosity

``` hs
data Athlete = Athlete { athleteName :: String }
data Club = Club { clubName :: String }

blankAthlete = Athlete { athleteName = "" }
athlete = blankAthlete { athleteName = "Taylor Fausak" }
athleteName athlete

blankClub = Club { clubName = "" }
club = blankClub { clubName = "Fixed Touring" }
clubName club
```

now we can have everything in the same namespace
so we could reexport them if we wanted
but we end up being pretty verbose
with no apparent way to avoid that
we can add a typeclass to help

``` hs
class HasName a where
  getName :: a -> String
  setName :: a -> String -> a

instance HasName Athlete where
  getName = athleteName
  setName athlete name = athlete { athleteName = name }

instance HasName Club where
  getName = clubName
  setName club name = club { clubName = name }

blankAthlete = Athlete { athleteName = "" }
athlete = setName blankAthlete "Taylor Fausak"
getName athlete

blankClub = Club { clubName = "" }
club = setName blankClub "Fixed Touring"
getName club
```

now the verbosity is contained in the type class
we can succinctly access the fields
this seems like a perfect solution
but theres one problem
what happens if the fields have different types
imagine that clubs don't have to have names
then we can change their representation
to have `name :: Maybe String`

``` hs
data Club = Club { clubName :: Maybe String }
```

if we keep everything else the same
well get an error

> Couldn't match type `Maybe String' with `[Char]'
> Expected type: Club -> String
>   Actual type: Club -> Maybe String
> In the expression: clubName
> In an equation for `name': name = clubName
> In the instance declaration for `HasName Club'

our type class requires that `name` be of type `String`
but we want it to be anything
since it can vary from type to type
so lets add another variable to the type class

``` hs
class HasName a b where
  getName :: a -> b
  setName :: a -> b -> a
```

here a is the record type
and b is the field type
this expresses what we want
but ghc isnt happy with it

> Too many parameters for class `HasName'

fortunately we can work around this with a language extension

``` hs
{-# LANGUAGE MultiParamTypeClasses #-}

instance HasName Athlete String where
  getName = athleteName
  setName athlete name = athlete { athleteName = name }
```

again we hit a problem
ghc complains that we need another extension

> Illegal instance declaration for `HasName Athlete String'

so lets add it in
but before we can go on
ghc complains again

> Illegal instance declaration for `HasName Athlete String'

its the same warning
but it recommends another extension
lets add both in
and move on

``` hs
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

instance HasName Club (Maybe String) where
  getName = clubName
  setName club name = club { clubName = name }

blankAthlete = Athlete { athleteName = "" }
athlete = setName blankAthlete "Taylor Fausak"
getName athlete
```

this looks like it should work
but we get an error

> The type variable `a0' is ambiguous

ghc doesnt know the return type of `getName`
even though we defined an instance of `HasName`
this is because `a` and `b` are independent
we can work around this by specifying the types

``` hs
getName athlete :: String

blankClub = Club { clubName = Nothing }
club = setName blankClub (Just "Fixed Touring")
getName club :: Maybe String
```

this works
but its gross
basically not better than java
our implementation is giving us flexibility we dont want
for instance
we could define another instance of `HasName`

``` hs
instance HasName Athlete (Maybe String) where
  getName = Just . athleteName
  setName athlete maybeName = athlete { athleteName = maybe "" id maybeName }

getName athlete :: Maybe String
```

that might be interesting
but for our purposes
we want each input type (athlete) to be paired with an output type (string)
fortunately we can do this
we can add another piece to the typeclass

``` hs
class HasName a b | a -> b where
  getName :: a -> b
  setName :: a -> b -> a
```

this says that b depends solely on a
what that means for us is that given a
we already know b
based on the instances
so this is great
the only problem is it doesnt compile

> Fundeps in class `HasName'

we have to add another language extension

``` hs
{-# LANGUAGE FunctionalDependencies #-}
```

now when we try to compile the example
we get another error

> Functional dependencies conflict between instance declarations:

this one is telling us that the hasname instance for athlete
can return either a string or a maybe string
but it cant return both
so we can get rid of the maybe string one
and have a working program

``` hs
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

data Athlete = Athlete { athleteName :: String }
data Club = Club { clubName :: Maybe String }

class HasName a b | a -> b where
  getName :: a -> b
  setName :: a -> b -> a

instance HasName Athlete String where
  getName = athleteName
  setName athlete name = athlete { athleteName = name }
instance HasName Club (Maybe String) where
  getName = clubName
  setName club name = club { clubName = name }

blankAthlete = Athlete { athleteName = "" }
athlete = setName blankAthlete "Taylor Fausak"
getName athlete

blankClub = Club { clubName = Nothing }
club = setName blankClub (Just "Fixed Touring")
getName club
```

this is pretty great
weve got short accessors
and everything type checks
and the compiler is happy
what more could you want?
well itd be nice if the functions werent prefixed with get and set
lets take a step toward that
by introducing two new functions: get and set

``` hs
get :: (a -> b) -> a -> b
get getter record = getter record

set :: (a -> b -> a) -> a -> b -> a
set setter record field = setter record field
```

those are both synonyms for `id`
but well come back to that
lets use them

``` hs
newAthlete = set setName athlete "Taylor Fausak!"
get getName newAthlete

newClub = set setName club (Just "Fixed Touring!")
get getName newClub
```

wait a minute
we were trying to get rid of verbosity
not introduce more
what gives?
well these two functions make up a lens
thats all it is
a getter and a setter all in one
so lets write it like that

``` hs
data Lens a b = Lens
  { get :: a -> b
  , set :: a -> b -> a
  }

athleteNameLens :: Lens Athlete String
athleteNameLens = Lens
  { get = athleteName
  , set = \ athlete newName -> athlete { athleteName = newName }
  }

clubNameLens :: Lens Club (Maybe String)
clubNameLens = Lens
  { get = clubName
  , set = \ club newName -> club { clubName = newName }
}
```

then we can redefine our type class to use lenses

``` hs
class HasName a b | a -> b where
  name :: Lens a b

instance HasName Athlete String where
  name = athleteNameLens

instance HasName Club (Maybe String) where
  name = clubNameLens
```

and finally we can use these lenses and typeclasses to write clean short code

```hs
blankAthlete = Athlete { athleteName = "" }
athlete = set name blankAthlete "Taylor Fausak"
get name athlete

blankClub = Club { clubName = Nothing }
club = set name blankClub (Just "Fixed Touring")
get name club
```

we could stop here
what we have are bona fide lenses
and they're just as usable as what the lens package provides
(minus some type shenanigans)
but what if we could represent our lenses simply as a type alias?
then people could use them without importing our data type
the first step toward doing that is getting away from record syntax

``` hs
data Lens a b = Lens
  (a -> b)
  (a -> b -> a)

get :: Lens a b -> a -> b
get (Lens getter _) = getter

set :: Lens a b -> a -> b -> a
set (Lens _ setter) = setter
```

this is the reverse of what we did with our own record at the beginning
it makes this next part easier
since we can convert the data type into a type alias
without too much trouble

``` hs
type Lens a b =
  ( a -> b
  , a -> b -> a
  )

get :: Lens a b -> a -> b
get (getter, _) = getter

set :: Lens a b -> a -> b -> a
set (_, setter) = set
```

now instead of a custom data type
we just have a tuple
and it has an interesting property
the first parameter of each function is the same
so lets pull that out
and turn the whole alias
into a function
that takes an a
and returns a tuple

``` hs
type Lens a b = a ->
  ( b
  , b -> a
  )

get :: Lens a b -> a -> b
get lens thing = fst (lens thing)

set :: Lens a b -> a -> b -> a
set lens thing value = snd (lens thing) value
```

this achieves the same thing as before
but its more verbose
so why do we want it?
because it presents the structure of a lens
in a clean way
give the lens an object (a)
and it will return
the field you want (b)
and a function to update that field (b -> a)

so thats the core of lenses
one thing that theyre good at
that we havent covered yet
is composition
lets say we change our club type
to have an athlete as its owner

``` hs
data Club = Club
  { clubName :: Maybe String
  , clubOwner :: Athlete
  }

clubOwnerLens :: Lens Club Athlete
clubOwnerLens club =
  ( clubOwner
  , \ newOwner -> club { clubOwner = newOwner }
  )

class HasOwner a b | a -> b where
  owner :: Lens a b

instance HasOwner Club Athlete where
  owner = clubOwnerLens
```

now how can we change the club owners name?
lenses make it better than the alternative
but it's still gross

``` hs
athlete = Athlete { athleteName = "Taylor Fausak" }
club = Club { clubName = Just "Fixed Touring", clubOwner = athlete }
set owner (set name "Taylor Fausak!" (get owner club)) club
get name (get owner club)
```

theres got to be a better way
lets define a composition operator
well call it >-
which kind of looks like a lens focusing a beam of light
it will take two lenses
and return a lens that goes from the input of the first to the output of the last

``` hs
(>-) :: Lens a b -> Lens b c -> Lens a c
f >- g = \ x ->
  ( get g (get f x)
  , \ y -> set f (set g y (get f x) x)
  )
```

and with that
we can compose lenses
this lets us traverse structures easily
to read or write values in them

``` hs
athlete = Athlete { athleteName = "Taylor Fausak" }
club = Club { clubName = Just "Fixed Touring", clubOwner = athlete }
set (owner >- name) "Taylor Fausak!" club
get (owner >- name) club
```
