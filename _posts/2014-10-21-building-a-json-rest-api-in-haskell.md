---
title: Building a JSON REST API in Haskell
---

I spent the last couple weeks building a JSON REST API in Haskell. I called it
Hairy. It's available [on GitHub][1] and [on Hackage][2]. I learned a lot in the
process and wanted to share what I learned. I also wanted to put my code out
there to see what I could do better. Patches welcome!

Before we dive into the code, let's take a look at what we'll end up with.
Assuming you're running it on port 3000, which is the default, here's what some
CRUD actions look like.

{% highlight sh %}
$ http post :3000/tasks \
  content='Do something!' created='2014-10-21T00:00:00Z'
{ "content": "Do something!"
, "created": "2014-10-21T00:00:00.000Z"
}
$ http :3000/tasks
[ { "content": "Do something!"
  , "created": "2014-10-21T00:00:00.000Z"
  , "id": 2
  }
]
$ http :3000/tasks/2
{ "content": "Do something!"
, "created": "2014-10-21T00:00:00.000Z"
}
$ http put :3000/tasks/2 \
  content='Do something else!' created='2014-10-21T00:00:00Z'
{ "content": "Do something else!"
, "created": "2014-10-21T00:00:00.000Z"
}
$ http delete :3000/tasks/2
null
{% endhighlight %}

This is a Literate Haskell file. Only lines that start with `>` are program
code. Assuming you have [the dependencies][3], you should be able to run it with
this command:

{% highlight sh %}
$ runhaskell -optL -q this-post.lhs Hairy/Models.hs
{% endhighlight %}

Without any further ado, let's get to it!

- [Boilerplate](#boilerplate)
- [Main](#main)
- [Configuration](#configuration)
- [Environment](#environment)
- [Database](#database)
- [Transformers](#transformers)
- [Options](#options)
- [Settings](#settings)
- [Errors](#errors)
- [Application](#application)
- [Middleware](#middleware)
- [Actions](#actions)
- [Utilities](#utilities)

Boilerplate
---

Before we can begin, we need to enable a few language extensions. The first
allows string literals (like `"cheese"`) to represent string-like types such as
`Text`. It's not strictly required since you can accomplish the same thing using
`pack`. But it's so convenient that it's hard to live without.

{% highlight hs %}
> {-# LANGUAGE OverloadedStrings #-}
{% endhighlight %}

These next couple are a little harder to explain, so I'll hold off doing that
until we use them.

{% highlight hs %}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{% endhighlight %}

Now that we've enabled the language extensions that we need, we have to let GHC
know that our module is called `Hairy`. If we didn't do this, it would assume
it's called `Main`.

{% highlight hs %}
> module Hairy where
{% endhighlight %}

Imports make up the last bit of the boilerplate. These are a little
overly-specific in order to make it easier to see where everything comes from.
In the real world you might import everything from `Web.Scotty.Trans` instead of
explicitly listing the things you need from it.

For the most part, you shouldn't worry about these. If you're curious about
something later on, come back up here to see where it's imported from. Then look
it up on Hackage.

{% highlight hs %}
> import Control.Applicative (Applicative)
> import Control.Monad.IO.Class (MonadIO, liftIO)
> import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
> import Control.Monad.Reader (MonadReader, ReaderT, asks,
>   runReaderT)
> import Control.Monad.Trans.Class (MonadTrans, lift)
> import Data.Aeson (Value (Null), (.=), object)
> import Data.Default (def)
> import qualified Data.Text as T
> import Data.Text.Encoding (encodeUtf8)
> import Data.Text.Lazy (Text)
> import qualified Database.Persist as DB
> import qualified Database.Persist.Postgresql as DB
> import Network.HTTP.Types.Status (created201,
>   internalServerError500, notFound404)
> import Network.Wai (Middleware)
> import Network.Wai.Handler.Warp (Settings, defaultSettings,
>   setFdCacheDuration, setPort)
> import Network.Wai.Middleware.RequestLogger (logStdout,
>   logStdoutDev)
> import System.Environment (lookupEnv)
> import Web.Heroku (parseDatabaseUrl)
> import Web.Scotty.Trans (ActionT, Options, ScottyT,
>   defaultHandler, delete, get, json, jsonData, middleware,
>   notFound, param, post, put, scottyOptsT, settings,
>   showError, status, verbose)
{% endhighlight %}

This next import is special. If you're following along at home, you'll need to
create a folder called `Hairy` and put [`Models.hs`][4] in it. I didn't include
the models in this file because I couldn't get the Template Haskell to play
nicely with the Literate Haskell.

{% highlight hs %}
> import Hairy.Models (Task, TaskId, migrateAll)
{% endhighlight %}

Main
---

With all that out of the way, we can start on the program itself. The top-level
entry point, `main`, only has two responsibilities: get the current
configuration and run the application with that configuration.

{% highlight hs %}
> main :: IO ()
> main = do
>   c <- getConfig
>   runApplication c
{% endhighlight %}

This is the same as the more succinct point-free version.

{% highlight hs %}
main :: IO ()
main = getConfig >>= runApplication
{% endhighlight %}

Configuration
---

Getting the current configuration involves reading the environment from the
system and then setting up the database connection pool. After doing both of
those, we create a new `Config` value with the environment and pool.

{% highlight hs %}
> getConfig :: IO Config
> getConfig = do
>   e <- getEnvironment
>   p <- getPool e
>   return Config
>     { environment = e
>     , pool = p
>     }
{% endhighlight %}

The data type for `Config` is pretty simple. It has two fields. The first is the
environment, which uses a data type we'll define shortly. The second is
Persistent's `ConnectionPool`.

{% highlight hs %}
> data Config = Config
>   { environment :: Environment
>   , pool :: DB.ConnectionPool
>   }
{% endhighlight %}

Environment
---

We want to read the environment from the `SCOTTY_ENV` environment variable, then
parse that string as our `Environment` data type and return it. If it doesn't
parse, we'll just blow up.

{% highlight sh %}
$ env SCOTTY_ENV=not-an-environment cabal run
hairy: Prelude.read: no parse
{% endhighlight %}

If we wanted to handle it more gracefully, we could use `Text.Read.readMaybe`.
However I'm ok with the exception here.

{% highlight hs %}
> getEnvironment :: IO Environment
> getEnvironment = do
>   m <- lookupEnv "SCOTTY_ENV"
>   let e = case m of
>         Nothing -> Development
>         Just s -> read s
>   return e
{% endhighlight %}

This also could've been written in the point-free style.

{% highlight hs %}
getEnvironment :: IO Environment
getEnvironment = fmap
  (maybe Development read)
  (lookupEnv "SCOTTY_ENV")
{% endhighlight %}

Now that we've seen how to get the environment, let's see what the possible
environments are. You could add more environments, like `Staging`, to suite your
particular needs.

{% highlight hs %}
> data Environment
>   = Development
>   | Production
>   | Test
>   deriving (Eq, Read, Show)
{% endhighlight %}

The only instance we need to derive is `Read`. `Show` is handy for debugging and
error reporting. `Eq` is useful if you want to do different things based on the
environment without pattern matching.

Database
---

Next let's take a look at the database connection pool. It will be used by the
application to make database queries, so it's responsible for configuring the
database itself. That means logging, connection parameters, and pool size. To
start, the top-level function gets the connection parameters and pool size, then
determines which kind of logging to use.

{% highlight hs %}
> getPool :: Environment -> IO DB.ConnectionPool
> getPool e = do
>   s <- getConnectionString e
>   let n = getConnectionSize e
>   case e of
>     Development -> runStdoutLoggingT
>       (DB.createPostgresqlPool s n)
>     Production -> runStdoutLoggingT
>       (DB.createPostgresqlPool s n)
>     Test -> runNoLoggingT
>       (DB.createPostgresqlPool s n)
{% endhighlight %}

This function is a little weird. I wish it could be written like this:

{% highlight hs %}
getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
  s <- getConnectionString e
  let n = getConnectionSize e
      p = DB.createPostgresqlPool s n
      t = case e of
        Development -> runStdoutLoggingT
        Production -> runStdoutLoggingT
        Test -> runNoLoggingT
  t p
{% endhighlight %}

Unfortunately the type system won't allow it. `runStdoutLoggingT` and
`runNoLoggingT` work on different monad transformers. `createPostgresqlPool` is
fine with either of them, but it can't accept both simultaneously.

Anyway, just as we looked up the environment through `SCOTTY_ENV`, we're going
to look up the database connection parameters through `DATABASE_URL`. It's
expected to look like this: `postgres://user:pass@host:port/db`. If it doesn't
look like that, we'll blow up.

{% highlight sh %}
$ env DATABASE_URL=not-a-database-url cabal run
hairy: couldn't parse absolute uri
{% endhighlight %}

{% highlight hs %}
> getConnectionString :: Environment -> IO DB.ConnectionString
> getConnectionString e = do
>   m <- lookupEnv "DATABASE_URL"
>   let s = case m of
>         Nothing -> getDefaultConnectionString e
>         Just u -> createConnectionString (parseDatabaseUrl u)
>   return s
{% endhighlight %}

If it's not given at all, we'll fall back to using a hard-coded default based on
the environment.

{% highlight hs %}
> getDefaultConnectionString :: Environment -> DB.ConnectionString
> getDefaultConnectionString e =
>   let n = case e of
>         Development -> "hairy_development"
>         Production -> "hairy_production"
>         Test -> "hairy_test"
>   in  createConnectionString
>         [ ("host", "localhost")
>         , ("port", "5432")
>         , ("user", "postgres")
>         , ("dbname", n)
>         ]
{% endhighlight %}

We used a utility function that converts a list of text tuples into a database
connection string, which is byte string. It joins each tuple with an equals sign
and then joins each element in the list with a space.

{% highlight hs %}
createConnectionString [("k1", "v1"), ("k2", "v2")]
-- "k1=v1 k2=v2"
{% endhighlight %}

This is necessary to convert what `Web.Heroku.parseDatabaseUrl` gives us into
something that Persistent can understand. It also makes our definition of
`getDefaultConnectionString` easier to read.

{% highlight hs %}
> createConnectionString
>   :: [(T.Text, T.Text)] -> DB.ConnectionString
> createConnectionString l =
>   let f (k, v) = T.concat [k, "=", v]
>   in  encodeUtf8 (T.unwords (map f l))
{% endhighlight %}

The last piece of the database puzzle is the size of the connection pool. In the
real world you'd need to benchmark performance using different sizes to see what
works best. A good baseline is two times the number of cores. That could be
expressed here using `GHC.Conc.numCapabilities`, but there's no guarantee that
the web server and the database server are even running on the same machine.

{% highlight hs %}
> getConnectionSize :: Environment -> Int
> getConnectionSize Development = 1
> getConnectionSize Production = 8
> getConnectionSize Test = 1
{% endhighlight %}

Transformers
---

So we've set up our environment and our database connection. That's enough to
let us move on to setting up the application itself. All we need to do here is
get the options for Scotty and set up a runner for reading the configuration.

{% highlight hs %}
> runApplication :: Config -> IO ()
> runApplication c = do
>   o <- getOptions (environment c)
{% endhighlight %}

This takes Scotty's monad `m` and adds the ability to read our custom config `c`
from it. This is called a [monad transformer stack][5]. It allows us to use any
monad in the stack. So after adding our reader monad, we can both deal with
requests (using Scotty's monad) and read our config (using our monad).

{% highlight hs %}
>   let r m = runReaderT (runConfigM m) c
>   scottyOptsT o r r application
{% endhighlight %}

Next we'll actually define our reader monad. This requires
`GeneralizedNewtypeDeriving` to easily and efficiently derive instances for our
type alias. The type signature of `runConfigM` tells us that it adds the ability
to read `Config` to the `IO` monad, which is the bottom of Scotty's monad
transformer stack.

{% highlight hs %}
> newtype ConfigM a = ConfigM
>   { runConfigM :: ReaderT Config IO a
>   } deriving (Applicative, Functor, Monad, MonadIO,
>     MonadReader Config)
{% endhighlight %}

Options
---

Let's circle back and see how we get Scotty's options. The data type exposed
only has two fields, so there's not a lot for us to do here.

{% highlight hs %}
> getOptions :: Environment -> IO Options
> getOptions e = do
>   s <- getSettings e
>   return def
>     { settings = s
>     , verbose = case e of
>       Development -> 1
>       Production -> 0
>       Test -> 0
>     }
{% endhighlight %}

I explicitly listed all of the environments here to ensure that I got all of
them. In the real world you might use a wildcard match.

{% highlight hs %}
verbose = case e of
  Development -> 1
  _ -> 0
{% endhighlight %}

Or you can accomplish the same thing on a single line. (This uses the `Eq`
instance we derived for `Environment`.)

{% highlight hs %}
verbose = fromEnum (e == Development)
{% endhighlight %}

Settings
---

Most of the real options are in Wai's settings. The defaults are good for most
of them, but we want to make two changes. First, we need to remove the file
cache so that static file changes will be picked up. We only want to do this in
development since static files should be static in other environments. Then if
the `PORT` environment variable exists, we want to use it to set the port.

{% highlight hs %}
> getSettings :: Environment -> IO Settings
> getSettings e = do
>   let s = defaultSettings
{% endhighlight %}

Here I'm using primes (`'`) to mark altered versions of the settings. There are
probably better ways to do this type of modification, but this works and is
fairly straightforward.

{% highlight hs %}
>       s' = case e of
>         Development -> setFdCacheDuration 0 s
>         Production -> s
>         Test -> s
>   m <- getPort
>   let s'' = case m of
>         Nothing -> s'
>         Just p -> setPort p s'
>   return s''
{% endhighlight %}

Finally we need to handle looking up the port. Like our other functions that
read from environment variables, this one will blow up if you give it something
it's not expecting.

{% highlight sh %}
$ env PORT=not-a-port cabal run
hairy: Prelude.read: no parse
{% endhighlight %}

{% highlight hs %}
> getPort :: IO (Maybe Int)
> getPort = do
>   m <- lookupEnv "PORT"
>   let p = case m of
>         Nothing -> Nothing
>         Just s -> Just (read s)
>   return p
{% endhighlight %}

Errors
---

The last bit of configuration is to set up our error type. We're going to make
it an alias for `Text`. You could do something fancier here by enumerating the
possible error states for your application.

{% highlight hs %}
data Error = NotFoundError | ForbiddenError | ...
instance ScottyError Error where ...
{% endhighlight %}

We're alright with the default textual errors, so we don't need anything that
fancy yet.

{% highlight hs %}
> type Error = Text
{% endhighlight %}

That wraps up all of the configuration, options, and settings. Everything from
here on out deals with the application itself.

Application
---

Our application has several responsibilities. It needs to run database
migrations, set up middleware, install a default exception handler, and define
routes. Since everything else could conceivably depend on the database, we'll
run the migrations first.

{% highlight hs %}
> application :: ScottyT Error ConfigM ()
> application = do
>   runDB (DB.runMigration migrateAll)
{% endhighlight %}

`runDB` is a utility function we'll define a little later. It basically lifts a
database operation into the current Scotty monad. `migrateAll` comes from
`Hairy.Models` and is generated by Persistent's `mkMigrate` Template Haskell
function.

Now that the database has been migrated, we can set up middleware and exception
handlers. Both of them depend on the environment, so we have to get that from
our reader monad first.

{% highlight hs %}
>   e <- lift (asks environment)
>   middleware (loggingM e)
>   defaultHandler (defaultH e)
{% endhighlight %}

Finally we can do the routing for our application. All we need is the HTTP
method, the path, and the action to route it to.

{% highlight hs %}
>   get "/tasks" getTasksA
>   post "/tasks" postTasksA
>   get "/tasks/:id" getTaskA
>   put "/tasks/:id" putTaskA
>   delete "/tasks/:id" deleteTaskA
{% endhighlight %}

Routes are matched top down, so if nothing else matched we'll render our not
found action.

{% highlight hs %}
>   notFound notFoundA
{% endhighlight %}

That's it! As your application grows you'll add more routes and middleware, but
the basic structure shouldn't change too much.

Let's take a look at that `runDB` helper we used. It takes a SQL query `q` and
runs it inside our monad transformer stack. It does this by asking the reader
monad for the database connection pool, then running the query with that pool in
the IO monad.

{% highlight hs %}
> runDB :: (MonadTrans t, MonadIO (t ConfigM)) =>
>   DB.SqlPersistT IO a -> t ConfigM a
> runDB q = do
>   p <- lift (asks pool)
>   liftIO (DB.runSqlPool q p)
{% endhighlight %}

Middleware
---

Up next is the logging middleware. In development we want colorful multiline
logs flushed every request. In production we want plain log lines flushed
sometimes. In testing we don't want logging at all.

{% highlight hs %}
> loggingM :: Environment -> Middleware
> loggingM Development = logStdoutDev
> loggingM Production = logStdout
> loggingM Test = id
{% endhighlight %}

Before we define our default exception handler, let's create an alias for our
Scotty actions. They're all going to have the same type, so we don't want to
repeat ourselves over and over again.

{% highlight hs %}
> type Action = ActionT Error ConfigM ()
{% endhighlight %}

Since our default exception handler handles uncaught exceptions in our
application, we want it print out the exceptions in development but swallow them
in production. We don't really care what happens to them in testing. In the
real world you might send the exception to another service.

{% highlight hs %}
> defaultH :: Environment -> Error -> Action
> defaultH e x = do
>   status internalServerError500
>   let o = case e of
>         Development -> object ["error" .= showError x]
>         Production -> Null
>         Test -> object ["error" .= showError x]
>   json o
{% endhighlight %}

Actions
---

At long last we can get to the meat of our application: the actions. This is
where all of your business logic lives. Since Hairy is just a basic CRUD app,
there's not a lot going on here.

This action gets all the tasks from the database and renders them as JSON.

{% highlight hs %}
> getTasksA :: Action
> getTasksA = do
>   ts <- runDB (DB.selectList [] [])
>   json (ts :: [DB.Entity Task])
{% endhighlight %}

This one allows you to create new tasks by posting JSON to it. If the JSON isn't
valid, an exception will be raised. That means in development you'll get a
helpful error message, but in production you'll get a blank 500.

{% highlight sh %}
$ curl -X POST localhost:3000/tasks -d not-json
{"error":"jsonData - no parse: not-json"}
{% endhighlight %}

{% highlight hs %}
> postTasksA :: Action
> postTasksA = do
>   t <- jsonData
>   runDB (DB.insert_ t)
>   status created201
>   json (t :: Task)
{% endhighlight %}

This action gets a task from the database. If it was found, it renders it as
JSON. If it wasn't, it renders the generic not found action.

{% highlight hs %}
> getTaskA :: Action
> getTaskA = do
>   i <- param "id"
>   m <- runDB (DB.get (toKey i))
>   case m of
>     Nothing -> notFoundA
>     Just t -> json (t :: Task)
{% endhighlight %}

This one will either update an existing task or create a new one with the given
ID. Then it renders the task as JSON.

{% highlight hs %}
> putTaskA :: Action
> putTaskA = do
>   i <- param "id"
>   t <- jsonData
>   runDB (DB.repsert (toKey i) t)
>   json (t :: Task)
{% endhighlight %}

This is the last action. It will delete a task with the given ID. If there is no
such task, it returns 200 anyway. In either case, `null` is returned.

{% highlight hs %}
> deleteTaskA :: Action
> deleteTaskA = do
>   i <- param "id"
>   runDB (DB.delete (toKey i :: TaskId))
>   json Null
{% endhighlight %}

Utilities
---

That wraps up the business logic. We only have a couple other things to attend
to. We used `toKey`, a helper function that converts a request parameter into a
database key. It allows us to query for stuff from the database using request
parameters.

This helper function requires the `FlexibleContexts` language extension,
although I can't really tell you why. If you don't have it, GHC complains. If
you do have it, everything works fine.

{% highlight hs %}
> toKey :: DB.ToBackendKey DB.SqlBackend a =>
>   Integer -> DB.Key a
> toKey i = DB.toSqlKey (fromIntegral (i :: Integer))
{% endhighlight %}

The last thing we need to do is define our not found action. All it does is set
the HTTP status to 404 and render `null`.

{% highlight hs %}
> notFoundA :: Action
> notFoundA = do
>   status notFound404
>   json Null
{% endhighlight %}

That's all there is to it! With less than 200 lines of code we've created a JSON
REST API with some CRUD actions. It's all backed by a database and can be
configured to run in development mode on your machine or in production on
Heroku.

I used a few excellent resources while working on this post, including:

- [Making A Website With Haskell][6] by Aditya Bhargava
- [Hello Scotty][7] by Miëtek Bak
- [Persistent][8] by Michael Snoyman

[1]: https://github.com/tfausak/hairy
[2]: http://hackage.haskell.org/package/hairy
[3]: https://github.com/tfausak/hairy/blob/22145de/hairy.cabal#L31-L47
[4]: https://github.com/tfausak/hairy/blob/22145de/library/Hairy/Models.hs
[5]: http://book.realworldhaskell.org/read/monad-transformers.html
[6]: http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html
[7]: https://github.com/mietek/haskell-on-heroku-examples/tree/master/hello-scotty
[8]: http://www.yesodweb.com/book/persistent