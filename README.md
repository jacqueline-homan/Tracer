# QuickTracker

This project is a simple redirector that records requests for a
resource, then redirects them to the actual target destination.

This is meant to be an auditing tool for promotional campaigns to
ensure that reported clicks are actually made through to the target
site.

# Getting Started

Let's set some sensible defaults in Git. First, we only want it to do
"fast-forward" merges:

    git config --global merge.ff=only

Next, let's make sure that when we're pulling changes in from other
branches, we rebase on top of them to avoid merge commits:

    git config --global branch.autosetuprebase=always
    git config --global branch.autosetupmerge=0
    git config --global pull.rebase=true

Merge commits are problematic for any number of reasons, but then so is rebasing. But rebasing and keeping the git history linear makes following the changes of a project and working with remote
repositories much easier.

## Cabal

### First Initialization

First, we should make sure we have the latest cabal package list:

    cabal update

This will also create a default configuration in
`$HOME/.cabal/config`. Next, we should edit this configuration
file. In your favorite text editor, open this file, and find the line
`default-user-config:`. Underneath that, add the following line:

    require-sandbox: True

This will help prevent accidentally installing packages outside of a
sandbox, which will definitely impact the system.

### Sandbox Setup

Now, we
can
[initialize the Cabal sandbox](https://www.haskell.org/cabal/users-guide/installing-packages.html#sandboxes-basic-usage). If
we do it outside the project directory, we can reuse it for other
projects as well. So, first:

    export BFWP_SANDBOX=$HOME/.local/share/bfwp/cabal-sandbox
    mkdir -p $BFWP_SANDBOX
    cd $BFWP_SANDBOX
    cabal sandbox init

This will create a sandbox in
`$HOME/.local/share/bfwp/cabal-sandbox`. We'll also need to add the
`bin` directory there to our shell `PATH`. Run the following command
to add it to your profile:

    cat >> $HOME/.profile <<EOF

    BFWP_SANDBOX=$HOME/.local/share/bfwp/cabal-sandbox

    # Add the shared cabal sandbox to the PATH.
    export PATH=$$BFWP_SANDBOX/bin:$$PATH

    # Clean up
    unset BFWP_SANDBOX

    EOF

_NOTE: Make absolutely sure_ you use `cat >>` above, or you'll
overwrite your profile instead of adding to it.

At this point, we can also add it in the current shell:

    export PATH=$BFWP_SANDBOX/bin:$PATH

### Overall Setup Complete

At this point, we're ready to move on to working with the actual
project. These steps only need to be completed once. From here on out,
we'll just be cloning repositories and reusing the sandbox we've
created.

# The `QuickTracker` Lesson

Now, we can clone the lesson repository. Pick a spot you'd like to
work from in your home, and we can clone the repository there:

    git clone git@gitlab.com:bfwp/Examples/QuickTracker.git
    cd QuickTracker

We need to configure it to use the sandbox we created:

    cabal sandbox init --sandbox=$HOME/.local/bfwp/cabal-sandbox

Then we need to install the packages it depends on:

    cabal install --only-dependencies

This might take some time, since it has to download all the packages
`QuickTracker` needs and build them.

# Code Walkthrough

## Language Configuration

We enable
the [TemplateHaskell](https://wiki.haskell.org/Template_Haskell)
language extension so that when we define new types, we can
generate [Lenses and Prisms](http://hackage.haskell.org/package/lens)
automatically. We'll cover those a bit later.

## Module Name and Imports

Next, we declare that this module is named `Main`, and then import
other modules. The first block of imports are system libraries,
including our
own [liblawless](http://hackage.haskell.org/package/liblawless) that
provides a lot of extra utility functions that are total by
default. The
standard
[Prelude](http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html) is
filled
with
[partial functions](https://wiki.haskell.org/List_of_partial_functions) that
will cause difficult to track runtime errors.

The second block is where we
import
[Happstack-specific](http://hackage.haskell.org/package/happstack-server) modules
and types. Most of these are used in our type declarations later, to
help the compiler understand just what we're trying to do so it can
help us when we've expressed something poorly or incorrectly.

## Type Definitions

In this section, we define the types we'll be using to build up our
application. The first one is `RedirectTo`, which wraps
the
[`URI`](http://hackage.haskell.org/package/network-uri-2.6.1.0/docs/Network-URI.html#t:URI) type. This
is one of the types that Happstack can use as a URL when it's
expecting
one. The
[`Network.URI`](http://hackage.haskell.org/package/network-uri) also
comes with
several
[parser functions](http://hackage.haskell.org/package/network-uri-2.6.1.0/docs/Network-URI.html#g:2) that
we can use to validate that the argument passed for redirection is
actually something browsers will handle.

We then show
the
[standard `deriving` mechanism](https://www.haskell.org/tutorial/stdclasses.html) for
[`Eq`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Eq.html#t:Eq),
[`Ord`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Ord.html#t:Ord),
and
[`Show`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Text-Show.html#t:Show) type
classes. These use the underlying types of
our [`newtype`](https://wiki.haskell.org/Newtype) to define the
equality, ordering, and simple string conversion functions for our
type.

We also show an example of
the
[`GeneralizedNewtypeDeriving`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-derived-instances-for-newtypes) mechanism. This
lets us also have the compiler write the methods
of
[`ToSURI`](http://hackage.haskell.org/package/happstack-server-7.4.6.4/docs/Happstack-Server-SURI.html#t:ToSURI) for
us
using
[the underlying `URI`'s ](http://hackage.haskell.org/package/happstack-server-7.4.6.4/docs/Happstack-Server-SURI.html#t:ToSURI) `toSURI` implementation.

Generally, when using `newtype` for wrapping other types, this is
really effective at helping the compiler protect you from
implementation errors without having to write a lot of code that
already exists.

## `makePrisms`

Now, since we have a `newtype` wrapper, we want to be able to easily
(but not to easily) unwrap the `newtype` and get the underlying
value. the [`lens`](http://hackage.haskell.org/package/lens) package
gives us a `TemplateHaskell`
function,
[`makePrisms`](http://hackage.haskell.org/package/lens-4.15.2/docs/Control-Lens-TH.html#g:3),
that will use the type information at compile time to create
a
[`Prism`](http://hackage.haskell.org/package/lens-4.15.2/docs/Control-Lens-Prism.html) for
our `newtype` that can be used to "unwrap" the underlying value using
various generic functions the package provides.

## The `App` Itself

This `data` definition is essentially our application
configuration. We generate it later from the command line
arguments. It uses another bit of the `lens`
package,
[`makeLenses`](http://hackage.haskell.org/package/lens-4.15.2/docs/Control-Lens-TH.html#g:1) to
create
[`lens`es](http://hackage.haskell.org/package/lens-4.15.2/docs/Control-Lens-Lens.html) for
getting and setting the different component values of the `App`.

A `lens` is a data type that combines a "getter" function and a
"setter" function, and there are various helper functions that can use
a `lens` to extract parts of a composite value, or replace
parts. Since they're still just functions, they can also be chained,
or composed, into complex patterns to match very specific parts of
intricate data structures.

We're definitely not doing anything like that here, but we are
introducing the basic idea.

## The Request Logger

Next we define the function that
the
[`ServerPartT`](http://hackage.haskell.org/package/happstack-server-7.4.6.4/docs/Happstack-Server-Internal-Monads.html#t:ServerPartT) uses
to report every request from the browser. In our case, we write it to
"standard error", which is one of the three standard streams every
UNIX process receives when it's started.

Now, an interesting thing about our `logRequest` function is
the
[Existential Type Variable](https://wiki.haskell.org/Existential_type). So,
normally, if a data type uses a type variable, the data structure
itself has to declare it as part of its definition. For example:

    data Conf a = Conf {
        message :: Text,
        translate :: Text -> a
    }

In this definition of `Conf`, `Conf` has a value named
`translate`. `translate` is any of a class of functions that takes a
`Text` and returns whatever the type of `a` is when a specific `Conf`
is created.

Now, the problem with this is that only a single member of `Conf`
needs the variable, but now every function and every type that uses a
`Conf` has to also include the extra type variable `a`. So, for a
function that would just need to use the `message` of `Conf`, it would
still need to account for the variable `a`:

    showMessage :: Conf a -> String
    showMessage (Conf{..}) = show message

If you have large data structures, or lots of functions for them, this
can get pretty tedious. So, what Happstack does with
its
[`Conf`](http://hackage.haskell.org/package/happstack-server-7.4.6.4/docs/Happstack-Server-Internal-Types.html#t:Conf) is
specify the type variable `t` as part of the type of its `logAccess`
member. What this does is let any function that has the right kind of
`t` be used for `logAccess`, and just for `logAccess`, without
affecting any other function that operates on `Conf`.

Our `logRequest` function matches the `logAccess` signature exactly,
and so it's a perfect fit.

## The Actual Application

At this point, we have enough to put together the functions that will
actually run the application itself. First, we define a specific
`Conf` that has our `logRequest` function inside. `Conf` actually
wraps the function `logAccess` uses with
a
[`Maybe`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Maybe.html#t:Maybe). This
type is a way of specifying optional values in a way that the compiler
can check whether you've handled both the case where the value is
specified and where it isn't, or where a function returns a result or
fails in some way.

`Maybe` also has some other interesting properties that let you chain
them together without having to check at each stage of the chain
whether there's a value (a `Just a`), or no value (`Nothing`). The
combining operator in Haskell for these kinds of types, `>>=`, or
`bind`, is defined specifically for each one. In the case of the
`Maybe` type, it checks for you if it's getting a `Nothing` and then
just sends another `Nothing` along. We use this behavior later when
building our `App`.

Next, we have the actual `server` function. This will take a
Happstack
[`ServerPartT`](http://hackage.haskell.org/package/happstack-server-7.4.6.4/docs/Happstack-Server-Internal-Monads.html#t:ServerPartT) as
an argument, and then after calling `logAccess` on the request, it'll
pass the request through the `ServerPartT` we specify, and use
the
[`Response`](http://hackage.haskell.org/package/happstack-server-7.4.6.4/docs/Happstack-Server-Internal-Types.html#t:Response) it
creates as the reply to the browser. It continues doing this
essentially until we signal the process to terminate. The only
requirement of the `ServerPartT` we pass in is that whatever it
returns is
a
[`ToMessage`](http://hackage.haskell.org/package/happstack-server-7.4.6.4/docs/Happstack-Server-Response.html#t:ToMessage),
which is the general class of types that Happstack can convert into
messages to send to the browser.

Our specific `ServerPartT` uses our own `RedirectBody` type from
earlier which is an instance of `ToMessage`. In fact, we use
`GeneralizedNewtypeDeriving` to make it so by just reusing the
`ToMessage` function of the underlying `Text` type.

And so `redirectApp` takes our `App` configuration value, pulls out
the `_appRedirectTo` and `_appBody` using the `lens`es we created for
the `App` using `makeLenses`, passes them to Happstack's
own
[`tempRedirect`](http://hackage.haskell.org/package/happstack-server-7.4.6.4/docs/Happstack-Server-Response.html#v:tempRedirect),
and let's that handle most of the rest of the `Response`.

Basically we're writing a very thin layer on top of the already
existing Happstack scaffolding so we don't have to manage the full
stack of the HTTP request/response model ourselves.

## Configuring the `App`

Now, we have an interesting function with a general and short name,
`app`. This function demonstrates `let` bindings, where we define
functions inside of functions that have access to each other and to
the arguments of the outer function. We also have three functions that
return `Maybe` values. And what that means, of course, is that we can
build our `App`, and if we fail to build it at any stage, the `app`
function will return `Nothing` without us having to check every stage
for errors.

In fact, the only place we do any specific check is in the `alen`
function, and that just makes sure we have at least enough arguments
to create a `RedirectTo` and a `RedirectBody`. It returns a `Nothing`
for the next stage if we don't.

These kinds of flow control by using data structures are very common
in Haskell, so much so that the language itself only has a few
built-in branching primitives. Everything more complex is built using
data.

So, we take the three `let`-bound functions, `alen`, `rt`, and `msg`,
and then use them to build up our `App` configuration. Now, what's
also interesting about how we do that is
the
[`Applicative`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Applicative.html) form
we use to essentially pull the `App` _inside_ the `Maybe`.

So first we start with the more standard `>>=` operator from `alen`,
which we then pass to a function that unpacks the list of arguments if
`alen` returns `Just as`, i.e. if we have at least two arguments. It
unpacks the first argument into `auri`, and all the rest into
`msgs`. That's what the `:` operator does. It builds and unbuilds
lists by taking the first element, and then taking all the rest.

So then, once we have `auri` and `msgs`, we use the `Applicative`
operator
[`<$>`](http://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Applicative.html#v:-60--36--62-) to
essentially sequence the `rt` function, which tries to parse `auri` as
a `URI` and then build a `RedirectTo` from it if it succeeds (also
using `<$>`). And then it combines all of the `msgs` with the `msg`
function into a `Just RedirectBody`, since we know at this point from
`alen` whether we have enough arguments to at least have one `Arg` in
`msgs`. The `<$>` operator both after `App` and in `rt auri` ensure
that if there's a `Nothing`, instead of getting a `Just RedirectTo` or
a `Just App`, we get a `Nothing`.

## Exiting on Failure

We also define a small, simple function `exitFail` that will print a
help message if `app` fails to create an `App`. It uses the
standard
[`exitWith`](http://hackage.haskell.org/package/base-4.9.1.0/docs/System-Exit.html#v:exitWith) function
with
an
[`ExitFailure`](http://hackage.haskell.org/package/base-4.9.1.0/docs/System-Exit.html#t:ExitCode) to
notify the operating system that we ended abnormally.

## The `main` Function

And, now, finally, we get to the function that starts every program:
`main`. Our `main` function again uses the `<$>` operator to call
`app` on the program arguments. Now, `args` operates in a different
control type, the `IO` type. But this is the same class of types as
`Maybe`, and so the `<$>` operator will still work for running a
function on a value within the `IO`, just like it does with `Maybe`.

We use the special `do` notation to automatically cause each line
inside the `do` block to be implicitly combined with the `>>=`
operator, and so we can (mostly) pretend that we're just writing a
series of function calls instead of a single larger composed function.

The `<-` operator is the same as defining a function on the right side
of the `>>=` operator, so `a <- app <$> args` is exactly the same as
`(app <$> args) >>= \a ->`. In fact, the `->` and the `<-` are
deliberately mirror images of each other to reinforce this.

And then we have the other point where we do an explicit condition
check, this time with `case` instead of `if ... then
... else`. Basically, if we could build an `App`, we run
it. Otherwise, we exit with a helpful message.

And really, that's the redirection application. It's short, simple,
and has a lot of structures that can be built on, or "composed", as
Functional Programmers like to say, and, really, basically every
operator and every syntax is in some way supporting this kind of
composition.

To run the program, simply type the following command in your terminal:

cabal run "<website_url>" "<whatever_message_u_want>"

For example: 

cabal run "https://www.patreon.com/teamunixman" "check this out"