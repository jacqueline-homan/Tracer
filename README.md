# QuickTracker

This project is a simple redirector that records requests for a
resource, then redirects them to the actual target destination.

This is meant to be an auditing tool for promotional campaigns to
ensure that reported clicks are actually made through to the target
site.

# Git

Let's set some sensible defaults in Git. First, we only want it to do
"fast-forward" merges:

    git config --global merge.ff=only

Next, let's make sure that when we're pulling changes in from other
branches, we rebase on top of them to avoid merge commits:

    git config --global branch.autosetuprebase=always
    git config --global branch.autosetupmerge=0
    git config --global pull.rebase=true

Merge commits are problematic for any number of reasons, but then so
is rebasing. But rebasing and keeping the git history linear makes
following the changes of a project and working with remote
repositories much easier.

# Cabal

## First Initialization

First, we should make sure we have the latest cabal package list:

    cabal update

This will also create a default configuration in
`$HOME/.cabal/config`. Next, we should edit this configuration
file. In your favorite text editor, open this file, and find the line
`default-user-config:`. Underneath that, add the following line:

    require-sandbox: True

This will help prevent accidentally installing packages outside of a
sandbox, which will definitely impact the system.

## Sandbox Setup

Now, we
can
[initialize the Cabal sandbox](https://www.haskell.org/cabal/users-guide/installing-packages.html#sandboxes-basic-usage). If
we do it outside the project directory, we can reuse it for other
projects as well. So, first:

    mkdir -p $HOME/Haskell/cabal-sandbox
    cd $HOME/Haskell/cabal-sandbox
    cabal sandbox init

This will create a sandbox in `$HOME/Haskell/cabal-sandbox`. We'll
also need to add the `bin` directory there to our shell `PATH`. Run
the following command to add it to your profile:

    cat >> $HOME/.profile <<EOF

    # Add the shared cabal sandbox to the PATH.
    export PATH=$$HOME/Haskell/cabal-sandbox/bin:$$PATH

    EOF

_NOTE: Make absolutely sure_ you use `cat >>` above, or you'll
overwrite your profile instead of adding to it.

At this point, we can also add it in the current shell:

    export PATH=$HOME/Haskell/cabal-sandbox/bin:$PATH

## Overall Setup Complete

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

    cabal sandbox init --sandbox=$HOME/Haskell/cabal-sandbox

Then we need to install the packages it depends on:

    cabal install --only-dependencies

This might take some time, since it has to download all the packages
`QuickTracker` needs and build them.

# Code Walkthrough
