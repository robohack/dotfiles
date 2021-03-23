[//]: # (-*- markdown -*-)

# dotfiles -- shell and tool configuration files from $HOME

:copyright: Copyright 1989-2020 Greg A. Woods, and other contributors.

See the file [copyright](copyright) for details.


## How To Track Dotfiles Changes With Git

Various people[*](#references) suggest starting a "dotfiles" repository
in a wide variety of ways, some quite convoluted.

The simplest though is to simply make your `$HOME` a Git working
repository (one containing only your (public) dotfiles).

Thanks to [Drew
DeVault](https://drewdevault.com/2019/12/30/dotfiles.html) for giving me
the one tiny clue I needed to making this safe and simple to use.  The
trick is to create a one-line `$HOME/.gitignore` file (and be sure to
add and commit it to your Git repository:

    *

This means you will have to explicitly add files you want to track
(i.e. with `git add -f .dotfile`), but it also means Git won't be
constantly nagging you about files you don't want to track, and most
critically it will make it (relatively?) safe to use Git for other
repositories in any sub-directory of your `$HOME`.


## Moving from SCCS to Git(Hub)

I've created my dotfiles repo from my original `$HOME/SCCS` repository
using my modified version of
[git-sccsimport](https://github.com/robohack/git-sccsimport):

	git-sccsimport --move-date=2010/11/5T00:00:00 --move-offset=3 --expand-kw --maildomain=robohack.ca --git-dir="$HOME/work/home" --dirs SCCS

(Note:  On 2010/11/04 I arrived in Kelowna from Toronto, so since that
time the local SCCS timestamps are three hours less than they were, so
if an SCCS timestamp is from before that date, then I tell
`git-sccsimport` to add three hours to it.  Combined with use of
`--expand-kw` it is then possible to see the exact correspondence
between the new Git dates and the dates in all the SCCS headers using
"`git log --date=local -p --full-diff`".)

Then I published my dotfiles!

	cd ~/work/home
	hub create robohack/dotfiles

To use Git to record changes to my dotfiles I just moved (or copied) the
`.git` directory into place:

	mv ~/work/home/.git ~/

_Note:_ Until I test this more I won't likely be using Git directly on
my main home server to record changes -- I'll continue using SCCS and
incrementally update [dotfiles](https://github.com/robohack/dotfiles)
from them.


## Replicating Your `$HOME` On Another Machine

You can now _"clone"_ to a new machine (or share with others) with this
set of commands:

    cd
    git init
    git remote add origin git@github.com:USERNAME/dotfiles.git
    git fetch
    git checkout -f master


## More Notes and Open Questions {#more}

When you use the default name name for your `GIT_DIR` of "`~/.git`", as
I've show above, it can be **very** dangerous if your `~/.gitignore`
file ever ends up missing or with anything but a lone `*` in it!  In
such a case an inadvertent "`git clean`" in a non-Git-controlled
sub-directory of `$HOME` would **wipe your world** (well but what's
tracked in `~/.git`)!!!  Indeed any other "`git`" command in a
non-Git-controlled sub-directory of `$HOME` could cause you headaches.


## References

Another detailed description of this kind of setup is found at:
[https://www.atlassian.com/git/tutorials/dotfiles]

#ident	"@(#)HOME:README.md	37.1	21/03/23 11:43:08 (woods)"
