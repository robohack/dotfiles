-*- markdown -*-

# dotfiles -- shell and tool configuration files from $HOME

:copyright: Copyright 1989-2019 Greg A. Woods, and other contributors.

See the file [copyright](copyright) for details.


## How To Track Dotfiles Changes With Git

Various people[*](#references) suggest starting a "dotfiles" repository with:

	git init --bare $HOME/.dotfiles-git

(Here the "`--bare`" avoids creating a sub-directory first, and is in
effect the way to name the `GIT_DIR` in the current directory instead of
having it default to "`.git`".)

Tell git to find the actual worktree in the parent of the repository:

	git config --git-dir=$HOME/.dotfiles-git core.worktree ..
	git config --git-dir=$HOME/.dotfiles-git core.bare false

(the above doesn't work right if you're not within your `$HOME` when
running git)

Then use a shell alias to D.T.R.T. to operate on the repository and your
`$HOME`:

	alias dfgit='git --git-dir=$HOME/.dotfiles-git/'

Or better yet a more portable shell function:

	dfgit ()
	{
		git --git-dir=$HOME/.dotfiles-git/ ${1+"${@}"}
	}


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

	mv ~/work/home/.git ~/.dotfiles-git

If you do this then you don't need to add the `core.bare = true` git
config entry as "_By default a repository that ends in "`/.git`" is
assumed to be not bare (`bare = false`), while all other repositories
are assumed to be bare (`bare = true`)._"

You can avoid having git complain about all the other things you keep in
`$HOME` not being tracked by turning off that feature:

	dfgit config --local status.showUntrackedFiles no

Then you can clone to a new machine with (again note "`--bare`", which
you do need here else you'll get the repository cloned into
`~/.dotfiles-git/.git` and everything initially checked out in
`~/.dotfiles-git`):

	git clone --bare https://github.com/USERNAME/dotfiles.git $HOME/.dotfiles-git
	git --git-dir=$HOME/.dotfiles-git/ --work-tree=$HOME checkout
	. ~/.shrc	# to get "dfgit"

_Note:_  Until I figure out the Magit vs. `GIT_DIR` problem[*](#more) I
won't likely be using Git directly to record changes -- I'll continue
using SCCS and incrementally update
[dotfiles](https://github.com/robohack/dotfiles) from them.


## More Notes and Open Questions {#more}

Maybe (???) you can add the following to the end of your
`~/.dotfiles-git/config` file to enable overriding from another (local)
repository:

	[include]
		path = ~/.dotfiles_local-git

Note you could just name the GIT_DIR "`~/.git`", but that is very
dangerous!  An inadvertent "`git clean`" in a non-git-controlled
sub-directory of `$HOME` would **wipe your world** (well all but what's
tracked in `~/.git`)!!!  Indeed any other "`git`" command in a
non-git-controlled sub-directory of `$HOME` could cause you headaches.

Maybe the `GIT_DIR` should be called "`$HOME/.home-git`" since it can track
any file under `$HOME`.

But how to manage changes with Magit???

- That is the question -- how do we get Magit to use a different `GIT_DIR`
  for just one repository!?!?!?


## References

Another detailed description of this kind of setup is found at:
[https://www.atlassian.com/git/tutorials/dotfiles]

#ident	"@(#)HOME:README.md	36.1	19/11/03 17:03:02 (woods)"
