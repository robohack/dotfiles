#
#	.gdbinit -- startup configuration for GDB
#
#ident	"@(#)HOME:.gdbinit	36.2	19/11/16 16:17:21 (woods)"
#
# See github.com/gdbinit/Gdbinit for a huge pile of related setup.
#
set history filename ~/.gdb_history
set history save
#
# These would make gdb never pause in its output
#set height 0
#set width 0
#
define bpl
  info breakpoints
end
document bpl
Syntax: bpl
| List all breakpoints.
end
#
# For modern macOS with code signing
#
# First create a self-signed root certificate for sigining.
# (see https://forward-in-code.blogspot.com/2018/11/mojave-vs-gdb.html)
# (if new, restart taskgated)
#
# Then sign the gdb binary:
#
#	codesign -s 'Kode Signing' /usr/local/bin/gdb-8
#
# Finally you must run GDB in a terminal window on the "desktop" in
# order for the system to present a secure password prompt.
#
# N.B.:  Because of how debuggers work with macOS kernel security the target
# must be run directly, not with a shell wrapper, though this means there cannot
# be any shell I/O redirection, etc. done either, so we only want to do it when
# the target is running on macOS.
#
# xxx unfortunately $osabi isn't set until the target is running....
#
#if $osabi = "darwin"
#  set startup-with-shell off
#end
define macos-setup
  set startup-with-shell off
end
#
define mallocdb
  # For NetBSD with jemalloc(3):
  #
  set env MALLOC_OPTIONS AJP
  #
  # for Mac OS X:
  #
  # MallocGuardEdges=1 MallocScribble=1 MallocCheckHeapStart=1 MallocCheckHeapEach=1 MallocErrorAbort=1
  #
  set env MallocGuardEdges 1
  set env MallocScribble 1
  set env MallocCheckHeapStart 1
  set env MallocCheckHeapEach 1
  set env MallocCheckHeapAbort 1
  set env MallocErrorAbort 1
  # for testing stuff on linux (i.e. with glibc) (don't use these when running
  # under valgrind-memcheck) [see also mallopt(3)]:
  #
  #	The numeric value of Malloc_PERTURB_ is used as the byte to
  #	initialize storage returned by malloc().  The bitwise inverse
  #	of this value is used to clear memory when the storage is
  #	freed by free().  Setting MALLOC_PERTURB_ to zero disables the
  #	feature.
  #
  # Almost equivalent to calling mallopt(M_PERTURB, 0xA5) at the top of main().
  #
  # Setting to 0xA5, cleared to 0x5A (as with jemalloc's MALLOC_OPTIONS=J)
  #
  set env MALLOC_PERTURB_=165
  #
  # Or setting to 0xAA, cleared to 0x55
  # 
  #set env MALLOC_PERTURB_=170
  #
  #	 glibc (2.x) When MALLOC_CHECK_ is set, a special (less
  #	 efficient) implementation is used which is designed to be
  #	 tolerant against simple errors, such as double calls of
  #	 free() with the same argument, or overruns of a single byte
  #	 (off-by-one bugs).  If MALLOC_CHECK_ is set to 0, any
  #	 detected heap corruption is silently ignored; if set to 1, a
  #	 diagnostic is printed on stderr; if set to 2, abort is called
  #	 immediately. This can be useful because otherwise a crash may
  #	 happen much later, and the true cause for the problem is then
  #	 very hard to track down.  When MALLOC_CHECK_ is set to 3
  #	 (i.e. 1|2), a diagnostic message is printed on stderr and the
  #	 program is aborted.
  #
  set env MALLOC_CHECK_=3
end
#
define rmmallocdb
  unset env MALLOC_OPTIONS
  unset env MallocGuardEdges
  unset env MallocScribble
  unset env MallocCheckHeapStart
  unset env MallocCheckHeapEach
  unset env MallocCheckHeapAbort
  unset env MallocErrorAbort
  unset env MALLOC_PERTURB_
  unset env MALLOC_CHECK_
end
#
# for debugging libcoro...
#
define libcoro
  handle SIGUSR2 pass nostop noprint
end
#
# and for NetBSD system binaries
#
set debug-file-directory /usr/libdata/debug
#
# This will print a warning when GDB starts up if the file does not exist...
#
source ~/.gdbinit.local
