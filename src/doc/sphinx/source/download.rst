Download
========

.. contents:: Table of Contents
   :local:


Official releases
-----------------

https://github.com/fricas/fricas/releases/


Binary distribution
^^^^^^^^^^^^^^^^^^^
- ``fricas-x.y.z.amd64.tar.bz2`` (64-bit binary for amd64 (x86_64) Linux)
- ``fricas-x.y.z.i386.tar.bz2`` (32-bit binary for i386 Linux)
- ``fricas-x.y.z-macOS-x64.dmg`` ((beta) 64 binary for macOS)
- ``fricas-x.y.z-windows-x64.zip`` ((beta) 64 binary for Windows)


Source distribution
^^^^^^^^^^^^^^^^^^^

- ``fricas-x.y.z-full.tar.bz2`` is a "full" source tarball.
  It is identical to the respective version in the |git repository|,
  but additionally contains pregenerated (machine independent) files,
  i.e., compilation from these tarballs takes less time than from the
  git sources.

- ``x.y.z.tar.gz`` and ``x.y.z.zip`` source as in git repository

FriCAS x.y.z should build on Linux and many Unix like systems (for
example Mac OSX and Cygwin).

Official |git repository| (read-only access). Fork it on github if
you like.
::

   git clone https://github.com/fricas/fricas.git

Alternative distribution platform (includes older releases)
-----------------------------------------------------------

https://sourceforge.net/projects/fricas/files/fricas/

Other forms of distribution
---------------------------

Here is a `list of versions of FriCAS
<https://repology.org/project/fricas/versions>`_
that can be installed by the package manager of your operating
system. Note, however, that these packages are not provided by
the FriCAS developers, but by other volunteers.

* `Debian package <https://packages.debian.org/stable/math/fricas>`_
  (build on GCL_)

* `ArchLinux package <https://aur.archlinux.org/packages/fricas/>`_
* `Gentoo package <https://packages.gentoo.org/packages/sci-mathematics/fricas>`_
* `NetBSD package <http://pkgsrc.se/math/fricas>`_
* `MacPort <https://github.com/macports/macports-ports/tree/master/math/fricas>`_


Nightly builds
--------------

https://github.com/fricas/fricas-nightly-builds/releases/tag/nightly

A few downsides:

#. It only exists for a short period. (90 days?)
#. The packaging is very crude for now, not out-of-box user experience.
   (You have to "cd" in the shell and set "FRICAS" env variable.)


Installation Guide
------------------

.. toctree::
   :maxdepth: 1

   install



.. _GCL: https://www.gnu.org/software/gcl/
