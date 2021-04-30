Download
--------

* Official |git repository| (read-only access). Fork it on github if
  you like.
  ::

    git clone https://github.com/fricas/fricas.git

* Release tarballs (up to version  1.3.6):
  https://sourceforge.net/projects/fricas/files/fricas/.

  The release tarballs are identical to the respective version in the
  |git repository|, but additionally contain pregenerated (machine
  independent) files, i.e., compilation from release tarballs takes
  less time.

* `FriCAS Docker Image <https://hub.docker.com/r/nilqed/fricas/>`_
  (`Announcement <https://groups.google.com/d/msgid/fricas-devel/1d9d4a04-1489-f879-f2ca-8798359540d0%40gmail.com>`_)

* FriCAS on Windows
  https://groups.google.com/d/msg/fricas-devel/zMY1IADEx3A/dO4c0-UlBAAJ

  https://drive.google.com/file/d/1CI8IF9ScrAGG0KIhbldrRpVkXo-Q7S-Y/view?usp=sharing

* Nightly builds::

  * linux: https://github.com/oldk1331/fricas/actions/runs/654525090
  * macos: https://github.com/oldk1331/fricas/actions/runs/654525087
  * windows: https://github.com/oldk1331/fricas/actions/runs/654525092

  A few downsides:

  #. You have to login GitHub to download this "CI artifact".
     (Not necessarily a bad thing, I don't want to pollute the
     "Release" page in GitHub.)
  #. It only exists for a short period. (90 days?)
  #. The packaging is very crude for now, not out-of-box user experience.
     (You have to "cd" in the shell and set "FRICAS" env variable.)
  #. It's double compressed, you have to unpack twice.

* Here is a `list of versions of FriCAS
  <https://repology.org/project/fricas/versions>`_
  that can be installed by the package manager of your operating
  system. Note, however, that these packages are not provided by
  the FriCAS developers, but by other volunteers.

   .. * Debian package

  * `ArchLinux package <https://aur.archlinux.org/packages/fricas/>`_
  * `Gentoo package <https://packages.gentoo.org/packages/sci-mathematics/fricas>`_
  * `NetBSD package <http://pkgsrc.se/math/fricas>`_
  * `MacPort <https://github.com/macports/macports-ports/tree/master/math/fricas>`_



.. toctree::
   :maxdepth: 1

   install
