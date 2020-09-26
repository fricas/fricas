Help
====

If you  run into any  problems with |FriCAS|  do not hesitate  to send
your  problem to  |PACKAGE_BUGREPORT|,  i.e., the  |mailing list|.
To prevent spam, you have to subscribe to the |mailing list|.

There is also a
`read-only mirror
<https://www.mail-archive.com/fricas-devel@googlegroups.com/>`_
of the mailing list.


We are well aware of the steep learning curve of |FriCAS|. By
asking your questions you not only give you a chance to solve your
problem quickly, but also help us to improve |FriCAS| by letting
us know of what a user finds hard to accomplish with the system.

Apart from the |home page| (in particular the |PACKAGE_BOOK|) there
is also help available locally. You can generate a local copy of the
|home page| by doing the following.
::

   git clone https://github.com/fricas/fricas
   cd fricas
   ./configure
   make
   cd src/doc
   make localhtml
   firefox html/index.html

..
   In order to link to another code repository and another branch, put
   the following variables assignments onto the "make localhtml"
   commandline

   PACKAGE_SOURCE=https://github.com/hemmecke/fricas BRANCH=formatted

..
   Open ``.spad`` file from localhtml

   * add
       text/x-spad       spad
    to ``/etc/mime.types``
   * in firefox add associate text/x-spad with your editor

If you are working with |FriCAS| in an operating system that has
the `X Window System <https://en.wikipedia.org/wiki/X_Window_System>`_
available, you can find most of the information
in HyperDoc (see the |PACKAGE_BOOK| for more information about
HyperDoc), which is started when you start |FriCAS|.

In a |FriCAS| session there are special commands starting with a
closing parenthesis in the first column. You can get more information
about them by typing

::

  )help help

  )help set

In particular, for the `)set` command you can find the output
`here <http://axiom-wiki.newsynthesis.org/FriCASHelpSet>`_.
