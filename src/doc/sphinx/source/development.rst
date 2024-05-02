Development
===========

Development is currently driven by the needs and wishes of the various
contributors.

Bug reports are collected through
`issues <https://github.com/fricas/fricas/issues>`_
at github, but can also be sent to the |mailing list|.

In fact, almost all public discussion currently happens on that list.

You can get the sources from the |git repository|::

  git clone https://github.com/fricas/fricas.git

Better, press the fork button on the |git repository| and send
an ordinary `Github pull request`_.


Code review policy
------------------

Normally patches go to the |mailing list| for review or are reviewed
in a `Github pull request`_.

A patch should go in as reviewed---if there is need for change new
patch should be provided.

Obvious things can go in without review. Really minor changes to
patches also can go without extra round of review.

Of course patches should satisfy technical requirements (pass test,
update documentation when appropriate, contain tests for changes).

Patches should be logical unit of change. Do not join unrelated things
unless it is a cleanup type patch that functionally should be a no-op.

Do not split patches that implement some functionality into small
steps---if several patches have a common purpose and there are
dependencies between them, then they probably should go in as one
patch. This is not a hard rule, it makes sense to split really large
patches (say more than one thousand lines) and sometimes part of the
functionality is ready and may be committed before the feature is
complete.


Other resources
---------------

.. toctree::
   :maxdepth: 1

   spadstyleguide
   contributors

* `Debugging FriCAS <http://wiki.fricas.org/DebuggingFriCAS>`_

* `Language Differences <http://wiki.fricas.org/LanguageDifferences>`_

* `The )set command <fricas-notebooks/system-command-set.html>`_

* `Aldor User Guide <http://www.aldor.org/docs/aldorug.pdf>`_

* `Description of the BOOT language (txt) <http://fricas.sourceforge.net/doc/boot.notes>`_

* `Description of the BOOT language <http://www.euclideanspace.com/prog/scratchpad/internals/boot/index.htm>`_



.. _Github pull request: https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/creating-an-issue-or-pull-request
