Development
===========

Development is currently driven by the needs and wishes of the various
contributors.

Bugreports and patches should be sent to the |mailing list|.
In fact, all public discussion currently happens on that list.

You can get the sources from the |git repository|::

  git clone http://github.com/fricas/fricas.git

Press the fork button on the |git repository| and send patches in the
form of Github patch links, for example, as in `tU9DLT1lLes
<https://groups.google.com/forum/#!topic/fricas-devel/tU9DLT1lLes>`_

You can also send ordinary
`Github pull requests
<https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/creating-an-issue-or-pull-request>`_.


Code review policy
------------------

Normally patches should go to the |mailing list| for review.

A patch should go in as reviewed---if there is need for change new
patch should go to the list.

Obvious things can go in without review. Really minor changes to
patches also can go without extra round of review.

Of course patches should satisfy technical requirements (pass test,
update documentation when apropriate, contain tests for changes).

Patches should be logical unit of change. Do not join unrelated things
unless it is a cleanup type patch that functionally should be a no-op.

OTOH do not split patches that implement some functionalty into small
steps---if several patches have a common purpose and there are
dependencies between them, then they probably should go in as one
patch. This is not a hard rule, it make sense to split really large
patches (say more than thousend lines) and sometimes part of the
functionality is ready and may be commited before the feature is
complete.


Development resources
---------------------

.. toctree::
   :maxdepth: 1

   spadstyleguide

* `Conventions <http://axiom-wiki.newsynthesis.org/SpadFileConvention>`_

* `Lanuage Differences <http://axiom-wiki.newsynthesis.org/LanguageDifferences>`_

* `The )set command <http://axiom-wiki.newsynthesis.org/FriCASHelpSet>`_

* `Aldor User Guide <http://www.aldor.org/docs/aldorug.pdf>`_

* `Description of the BOOT language (txt) <http://fricas.sourceforge.net/doc/boot.notes>`_

* `Description of the BOOT language <http://www.euclideanspace.com/prog/scratchpad/internals/boot/index.htm>`_

* `Manual for the Boot Language <http://daly.axiom-developer.org/boot.tgz>`_
