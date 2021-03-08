;;; This file contains routines to set up the host Lisp.

#+sbcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (ignore-errors (require "SB-SPROF")))

#+cmu
(eval-when (:execute :compile-toplevel :load-toplevel)
  (setf STREAM::*DEFAULT-EXTERNAL-FORMAT* :ISO8859-1))

#+poplog
(eval-when (:compile-toplevel :execute :load-toplevel)
  (set-syntax-from-char #\@ #\@))

#+ecl
(progn
  (require 'cmp)
  (eval-when (:execute :compile-toplevel :load-toplevel)
    (proclaim '(optimize (safety 0)))))

#+openmcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (setf *features* (remove ':CCL *features*)))
