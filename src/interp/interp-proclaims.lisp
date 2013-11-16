#+:GCL
(progn
  (eval-when (:execute :compile-toplevel :load-toplevel)
                 (proclaim '(optimize (safety 1) (debug 3)))))
