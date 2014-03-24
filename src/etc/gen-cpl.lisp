(DEFUN do_command_list (fname)
    (with-open-file (f fname
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
        (dolist (el (|allOperations|))
             (format f "~A~&" (symbol-name el)))
        (dolist (el (|allConstructors|))
             (if (not (|isDefaultPackageForm?| (list el)))
                  (progn
                      (format f "~A~&" (symbol-name el))
                  )
             )
        )
    )
)
