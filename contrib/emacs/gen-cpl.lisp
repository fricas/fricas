(with-open-file (f "fricas-cpl.in"
                   :direction :output
                   :if-does-not-exist :create)
    (dolist (el (|allOperations|))
         (format f "~S~&" (symbol-name el)))
    (dolist (el (|allConstructors|))
         (if (not (|isDefaultPackageForm?| (list el)))
              (progn
                  (format f "~S~&" (symbol-name el))
                  (let ((al (getdatabase el 'ABBREVIATION)))
                       (if al
                           (format f "~S~&"
                                (symbol-name al))
                       )
                  )
              )
         )
    )
)
