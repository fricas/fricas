))
	 (a (make-vector (length l) 0)))
    (dolist (name l a) (intern name a))))

(provide 'fricas-cpl)
