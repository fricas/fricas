(in-package "BOOT")

; Clean old data
(defun |clean_symbols|()
    (do-symbols (symbol)
        (when (get symbol 'database)
            (setf (get symbol 'database) nil))
        (when (get symbol 'abbreviationfor)
            (setf (get symbol 'abbreviationfor) nil)))
)

(defun |clean_symbols2|()
    (do-symbols (symbol)
        (when (get symbol 'database)
            (setf (get symbol 'database) nil)))
)

;; Helpers for reading databases

(defun |fetch_data_from_alist| (alist index)
    (cdr (assoc index alist :test #'string=)))

(defun |fetch_data_from_file| (ds index)
    (let (pos (alist (first ds)) (in (second ds)))
        (setf pos (third (assoc index alist :test #'string=)))
        (when pos
            (file-position in pos)
            (read in)))
)

;; Iteration over hash table
(defun |H_KEY_VALS|(table)
   (let (key_vals)
      (MAPHASH
        #'(lambda (key val) (push (cons key val) key_vals)) table)
        key_vals))

;; Write the timestamp
(defun |write_stamp|(masterpos out)
  #+:GCL (force-output out)

  (file-position out 0)
  (print (cons masterpos (get-universal-time)) out)
  #+:GCL (force-output out)
)

;------

(defun |maybe_gc|()
    nil
#+:GCL (SI::gbc t)
)
