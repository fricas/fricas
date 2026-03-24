;; Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     - Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     - Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;;     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
;;       names of its contributors may be used to endorse or promote products
;;       derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package "BOOT")

; Assumes that file is postioned after end of actual data, so that
; new indextable should overwrite old indextable (or part of it),
; but should not overwrite data.
(defun |write_indextable| (stream indextable)
  (let ((pos (file-position stream)))
    (write indextable :stream stream :level nil :length nil :escape t)
    #+:GCL (force-output stream)
    (file-position stream 0)
    (princ pos stream)
    #+:GCL (force-output stream)))

(defun |write_to_stream| (val stream)
    (write val :stream stream :level nil :length nil
                 :circle t :array t :escape t)
    (terpri stream))

#+:GCL
(defun spad-fixed-arg (fname )
   (and (equal (symbol-package fname) (find-package "BOOT"))
        (not (get fname 'compiler::spad-var-arg))
        (search ";" (symbol-name fname))
        (or (get fname 'compiler::fixed-args)
            (setf (get fname 'compiler::fixed-args) t)))
   nil)

#+:GCL
(defun |compile_lib_file|(fn)
  (unwind-protect
      (progn
        (trace (compiler::fast-link-proclaimed-type-p
                :exitcond nil
                :entrycond (spad-fixed-arg (car system::arglist))))
        (trace (compiler::t1defun :exitcond nil
                :entrycond (spad-fixed-arg (caar system::arglist))))
        (compile-file fn))
    (untrace compiler::fast-link-proclaimed-type-p compiler::t1defun)))
#-:GCL
(defun |compile_lib_file|(fn)
  (if FRICAS-LISP::algebra-optimization
      (proclaim (cons 'optimize FRICAS-LISP::algebra-optimization)))
  (compile-file fn))

;; cms file operations

(defun |make_full_namestring| (filearg)
    (namestring (merge-pathnames filearg)))

;; ($ERASE filearg) -> 0 if succeeds else 1
(defun |erase_lib|(filearg)
  (if (|fricas_probe_file| filearg)
      #+:fricas_has_remove_directory
          (|remove_directory| filearg)
      #-:fricas_has_remove_directory
          (delete-directory filearg)
      1))

#+(or :abcl :clisp :cmu :ecl :gcl :lispworks :poplog)
(defun delete-directory (dirname)
   (|run_program| "rm" (list "-r" dirname)))

#+:sbcl
(defun delete-directory (dirname)
  (if (sb-ext:delete-directory dirname :recursive t)
      0
      1))

#+:openmcl
(defun delete-directory (dirname)
  (if (ccl:delete-directory dirname)
      0
      1))

(defun |replace_lib|(filespec2 filespec1)
    (|erase_lib| (setq filespec1 (|make_full_namestring| filespec1)))
    #-(or :clisp :openmcl :ecl)
    (rename-file (|make_full_namestring| filespec2) filespec1)
    #+(or :clisp :openmcl :ecl)
    (|run_program| "mv" (list (|make_full_namestring| filespec2) filespec1))
 )


(defun |copy_file|(filespec1 filespec2)
    (let ((name1 (|make_full_namestring| filespec1))
          (name2 (|make_full_namestring| filespec2)))
        (copy-lib-directory name1 name2)
))

(defun copy-lib-directory (name1 name2)
   (|run_program| "cp" (list "-r" name1 name2)))

;;; moved from fname.lisp

;;
;; Lisp support for cleaned up FileName domain.
;;
;; Created: June 20, 1991 (Stephen Watt)
;;


;; E.g.  "/"  "/u/smwatt"  "../src"

(defun |myWritable?| (s)
  (if (not (stringp s)) (|error| "``myWritable?'' requires a string arg."))
  (if (string= s "") (setq s "."))
  (if (not (|fnameExists?| s)) (setq s (|file_directory| s)))
  (if (string= s "") (setq s "."))
  (if (> (|writeablep| s) 0) 't nil) )

(defun |fnameExists?| (f)
  (if (|fricas_probe_file| (namestring f)) 't nil))

(defun |fnameReadable?| (f)
  (let ((s
          (ignore-errors (open f :direction :input :if-does-not-exist nil))
        ))
    (cond (s (close s) 't) ('t nil)) )
  )

(defun |fnameWritable?| (f)
    (|myWritable?| f))

(defun |fnameNew| (d n e)
  (if (not (|myWritable?| d))
    nil
    (do ((fn))
        (nil)
        (setq fn (|make_fname| d (string (gensym n)) e))
        (if (not (|fricas_probe_file| (namestring fn)))
           (return-from |fnameNew| fn)) )))
