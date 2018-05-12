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


(in-package "BOOTTRAN")

;; The following comes from file previously known as npextras.lisp
(defun double (x) (float x 1.D0))

(defun pname (x)
  (cond ((symbolp x) (symbol-name x))
        ((characterp x) (string x))
        (t nil)))

(defun |char| (x) (CHAR (PNAME x) 0))

(defun |shoeCOMPILE-FILE| (fn) (compile-file fn ))
(defun setdifference (x y) (set-difference x y))
(defun make-cvec (sint) (make-string sint))
(defun MAKE_VEC (n) (make-array n))
(defun concat (&rest l)
  (progn
    (setq l (mapcar #'string l))
    (apply #'concatenate 'string l)))

(defun |shoeInputFile| (filespec )
  (open filespec :direction :input :if-does-not-exist nil))

(defun |shoeCLOSE| (s) (close s))

;;; ------------------------------------------------

(defun |shoeConsole| (line)  (write-line line *terminal-io*))

(defun shoeprettyprin1 (x stream)
  (let ((*print-pretty* t)
        (*print-array* t)
        (*print-circle* t)
        (*print-level* nil)
        (*print-length* nil))
    (prin1 x stream)))

(defun reallyprettyprint (x &optional (stream *terminal-io*))
  (shoeprettyprin1 x stream) (terpri stream))

(defun shoeprettyprin0 (x stream)
  (let ((*print-pretty* nil)
        (*print-array* t)
        (*print-circle* t)
        (*print-level* nil)
        (*print-length* nil))
    (prin1 x stream)))

(defun shoenotprettyprint (x stream)
  (shoeprettyprin0 x stream) (terpri stream))

(defun make-full-cvec (sint &optional (char #\space))
  (make-string sint :initial-element (character char)))

(defun |shoeread-line| (st &optional (eofval nil))
  (read-line st nil eofval))

(defun |shoePLACEP| (item)
  (eq item nil))

(defun substring (cvec start length)
  (if length (subseq cvec start (+ start length))
    (subseq cvec start)))

(defun MAKE_HASHTABLE (id1)
  (let ((test (case id1
                    ((EQ ID) #'eq)
                    (CVEC #'equal)
                    ((UEQUAL EQUAL) #'equal)
                    (otherwise (error "bad arg to make-hashtable")))))
    (make-hash-table :test test)))

(defun HKEYS (table)
  (let (keys)
    (maphash #'(lambda (key val)
                 (declare (ignore val))
                 (push key keys)) table)
    keys))


(defun HPUT (table key value)
  (setf (gethash key table) value))


(defun stringimage (x)
  (write-to-string x))

(defun charmem (a b)
  (member  a  b :test #'eql))

(defun |shoeCloser| (w)
  (MEMQ (|shoeKeyWord| w) '(CPAREN CBRACK)))

(defun |shoeIdChar| (x)
  (or (ALPHANUMERICP x)
      (charmem x '(#\' #\? #\%))))

(defun |shoeStartsId| (x)
  (or (alpha-char-p x)
      (charmem x '(#\$ #\? #\%))))

(defun strpos (what in start dontcare)
  (setq what (string what) in (string in))
  (if dontcare (progn (setq dontcare (character dontcare))
                      (search what in :start2 start
                              :test #'(lambda (x y) (or (eql x dontcare)
                                                        (eql x y)))))
    (search what in :start2 start)))


(defun strposl (table cvec sint item)
  (setq cvec (string cvec))
  (if (not item)
      (position table cvec :test #'(lambda (x y) (position y x)) :start sint)
    (position table cvec :test-not #'(lambda (x y) (position y x))
              :start sint  )))

(defun  bvec-make-full (n x)
  (make-array (list n) :element-type 'bit :initial-element x))

(defun make-bvec (n)
  (bvec-make-full n 0))

(defun size (l)
  (cond ((vectorp l) (length l))
        ((consp l) (list-length l))
        (t 0)))

(defun identp (a)
  (and (symbolp a) a))

(defun shoeGREATERP (s1 s2)
  (string> (string s1) (string s2)))

(defun |shoeReadLisp| (s n)
  (multiple-value-list (read-from-string s nil nil :start n)))

(defun |last| (x)
  (car (last x)))
