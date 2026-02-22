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

;;;  PURPOSE:
;;;    Provide generally useful macros and functions for MetaLanguage
;;;    and Boot code.  Contents are organized along Common Lisp datatype
;;;    lines, with sections numbered to match the section headings of the
;;;    Common Lisp Reference Manual, by Guy Steele, Digital Press, 1984,
;;;    Digital Press Order Number EY-00031-DP.  This way you can
;;;    look up the corresponding section in the manual and see if
;;;    there isn't a cleaner and non-VM-specific way of doing things.

(provide 'Boot)

(in-package "BOOT")

; moved from bootfuns.lisp

;          Provide forward references to Boot Code for functions to be at
;          defined at the boot level, but which must be accessible
;          not defined at lower levels.

(defmacro def-boot-var (p where) `(defparameter ,p nil ,where))

(defmacro def-boot-val (p val where) `(defparameter ,p ,val ,where))

(def-boot-val |$escape_val|  (string (code-char 27))
   "string for single escape character")
(def-boot-val |$boldString| (concatenate 'string |$escape_val| "[1m")
  "switch into bold font")
(def-boot-val |$normalString| (concatenate 'string |$escape_val| "[0;10m")
  "switch back into normal font")
(def-boot-val |$BreakMode| '|query|                 "error.boot")

(def-boot-var |$reportBottomUpFlag|                 "Interpreter>SetVarT.boot")
(def-boot-val |$SetFunctions| nil  "checked in SetFunctionSlots")

(def-boot-val |$inputPromptType| '|step|  "checked in MKPROMPT")
(def-boot-val |$IOindex| 0                 "step counter")

; End of moved fragment

; 5 PROGRAM STRUCTURE

; 5.3.2 Declaring Global Variables and Named Constants

(defun |functionp| (fn)
   (if (identp fn) (and (fboundp fn) (not (macro-function fn))) (functionp fn)))
(defun |macrop| (fn) (and (identp fn) (macro-function fn)))

; 6 PREDICATES

; Ordering

(DEFUN |lt_sexp| (U V)
  "Multiple-type ordering relation."
;;; Result negated compared to LEXGREATERP and GGREATERP
;;;   Order of types: nil number symbol string vector cons"
  (COND ((EQ U V) NIL)
        ((NULL U) T)
        ((NULL V) NIL)
        ((ATOM U)
         (if (ATOM V)
             (COND ((NUMBERP U) (if (NUMBERP V) (> V U) T))
                   ((NUMBERP V) NIL)
                   ((IDENTP U) (AND (IDENTP V)
                                    (string> (SYMBOL-NAME V) (SYMBOL-NAME U))
                                    T))
                   ((IDENTP V) NIL)
                   ((STRINGP U) (AND (STRINGP V) (string> V U) T))
                   ((STRINGP V) NIL)
                   ((BREAK)))
               T))
        ((ATOM V) NIL)
        ((EQUAL (CAR U) (CAR V))
           (|lt_sexp| (CDR U) (CDR V)))
        ((|lt_sexp| (CAR U) (CAR V)))
))

(defun GLESSEQP (X Y) (|lt_sexp| X Y))

(defun LEXLESSEQP (X Y) (|lt_sexp| X Y))


; 10.3 Creating Symbols

(DEFUN IS_GENVAR (X)
  (AND (IDENTP X)
       (let ((y (symbol-name x)))
         (and (char= #\$ (elt y 0)) (> (size y) 1) (digitp (elt y 1))))))

; 14 SEQUENCES

; 14.2 Concatenating, Mapping, and Reducing Sequences

(defun |delete| (item sequence)
   (cond ((symbolp item) (remove item sequence :test #'eq))
         ((and (atom item) (not (arrayp item))) (remove item sequence))
         (T (remove item sequence :test #'equalp))))

; 15 LISTS

; 15.2 Lists

(DEFUN LASTATOM (L) (if (ATOM L) L (LASTATOM (CDR L))))

; 15.4 Substitution of Expressions

;; needed for substNames (always copy)
(DEFUN SUBSTQ (NEW OLD FORM)
  "Version of SUBST that uses EQ rather than EQUAL on the world."
  (PROG (NFORM HNFORM ITEM)
        (SETQ HNFORM (SETQ NFORM (CONS () ())))
     LP    (RPLACD NFORM
                   (COND ((EQ FORM OLD) (SETQ FORM ()) NEW )
                         ((NOT (PAIRP FORM)) FORM )
                         ((EQ (SETQ ITEM (CAR FORM)) OLD) (CONS NEW ()) )
                         ((PAIRP ITEM) (CONS (SUBSTQ NEW OLD ITEM) ()) )
                         ((CONS ITEM ()))))
        (if (NOT (PAIRP FORM)) (RETURN (CDR HNFORM)))
        (SETQ NFORM (CDR NFORM))
        (SETQ FORM (CDR FORM))
        (GO LP)))

; 15.5 Using Lists as Sets

(DEFUN |set_sum| (X Y)
  (COND ((ATOM Y) X)
        ((ATOM X) Y)
        ((MEMBER (CAR X) Y :test #'equal) (|set_sum| (CDR X) Y))
        ((|set_sum| (CDR X) (CONS (CAR X) Y)))))

(defun |set_difference| (l1 l2) (set-difference l1 l2 :test #'equal))

(defun remdup (l) (remove-duplicates l :test #'equalp))

; 15.6 Association Lists

(defun DELASC (u v) "Returns a copy of a-list V in which any pair with key U is deleted."
   (cond ((atom v) nil)
         ((or (atom (car v))(not (equal u (caar v))))
          (cons (car v) (DELASC u (cdr v))))
         ((cdr v))))

(DEFUN ADDASSOC (X Y L)
  "Put the association list pair (X . Y) into L, erasing any previous association for X"
  (COND ((ATOM L) (CONS (CONS X Y) L))
        ((EQUAL X (CAAR L)) (CONS (CONS X Y) (CDR L)))
        ((CONS (CAR L) (ADDASSOC X Y (CDR L))))))

(DEFUN DELLASOS (U V)
  "Remove any association pair (U . X) from list V."
  (COND ((ATOM V) NIL)
        ((EQUAL U (CAAR V)) (CDR V))
        ((CONS (CAR V) (DELLASOS U (CDR V))))))

(DEFUN ASSOCLEFT (X)
  "Returns all the keys of association list X."
  (if (ATOM X) X (mapcar #'car x)))

(DEFUN ASSOCRIGHT (X)
  "Returns all the datums of association list X."
  (if (ATOM X) X (mapcar #'cdr x)))

(DEFUN LASSOC (X Y)
  "Return the datum associated with key X in association list Y."
  (PROG ()
     A  (COND ((ATOM Y) (RETURN NIL))
              ((EQUAL (CAAR Y) X) (RETURN (CDAR Y))) )
        (SETQ Y (CDR Y))
        (GO A)))

(DEFUN |rassoc| (X Y)
  "Return the datum associated with key X in association list Y."
  (PROG ()
     A  (COND ((ATOM Y) (RETURN NIL))
              ((EQUAL (CDAR Y) X) (RETURN (CAAR Y))) )
        (SETQ Y (CDR Y))
        (GO A)))

(defun QLASSQ (p a-list) (cdr (assq p a-list)))

;;; Operations on Association Sets (AS)

(defun AS_INSERT (A B L)
    (let ((pp (assoc A L :test #'equal)))
        (if pp
            (progn
                 (setf (cdr pp) B)
                 L))
         (cons (cons A B) L)))

; 22 INPUT/OUTPUT

; 22.2 Input Functions

; 22.2.1 Input from Character Streams

; 22.3 Output Functions

; 22.3.1 Output to Character Streams

(defun |get_lisp_std_out| () *standard-output*)

(defun |get_lisp_error_out| () *error-output*)

(defvar |$highlightFontOn| (concat " " |$boldString|)
                     "switch to highlight font")
(defvar |$highlightFontOff| (concat |$normalString| " ")
                     "return to normal font")

(defun SAY (&rest x) (progn (MESSAGEPRINT X) (TERPRI)))

(DEFUN MESSAGEPRINT (X) (mapc #'messageprint-1 X))

(DEFUN MESSAGEPRINT-1 (X)
  (COND ((OR (EQ X '|%l|) (EQUAL X "%l")) (TERPRI))
        ((STRINGP X) (PRINC X))
        ((IDENTP X) (PRINC X))
        ((ATOM X) (PRINC X))
        ((PRINC "(") (MESSAGEPRINT-1 (CAR X))
         (MESSAGEPRINT-2 (CDR X)) (PRINC ")"))))

(DEFUN MESSAGEPRINT-2 (X)
  (if (ATOM X)
      (if (NULL X) NIL (progn (PRINC " . ") (MESSAGEPRINT-1 X)))
      (progn (PRINC " ") (MESSAGEPRINT-1 (CAR X)) (MESSAGEPRINT-2 (CDR X)))))

; 24 ERRORS

; 24.2 Specialized Error-Signalling Forms and Macros

(defun MOAN (&rest x) (|sayBrightly| `(|%l| "===> " ,@X |%l|)))

(DEFUN FAIL () (|systemError| '"Antique error (FAIL ENTERED)"))

; 25 MISCELLANEOUS FEATURES

(defun DROPTRAILINGBLANKS  (LINE)
     (let ((l (length LINE)))
         (if (and (> l 0)
                  (char= (char LINE (1- l)) #\ ))
             (string-right-trim " " LINE)
             LINE)))

; This function was modified by Greg Vanuxem on March 31, 2005
; to handle the special case of #'(lambda ..... which expands
; into (function (lambda .....
;
; The extra if clause fixes bugs #196 and #114
;
; an example that used to cause the failure was:
; )set func comp off
; f(xl:LIST FRAC INT): LIST FRAC INT == map(x +-> x, xl)
; f [1,2,3]
;
; which expanded into
;
; (defun |xl;f;1;initial| (|#1| |envArg|)
;  (prog (#:G1420)
;   (return
;    (progn
;     (lett #:G1420 'uninitialized_variable |f| |#1;f;1:initial|)
;      (spadcall
;       (cons (|function| (lambda (#:G1420 |envArg|) #:G1420)) (vector))
;       |#1|
;       (qrefelt |*1;f;1;initial;MV| 0))))))
;
; the (|function| (lambda form used to cause an infinite expansion loop
;
(defun macroexpandall (sexpr)
 (cond
  ((atom sexpr) sexpr)
  ((eq (car sexpr) 'quote) sexpr)
  ((eq (car sexpr) 'defun)
   (cons (car sexpr) (cons (cadr sexpr)
       (mapcar #'macroexpandall (cddr sexpr)))))
  ((and (symbolp (car sexpr)) (macro-function (car sexpr)))
   (do ()
       ((not (and (consp sexpr) (symbolp (car sexpr))
                  (macro-function (car sexpr)))))
     (setq sexpr (macroexpand sexpr)))
   (if (consp sexpr)
      (let ((a (car sexpr)) (b (cadr sexpr)))
         (if (and (eq a 'function) (consp b) (eq (car b) 'lambda))
            (cons a (list (cons (car b)
                                (mapcar #'macroexpandall (cdadr sexpr)))))
            (mapcar #'macroexpandall sexpr)))
      sexpr))
  ('else
    (mapcar #'macroexpandall sexpr))))

(defun |print_full2| (expr stream)
   (let ((*print-circle* t) (*print-array* t) *print-level* *print-length*)
     (print expr stream)
     (terpri stream)))

(defun |print_full1| (expr) (|print_full2| expr *standard-output*))

;; moved here from preparse.lisp

(defvar tab-size-in-spaces 8
  "How many spaces do we consider a #\Tab character?")

(defun NEXT-TAB-LOC (i)
  "Given a character position I, on what position would a #\Tab land
us?"
  (* tab-size-in-spaces (1+ (truncate i tab-size-in-spaces))))

(defun EXPAND_TABS (str)
  "Given a string STR, expand all #\Tab characters to spaces, minding
the correct column each #\Tab would carry us to.

This function respects intermediate #\Newline characters and drops
#\Return characters."
  (cond
    ((stringp str)
     (with-output-to-string (s)
       (loop :with column := 0
             :for c :across str
             :do (case c
                   (#\Tab
                    ;; How many spaces does our tab carry us forward
                    ;; by?
                    (let ((num-spaces (- (next-tab-loc column) column)))
                      (incf column num-spaces)
                      ;; This format string just writes something N
                      ;; times without consing up garbage.
                      (format s "~v@{~C~:*~}" num-spaces #\Space)))
                   (#\Newline
                    (setf column 0)
                    (write-char #\Newline s))
                   (#\Return
                    ;; Drop this character completely.
                    nil)
                   (t
                    (incf column)
                    (write-char c s))))))
    (t
     str)))

;; stream handling for paste-in generation

(defun |applyWithOutputToString| (func args)
  ;; returns the cons of applying func to args and a string produced
  ;; from standard-output while executing.
  (let* ((*standard-output* (make-string-output-stream))
         (curoutstream *standard-output*)
         (*error-output* *standard-output*)
         (|$algebraOutputStream| (CONS NIL *standard-output*))
        val)
    (declare (special curoutstream
                      |$algebraOutputStream|))
    (setq val (catch 'spad_reader
                  (apply (symbol-function func) args)))
    (cons val (get-output-stream-string *standard-output*))))

(defun |breakIntoLines| (str)
  (let ((bol 0) (eol) (line-list nil))
    (loop
     (setq eol (position #\Newline str :start bol))
     (if (null eol) (return))
     (if (> eol bol)
         (setq line-list (cons (subseq str bol eol) line-list)))
     (setq bol (+ eol 1)))
    (nreverse line-list)))

; moved from comp.lisp

;;; Common Block section

(defun |do_comp_quietly| (driver fn |comp370_apply|)
  (let ((*compile-verbose* nil)
     ;; following creates a null outputstream if $InteractiveMode
        (*standard-output*
         (if |$InteractiveMode| (make-broadcast-stream)
           *standard-output*)))
    (handler-bind ((warning #'muffle-warning)
                   #+:sbcl (sb-ext::compiler-note #'muffle-warning))
      (funcall driver fn |comp370_apply|)
    )
))

;; used to be called POSN - but that interfered with a CCL function
(DEFUN POSN1 (X L) (position x l :test #'equal))

; end of moved fragment

;;; moved from debug.lisp

; NAME:    Debugging Package
; PURPOSE: Debugging hooks for Boot code

(defun ENABLE_BACKTRACE (&rest arg))

(defun |adjoin_equal|(x y) (ADJOIN x y :test #'equal))

(defun |remove_equal|(x y) (REMOVE x y :test #'equal))

(defun WHOCALLED(n) nil) ;; no way to look n frames up the stack

(defun heapelapsed ()
  #+:clisp
  (multiple-value-bind (used room static gc-count gc-space gc-time) (sys::%room)
    (+ used gc-space))
  #+:cmu (ext:get-bytes-consed)
  #+:ecl (si:gc-stats t)
  #+:openmcl (ccl::total-bytes-allocated)
  #+:sbcl (sb-ext:get-bytes-consed)
  #+:lispworks (hcl:total-allocation)
  #-(or :clisp :cmu :ecl :openmcl :sbcl :lispworks)
  0)

(defun |goGetTracerHelper| (dn f oname alias options modemap)
    (lambda(&rest l)
         (|goGetTracer| l dn f oname alias options modemap)))

(defun |setSf| (sym fn) (SETF (SYMBOL-FUNCTION sym) fn))

(DEFUN IS_SHARP_VAR (X)
  (AND (IDENTP X)
       (EQL (ELT (PNAME X) 0) #\#)
       (INTEGERP (parse-integer (symbol-name X) :start 1))))

(defun |char_to_digit|(x) (digit-char-p x))

(defun SPADSYSNAMEP (STR)
  (let (n i j)
    (AND (SETQ N (MAXINDEX STR))
         (SETQ I (position #\. STR :start 1))
         (SETQ J (position #\, STR :start (1+ I)))
         (do ((k (1+ j) (1+ k)))
             ((> k n) t)
           (if (not (digitp (elt str k))) (return nil))))))

; **********************************************************************
;            Utility functions for Tracing Package
; **********************************************************************

(MAKEPROP '|coerce| '/TRANSFORM '(& & *))
(MAKEPROP '|comp| '/TRANSFORM '(& * * &))
(MAKEPROP '|compIf| '/TRANSFORM '(& * * &))

;  by having no transform for the 3rd argument, it is simply not printed

(MAKEPROP '|compFormWithModemap| '/TRANSFORM '(& * * & *))

;;; A "resumable" break loop for use in trace etc. Unfortunately this
;;; only worked for CCL. We need to define a Common Lisp version. For
;;; now the function is defined but does nothing.
(defun interrupt (&rest ignore))

;;; end of moved fragment
