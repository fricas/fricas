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


; NAME:    Scratchpad Package
; PURPOSE: This is an initialization and system-building file for Scratchpad.

(in-package "BOOT")

;;; Common  Block

(defvar |$UserLevel| '|development|)
(defvar |$reportInstantiations| nil)
(defvar |$reportEachInstantiation| nil)
(defvar |$reportCounts| nil)
(defvar |$compileOnlyCertainItems| nil "list of functions to compile")
(defvar |$doNotCompileJustPrint| nil "switch for compile")
(defvar |$PrintCompilerMessageIfTrue| t)
(defvar |$Rep| '|$Rep| "should be bound to gensym? checked in coerce")
(defvar |$scanIfTrue| nil "if t continue compiling after errors")
(defvar |$Representation| nil "checked in compNoStacking")
(defvar |$definition| nil "checked in DomainSubstitutionFunction")
(defvar |$env| nil "checked in isDomainValuedVariable")
(defvar |$e| nil "checked in isDomainValuedVariable")
(defvar |$getPutTrace| nil)

;************************************************************************
;         SYSTEM COMMANDS
;************************************************************************

(defun |fin| ()
  (SETQ *EOF* 'T)
  (THROW 'SPAD_READER NIL))

(defun QUOTIENT2 (X Y) (values (TRUNCATE X Y)))

(defun INTEXQUO(X Y)
    (multiple-value-bind (quo remv) (TRUNCATE X Y)
         (if (= 0 remv) quo nil)))

(defun DIVIDE2 (X Y) (multiple-value-call #'cons (TRUNCATE X Y)))

(defun |makeSF| (mantissa exponent)
  (FLOAT (/ mantissa (expt 2 (- exponent))) 0.0d0))

;; This is used in the domain Boolean (BOOLEAN.NRLIB/code.lsp)
(defun |BooleanEquality| (x y) (if x y (null y)))

(MAKEPROP 'END_UNIT 'KEY T)

;;; (defun |evalSharpOne| (x \#1) (declare (special \#1)) (EVAL x))
(defun |evalSharpOne| (x |#1|)
   (declare (special |#1|))
      (EVAL `(let () (declare (special |#1|)) ,x)))

(DEFUN ASSOCIATER (FN LST)
  (COND ((NULL LST) NIL)
        ((NULL (CDR LST)) (CAR LST))
        ((LIST FN (CAR LST) (ASSOCIATER FN (CDR LST))))))

; **** X. Random tables

(MAKEPROP 'TAG '|Led| '(TAG TAG 122 121))
(MAKEPROP 'EQUATNUM '|Nud| '(|dummy| |dummy| 0 0))
(MAKEPROP 'EQUATNUM '|Led| '(|dummy| |dummy| 10000 0))
(MAKEPROP 'LET '|Led| '(|:=| LET 125 124))
(MAKEPROP 'SEGMENT '|Led| '(\.\. SEGMENT 401 699 (|boot-Seg|)))
(MAKEPROP 'SEGMENT '|isSuffix| 'T)

;; function to create byte and half-word vectors in new runtime system 8/90

(defun |makeByteWordVec| (initialvalue)
  (let ((n (cond ((null initialvalue) 7) ('t (reduce #'max initialvalue)))))
    (make-array (length initialvalue)
      :element-type (list 'mod (1+ n))
      :initial-contents initialvalue)))

(defun |makeByteWordVec2| (maxelement initialvalue)
  (let ((n (cond ((null initialvalue) 7) ('t maxelement))))
    (make-array (length initialvalue)
      :element-type (list 'mod (1+ n))
      :initial-contents initialvalue)))

(defun |knownEqualPred| (dom)
  (let ((fun (|compiledLookup| '= '((|Boolean|) $ $) dom)))
    (if fun (get (bpiname (car fun)) '|SPADreplace|)
      nil)))

(defun |hashable| (dom)
  (memq (|knownEqualPred| dom)
        #-Lucid '(EQ EQL EQUAL)
        #+Lucid '(EQ EQL EQUAL EQUALP)
        ))
