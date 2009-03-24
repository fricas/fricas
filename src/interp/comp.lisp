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


; NAME:    Compiler Utilities Package

; PURPOSE: Comp is a modified version of Compile which is a preprocessor for
;          calls to Lisp Compile.  It searches for variable assignments that use
;          (SPADLET a b). It allows you to create local variables without
;          declaring them local by moving them into a PROG variable list.
;          This is not an ordinary SPADLET.  It looks and is used like a SETQ.
;          This preprocessor then collects the uses and creates the PROG.
;
;          SPADLET is defined in Macro.Lisp.
;
;          Comp recognizes as new lambda types the forms SPADSLAM, SLAM,
;          and entries on $clamList.  These cache results.  ("Saving LAMbda".)
;          If the function is called with EQUAL arguments, returns the previous
;          result computed.
;
;          The package also causes traced things which are recompiled to
;          become untraced.

(in-package "BOOT")

(export '(Comp OptionList SLAM SPADSLAM FLUID))

;;; Common Block section

(defparameter |$fluidVars| nil)
(defparameter |$locVars| nil)

(defun |compAndDefine| (L)
  (let ((*comp370-apply* (function print-and-eval-defun)))
    (declare (special *comp370-apply*))
    (COMP L)))

(defun COMP (L) (MAPCAR #'COMP-2 (MAPCAN #'COMP-1 L)))

(defun comp_quietly_using_driver (driver fn)
  (let ((*comp370-apply*
         (if |$InteractiveMode|
             (if |$compileDontDefineFunctions| #'compile-defun #'eval-defun)
           #'print-defun))
     ;; following creates a null outputstream if $InteractiveMode
        (*standard-output*
         (if |$InteractiveMode| (make-broadcast-stream)
           *standard-output*))
        (*compile-verbose* nil))
    #-:GCL
    (handler-bind ((warning #'muffle-warning)
                   #+:sbcl (sb-ext::compiler-note #'muffle-warning))
      (funcall driver fn)
      )
    #+:GCL
      (funcall driver fn)
))

(defun |compQuietly| (fn)
    (comp_quietly_using_driver #'COMP fn))

(defun |compileFileQuietly| (fn)
    (comp_quietly_using_driver #'COMPILE-FILE fn))

(defun |compileQuietly| (fn)
    (comp_quietly_using_driver #'COMP370 fn))

;; used to be called POSN - but that interfered with a CCL function
(DEFUN POSN1 (X L) (position x l :test #'equal))

; NOTE: It is potentially dangerous to assume every occurrence of element of
; $COMP-MACROLIST is actually a macro call

(defparameter $COMP-MACROLIST
  '(COLLECT REPEAT SUCHTHATCLAUSE THETA COLLECTV COLLECTVEC
            THETA1 SPADREDUCE SPADDO)
  "???")

(defmacro RESET (L) `(spadlet . ,L))
