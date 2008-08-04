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


#|
GCL keeps noting the fact that the compiler is performing tail-recursion.
Bill Schelter added this as a debugging tool for Axiom and it was never
removed. Patching the lisp code in the GCL build fails as the system
is actually built from the pre-compiled C code. Thus, we can only step
on this message after the fact. The cmpnote function is used nowhere
else in GCL so stepping on the function call seems best. We're unhappy
with this hack and will try to convince the GCL crowd to fix this.
|#
#+:gcl (defun compiler::cmpnote (&rest x))

(in-package "BOOT")

(defun use-fast-links (arg)
;  (format t "use-fast-links called with ~A~%" arg)
#+:GCL  (si::use-fast-links arg)
 )

(defun enable-backtrace (&rest arg)
#+:ccl
  (format t "protected-symbol-warn called with ~A~%" arg))

;; NOTE: JoinInner is defined in CATEGORY BOOT
;; following code needs to run interpreted to overcome arglist length limits
(defun |Join| (&rest L)
  (|JoinInner| L (if (OR (not (boundp '|$e|)) (NULL |$e|) |$InteractiveMode|)
                   |$CategoryFrame|
                   |$e|)))

