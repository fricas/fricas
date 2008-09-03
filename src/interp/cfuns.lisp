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

;; Duplicates code in hashcode.boot -- probably should remove this
;; version.
;;stolen from AXIOM-XL src/strops.c
#+(AND GCL (NOT ELF))
(LISP::clines 
"MYHASH(s)"
"char *s;"
"{"
" register unsigned int   h = 0;"
" register int    c;"
""
" while ((c = *s++) != 0) {"
"  h ^= (h << 8);"
"  h += ((c) + 200041);"
"  h &= 0x3FFFFFFF;"
" }"
" return h;"
"}"
)
#+(AND :GCL (NOT :ELF))
(LISP::defentry |hashString| (LISP::string) (LISP::int "MYHASH"))
#+(AND KCL ELF)
(defun |hashString| (string) (system:|hashString| string))

#+(AND :GCL (NOT :ELF))
(LISP::clines
"int MYCOMBINE(i,j)"
"int i,j;"
"{"
"return ( (((((unsigned int)j) & 16777215) << 6)+((unsigned int)i)) % 1073741789);"
"}"
)
#+(AND :GCL (NOT :ELF))
(LISP::defentry |hashCombine| (LISP::int LISP::int) (LISP::int "MYCOMBINE"))
#+(AND :GCL  :ELF)
(defun |hashCombine| (x y) (system:|hashCombine| x y))


