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
; PURPOSE: This file sets up properties which are used by the Boot lexical
;          analyzer for bottom-up recognition of operators.  Also certain
;          other character-class definitions are included, as well as
;          table accessing functions.
;
; ORGANIZATION: Each section is organized in terms of Creation and Access code.
;
;               1. Led and Nud Tables
;               2. GLIPH  Table
;               3. RENAMETOK Table
;               4. GENERIC Table
;               5. Character syntax class predicates 
; **** 1. LED and NUD Tables
 
; ** TABLE PURPOSE
 
; Led and Nud have to do with operators. An operator with a Led property takes
; an operand on its left (infix/suffix operator).
 
; An operator with a Nud takes no operand on its left (prefix/nilfix).
; Some have both (e.g. - ).  This terminology is from the Pratt parser.
; The translator for Scratchpad II is a modification of the Pratt parser which
; branches to special handlers when it is most convenient and practical to
; do so (Pratt's scheme cannot handle local contexts very easily).
 
; Both LEDs and NUDs have right and left binding powers.  This is meaningful 
; for prefix and infix operators.  These powers are stored as the values of 
; the LED and NUD properties of an atom, if the atom has such a property. 
; The format is:
 
;       <Operator Left-Binding-Power  Right-Binding-Power <Special-Handler>>
 
; where the Special-Handler is the name of a function to be evaluated when that
; keyword is encountered.
 
; The default values of Left and Right Binding-Power are NIL.  NIL is a 
; legitimate value signifying no precedence.  If the Special-Handler is NIL,
; this is just an ordinary operator (as opposed to a surfix operator like 
; if-then-else).
 
; ** TABLE CREATION
 
(defun MAKENEWOP (X Y) (MAKEOP X Y))
 
(defun MAKEOP (X Y)
  (if (OR (NOT (CDR X)) (NUMBERP (SECOND X)))
      (SETQ X (CONS (FIRST X) X)))
  (MAKEPROP (FIRST X) Y X)
  (SECOND X))
 
(mapcar #'(LAMBDA(J) (MAKENEWOP J '|Led|))
        '((* 800 801)   (|rem| 800 801)   (|mod| 800 801)
          (|quo| 800 801)   (|div| 800 801)
          (/ 800 801)    (** 900 901)  (^ 900 901)
          (|exquo| 800 801) (+ 700 701)
          (\- 700 701)    (\-\> 1001 1002)  (\<\- 1001 1002)
          (\: 996 997)    (\:\: 996 997)
          (\@ 996 997)    (|pretend| 995 996)
          (\.)            (\! \! 1002 1001)
          (\, 110 111)
          (\; 81 82 (|PARSE-SemiColon|))
          (\< 400 400)    (\> 400 400)
          (\<\< 400 400)  (\>\> 400 400)
          (\<= 400 400)   (\>= 400 400)
          (= 400 400)     (^= 400 400)
          (\~= 400 400)
          (|in| 400 400)    (|case| 400 400)
          (|add| 400 120)   (|with| 2000 400 (|PARSE-InfixWith|))
          (|has| 400 400)
          (|where| 121 104)     ; must be 121 for SPAD, 126 for boot--> nboot
          (|when| 112 190)
          (|otherwise| 119 190 (|PARSE-Suffix|))
          (|is| 400 400)    (|isnt| 400 400)
          (|and| 250 251)   (|or| 200 201)
          (/\\ 250 251)   (\\/ 200 201)
          (\.\. SEGMENT 401 699 (|PARSE-Seg|))
          (=\> 123 103)
          (+-\> 998 112)
          (== DEF 122 121)
          (==\> MDEF 122 121)
          (\| 108 111)                          ;was 190 190
          #|(\:- LETD 125 124)|# (\:= LET 125 124)))
 
(mapcar #'(LAMBDA (J) (MAKENEWOP J `|Nud|))
        '((|for| 130 350 (|PARSE-Loop|))
          (|while| 130 190 (|PARSE-Loop|))
          (|until| 130 190 (|PARSE-Loop|))
          (|repeat| 130 190 (|PARSE-Loop|))
          (|import| 120 0 (|PARSE-Import|) )
          (|unless|)
          (|add| 900 120)
          (|with| 1000 300 (|PARSE-With|))
          (|has| 400 400)
          (\- 701 700)  ; right-prec. wants to be -1 + left-prec
;;        (\+ 701 700)
          (\# 999 998)
          (\! 1002 1001)
          (\' 999 999 (|PARSE-Data|))
          (\<\< 122 120 (|PARSE-LabelExpr|))
          (\>\>)
;;          (^ 260 259 NIL) ;; using ^ as negation is confusing
          (\-\> 1001 1002)
          (\: 194 195)
          (|not| 260 259 NIL)
          (\~ 260 259 nil)
          (\= 400 700)
          (|return| 202 201 (|PARSE-Return|))
          (|leave| 202 201 (|PARSE-Leave|))
          (|exit| 202 201 (|PARSE-Exit|))
          (|from|)
          (|iterate|)
          (|yield|)
          (|if| 130 0 (|PARSE-Conditional|))    ; was 130
          (\| 0 190)
          (|suchthat|)
          (|then| 0 114)
          (|else| 0 114)))
