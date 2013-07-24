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

; 1C. Token

; FUNCTIONS DEFINED IN THIS SECTION:
;
;       Make-Token, Token-Symbol, Token-Type, Token-Install, Token-Print

(defstruct Token
  "A token is a Symbol with a Type.
The type is either NUMBER, IDENTIFIER or SPECIAL-CHAR.
NonBlank is true if the token is not preceded by a blank."
  (Symbol nil)
  (Type nil)
  (NonBlank t)
  (Line_num 0)
  (Char_num 0)
)

(defparameter Prior-Token (make-token) "What did I see last")
(defparameter Current-Token (make-token) "Token at head of input stream.")
(defparameter Next-Token (make-token)    "Next token in input stream.")
(defparameter Valid-Tokens 0               "Number of tokens in buffer (0, 1 or 2)")

(defun |token_install| (symbol type nonblank lpos cpos token)
  (setf (token-symbol token) symbol)
  (setf (token-type token) type)
  (setf (token-nonblank token) nonblank)
  (setf (token-Line_num token) lpos)
  (setf (token-Char_num token) cpos)
  token)

(defun Token-Print (token)
  (format t "(token (symbol ~S) (type ~S))~%"
          (Token-Symbol token) (Token-Type token)))

; 1D. A Reduction
;

; 2. Recursive descent parsing support routines (semantically related to MetaLanguage)
;
; This section of the code contains:
;
;               A. Routines for stacking and retrieving reductions of rules.
;               B. Routines for applying certain metagrammatical elements
;                  of a production (e.g., Star).
;               C. Token-level parsing utilities (keywords, strings, identifiers).


; 3A.  Manipulating the token stack and reading tokens

; This section is broken up into 3 levels:
;
;       (1) Token handling:     Current Token, Next Token, Advance Token
;       (2) Character handling: Current Char, Next Char, Advance Char
;       (3) Line handling:      Next Line, Print Next Line
;       (X) Random Stuff


; 3A (1) Token Handling.

; Tokens are acquired from a stream of characters.  Lexical analysis is performed
; by the functiond Get Token.  One-token lookahead is maintained in variables
; Current-Token and Next-Token by procedures Current Token, Next Token, and
; Advance Token.  The functions Match Current Token and Match Next Token recognize
; classes of tokens, by type, or by type and symbol.  The current and next tokens
; can be shoved back on the input stream (to the current line) with Unget-Tokens.

(defun token-stack-show ()
  (if (= Valid-Tokens 0) (format t "~%There are no valid tokens.~%")
      (format t "~%The number of valid tokens is ~S.~%" Valid-Tokens))
 (if (token-type prior-token)
      (progn (format t "The prior token was ~S~%" prior-token)
             ))
  (if (> Valid-Tokens 0)
      (progn (format t "The current token is ~S~%" current-token)
             ))
  (if (> Valid-Tokens 1)
      (progn (format t "The next token is ~S~%" next-token)
             ))
)

(defun token-stack-clear ()
   (progn (setq Valid-Tokens 0)
          (|token_install| nil nil nil 0 0 current-token)
          (|token_install| nil nil nil 0 0 next-token)
          (|token_install| nil nil nil 0 0 prior-token)))

; *** Match Token

(defun match-token (token type symbol)
  (if (and token (eq (token-type token) type))
      (if symbol (if (eq symbol (token-symbol token)) token) token)))

(defun |match_current_token|(type symbol)
  "Returns the current token if it has EQ type and (optionally) equal symbol."
  (match-token (|current_token|) type symbol))

(defun |match_next_token|(type symbol)
  "Returns the next token if it has equal type and (optionally) equal symbol."
  (match-token (|next_token|) type symbol))

; *** Current Token, Next Token, Advance Token

(defun try-get-token (token)
  (let ((tok (|ntokreader| token)))
    (if tok (progn (setf Valid-Tokens (|inc_SI| Valid-Tokens)) token))))

(defun |current_symbol|() (make-symbol-of (|current_token|)))

(defun |next_symbol|() (make-symbol-of (|next_token|)))

(defun make-symbol-of (token)
  (let ((u (and token (token-symbol token))))
    (cond ((not u) nil)
          (u))))

(defun |current_token|()
  "Returns the current token getting a new one if necessary."
  (if (|greater_SI| Valid-Tokens 0)
      Current-Token
      (try-get-token Current-Token)))

(defun |next_token| ()
  "Returns the token after the current token, or NIL if there is none after."
  (|current_token|)
  (if (|greater_SI| Valid-Tokens 1)
      Next-Token
      (progn
          (try-get-token Next-Token))))

(defun |advance_token|()
  "Makes the next token be the current token."
  (case Valid-Tokens
    (0 (try-get-token Current-Token))
    (1 (setf  Valid-Tokens (|dec_SI| Valid-Tokens))
       (setq Prior-Token (copy-token Current-Token))
       (try-get-token Current-Token))
    (2 (setq Prior-Token (copy-token Current-Token))
       (setq Current-Token (copy-token Next-Token))
       (setf Valid-Tokens (|dec_SI| Valid-Tokens)))))
