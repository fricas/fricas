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


; NAME:         BootLex.lisp
; PURPOSE:      Parsing support routines for Boot and Spad code
; CONTENTS:
;
;               0. Global parameters
;               1. BOOT File Handling
;               2. BOOT Line Handling
;               3. BOOT Token Handling
;               4. BOOT Token Parsing Actions
;               5. BOOT Error Handling

(in-package "BOOT")

; *** 0. Global parameters

(defparameter Boot-Line-Stack nil       "List of lines returned from PREPARSE.")

(defun Next-Lines-Clear () (setq Boot-Line-Stack nil))

(defparameter |$MaxLineShow| 5)

(defun Next-Lines-Show ()
  (and Boot-Line-Stack (format t "First currently preparsed lines are:~%~%"))
  (let ((cnt 0))
      (mapcar #'(lambda (line)
           (if (< cnt |$MaxLineShow|)
               (progn
                    (setf cnt (+ cnt 1))
                    (format t "~&~5D> ~A~%" (car line) (cdr Line)))))
          Boot-Line-Stack)))

; *** 1. BOOT file handling

(defvar SPADERRORSTREAM)

(defun init-boot/spad-reader ()
  (setq $SPAD_ERRORS (VECTOR 0 0 0))
  (setq SPADERRORSTREAM *standard-output*)
  (setq XTokenReader #'get-BOOT-token)
  (setq Line-Handler 'next-BOOT-line)
  (setq Meta_Error_Handler 'spad_syntax_error)
  (setq File-Closed nil)
  (Next-Lines-Clear)
  (setq Boot-Line-Stack nil)
  (ioclear))

(defun |oldParserAutoloadOnceTrigger| () nil)

(defvar |$compiler_output_stream|)

(defun print-defun (name body)
    (let ((st |$compiler_output_stream|))
        (print-full body st) ))

(defvar |$MacroTable|)
(defvar |$restore_list|)

(defun |spadCompile| (spad-input-file
             &aux
           (|$comp370_apply| (function print-defun))
           (*fileactq-apply* (function print-defun))
         ;;  (|$InteractiveMode| nil)
           ($SPAD T)
           ($BOOT nil)
           (XCape #\_)
           (*EOF* NIL)
           (File-Closed NIL)
           in-stream)
  (declare (special |$comp370_apply| *fileactq-apply* *EOF*
                    in-stream File-Closed Xcape))
  ;; only rebind |$InteractiveFrame| if compiling
  (progv (if (not |$InteractiveMode|) '(|$InteractiveFrame|))
         (if (not |$InteractiveMode|)
             (list (|addBinding|
                    '|$DomainsInScope|
                    `((FLUID . |true|)
                      (|special| . ,(COPY-TREE |$InitialDomainsInScope|)))
                    (|addBinding| '|$Information| NIL (|makeInitialModemapFrame|)))))
  (init-boot/spad-reader)
  (unwind-protect
    (progn
      (setf in-stream (open spad-input-file :direction :input))
      (initialize-preparse in-stream)
      (setq CUROUTSTREAM *standard-output*)
      (setf |$MacroTable| (make-hash-table))
      (setf |$restore_list| nil)
      (loop
       (if (or *eof* file-closed) (return nil))
       (catch 'SPAD_READER
         (if (setq Boot-Line-Stack (PREPARSE in-stream))
             (let ((LINE (cdar Boot-Line-Stack)))
               (declare (special LINE))
               (|parse_new_expr|)
               (let ((parseout (|pop_stack_1|)) )
                 (when parseout
                       (|S_process| parseout)
                       (format CUROUTSTREAM "~&")))
               )))
      (IOClear)))
    (shut in-stream)
    )
  T))

;  *** 2. BOOT Line Handling ***

; See the file PREPARSE.LISP for the hard parts of BOOT line processing.

(defun next-BOOT-line (&optional (in-stream t))

  "Get next line, trimming trailing blanks and trailing comments.
One trailing blank is added to a non-blank line to ease between-line
processing for Next Token (i.e., blank takes place of return).  Returns T
if it gets a non-blank line, and NIL at end of stream."

  (if Boot-Line-Stack
      (let ((Line-Number (caar Boot-Line-Stack))
            (Line-Buffer (suffix #\Space (cdar Boot-Line-Stack))))
        (prev-line-set Current-Line)
        (pop Boot-Line-Stack)
        (Line-New-Line Line-Buffer Current-Line Line-Number)
        (setq |$currentLine| (setq LINE Line-Buffer))
        Line-Buffer)))

;  *** 3. BOOT Token Handling ***

(defparameter xcape #\_ "Escape character for Boot code.")

(defparameter *after_dot* nil)

(defun get-BOOT-token (token)

  "If you have an _, go to the next line.
If you have a . followed by an integer, get a floating point number.
Otherwise, get a .. identifier."

  (if (not (boot-skip-blanks))
      nil
      (let ((token-type (boot-token-lookahead-type (current-char))))
        (if (not (or (eq token-type '\.) (eq token-type 'num)))
            (setf *after_dot* nil))
        (case token-type
          (eof                  (token-install nil '*eof token nonblank))
          (escape               (advance-char)
                                (get-boot-identifier-token token t))
          (argument-designator  (get-argument-designator-token token))
          (id                   (get-boot-identifier-token token))
          (num                  (get-spadnum-token token))
          (string               (get-SPADSTRING-token token))
          (special-char         (get-special-token token))
          (t                    (get-gliph-token token token-type))))))

(defun boot-skip-blanks ()
  (setq nonblank t)
  (loop (let ((cc (current-char)))
          (if (not cc) (return nil))
          (if (or (char= cc #\Space) (char= cc #\Tab) (char= cc #\Return))
              (progn
                    (setq nonblank nil)
                    (if (not (advance-char))
                        (return nil)))
              (return t)))))

;;;  Gliph Table

;;; Gliphs are symbol clumps. The gliph property of a symbol gives
;;; the tree describing the tokens which begin with that symbol.
;;; The token reader uses the gliph property to determine the longest token.
;;; Thus [[:=]] is read as one token not as [[:]] followed by [[=]].

(defparameter *gliph-table* '(
    (#\| \|)
    (#\* (* (#\* |**|)))
    (#\( |(|)
    (#\+ (+  (#\- (|+-| (#\> |+->|)))))
    (#\- (-  (#\> |->|)))
    (#\< (<  (#\= |<=|) (#\< |<<|)))
    (#\\ (\\ (#\/ |\/|)))
    (#\> (>  (#\= |>=|)))
    (#\= (=  (#\= (|==| (#\> |==>|))) (#\> |=>|)))
    (#\. (\. (#\. |..|)))
    (#\^ (^  (#\= |^=|)))
    (#\~ (\~ (#\= |~=|)))
    (#\: (\: (#\= |:=|) (#\- |:-|) (#\: |::|)))
))

(defun boot-token-lookahead-type (char)
  "Predicts the kind of token to follow, based on the given initial character."
  (cond ((not char)                                        'eof)
        ((char= char #\_)                                  'escape)
        ((and (char= char #\#) (digitp (next-char)))       'argument-designator)
        ((digitp char)                                     'num)
        ((and (char= char #\$) $boot
              (alpha-char-p (next-char)))                  'id)
        ((or (char= char #\%) (char= char #\?)
             (char= char #\!) (alpha-char-p char))         'id)
        ((char= char #\")                                  'string) ;"
        ((member char
                 '(#\Space #\Tab #\Return)
                 :test #'char=)                            'white)
        ((assoc char *gliph-table*))
        (t                                                 'special-char)))

(defun get-argument-designator-token (token)
  (advance-char)
  (token-install (intern (strconc "#" (format nil "~D"
                                         (read-from-string (get-intval)))))
                 'argument-designator token nonblank))

(defvar Keywords 
   '(|add| |and| |by| |catch| |case| |else| |exit|
     |export| |exquo| |finally| |for| |fluid| |from|
     |has| |if| |import| |in| |is| |isnt| |iterate|
     |leave| |local| |mod| |not| |or| |pretend| |quo|
     |rem| |repeat| |return| |then| |try| |until|
     |where| |while| |with| |yield|
    )

"Alphabetic literal strings occurring in the New Meta code constitute
keywords.   These are recognized specifically by the AnyId production,
GET-BOOT-IDENTIFIER will recognize keywords but flag them
as keywords.")

(defun remove-escapes (s1 k)
   (let* ((n1 (length s1))
          (n2 (- n1 k))
          (i2 0)
          (normal t)
          (s2 (make-string n2)))
      (dotimes (i1 n1)
          (let ((c (aref s1 i1)))
               (if (and normal (char= c XCape))
                   (setf normal nil)
                   (progn
                        (if (and (not normal) (|idChar?| c))
                            (progn
                                 (setf (aref s2 i2) XCape)
                                 (incf i2)))
                        (setf normal t)
                        (setf (aref s2 i2) c)
                        (incf i2)))))
      s2))

(defun get-boot-identifier-token (token &optional (escaped? nil))
  "An identifier consists of an escape followed by any character, a %, ?,
or an alphabetic, followed by any number of escaped characters, digits,
or the chracters ?, !, ' or %"
  (prog (
         (sbuf nil)
         (start-pos (Current-Char-Index))
         (num-escaped 0)
         (nbuf nil)
         (nc NIL)
        )
      (advance-char)
   id (let ((cur-char (current-char)))
         (cond ((char= cur-char XCape)
                (if (not (setf nc (advance-char))) (go bye))
                (setq escaped? t)
                (if (not (|idChar?| nc))
                    (incf num-escaped))
                (if (not (advance-char)) (go bye))
                (go id))
               ((|idChar?| cur-char)
                (if (not (advance-char)) (go bye))
                (go id))))
  bye
      (if (> num-escaped 0)
          (setf nbuf (remove-escapes (Line-subseq-from start-pos)
                                      num-escaped))
          (setf nbuf (Line-subseq-from start-pos)))
#|
      (if (not (string= buf nbuf))
          (progn
              (format t "buf is ->~S<-~%" buf)
              (format t "nbuf is ->~S<-~%" nbuf)))
|#
      (setq sbuf (intern nbuf "BOOT"))
      (return (token-install
                sbuf
                (if (and (not escaped?)
                         (member sbuf Keywords :test #'eq))
                    'keyword 'identifier)
                token
                nonblank
                ))))

(defun get-gliph-token (token gliph-list)
   (prog ((ress nil)
         )
    loop
       (advance-char)
       (setf gliph-list (nth 1 gliph-list))
       (if (pairp gliph-list)
           (progn
                 (setf ress (car gliph-list))
                 (setf gliph-list (cdr gliph-list)))
           (progn
                 (setf ress gliph-list)
                 (setf gliph-list nil)))
       (setf gliph-list (assoc (current-char) gliph-list))
       (if gliph-list
           (go loop)
           (progn
                 (setf *after_dot* (eq ress '\.))
                 (return (token-install ress 'keyword token
                                         nonblank))))))

(defun make-adjustable-string (n)
  (make-array (list n) :element-type 'character :adjustable t))

(defun get-SPADSTRING-token (token)
   "With TOK=\" and ABC\" on IN-STREAM, extracts and stacks string ABC"
  (PROG ((BUF (make-adjustable-string 0)))
        (if (char/= (current-char) #\") (RETURN NIL)) ;"
        (loop
            (let ((cc (advance-char)))
                (if (null cc) ;;end of line
                    (PROGN (|sayBrightly| "Close quote inserted")
                                   (RETURN nil)))
                (if (char= cc #\") (return nil)) ;"
                (if (char= cc XCape)
                    (prog ((nc (advance-char)))
                        (if (null nc)
                            (PROGN (|sayBrightly| "Close quote inserted")
                                   (RETURN nil)))
                        (if (|idChar?| nc)
                            (SUFFIX cc BUF))
                        (SUFFIX nc BUF))
                    (SUFFIX cc BUF)))
         )
        (advance-char)
        (return (token-install (copy-seq buf) ;should make a simple string
                               'spadstring token t))))

(defun get-special-token (token)
  "Take a special character off the input stream.  We let the type name of each
special character be the atom whose print name is the character itself."
  (let* ((char (current-char))
         (as (assoc char
                    '((#\# |#|) (#\$ |$|) (#\@ |@|) #| ( |#
                      (#\) |)|) (#\, |,|) (#\' |'|) (#\{ |{|) (#\} |}|)
                      (#\/ |/|) (#\; |;|) (#\[ |[|) (#\] |]|))))
         (symbol (if as (second as) char)))
    (advance-char)
    (token-install symbol 'keyword token t)))

(defun get-intval ()
    (prog (buf
           (start-pos (Current-Char-Index))
          )
         (advance-char)
      nu (let ((cur-char (current-char)))
              (if (digitp cur-char)
                  (progn
                       (advance-char)
                       (go nu))))
         (setf buf (Line-subseq-from start-pos))
         (return buf))
)

(defun char-eq (x y) (char= (character x) (character y)))

(defun get-spadnum-token (token)
  "Take a number off the input stream."
  (prog (buf cur-char
          intval (fracval 0) (fraclen 0) (expval 0)
        )
        (setf intval (read-from-string (get-intval)))
        (setf cur-char (current-char))
        (cond
              (*after_dot*
                   (go formint))
              ((char-eq cur-char #\.)
                   (go fracpart))
              ((char-eq (upcase cur-char) #\E)
                   (setf fracval 0)
                   (setf fraclen 0)
                   (go exppart))
              (t
                     (go formint))
        )
    fracpart
        (if (not (digitp (next-char)))
            (go formint))
        (advance-char)
        (setf buf (get-intval))
        (setf fracval (read-from-string buf))
        (setf fraclen (size buf))
        (setf cur-char (current-char))
        (cond
              ((char-eq (upcase cur-char) #\E)
                   (go exppart))
              (t
                   (go formfloat))
        )
    exppart
        (if (not
                (or
                    (digitp (next-char))
                    (char-eq (next-char) #\-)
                    (char-eq (next-char) #\+)))
            (go formfloat))
        (advance-char)
        (setf expval (read-from-string (get-intval)))
    formfloat
        (setf *after_dot* nil)
        (return (token-install
                (|make_float| intval fracval fraclen expval)
                'spadfloat token
                 t))
 formint
        (setf *after_dot* nil)
        (return (token-install
                  intval
                  'number token
                  t
                  ))))

; **** 4. BOOT token parsing actions

; Parsing of operator tokens depends on tables initialized by BOTTOMUP.LISP

(defun |parse_AKEYWORD|()
    (AND (MEMBER (|current_symbol|) KEYWORDS)
         (|parse_KEYWORD|)))

(defun TRANSLABEL (X AL) (TRANSLABEL1 X AL) X)

(defun TRANSLABEL1 (X AL)
 "Transforms X according to AL = ((<label> . Sexpr) ..)."
  (COND ((REFVECP X)
         (do ((i 0 (1+ i))
              (k (maxindex x)))
             ((> i k))
           (if (LET ((Y (LASSOC (ELT X I) AL))) (SETELT X I Y))
               (TRANSLABEL1 (ELT X I) AL))))
        ((ATOM X) NIL)
        ((LET ((Y (LASSOC (FIRST X) AL)))
           (if Y (setf (FIRST X) Y) (TRANSLABEL1 (CDR X) AL))))
        ((TRANSLABEL1 (FIRST X) AL) (TRANSLABEL1 (CDR X) AL))))

; **** 5. BOOT Error Handling

(defparameter DEBUGMODE 'YES "Can be either YES or NO")

(defun SPAD_SYNTAX_ERROR (&rest byebye)
  "Print syntax error indication, underline character, scrub line."
  (BUMPERRORCOUNT '|syntax|)
  (COND ((EQ DEBUGMODE 'YES)
           (SPAD_LONG_ERROR))
        ((SPAD_SHORT_ERROR)))
  (IOClear)
  (setf |$reduction_stack| nil)
  (throw 'spad_reader nil))

(defun SPAD_LONG_ERROR ()
  (SPAD_ERROR_LOC SPADERRORSTREAM)
  (iostat)
  (unless (EQUAL OUT-STREAM SPADERRORSTREAM)
    (SPAD_ERROR_LOC OUT-STREAM)
    (TERPRI OUT-STREAM)))

(defun SPAD_SHORT_ERROR () (current-line-show))

(defun SPAD_ERROR_LOC (STR)
  (format str "******** Boot Syntax Error detected ********"))

(defun BUMPERRORCOUNT (KIND)
  (unless |$InteractiveMode|
          (LET ((INDEX (case KIND
                         (|syntax| 0)
                         (|precompilation| 1)
                         (|semantic| 2)
                         (T (ERROR "BUMPERRORCOUNT")))))
            (SETELT $SPAD_ERRORS INDEX (1+ (ELT $SPAD_ERRORS INDEX))))))
