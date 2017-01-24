; This file does a number of things.
; 1) generate-init
;    It generates initial init_X.ap files for domains X that will be
;    extended during the build of libaxiom.al. These domains are listed
;    in init-domains.
; 2) generate-deps
;    It generates dependencies for domains from the Axiom algebra library.
; 3) generate-deps-from-ap
;    It generates dependencies from the .ap files of a number of aldor
;    domains.

; The following list contains domains that initially are build with
; a dummy definition (see initlist.as for the origin).
; We filter here out everything that is not an export declaration.
; The file baslist.ap approximately looks like
;(|Sequence|
;  (|Declare| |Category| (|With| () ()))
;  (|Declare| |Type| (|With| () ()))
;  (|Declare| |Tuple| (|With| () ()))
;  (|Declare| |->| (|With| () ()))
;  (|Declare| |Field| |Category|)
;  ...
;  (|Export| (|Declare| |Boolean| (|With| () ())) () ())
;  (|Export| (|Declare| |Field| (|With| () ())) () ())
;  ...
;)

(defun init-forms (filename)
  (let ((ilist (cdr (with-open-file (str (pathname filename))
                      (read str)))))
    (mapcan #'(lambda (x) (and (eq (car x) '|Export|) (list x))) ilist)))

; *initforms* lists all the export forms of "initlist.ap"
(setq *initforms* (init-forms "initlist.ap"))

(defun type-name (initform)
  (cadr (cadr initform)))

; That is an Axiom function that sets the variable |$extendedDomains|.
; |$extendedDomains| have relevance during the call to
; |makeAxExportForm|.
; The execution of this function is necessary to make loading the code
; for |makeAxExportForm|.
; Boolean is special. It counts as a 'extended domain' but its definition
; is in boolean0.as and it will be available for the compilation of
; any file.
(|setExtendedDomains| (cons '|Boolean| (mapcar 'type-name *initforms*)))
; The following variable specifies whether or not the form originates
; from an Aldor source file. In particular for axlit.as and axextend.as
; we have to treat some tags differently. For example, an |Extend|
; should require full information of the extended domain.
(setq *extends-axiom-domains* nil)

; We introduce some debugging information.
(defun debug-print (x y)
  (format t "~a: ~a~%" x y))

(debug-print "|$extendedDomains|" |$extendedDomains|)


; Let us call a type constructor 'easy' if all its arguments are only
; required to be 'Type'. Univariate examples are, (approximately)
; those types that are listed via
;; grep '^[A-Z][A-Za-z]*([^:]*: *Type)' ${abs_top_builddir}/src/algebra/*.spad
;;
; Among them are List, VectorCategory, and CoercibleTo.
; We need a list of such types since if some type T in find-deps
; appears in an apply context (|Apply| E) with the function E being an
; easy type then we do not need full information from T.
(defun generate-easy (filename)
  (let ((fname (pathname filename))
        (saved-extended-domains |$extendedDomains|))
    (with-open-file (str fname :direction :output)
      ; We don't have to bother with initforms here!!!
      (|setExtendedDomains| nil)
      (print (append (mapcan 'easy-type (all-constructors))
                     '(|Tuple| |->|)) ; special "easy" constructors
             str))))


(defun not-defaultpackage-name (x)
  (unless (|isDefaultPackageName| x) (list x)))
(defun all-constructors ()
  (mapcan 'not-defaultpackage-name (|allConstructors|)))
(defun easy-type (constructor)
  (test-easy (|makeAxExportForm| "UnusedArgument" (list constructor))))

; Usually the apform looks like (|Sequence| ...). Relevant in such a
; sequence are only the subforms that start with |Define|, |Export|,
; and |Extend|.
(defun test-easy (apform)
  (if (atom apform) nil
    (cond ((eq (car apform) '|Sequence|) (mapcan 'test-easy (cdr apform)))
          ((eq (car apform) '|Extend|) (test-easy (cadr apform)))
          ((member (car apform) '(|Define| |Export|))
             (test-easy1 (cadr apform)))
          (t nil))))

; Now we have to check whether the form looks like
; (|Declare| |SomeName| (|Apply| -> (|Declare| |#1| |Type|) ...))
; If it is of this form, then (list |SomeName|) is returned.
(defun test-easy1 (apform)
  (cond ((atom apform) nil)
        ((not (eq (car apform) '|Declare|)) nil)
        ((test-easy2 (caddr apform)) (list (cadr apform)))
        (t nil)))

; Check whether the apform looks like
; (|Apply| -> (|Declare| |#1| |Type|) ..)
; or
; (|Apply| -> (|Comma| (|Declare| |#1| |Type|) ..) ..)
; where all elements in the (|Comma| ..) look like (|Declare| |#1| |Type|).
(defun test-easy2 (apform)
  (cond ((not (match apform '(|Apply| -> . ?))) nil)
    ((match (caddr apform) '(|Comma| . ?))
     (all (mapcar 'test-easy3 (cdr (caddr apform)))))
    (t (test-easy3 (car (cddr apform))))))

; Check whether the apform looks like
; (|Declare| |#1| |Type|)
(defun test-easy3 (apform)
  (match apform '(|Declare| ? |Type|)))

; Check whether form e1 matches form e2. e2 can contain '? which counts
; as a wildcard, i.e., matches everything.
(defun match (e1 e2)
  (cond ((wild-card? e2) t)
    ((atom e1) (equal e1 e2))
    ((listp e1) (and (listp e2)
             (match (car e1) (car e2))
             (match (cdr e1) (cdr e2))))
    (t nil)))

; A question mark counts as a wildcard in a "match" call.
(defun wild-card? (x) (eq x '?))

; return true if all elements in list l are true
(defun all (l)
  (cond ((null l) t)
    ((not (car l)) nil)
    (t (all (cdr l)))))



;;
;; Creating the base file set
;;

(defun generate-init (lstfilename)
  (generate-init-list lstfilename)
  (generate-init-ap)
  (generate-init-deps))

(defun generate-init-list (dest)
  ; actually for init files it doesn't make any difference whether
  ; *aldor-source* is set to t or nil.
  (setq *extends-axiom-domains* nil)
  ;; list of file names (abbreviated type names) into the file 'dest'.
  (with-open-file (bstr (pathname dest) :direction :output)
    (dolist (initform *initforms*)
      (format bstr "init_~a~%" (|abbreviate| (type-name initform))))))

; Generate .ap files for all the exports from initlist.ap.
(defun generate-init-ap ()
  (dolist (initform *initforms*)
    (let* ((abbrev (|abbreviate| (type-name initform)))
           (filename (pathname (format nil "init_ap/init_~a.ap" abbrev))))
      (with-open-file (apstream filename :direction :output)
        (prin1 initform apstream)))))

;; Create init dependency files
(defun generate-init-deps ()
  (dolist (initform *initforms*)
    (let ((abbrev (|abbreviate| (type-name initform))))
      (print-dependencies initform (format nil "init_~a" abbrev)))))





;; Generate .ap files from Axiom types.
(defun generate-ax-file (name content inits)
  (let* ((*print-circle* nil)
         (filename (pathname (format nil "ap/~a.ap" name)))
         (constructors (mapcar '|abbreviation?| content))
         (apform (|makeAxExportForm| "UnusedArgument" constructors)))
  (with-open-file (str filename :direction :output)
    (pprint (append apform (mapcar 'load-init inits)) str))))

(defun load-init (initname)
  (with-open-file (str (pathname (format nil "init_ap/~a.ap" initname)))
    (read str)))

;; .dep from SPAD constructor
(defun generate-deps (namelist)
  (setq *extends-axiom-domains* nil)
  (dolist (name namelist)
    (let* ((constructor (|abbreviation?| name))
           (apform (|makeAxExportForm| "UnusedArgument" (list constructor))))
      (debug-print "generate-deps" constructor)
      (print-dependencies apform name))))

;; .dep from .ap file (corresponds to .as sources)
; Actually, it would suffice to set *extends-axiom-domains* to t only for
; the files axlit.as and axextend.as since all the other .as files
; will be compiled before any Axiom related stuff anyway.
(defun generate-deps-from-ap-files (namelist)
  (setq *extends-axiom-domains* t)
;  (trace find-deps)
;  (trace find-deps-with)
;  (trace find-deps-apply)
;  (trace find-deps-apply-list)
;  (trace find-deps-apply-item)
;  (trace find-arg-names)
;  (trace full-or-init)
;  (trace dep-type-or-nil)
;  (trace could-be-init)
  (dolist (name namelist)
    (with-open-file (instr (pathname (format nil "ap/~a.ap" name)))
      (let ((apform (read instr)))
        (print-dependencies apform name)))))



; Find and print dependencies for 'apform' to a file with name 'name'.
(defun print-dependencies (apform name)
  (let ((filename (pathname (format nil "gendeps/~a.dep" name))))
    ; *easytypes* is the list of all type constructors whose argument
    ; types are all of type 'Type'.
    (setq *easytypes*
          (with-open-file (str (pathname "easylist.lsp")) (read str)))
    (with-open-file (str filename :direction :output)
      (debug-print "print-dependencies (name)" name)
      (debug-print "print-dependencies (apform)" apform)
      (dolist (dep (find-deps (make-null-env) apform))
        (format str "~a~%" dep)))))








; The following code walks through the .ap form of aldor code and
; returns a list of dependencies. It basically looks for any Axiom
; type and tries to decide according the list of nodes to it (its
; path) whether full information is needed or whether initial
; information in the form of "D: with == add" would be sufficient. If
; a type T is tagged with "init" then (later in the build process of
; libaxiom.al) initial information will be taken only if a file
; init_T.ap is actually available. If no such file is available,
; "init" will be ignored and "full" information is taken.

; http://groups.google.com/group/fricas-devel/browse_thread/thread/fcdc842baaab7e5c/ab36c1403e87c226?lnk=st&q=#ab36c1403e87c226

; So we need to find out when some type is "used in an export" in
; contrast to when it is "used in a type expression".

; The function find-deps will thus return a list of dependencies where
; some of the dependencies will be of the form (|init| D) which
; signals that D is only "used in an export" and thus initial information
; for it would be sufficient.

; Note: find-deps might also signal an |init| for categories.
; Although that makes no sense, it also does not hurt, since we will
; not have categories in initlist.as and thus always full information
; is replaced later anyway.
(defun find-deps (env stmt)
  (if (atom stmt) (dep-type-or-nil env stmt)
    (let* ((tag (car stmt))
           (args (cdr stmt))
           (e (env-add-path env tag)))
      (cond
        ((eq tag '|Add|)        (find-deps-add      e args))
        ((eq tag '|And|)        (find-deps-and      e args))
        ((eq tag '|Apply|)      (find-deps-apply    e args))
        ((eq tag '|Assign|)     (find-deps-assign   e args))
        ((eq tag '|Builtin|)    (find-deps-builtin  e args))
        ((eq tag '|CoerceTo|)   (find-deps-coerce   e args))
        ((eq tag '|Comma|)      (find-deps-comma    e args))
        ((eq tag '|Declare|)    (find-deps-declare  e args))
        ((eq tag '|Default|)    (find-deps-default  e args))
        ((eq tag '|Define|)     (find-deps-define   e args))
        ((eq tag '|Exit|)       (find-deps-exit     e args))
        ((eq tag '|Export|)     (find-deps-export   e args))
        ((eq tag '|Extend|)     (find-deps-extend   e args))
        ((eq tag '|For|)        (find-deps-for      e args))
        ((eq tag '|Foreign|)    (find-deps-foreign  e args))
        ((eq tag '|Generate|)   (find-deps-generate e args))
        ((eq tag '|Has|)        (find-deps-has      e args))
        ((eq tag '|If|)         (find-deps-if       e args))
        ((eq tag '|Import|)     (find-deps-import   e args))
        ((eq tag '|Inline|)     (find-deps-inline   e args))
        ((eq tag '|Label|)      (find-deps-label    e args))
        ((eq tag '|Lambda|)     (find-deps-lambda   e args))
        ((eq tag '|LitInteger|) (find-deps-literal  e args))
        ((eq tag '|LitString|)  (find-deps-literal  e args))
        ((eq tag '|Local|)      (find-deps-local    e args))
        ((eq tag '|Not|)        (find-deps-not      e args))
        ((eq tag '|Or|)         (find-deps-or       e args))
        ((eq tag '|PretendTo|)  (find-deps-pretend  e args))
        ((eq tag '|Qualify|)    (find-deps-qualify  e args))
        ((eq tag '|Repeat|)     (find-deps-repeat   e args))
        ((eq tag '|RestrictTo|) (find-deps-restrict e args))
        ((eq tag '|Sequence|)   (find-deps-sequence e args))
        ((eq tag '|Test|)       (find-deps-test     e args))
        ((eq tag '|With|)       (find-deps-with     e args))
        ((eq tag '|While|)      (find-deps-while    e args))
        ((eq tag '|Yield|)      (find-deps-yield    e args))
        (t                      (format t "Unknown form ~a~%" stmt)
                                (throw '|UndefinedTag| stmt))))))

; (Add A B)
;   syntax: A add B
;   A == a domain
;   B == the add-body
(defun find-deps-add (env args)
  (if *extends-axiom-domains*
      (map-find-deps env args)
    nil))

; (And A B)
;   syntax: A and B
;   A == some expression
;   B == some expression
(defun find-deps-and (env args)
  (map-find-deps env args))

; (Apply A B1 B2 ... Bn)
;   syntax: A(B1, B2, ..., Bn)
;     for special operators it could also be
;     B1 A B2, for example B1 -> B2 or B1 + B2.
;   A == an operator
;   Bi == ?
; We need full information for the Bi if A is different
; from |->|. We will find out about the actual function since the path
; will contain its name.
(defun find-deps-apply (env args)
  (let* ((fun (car args))
         (e (env-add-path-apply-fun env fun)))
    (find-deps-apply-list e args)))

; Now we check the arguments of |Apply|.
; We must make sure that declared argument names are not considered
; as types later. So we first look for them.
(defun find-deps-apply-list (env args)
  (if (null args) nil
    (let ((deps (find-deps-apply-item env (car args))))
      (if (null (cdr args)) deps
           (let* ((vars (find-arg-names (car args)))
                  (e (env-add-vars env vars)))
             (append deps (find-deps-apply-list e (cdr args))))))))

(defun find-deps-apply-item (env args)
  (if (atom args)
      (dep-type-or-nil env args)
    (find-deps env args)))

; (Assign I V)
;   syntax: I := V
;   I == an idenitfier or a declaration
;   V == an expression
(defun find-deps-assign (env args)
  (map-find-deps env args))

; (Builtin D)
;   syntax: import D from Builtin
;   D == a declaration or sequence of declarations
(defun find-deps-builtin (env args)
  nil)

; (Comma A B ...)
;   syntax: A, B, ...
;   represents a parameter list
;   A == ? anything
;   B == ? anything
(defun find-deps-comma (env args)
  ; We can treat a comma list like the argument list of |Apply|.
  (find-deps-apply-list env args))

; (CoerceTo A B)
;   syntax: A :: B;
;   A == an element
;   B == a domain
(defun find-deps-coerce (env args)
  (map-find-deps env args))

; (Declare N T)
;   syntax: N: T
;   N == an identifier or a |Declare| statement
;   T == its declared type
(defun find-deps-declare (env args)
  (find-deps (env-add-var env (car args)) (cadr args)))

; (Default B)
;   syntax: default B
;   B == an add-body
;        |Define|
(defun find-deps-default (env args)
  (if *extends-axiom-domains*
      (map-find-deps env args)
    nil))

; (Define N V)
;   syntax: N == V
;   N == an identifier or declaration
;   V == a value
(defun find-deps-define (env args)
  (map-find-deps env args))

; (Exit T V)
;   syntax: T => V
;   T == a boolean expression
;   V == an expression that is executed if T is true
(defun find-deps-exit (env args)
  (map-find-deps env args))

; (Export A B C)
;   syntax: export ? ? ?
;   A == ?
;   B == ?
;   C == ?
; We only need the A part.
(defun find-deps-export (env args)
  (find-deps env (car args)))

; (Extend X)
;   syntax: ?
;     (found for example in PI.ap)
;   X == ?
(defun find-deps-extend (env args)
  ; Here we assume that list looks like
  ; ((Define (Declare var ...))
  (let* ((def-stmt (car args))
         (decl-stmt (cadr def-stmt))
         (var (cadr decl-stmt)))
    ; Note that var appears in context |Extend|, see dep-type-or-nil.
    (append (find-deps env var) (find-deps env (car args)))))

; (For D G S)
;   syntax: for D in G | S
;   D == an identifier
;   G == a generator
;   S == a such that expression
(defun find-deps-for (env args)
  (map-find-deps env args))

; (Foreign D L)
;   syntax: ?
;   D == |Declare|
;   L == language
; rhx: doesn't seem to need it for the dependency computation
(defun find-deps-foreign (env args)
  nil)

; (Generate A B)
;   syntax: generate B
;   A == ?
;   B == a block containing a |Yield|
; rhx: doesn't seem to need it for the dependency computation
(defun find-deps-generate (env args)
  (if (null (car args))
      (find-deps env (cadr args))
    (progn (format t "Unknown tag in |Generate| ~a~%" args)
           (throw '|UndefinedTagInGenerate| args))))

; (Has D C)
;   syntax: A has B
;   D == a domain
;   C == a category
(defun find-deps-has (env args)
  (map-find-deps env args))

; (If B T E)
;   syntax: if B then T else E
;   B == (Test X) where X is a boolean expression
;   T == statements
;   E == statements
(defun find-deps-if (env args)
  (map-find-deps env args))

; (Import L D)
;   syntax: import L from D
;   L == ? list of functions (a |With| clause or NIL)
;   D == a domain or library
(defun find-deps-import (env args)
  (find-deps env (cadr args))) ; we only look at D

; (Inline L D)
;   syntax: inline L from D
;   L == ? list of functions (a |With| clause or NIL)
;   D == a domain or library
(defun find-deps-inline (env args)
  (find-deps env (cadr args))) ; we only look at D

; (Label N V)
;   syntax: V
;     but in a context of 'N == V'
;   N == an identifier
;   V == a value
(defun find-deps-label (env args)
  (find-deps env (cadr args)))

; (Lambda P R B)
;   syntax: P: R +-> B
;   P == a parameter list
;   R == result type
;   B == function body
(defun find-deps-lambda (env args)
  ; We can treat the arguments of a |Lambda| like the arguments of |Apply|.
  (find-deps-apply-list env args))

; (LitInteger "...")
;   syntax: 5
; (LitString "...")
;   syntax: "..."
; rhx: don't seem to need it for the dependency computation
; In fact, src/interp/ax.boot additionally adds
;   import {integer: Literal -> %} from PositiveInteger
; if it finds a integer-like literal in |makeAxExportForm|.
; So we will find a dependency on Literal anyway.
(defun find-deps-literal (env args)
  nil)

; (Local D)
;   syntax: local X
;   D == an identifier or declaration
(defun find-deps-local (env args)
  (find-deps env (cadr args)))

; (Not A)
;   syntax: not A
;   A == some expression
(defun find-deps-not (env args)
  (find-deps env (car args)))

; (Or A B)
;   syntax: A or B
;   A == some expression
;   B == some expression
(defun find-deps-or (env args)
  (map-find-deps env args))

; (PretendTo A B)
;   syntax: A pretend B
;   A == some domain
;   B == some category
(defun find-deps-pretend (env args)
  (map-find-deps env args))

; (Qualify A B)
;   syntax: A $ B
;   A == an identifier
;   B == a domain
(defun find-deps-qualify (env args)
  (map-find-deps env args))

; (Repeat B I)
;   syntax: I repeat B
;   I == some iterator
;   B == some expression
(defun find-deps-repeat (env args)
  (map-find-deps env args))

; (RestrictTo A B)
;   syntax: A @ B
;   A == some domain
;   B == some category
(defun find-deps-restrict (env args)
  (map-find-deps env args))

; (Sequence A B ...)
;   syntax: A; B; ...
;   represents a list of statements
;   A == any statement
;   B == any statement
(defun find-deps-sequence (env args)
  (map-find-deps env args))

; (Test X)
;   syntax: ?
(defun find-deps-test (env args)
  (find-deps env (car args)))

; (With C E)
;   syntax: C with E
;   C == a category or |Join|
;   E == a declaration or sequence of declarations
; Note that |makeAxExportForm| puts everything into the E part, while
; Aldor compiling to .ap puts named categories into a join in the C part
; and explicit declarations into E.
(defun find-deps-with (env args)
  (map-find-deps env args))

; (While C)
;   syntax: while C
;   C == a boolean condition
(defun find-deps-while (env args)
  (map-find-deps env args))

; (Yield E)
;   syntax: yield E
;   E == an expression
(defun find-deps-yield (env args)
  (find-deps env (car args)))

;------------------------------------------------------------------
; Now we check the last bit. The argument 'item' is supposed to be an atom.
(defun dep-type-or-nil (env item)
  (cond ((null item) nil)
        ((env-contains-variable env item) nil) ; remove variable names
        ; Need special treatment for a SPAD builtin type.
        ((eq item '|SubsetCategory|)
         '(|subsetc|))
        ; Since |isNameOfType| returns false for attributes, we must
        ; make attrib.as available for the compilation of every file.
        ((is-axiom-type-name item)
         (let ((type (full-or-init (env-path env) item)))
           (format t "Adding ~a ~a ~a~%" type item (env-path env))
           ; Note that every file depends on Boolean. We will have
           ; boolean0.as compiled into any library anyway, so we don't
           ; need to state an explicit dependency here and thus return NIL.
           (if (eq type '|init_BOOLEAN|) nil
             (list type))))
        (t nil)))

; Since |isNameOfType| returns false for attributes, we must
; make attrib.as available for the compilation of every file.
(defun is-axiom-type-name (item)
  ; Certain types are built into aldor. They will anyway be available
  ; through the base of libaxiom.al, see aldor_basics in Makefile.in and
  ; in particular the file lang.as.
  ; Maybe we have to reconsider later since Exit and Tuple have different
  ; types in axllib and Axiom. So there should perhaps be some
  ; 'init_Exit' and 'init_Tuple' in the directory 'init_ap'.
  ;

  (cond ((member item '(|Exit| |Tuple| |Type|)) nil)
        ((|isNameOfType| item) item)
        (t nil)))

(defun full-or-init (path item)
  (full-or-init-internal path item item))
; The third argument 'item-or-nil' is usually equal to the second
; 'item'. It is only used to reliably check that something is used in
; an |Apply| context and not the function name corresponding to this
; |Apply|. 'item-or-nil' is set to nil if we have already found out
; that 'item' is the function name of an |Apply| and we must look
; earlier in the path. In that way, we find out correctly that in
; List(List(Integer)) full information for List is needed even if this
; whole expression appears in a |Declare| context.
(defun full-or-init-internal (path item item-or-nil)
  (cond ((null path)
         (|abbreviate| item))

        ; "import from Boolean" is generated into every exportform
        ; by |makeAxExportForm|, but for compilation of the API,
        ; an init is enough.
        ((and (eq item '|Boolean|) (eq (car path) '|Import|))
         (could-be-init item))

        ((member (car path)
                 '(|Add| |And| |Assign| |Comma| |CoerceTo| |Define|
                   |For| |If| |Import| |Inline| |Label| |Lambda|
                   |Not| |Or| |PretendTo| |Qualify| |Repeat|
                   |RestrictTo| |Sequence| |Test| |While|))
         (full-or-init-internal (cdr path) item item-or-nil))

        ; If a type appears in (|Apply| ->) context, then we only need
        ; initial information. Note that forms coming from .as files
        ; will have |->| instead of just ->.
        ; In fact, we only need initial information for any argument of
        ; an "easy" type (see easy *easytypes*).
        ; If the context is (|Apply| D) and item=D then we check the
        ; parent context, but for a different value of item in order
        ; to correctly find the need of full information for List in
        ; a situation like (|Apply| |List| (|Apply| |List| X)).
        ; For example in XPBWPOLY appears
        ; (|Apply| |Module| (|Apply| |Fraction| |Integer|))
        ; In that case full information of |Fraction| is needed.
        ((not (atom (car path))); Then it is an (|Apply| fun) form.
         (let ((fun (cadr (car path))))
           (cond ((eq fun item-or-nil)
                  (full-or-init-internal (cdr path) item nil))
                 ((member fun *easytypes*)
                  (could-be-init item))
                 (t (|abbreviate| item)))))

        ; We only need initial information.
        ((member (car path) '(|Declare|))
         (could-be-init item))

        ; We need full information.
        ((member (car path) '(|Has| |With|))
         (|abbreviate| item))

        ; We need initial or full information depending on whether or
        ; not we actually extend Axiom domains.
        ((member (car path) '(|Extend|))
         (if *extends-axiom-domains*
             (|abbreviate| item)
           (could-be-init item)))

        (t (format t "Unknown tag in path ~a~%" path)
           (throw '|UndefinedTagInPath| path))))


(defun could-be-init (item)
  (let ((abbrev (|abbreviate| item)))
    (if (member item |$extendedDomains|)
        (intern (format nil "init_~a" abbrev))
      abbrev)))

;------------------------------------------------------------------
; Some helper functions...

; And environment is a tuple of the form (path variables)
; where path collects the nodes from the top node to the current one, and
; variables contains all the declared variable names so far.

; We alway start with "variables" |%| and |AxiomLib|, since they
; should never occur as dependencies.
(defun make-null-env ()
  (list nil '(|%| |AxiomLib|)))

; Adds the variable 'var' to the environment.
(defun env-add-var (env var)
  (list (env-path env) (cons var (env-vars env))))

; Adds the variables 'vars' to the environment.
(defun env-add-vars (env vars)
  (list (env-path env) (append vars (env-vars env))))

(defun env-add-path (env node)
  (list (cons node (env-path env)) (env-vars env)))

; Instead of |Apply| we want (|Apply| . fun) in the environment path where
; fun is just the token after the |Apply|.
(defun env-add-path-apply-fun (env fun)
  (list (cons (list '|Apply| fun) (cdr (env-path env))) (env-vars env)))

(defun env-path (env) (car  env))
(defun env-vars (env) (cadr env))

(defun env-contains-variable (env var)
  (member var (env-vars env)))

(defun map-find-deps (env lst)
  (mapcan #'(lambda (x) (find-deps env x)) lst))

; Return the list of argument names of a function.
; Only |Comma| and |Declare| is interesting here.
; Other forms we just ignore.
(defun find-arg-names (lst)
  (cond ((atom lst)                nil) ; covers sx=nil
        ((eq (car lst) '|Comma|)   (mapcan 'find-arg-names (cdr lst)))
        ((eq (car lst) '|Declare|) (list (cadr lst)))
        (t (format t "Ignored in find-arg-names: ~a~%" lst) nil)))
