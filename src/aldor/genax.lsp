; This file does a number of things.
; 1) generate-init-list
;    It generates initial init_X.ap files for domains X that will be
;    extended during the build of libaxiom.al. These domains are listed
;    in init-domains.
; 2) generate-deps
;    It generates dependencies for domains from the Axiom algebra library.
; 3) generate-deps-from-ap
;    It generates dependencies from the .ap files of a number of aldor
;    domains.

; The following list contains domains that initially are build with
; a dummy definition (see baselist.as for the origin).
; We filter here out everything that is not an export declaration.
; The file baslist.ap approximately looks like
;(|Sequence|
;  (|Declare| |Type| (|With| () ()))
;  (|Declare| |Tuple| (|With| () ()))
;  (|Declare| |->| (|With| () ()))
;  (|Export| (|Declare| |Boolean| (|With| () ())) () ())
;  (|Export| (|Declare| |Field| (|With| () ())) () ())
;  ...
;)
(defun init-list ()
  (let ((ilist (cdr (with-open-file (str (pathname "initlist.ap"))
		      (read str)))))
    (mapcan '(lambda (x) (and (eq (car x) '|Export|) (list x))) ilist)))
(setq initlist (init-list))

(defun type-name (initinfo)
  (cadr (cadr initinfo)))

; That is an Axiom function that sets the variable *extendedDomains*.
; *extendedDomains* have relevance during the call to |makeAxExportForm|.
(|setExtendedDomains| (mapcar 'type-name initlist))

;;
;; Creating the base file set
;;

(defun generate-init-list (dest)
  ;; list of file names (abbreviated type names) into the file 'dest'.
  (with-open-file (bstr (pathname dest) :direction :output)
    (dolist (initinfo initlist)
      (format bstr "init_~a~%" (|abbreviate| (type-name initinfo)))))
  ;; Create init dependency files
  (dolist (initinfo initlist)
    (let ((abbrev (|abbreviate| (type-name initinfo))))
      (format t "Writing dep ~a~%" abbrev)
      (setq *extension-file* t)
      (setq deps (find-deps (make-null-env) initinfo))	
      (pprint deps)
      (with-open-file (depstr (dep-file-name (format nil "init_~a" abbrev))
			      :direction :output)
	(dolist (dep deps)
	  (format depstr "~a~%" dep)))
      ;; Create init .ap fragments
      (format t "Writing ap ~a~%" abbrev)
      (with-open-file (axfile
		       (pathname (format nil "init_ap/init_~a.ap" abbrev))
		       :direction :output)
	(prin1 initinfo axfile)))))

(defun dep-file-name (name)
  (pathname (format nil "gendeps/~a.dep" name)))

;;
;; Generating ax files
;;

(defun generate-ax-file (fname content inits)
  (with-open-file (str (pathname (format nil "ap/~a.ap" fname))
                       :direction :output)
                  (format t "FILE: ~a Content: ~a Inits: ~a~%" fname content inits)
		  (pprint  (mapcar '|fileConstructors| content))
		  (pprint   (|makeAxExportForm| fname (mapcar '|fileConstructors|  content)))
                  (pprint (append  (|makeAxExportForm| fname (mapcan '|fileConstructors| content))
                                   (mapcar 'load-init inits))
                          str)))

(defun load-init (initname)
  (with-open-file (str (pathname (format nil "init_ap/~a.ap" initname)))
                  (read str)))

;;
;; Generating dependencies
;;

;; .dep from SPAD constructor

(defun generate-deps (filelist)
  (setq *extension-file* nil)
  (let ((tags nil))
    (dolist (file filelist)
            ;; (assert (listp clique))
            ;; (assert (or (symbolp (car clique)) (listp (car clique))))
            ;; Compute the tag.
      (let (exportform deps)
        (setq exportform (|makeAxExportForm| file (|fileConstructors| file)))
        (pprint exportform)
        (setq deps (find-deps (make-null-env) exportform))
        (print deps)
        (with-open-file (depstr (dep-file-name file) :direction :output)
                        (dolist (dep deps)
                          (format depstr "~a~%" dep)))))))

;; .dep from .ap file
; rhx: It is not really clear why there must be *extension-file* at all.
; I don't yet completely understand the .ap format and its difference
; to what  
(defun generate-deps-from-ap-files (filelist)
  (setq *extension-file* t)
  (let ((tags nil))
    (dolist (infile filelist)
      (print infile)
      (let (exportform deps)
        (with-open-file (instr (pathname infile))
                        (setq exportform (read instr)))
        (pprint exportform)
        (setq deps (find-deps (make-null-env) exportform))
        (print deps)
        (print (dep-file-name (pathname-name (pathname infile))))
        (with-open-file (depstr (dep-file-name (pathname-name (pathname infile))) :direction :output)
                        (dolist (dep deps)
                          (format depstr "~a~%" dep)))))))

;; Finding dependencies - just a syntax walker...

(setq *last-def* nil)

(defun find-deps (env stmt)
  (cond ((atom stmt) nil)
        ((eq (car stmt) '|Sequence|)
         (find-deps-sequence env (cdr stmt)))
        ((eq (car stmt) '|Import|)
         (find-deps-import env (cdr stmt)))
        ((eq (car stmt) '|Export|)
         (find-deps-export env (cdr stmt)))
        ((eq (car stmt) '|Inline|)
         (find-deps-export env (cdr stmt)))
        ((eq (car stmt) '|Foreign|)
         (find-deps-foreign env (cdr stmt)))
        ((eq (car stmt) '|Define|)
         (find-deps-define env (cdr stmt)))
        ((eq (car stmt) '|Declare|)
         (find-deps-declare env (cdr stmt)))
        ((eq (car stmt) '|Lambda|)
         (find-deps-lambda env (cdr stmt)))
        ((eq (car stmt) '|Comma|)
         (find-deps-comma env (cdr stmt)))
        ((eq (car stmt) '|Extend|)
         (find-deps-extend env (cdr stmt)))
        ((eq (car stmt) '|With|)
         (find-deps-with env (cdr stmt)))
        ((eq (car stmt) '|Add|)
         (find-deps-add env (cdr stmt)))
        ((eq (car stmt) '|Label|)
         (find-deps-label env (cdr stmt)))
        ((eq (car stmt) '|Apply|)
         (find-deps-apply env (cdr stmt)))
        ((eq (car stmt) '|RestrictTo|)
         (find-deps-restrict env (cdr stmt)))
        ((eq (car stmt) '|Default|)
         (find-deps-default env (cdr stmt)))
        ((eq (car stmt) '|If|)
         (find-deps-if env (cdr stmt)))
        ((eq (car stmt) '|Test|)
         (find-deps-test env (cdr stmt)))
        ((eq (car stmt) '|Has|)
         (find-deps-has env (cdr stmt)))
        ((eq (car stmt) '|Not|)
         (find-deps-not env (cdr stmt)))
        ((eq (car stmt) '|And|)
         (find-deps-and env (cdr stmt)))
        ((eq (car stmt) '|Or|)
         (find-deps-or env (cdr stmt)))
        ((eq (car stmt) '|PretendTo|)
         (find-deps-pretend env (cdr stmt)))
        ((eq (car stmt) '|PretendTo|)
         (find-deps-pretend env (cdr stmt)))
        ((eq (car stmt) '|LitString|)
         (find-deps-literal env (cdr stmt)))
        ((eq (car stmt) '|LitInteger|)
         (find-deps-literal env (cdr stmt)))
        (t
         (format t "Unknown form ~a~%" stmt)
         (throw 'up stmt))))

(defun find-deps-sequence (env list)
  (if (null list) nil
    (append (find-deps env (car list))
            (find-deps-sequence env (cdr list)))))

(defun find-deps-export (env list)
  (find-deps env (car list)))

(defun find-deps-import (env list)
  (find-deps-typeform env (cadr list)))

(defun find-deps-foreign (env list)
  nil)

(defun find-deps-default (env list)
  nil)

(defun find-deps-literal (env list)
  nil)

(defun find-deps-extend (env list)
  (let* ((def-stmt (car list))
         (decl-stmt (cadr def-stmt))
         (var (cadr decl-stmt)))
    (append (find-deps-typeform env var)
	    (find-deps env (car list)))))

(defun find-deps-restrict (env list)
  (append (find-deps env (car list))
          (find-deps env (cadr list))))

(defun find-deps-pretend (env list)
  (append (find-deps-typeform (env-add-context env 'pretend) (car list))
          (find-deps-typeform env (cadr list))))

(defun find-deps-has (env list)
  (append (find-deps-typeform env (car list))
          (find-deps-typeform env (cadr list))))

(defun find-deps-test (env list)
  (find-deps env (car list)))

(defun find-deps-not (env list)
  (find-deps env (car list)))

(defun find-deps-and (env list)
  (find-deps-sequence env (car list)))

(defun find-deps-or (env list)
  (find-deps-sequence env (car list)))

(defun find-deps-if (env list)
  (let ((test (car list))
        (if-part (cadr list))
        (else-part (caddr list)))
    (if (null else-part)
        (append (find-deps env test)
                (find-deps env if-part))
      (append (find-deps env test)
              (find-deps env if-part)
              (find-deps env else-part)))))



      
(defun find-deps-define (env list)
  (let ((newvars nil)) ;;(define-lhs-find-args (car list))
    (setq *last-def* (car list))
    (append (find-deps env (car list))
            (find-deps (env-extend env newvars)
                       (cadr list)))))

(defun find-deps-declare (env list)
  (setq *last-def* (car list))
  (let ((aa (find-deps-typeform (env-add-context env 'declare)
                                (cadr list))))
    (setq *last-def* nil)
    aa))

(defun find-deps-apply (env list)
  (find-deps-apply-list env list))

(defun find-deps-apply-list (env list)
  (find-deps-sequence env list)) 

(defun find-deps-lambda (env list)
  (let ((args (car list))
        (type (cadr list))
        (text (car (cddr list))))
    (let ((newenv (env-extend env (define-lhs-find-args args))))
      (append (find-deps env args)
              (find-deps-typeform newenv type)
              (find-deps newenv text)))))

(defun find-deps-label (env list)
  (find-deps env (cadr list)))

(defun find-deps-comma (env list)
  (find-deps-comma-list env list))

(defun find-deps-comma-list (env list)
  (cond ((null list) nil)
        ((atom (car list))
         (append (find-deps-typeform env (car list))
                 (find-deps-comma-list env (cdr list))))
        ((eq (caar list) '|Declare|)
         (let ((decl (cdr (car list))))
           (append (find-deps-typeform env (cadr decl))
                   (find-deps-comma-list (env-extend env (list (car decl)))
                                         (cdr list)))))
        (t 
         (let ((comma-vars (define-lhs-find-args (car list))))
           (append (find-deps env (car list))
                   (find-deps-comma-list (env-extend env comma-vars)
                                         (cdr list)))))))


;; With statements....

(defun find-deps-with (env list)
  (let ((env (env-add-context env 'with)))
    (append (if (null (car list)) nil
              (find-deps-typeform env (car list)))
            (find-deps-with-list env (cdr list)))))
  
(defun find-deps-with-list (env list)
  (if (null list) nil
    (append (find-deps-with-list-item env (car list))
            (find-deps-with-list env (cdr list)))))


(defun find-deps-with-list-item (env item)
  (cond ((atom item)
         (find-deps-typeform env item))
        ((eq (car item) '|Sequence|)
         (find-deps-with-list env (cdr item)))
        ((eq (car item) '|RestrictTo|)
         (find-deps-with-restrict env (cdr item)))
        ((eq (car item) '|If|)
         (find-deps-with-if env (cdr item)))
        ((eq (car item) '|Apply|)
         (find-deps-typeform-apply env (cdr item)))
        (t
         (find-deps env item))))

(defun find-deps-with-restrict (env list)
  (append (find-deps-with-list-item env (car list))
          (find-deps-typeform env (cadr list))))

(defun find-deps-with-if (env list)
  (let ((test (car list))
        (if-part (cadr list))
        (else-part (caddr list)))
    (if (null else-part)
        (append (find-deps env test)
                (find-deps-with-list-item env if-part))
      (append (find-deps env test)
              (find-deps-with-list-item env if-part)
              (find-deps-with-list-item env else-part)))))


(defun find-deps-typeform-apply (env list)
  (if (and nil (eq (car list) '|->|))
      (find-deps-typeform-apply-list env list)
    (append
     (find-deps-typeform-apply-list env (list (car list)))
     (find-deps-typeform-apply-list (env-add-context env 'apply) (cdr list)))))

(defun find-deps-typeform-apply-list (env list)
  (cond ((null list) nil)
        ((atom (car list))
         (append (find-deps-typeform env (car list))
                 (find-deps-typeform-apply-list env (cdr list))))
        ((eq (caar list) '|Declare|)
         (let ((decl (cdr (car list))))
           (append (find-deps-typeform (env-add-context env 'declare) (cadr decl))
                   (find-deps-typeform-apply-list (env-extend env (list (car decl)))
                                                  (cdr list)))))
        ((eq (caar list) '|With|)
           (append (find-deps-with env (cdar list))
                   (find-deps-typeform-apply-list env
                                                  (cdr list))))
        (t 
         (let ((apply-vars (define-lhs-find-args (car list))))
           (append (find-deps-typeform env (car list))
                   (find-deps-typeform-apply-list (env-extend env apply-vars)
                                                  (cdr list)))))))


;; Add statements
(defun find-deps-add (env list)
  (if (null (car list)) nil
    (find-deps env (car list))))

;; define lists

(defun define-lhs-find-args (sx)
  (cond ((atom sx) nil)
        ((eq (car sx) '|Comma|)
         (define-lhs-find-args-seq (cdr sx)))
        ((eq (car sx) '|Declare|)
         (list (cadr sx)))
        (t (append (define-lhs-find-args (car sx))
                   (define-lhs-find-args-seq (cdr sx))))))

(defun define-lhs-find-args-seq (sx)
  (if (null sx) nil
    (append (define-lhs-find-args (car sx))
            (define-lhs-find-args-seq (cdr sx)))))

;; environment 
; Env is (variables context parent)
; where context is apply, with, etc
 
(defun make-null-env ()
  (list (list '|%| '|AxiomLib|)
        nil
        nil))

(defun env-extend (env l)
  (list l (env-context env) env))

(defun env-add-context (env context)
  (list nil context env))

(defun env-parent (env)
  (nth 2 env))

(defun env-context (env)
  (nth 1 env))

(defun env-contains (env var)
  (cond ((null env) nil)
        ((member var (car env)) t)
        (t (env-contains (env-parent env) var))))

;; Utilities

(defun find-deps-typeform (env list)
  (cond ((atom list)
         (dep-type-or-nil env list))
        ((eq (car list) '|Apply|)
         (find-deps-typeform-apply env (cdr list)))
        ((eq (car list) '|With|)
         (find-deps-with env (cdr list)))
        (t 
         (append (dep-type-or-nil env (car list))
                 (find-deps-typelist env (cdr list))))))

(defun find-deps-typelist (env list)
  (if (null list) nil
    (append (find-deps-typeform env (car list))
            (find-deps-typelist env (cdr list)))))

(defun dep-type-or-nil (env item)
  (cond ((null item) nil)
        ((env-contains env item) nil)
        ((eq item '|With|) (throw 'up item))
        ((and (eq (env-context env) 'pretend)
              (not *extension-file*)
              (base-type-p item))
         (format t "Rejecting ~a - in pretend context~%" item)
         nil)
        ((and (null (eq (env-context env) 'apply))
              (not *extension-file*)
              (base-type-p item))
         (format t "Rejecting ~a - not in apply context~%" item)
         nil)
	; Need special treatment for a SPAD builtin type.
	((eq item '|SubsetCategory|)
	 (format t "Adding ~a ~a (~a)~%" (env-context env) item *last-def*)
	 '(SUBSETC))
        ((|isNameOfType| item) 
         (format t "Adding ~a ~a (~a)~%" (env-context env) item *last-def*)
         (list (|abbreviate| item)))
        (t nil)))

(defun base-type-p (item) (member item |$extendedDomains|))
