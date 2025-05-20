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
\section{Database structure}
In order to understand this program you need to understand some details
of the structure of the databases it reads. FriCAS has 5 databases,
the interp.daase, operation.daase, category.daase, and
browse.daase.

\subsection{KAF File Format}
This documentation refers to KAF files which are random access files.
NRLIB files are KAF files (look for NRLIB/index.KAF)
The format of a random access file is
\begin{verbatim}
byte-offset-of-key-table
first-entry
second-entry
...
last-entry
((key1 . first-entry-byte-address)
 (key2 . second-entry-byte-address)
 ...
 (keyN . last-entry-byte-address))
\end{verbatim}
The key table is a standard lisp alist.

To open a database you fetch the first number, seek to that location,
and (read) which returns the key-data alist. To look up data you
index into the key-data alist, find the ith-entry-byte-address,
seek to that address, and (read).

For instance, see src/share/algebra/USERS.DAASE/index.KAF

One existing optimization is that if the data is a simple thing like a
symbol then the nth-entry-byte-address is replaced by immediate data.

Indeed, a faster optimization is to simply read the whole database
into the image before it is saved. The system would be easier to
understand and the interpreter would be faster.

The system uses another optimization: database contains a stamp
(consisting of offset to the main list and build time).  Before
saving the image selected data is fetched to memory.  When the
saved image starts it checks if the stamp of saved data matches
in-core data -- in case of agreement in-core data is used.
Parts of the datatabase which was not pre-loaded is still
(lazily) fetched from the filesystem.

\subsection{Database Files}

Database files are very similar to KAF files except that there
is an optimization which makes the first
item a pair of two numbers. The first number in the pair is
the offset of the key-value table, the second is a time stamp.
If the time stamp in the database matches the time stamp in
the image the database is not needed (since the internal hash
tables already contain all of the information). When the database
is built the time stamp is saved in both the Lisp image and the
database.
|#

;;TTT 7/2/97
; Regarding the 'ancestors field for a category: At database build
; time there exists a |$ancestors_hash| hash table that gets filled
; with CATEGORY (not domain) ancestor information. This later provides
; the information that goes into interp.daase This |$ancestors_hash|
; does not exist at normal runtime (it can be made by a call to
; genCategoryTable). Note that the ancestor information in
; |$ancestors_hash| (and hence interp.daase) involves #1, #2, etc
; instead of R, Coef, etc. The latter thingies appear in all
; .NRLIB/index.KAF files. So we need to be careful when we )lib
; categories and update the ancestor info.


; In the old system each constructor (e.g. LIST) had one library directory
; (e.g. LIST.NRLIB). this directory contained a random access file called
; the index.KAF file. the interpreter needed this KAF file at runtime for
; two entries, the operationAlist and the ConstructorModemap.
; during the redesign for the new compiler we decided to merge all of
; these .NRLIB/index.KAF files into one database, INTERP.DAASE.
; requests to get information from this database are intended to be
; cached so that multiple references do not cause additional disk i/o.
; this database is left open at all times as it is used frequently by
; the interpreter. one minor complication is that newly compiled files
; need to override information that exists in this database.
;   The design calls for constructing a random read (KAF format) file
; that is accessed by functions that cache their results. when the
; database is opened the list of constructor-index pairs is hashed
; by constructor name. a request for information about a constructor
; causes the information to replace the index in the hash table. since
; the index is a number and the data is a non-numeric sexpr there is
; no source of confusion about when the data needs to be read.
;
; The format of this new database is as follows:
;
;first entry:
; an integer giving the byte offset to the constructor alist
; at the bottom of the file
;second and subsequent entries (one per constructor)
; (operationAlist)
; (constructorModemap)
; ....
;last entry: (pointed at by the first entry)
; an alist of (constructor . index) e.g.
;  ( (PI offset-of-operationAlist offset-of-constructorModemap)
;   (NNI offset-of-operationAlist offset-of-constructorModemap)
;    ....)
; This list is read at open time and hashed by the car of each item.

; the system has been changed to use the property list of the
; symbols rather than hash tables. since we already hashed once
; to get the symbol we need only an offset to get the property
; list. this also has the advantage that eq hash tables no longer
; need to be moved during garbage collection.
;  there are 3 potential speedups that could be done. the best
; would be to use the value cell of the symbol rather than the
; property list but i'm unable to determine all uses of the
; value cell at the present time.
;  a second speedup is to guarantee that the property list is
; a single item, namely the database structure. this removes
; an assoc but leaves one open to breaking the system if someone
; adds something to the property list. this was not done because
; of the danger mentioned.
;  a third speedup is to make the get_database call go away, either
; by making it a macro or eliding it entirely. this was not done
; because we want to keep the flexibility of changing the database
; forms.

; the new design does not use hash tables. the database structure
; contains an entry for each item that used to be in a hash table.
; initially the structure contains file-position pointers and
; these are replaced by real data when they are first looked up.
; the database structure is kept on the property list of the
; constructor, thus, (get '|DenavitHartenbergMatrix| 'database)
; will return the database structure object.

; each operation has a property on its symbol name called 'operation
; which is a list of all of the signatures of operations with that name.

; -- tim daly

(in-package "BOOT")

; this hash table is used to answer the question "does domain x
; have category y?". this is answered by constructing a pair of
; (x . y) and doing an equal hash into this table.

   ; note that constructorcategory information need only be kept for
   ; items of type category. this will be fixed in the next iteration
   ; when the need for the various caches are reviewed

   ; note that the *modemaps-hash* information does not need to be kept
   ; for system files. these are precomputed and kept in modemap.daase
   ; however, for user-defined files these are needed.
   ; currently these are added to the database for 2 reasons:
   ;  there is a still-unresolved issue of user database extensions
   ;  this information is used during database build time

;;;
;;; Below is support for files produced by Aldor compiler
;;;

(defvar |$asharp_flags| "-O -lfricas -Fasy -Flsp" "library compiler flags")

(defun ASHARP (file &optional (flags |$asharp_flags|))
 "call the aldor compiler"
 (#| system::system |#
   obey
   (concatenate 'string |$spadroot| "/compiler/bin/aldor "
    flags " " file)))

(defun |asharp_global_name| (name key code)
    (foam::axiomxl-global-name name key code))

; the variable NOPfuncall is a funcall-able object that is a dummy
; initializer for libfricas aldor domains.
(defvar NOPfuncall (cons 'identity nil))

(defun |createInitializers| ()
;; since libfricas is now built with -name=fricas following unnecessary
;; (dolist (con (|allConstructors|))
;;   (let ((sourcefile (|get_database| con 'sourcefile)))
;;     (if sourcefile
;;       (set (foam::axiomxl-file-init-name (pathname-name sourcefile))
;;             NOPfuncall))))
 (set (foam::axiomxl-file-init-name "fricas") NOPfuncall)
 (set (foam::axiomxl-file-init-name "filecliq") NOPfuncall)
 (set (foam::axiomxl-file-init-name "attrib") NOPfuncall)
 (|createInitializers2|))

;; following needs to happen inside restart since $FRICAS may change
(defun |createInitializers2| ()
 (let ((asharprootlib (strconc |$spadroot| "/aldor/lib/")))
   (set-file-getter (strconc asharprootlib "runtime"))
   (set-file-getter (strconc asharprootlib "lang"))
   (set-file-getter (strconc asharprootlib "attrib"))
   (set-file-getter (strconc asharprootlib "axlit"))
   (set-file-getter (strconc asharprootlib "minimach"))
   (set-file-getter (strconc asharprootlib "axextend"))))



;---------------------------------------------------------------------

; how the magic works:
;  when a )library is done on a new compiler file we set up multiple
;  functions (referred to as autoloaders). there is an autoloader
;  stored in the symbol-function of the G-filename (e.g. G-basic)
;  (see set-file-getter function)
;  and an autoloader stored in the symbol-function of every domain
;  in the basic.as file ( asharpMkAutoloadFunctor )
; When a domain is needed the autoloader for the domain is executed.
;  this autoloader invokes file-getter-name to get the name of the
;  file (eg basic) and evaluates the name. the FIRST time this is done
;  for a file the file will be loaded by its autoloader, then it will
;  return the file object. every other time the file is already
;  loaded and the file object is returned directly.
; Once the file object is gotten getconstructor is called to get the
;  domain. the FIRST time this is done for the domain the autoloader
;  invokes the file object. every other time the domain already
;  exists.

(defmacro |CCall| (fun &rest args)
  (let ((ccc (gensym)) (cfun (gensym)) (cenv (gensym)))
    `(let ((,ccc ,fun))
       (let ((,cfun (|ClosFun| ,ccc))
             (,cenv (|ClosEnv| ,ccc)))
         (funcall ,cfun ,@args ,cenv )))))

(defmacro |ClosFun| (x) `(car ,x))
(defmacro |ClosEnv| (x) `(cdr ,x))

(defun file-runner (name)
 (declare (special foam-user::|G-domainPrepare!|))
  (|CCall| foam-user::|G-domainPrepare!| (|CCall| name)))

(defun getConstructor (file-fn asharp-name)
 (|CCall| file-fn)
 (eval asharp-name))

(defun getop (dom op type)
 (declare (special foam-user::|G-domainGetExport!|))
  (|CCall| foam-user::|G-domainGetExport!| dom
      (|hashString| (symbol-name op)) type))

; the asharp compiler will allow both constant domains and domains
; which are functions. localasy sets the autoload property so that
; the symbol-function contains a function that, when invoked with
; the correct number of args will return a domain.

; this function is called if we are given a new compiler domain
; which is a function. the symbol-function of the domain is set
; to call the function with the correct number of arguments.

(defun wrapDomArgs (obj type?)
  (cond ((not type?) obj)
        (t (|makeOldAxiomDispatchDomain| obj))))

(defun |set_asharp_autoload_functor| (file cname asharp-name cosig)
  (setf (symbol-function cname)
  #'(lambda (&rest args)
     (let ((func (getconstructor (eval (file-getter-name file)) asharp-name)))
      (setf (symbol-function cname)
       (if (vectorp (car func))
        #'(lambda () func) ;; constant domain
        #'(lambda (&rest args)
            (apply (|ClosFun| func)
                   (nconc
                    (mapcar #'wrapDomArgs args (cdr cosig))
                    (list (|ClosEnv| func)))))))
      (apply cname args)))))


(defun |set_asharp_autoload_category| (file cname asharp-name cosig)
  (asharpMkAutoLoadFunctor file cname asharp-name cosig)
  (let ((packname (INTERN (STRCONC cname '"&"))))
    (setf (symbol-function packname)
  #'(lambda (self &rest args)
     (let ((func (getconstructor (eval (file-getter-name file)) asharp-name)))
      (setf (symbol-function packname)
       (if (vectorp (car func))
        #'(lambda (self)
            (|CCall| (elt (car func) 5) (cdr func) (wrapDomArgs self t))) ;; constant category
        #'(lambda (self &rest args)
            (let ((precat
                   (apply (|ClosFun| func)
                          (nconc
                           (mapcar #'wrapDomArgs args (cdr cosig))
                           (list (|ClosEnv| func))))))
              (|CCall| (elt (car precat) 5) (cdr precat) (wrapDomArgs self t))))))
      (apply packname self args))))))

(defun |set_asharp_autoload_function| (file asharpname)
  (set asharpname
   (cons
    #'(lambda (&rest l)
        (let ((args (butlast l))
              (func (getconstructor (eval (file-getter-name file)) asharpname)))
          (apply (car func) (append args (list (cdr func))))))
        ())))

; this function will return the internal name of the file object getter

(defun file-getter-name (filename)
   (foam::axiomxl-file-init-name (pathname-name filename)))

;;need to initialize |G-filename| to a function which loads file
;; and then returns the new value of |G-filename|

(defun set-file-getter (filename)
  (let ((getter-name (file-getter-name filename)))
    (set getter-name
         (cons #'init-file-getter  (cons getter-name filename)))))

(defun init-file-getter (env)
  (let ((getter-name (car env))
        (filename (cdr env)))
    (load filename)
    (|CCall| (eval getter-name))))

(defun set-lib-file-getter (filename cname)
  (let ((getter-name (file-getter-name filename)))
    (set getter-name
         (cons #'init-lib-file-getter  (cons getter-name cname)))))

(defun init-lib-file-getter (env)
  (let* ((getter-name (car env))
         (cname (cdr env))
         (filename (|get_database| cname 'object)))
    (load filename)
    (|CCall| (eval getter-name))))

;; following 2 functions are called by file-exports and file-imports macros
(defun foam::process-import-entry (entry)
  (let* ((asharpname (car entry))
         (stringname (cadr entry))
         (hcode (caddr entry))
         (libname (cadddr entry))
         (bootname (intern stringname 'boot)))
    (declare (ignore libname))
    (if (and (eq hcode 'foam-user::|initializer|) (not (boundp asharpname)))
        (error (format nil "Aldor file ~s is missing!" stringname)))
    (unless (or (not (numberp hcode)) (zerop hcode) (boundp asharpname))
          (when (|constructor?| bootname)
                (set asharpname
                     (if (|get_database| bootname 'niladic)
                         (|makeLazyOldAxiomDispatchDomain| (list bootname))
                       (cons '|runOldAxiomFunctor|  bootname)))))))

;(defun foam::process-export-entry (entry)
;  (let* ((asharpname (car entry))
;        (stringname (cadr entry))
;        (hcode (caddr entry))
;        (libname (cadddr entry))
;        (bootname (intern stringname 'boot)))
;    (declare (ignore libname))
;    (when (numberp hcode)
;         (setf (get bootname 'asharp-name)
;               (cons (cons |$this_file| asharpname)
;                     (get bootname 'asharp-name)))
;         )))
