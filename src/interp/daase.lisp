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
the interp.daase, operation.daase, category.daase, compress.daase, and
browse.daase. The compress.daase is special and does not follow the
normal database format.

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

Another existing one is a compression algorithm applied to the
data so that the very long names don't take up so much space.
We could probably remove the compression algorithm as 64k is no
longer considered 'huge'. The database-abbreviation routine
handles this on read and write-compress handles this on write.
The squeeze routine is used to compress the keys, the unsqueeze
routine uncompresses them. Making these two routines disappear
should remove all of the compression.

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


; This file contains the code to build, open and access the .DAASE
; files this file contains the code to )library NRLIBS and asy files

; There is a major issue about the data that resides in these
; databases.  the fundamental problem is that the system requires more
; information to build the databases than it needs to run the
; interpreter.  in particular, MODEMAP.DAASE is constructed using
; properties like "modemaps" but the interpreter will never ask for
; this information.

; So, the design is as follows:
;  first, the MODEMAP.DAASE needs to be built. this is done by doing
; a )library on ALL of the NRLIB files that are going into the system.
; this will bring in "modemap" information and add it to the
; *modemaps-hash* hashtable.
;  next, database build proceeds, accessing the "modemap" property
; from the hashtables. once this completes this information is never
; used again.
;  next, the interp.daase database is built. this contains only the
; information necessary to run the interpreter. note that during the
; running of the interpreter users can extend the system by do a
; )library on a new NRLIB file. this will cause fields such as "modemap"
; to be read and hashed.

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
;  a third speedup is to make the getdatabase call go away, either
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

(defstruct database
 abbreviation               ; interp.
 ancestors                  ; interp.
 constructor                ; interp.
 constructorcategory        ; interp.
 constructorkind            ; interp.
 constructormodemap         ; interp.
 cosig                      ; interp.
 defaultdomain              ; interp.
 modemaps                   ; interp.
 niladic                    ; interp.
 object                     ; interp.
 operationalist             ; interp.
 documentation              ; browse.
 constructorform            ; browse.
 predicates                 ; browse.
 sourcefile                 ; browse.
 parents                    ; browse.
 users                      ; browse.
 dependents                 ; browse.
 spare                      ; superstition
 ) ; database structure

; there are only a small number of categories that have default domains.
; rather than keep this slot in every domain we maintain a list here.

(defvar |$defaultdomain_list| '(
  (|MultisetAggregate| |Multiset|)
  (|FunctionSpace| |Expression|)
  (|AlgebraicallyClosedFunctionSpace| |Expression|)
  (|ThreeSpaceCategory| |ThreeSpace|)
  (|DequeueAggregate| |Dequeue|)
  (|ComplexCategory| |Complex|)
  (|LazyStreamAggregate| |Stream|)
  (|AssociationListAggregate| |AssociationList|)
  (|QuaternionCategory| |Quaternion|)
  (|PriorityQueueAggregate| |Heap|)
  (|PointCategory| |Point|)
  (|PlottableSpaceCurveCategory| |Plot3D|)
  (|PermutationCategory| |Permutation|)
  (|StringCategory| |String|)
  (|FileNameCategory| |FileName|)
  (|OctonionCategory| |Octonion|)))

; this hash table is used to answer the question "does domain x
; have category y?". this is answered by constructing a pair of
; (x . y) and doing an equal hash into this table.

(defvar |$operation_hash| nil "given an operation name, what are its modemaps?")
(defvar |$has_category_hash| nil "answers x has y category questions")

(defvar |$miss| nil "print out cache misses on getdatabase calls")

(defvar |$do_not_compress_databases| t)

   ; note that constructorcategory information need only be kept for
   ; items of type category. this will be fixed in the next iteration
   ; when the need for the various caches are reviewed

   ; note that the *modemaps-hash* information does not need to be kept
   ; for system files. these are precomputed and kept in modemap.daase
   ; however, for user-defined files these are needed.
   ; currently these are added to the database for 2 reasons:
   ;  there is a still-unresolved issue of user database extensions
   ;  this information is used during database build time



; this are the streams for the databases. they are always open.
; there is an optimization for speeding up system startup. if the
; database is opened and the ..-stream-stamp variable matches the
; position information in the database then the database is NOT
; read in and is assumed to match the in-core version

(defvar |$compress_vector| nil "a vector of things to compress in the databases")
(defvar |$compress_vector_length| 0 "length of the compress vector")
(defvar |$compress_stream| nil "an stream containing the compress vector")
(defvar |$compress_stream_stamp| 0 "|$compress_stream| (position . time)")

(defvar |$interp_stream| nil "an open stream to the interpreter database")
(defvar |$interp_stream_stamp| 0 "|$interp_stream| (position . time)")

; this is indexed by operation, not constructor
(defvar |$operation_stream| nil "the stream to operation.daase")
(defvar |$operation_stream_stamp| 0 "|$operation_stream| (position . time)")

(defvar |$browse_stream| nil "an open stream to the browser database")
(defvar |$browse_stream_stamp| 0 "|$browse_stream| (position . time)")

; this is indexed by (domain . category)
(defvar |$category_stream| nil "an open stream to the category table")
(defvar |$category_stream_stamp| 0 "|$category_stream| (position . time)")

(defvar |$all_constructors| nil "a list of all the constructors in the system")
(defvar |$all_operations| nil "a list of all the operations in the system")

(defvar |$asharp_flags| "-O -lfricas -Fasy -Flsp" "library compiler flags")

(defun asharp (file &optional (flags |$asharp_flags|))
 "call the aldor compiler"
 (#| system::system |#
   obey
   (concatenate 'string (|getEnv| "FRICAS") "/compiler/bin/aldor "
    flags " " file)))

(defun resethashtables ()
 "set all *_hash to clean values. used to clean up core before saving system"
 (setq |$has_category_hash| (make-hash-table :test #'equal))
 (setq |$operation_hash| (make-hash-table))
 (setq |$all_constructors| nil)
 (setq |$compress_vector| nil)
 (setq |$compress_stream_stamp| '(0 . 0))
 (|compressOpen|)
 (setq |$interp_stream_stamp| '(0 . 0))
 (|interpOpen|)
 (setq |$operation_stream_stamp| '(0 . 0))
 (|operationOpen|)
 (setq |$browse_stream_stamp| '(0 . 0))
 (|browseOpen|)
 (setq |$category_stream_stamp| '(0 . 0))
 (|categoryOpen|) ;note: this depends on constructorform in browse.daase
 (initial-getdatabase)
 (close |$interp_stream|)
 (close |$operation_stream|)
 (close |$category_stream|)
 (close |$browse_stream|)
#+:GCL (LISP::gbc t)
)

(defun initial-getdatabase ()
 "fetch data we want in the saved system"
 (let (hascategory constructormodemapAndoperationalist operation constr)
 (format t "Initial getdatabase~%")
 (setq hascategory '(
  (|Equation| . |Ring|)
  (|Expression| . |CoercibleTo|) (|Expression| . |CommutativeRing|)
  (|Expression| . |IntegralDomain|) (|Expression| . |Ring|)
  (|Float| . |RetractableTo|)
  (|Fraction| . |Algebra|) (|Fraction| . |CoercibleTo|)
  (|Fraction| . |OrderedSet|) (|Fraction| . |RetractableTo|)
  (|Integer| . |Algebra|) (|Integer| . |CoercibleTo|)
  (|Integer| . |ConvertibleTo|) (|Integer| . |LinearlyExplicitOver|)
  (|Integer| . |RetractableTo|)
  (|List| . |CoercibleTo|) (|List| . |FiniteLinearAggregate|)
  (|List| . |OrderedSet|)
  (|Polynomial| . |CoercibleTo|) (|Polynomial| . |CommutativeRing|)
  (|Polynomial| . |ConvertibleTo|) (|Polynomial| . |OrderedSet|)
  (|Polynomial| . |RetractableTo|)
  (|Symbol| . |CoercibleTo|) (|Symbol| . |ConvertibleTo|)
  (|Variable| . |CoercibleTo|)))
 (dolist (pair hascategory) (getdatabase pair 'hascategory))
 (setq constructormodemapAndoperationalist '(
  |BasicOperator|  |Boolean|
  |CardinalNumber| |Color|  |Complex|
  |Database|
  |Equation| |EquationFunctions2| |Expression|
  |Float| |Fraction| |FractionFunctions2|
  |Integer| |IntegralDomain|
  |Kernel|
  |List|
  |Matrix| |MappingPackage1|
  |Operator| |OutputForm|
  |NonNegativeInteger|
  |ParametricPlaneCurve| |ParametricSpaceCurve| |Point| |Polynomial|
  |PolynomialFunctions2| |PositiveInteger|
  |Ring|
  |SetCategory| |SegmentBinding| |SegmentBindingFunctions2| |DoubleFloat|
  |SparseMultivariatePolynomial| |SparseUnivariatePolynomial| |Segment|
  |String| |Symbol|
  |UniversalSegment|
  |Variable|  |Vector|))
 (dolist (con constructormodemapAndoperationalist)
  (getdatabase con 'constructormodemap)
  (getdatabase con 'operationalist))
 (setq operation '(
  |+| |-| |*| |/| |**| |coerce| |convert| |elt| |equation|
  |float| |sin| |cos| |map| |SEGMENT|))
 (dolist (op operation) (getdatabase op 'operation))
 (setq constr '( ;these are sorted least-to-most freq. delete early ones first
  |Factored| |SparseUnivariatePolynomialFunctions2| |TableAggregate&|
  |RetractableTo&| |RecursiveAggregate&| |UserDefinedPartialOrdering|
  |None| |UnivariatePolynomialCategoryFunctions2| |IntegerPrimesPackage|
  |SetCategory&| |IndexedExponents| |QuotientFieldCategory&| |Polynomial|
  |EltableAggregate&| |PartialDifferentialRing&| |Set|
  |UnivariatePolynomialCategory&| |FlexibleArray|
  |SparseMultivariatePolynomial| |PolynomialCategory&|
  |DifferentialExtension&| |IndexedFlexibleArray| |AbelianMonoidRing&|
  |FiniteAbelianMonoidRing&| |DivisionRing&| |FullyLinearlyExplicitOver&|
  |IndexedVector| |IndexedOneDimensionalArray| |LocalAlgebra| |Localize|
  |Boolean| |Field&| |Vector| |IndexedDirectProductObject| |Aggregate&|
  |PolynomialRing| |FreeModule| |IndexedDirectProductAbelianGroup|
  |IndexedDirectProductAbelianMonoid| |SingletonAsOrderedSet|
  |SparseUnivariatePolynomial| |Fraction| |Collection&| |HomogeneousAggregate&|
  |RepeatedSquaring| |IntegerNumberSystem&| |AbelianSemiGroup&|
  |AssociationList| |OrderedRing&| |SemiGroup&| |Symbol|
  |UniqueFactorizationDomain&| |EuclideanDomain&| |IndexedAggregate&|
  |GcdDomain&| |IntegralDomain&| |DifferentialRing&| |Monoid&| |Reference|
  |UnaryRecursiveAggregate&| |OrderedSet&| |AbelianGroup&| |Algebra&|
  |Module&| |Ring&| |StringAggregate&| |AbelianMonoid&|
  |ExtensibleLinearAggregate&| |PositiveInteger| |StreamAggregate&|
  |IndexedString| |IndexedList| |ListAggregate&| |LinearAggregate&|
  |Character| |String| |NonNegativeInteger| |SingleInteger|
  |OneDimensionalArrayAggregate&| |FiniteLinearAggregate&| |PrimitiveArray|
  |Integer| |List| |OutputForm|))
 (dolist (con constr)
  (let ((c (concatenate 'string
             (|getEnv| "FRICAS") "/algebra/"
             (string (getdatabase con 'abbreviation)) "." |$lisp_bin_filetype|)))
    (format t "   preloading ~a.." c)
    (if (probe-file c)
     (progn
      (put con 'loaded c)
      (load c)
      (format t "loaded.~%"))
     (format t "skipped.~%"))))
 (format t "~%")))

; format of an entry in interp.daase:
;  (constructor-name
;    operationalist
;    constructormodemap
;    modemaps            -- this should not be needed. eliminate it.
;    object              -- the name of the object file to load for this con.
;    constructorcategory -- note that this info is the cadar of the
;         constructormodemap for domains and packages so it is stored
;         as NIL for them. it is valid for categories.
;    niladic             -- t or nil directly
;    abbreviation        -- kept directly
;    cosig               -- kept directly
;    constructorkind     -- kept directly
;    defaultdomain       -- a short list, for %i
;    ancestors           -- used to compute new category updates
;  )
(defun |interpOpen| ()
 "open the interpreter database and hash the keys"
 (let (constructors pos stamp dbstruct)
  (setq |$interp_stream| (open (DaaseName "interp.daase")))
  (setq stamp (read |$interp_stream|))
  (unless (equal stamp |$interp_stream_stamp|)
   (format t "   Re-reading interp.daase")

   ; Clean old data
   (do-symbols (symbol)
      (when (get symbol 'database)
         (setf (get symbol 'database) nil))
      (when (get symbol 'abbreviationfor)
         (setf (get symbol 'abbreviationfor) nil)))
   (setq |$all_constructors| nil)

   (setq |$interp_stream_stamp| stamp)
   (setq pos (car stamp))
   (file-position |$interp_stream| pos)
   (setq constructors (read |$interp_stream|))
   (dolist (item constructors)
    (setq item (unsqueeze item))
    (setq |$all_constructors| (adjoin (first item) |$all_constructors|))
    (setq dbstruct (make-database))
    (setf (get (car item) 'database) dbstruct)
    (setf (database-operationalist dbstruct) (second item))
    (setf (database-constructormodemap dbstruct) (third item))
    (setf (database-modemaps dbstruct) (fourth item))
    (setf (database-object dbstruct) (fifth item))
    (setf (database-constructorcategory dbstruct) (sixth item))
    (setf (database-niladic dbstruct) (seventh item))
    (setf (database-abbreviation dbstruct) (eighth item))
    (setf (get (eighth item) 'abbreviationfor) (first item)) ;invert
    (setf (database-cosig dbstruct) (ninth item))
    (setf (database-constructorkind dbstruct) (tenth item))
    (setf (database-ancestors dbstruct) (nth 11 item))))
  (format t "~&")))

; this is an initialization function for the constructor database
; it sets up 2 hash tables, opens the database and hashes the index values

; there is a slight asymmetry in this code. sourcefile information for
; system files is only the filename and extension. for user files it
; contains the full pathname. when the database is first opened the
; sourcefile slot contains system names. the lookup function
; has to prefix the $spadroot information if the directory-namestring is
; null (we don't know the real root at database build time).
; a object-hash table is set up to look up nrlib and ao information.
; this slot is empty until a user does a )library call. we remember
; the location of the nrlib or ao file for the users local library
; at that time. a NIL result from this probe means that the
; library is in the system-specified place. when we get into multiple
; library locations this will also contain system files.


; format of an entry in browse.daase:
; ( constructorname
;     sourcefile
;     constructorform
;     documentation
;     predicates
; )

(defun |browseOpen| ()
 "open the constructor database and hash the keys"
 (let (constructors pos stamp dbstruct)
  (setq |$browse_stream| (open (DaaseName "browse.daase")))
  (setq stamp (read |$browse_stream|))
  (unless (equal stamp |$browse_stream_stamp|)
   (format t "   Re-reading browse.daase")
   (setq |$browse_stream_stamp| stamp)
   (setq pos (car stamp))
   (file-position |$browse_stream| pos)
   (setq constructors (read |$browse_stream|))
   (dolist (item constructors)
    (setq item (unsqueeze item))
    (unless (setq dbstruct (get (car item) 'database))
     (format t "browseOpen:~%")
     (format t "the browse database contains a constructor ~a~%" item)
     (format t "that is not in the interp.daase file. we cannot~%")
     (format t "get the database structure for this constructor and~%")
     (warn "will create a new one~%")
     (setf (get (car item) 'database) (setq dbstruct (make-database)))
     (setq |$all_constructors| (adjoin item |$all_constructors|)))
    (setf (database-sourcefile dbstruct) (second item))
    (setf (database-constructorform dbstruct) (third item))
    (setf (database-documentation dbstruct) (fourth item))
    (setf (database-predicates dbstruct) (fifth item))
    (setf (database-parents dbstruct) (sixth item))))
  (format t "~&")))

(defun |categoryOpen| ()
 "open category.daase and hash the keys"
 (let (pos keys stamp)
  (setq |$category_stream| (open (DaaseName "category.daase")))
  (setq stamp (read |$category_stream|))
  (unless (equal stamp |$category_stream_stamp|)
   (format t "   Re-reading category.daase")
   (setq |$category_stream_stamp| stamp)
   (setq pos (car stamp))
   (file-position |$category_stream| pos)
   (setq keys (read |$category_stream|))
   (setq |$has_category_hash| (make-hash-table :test #'equal))
   (dolist (item keys)
    (setq item (unsqueeze item))
    (setf (gethash (first item) |$has_category_hash|) (second item))))
  (format t "~&")))

(defun |operationOpen| ()
 "read operation database and hash the keys"
 (let (operations pos stamp)
  (setq |$operation_stream| (open (DaaseName "operation.daase")))
  (setq stamp (read |$operation_stream|))
  (unless (equal stamp |$operation_stream_stamp|)
   (format t "   Re-reading operation.daase")
   (setq |$operation_stream_stamp| stamp)
   (setq pos (car stamp))
   (file-position |$operation_stream| pos)
   (setq operations (read |$operation_stream|))

   ; Clean old data
   (setq |$operation_hash| (make-hash-table))
   (setq |$all_operations| nil)

   (dolist (item operations)
    (setq item (unsqueeze item))
    (setf (gethash (car item) |$operation_hash|) (cdr item))))
  (format t "~&")))

(defun addoperations (constructor oldmaps)
 "add ops from a )library domain to |$operation_hash|"
 (declare (special |$operation_hash|))
 (dolist (map oldmaps) ; out with the old
  (let (oldop op)
   (setq op (car map))
   (setq oldop (getdatabase op 'operation))
   (setq oldop (delete (cdr map) oldop :test #'equal))
   (setf (gethash op |$operation_hash|) oldop)))
 (dolist (map (getdatabase constructor 'modemaps)) ; in with the new
  (let (op newmap)
   (setq op (car map))
   (setq newmap (getdatabase op 'operation))
   (setf (gethash op |$operation_hash|) (cons (cdr map) newmap)))))

(defun showdatabase (constructor)
 (format t "~&~a: ~a~%" 'constructorkind
  (getdatabase constructor 'constructorkind))
 (format t "~a: ~a~%" 'cosig
  (getdatabase constructor 'cosig))
 (format t "~a: ~a~%" 'operation
  (getdatabase constructor 'operation))
 (format t "~a: " 'constructormodemap)
  (pprint (getdatabase constructor 'constructormodemap))
 (format t "~&~a: " 'constructorcategory)
  (pprint (getdatabase constructor 'constructorcategory))
 (format t "~&~a: " 'operationalist)
  (pprint (getdatabase constructor 'operationalist))
 (format t "~&~a: " 'modemaps)
  (pprint (getdatabase constructor 'modemaps))
 (format t "~&~a: ~a~%" 'hascategory
  (getdatabase constructor 'hascategory))
 (format t "~a: ~a~%" 'object
  (getdatabase constructor 'object))
 (format t "~a: ~a~%" 'niladic
  (getdatabase constructor 'niladic))
 (format t "~a: ~a~%" 'abbreviation
  (getdatabase constructor 'abbreviation))
 (format t "~a: ~a~%" 'constructor?
  (getdatabase constructor 'constructor?))
 (format t "~a: ~a~%" 'constructor
  (getdatabase constructor 'constructor))
 (format t "~a: ~a~%" 'defaultdomain
  (getdatabase constructor 'defaultdomain))
 (format t "~a: ~a~%" 'ancestors
  (getdatabase constructor 'ancestors))
 (format t "~a: ~a~%" 'sourcefile
  (getdatabase constructor 'sourcefile))
 (format t "~a: ~a~%" 'constructorform
  (getdatabase constructor 'constructorform))
 (format t "~a: ~a~%" 'constructorargs
  (getdatabase constructor 'constructorargs))
 (format t "~a: " 'predicates)
  (pprint (getdatabase constructor 'predicates))
 (format t "~&~a: ~a~%" 'documentation
  (getdatabase constructor 'documentation))
 (format t "~a: ~a~%" 'parents
  (getdatabase constructor 'parents)))

(defun setdatabase (constructor key value)
 (let (struct)
  (when (symbolp constructor)
   (unless (setq struct (get constructor 'database))
    (setq struct (make-database))
    (setf (get constructor 'database) struct))
   (case key
    (abbreviation
     (setf (database-abbreviation struct) value)
     (when (symbolp value)
      (setf (get value 'abbreviationfor) constructor)))
    (niladic
     (setf (database-niladic struct) value))
    (cosig
     (setf (database-cosig struct) value))
    (constructormodemap
     (setf (database-constructormodemap struct) value))
    (constructorcategory
     (setf (database-constructorcategory struct) value))
    (constructorkind
     (setf (database-constructorkind struct) value))))))

(defun deldatabase (constructor key)
  (when (symbolp constructor)
   (case key
    (abbreviation
     (setf (get constructor 'abbreviationfor) nil)))))

(defun getdatabase (constructor key)
 (declare (special $spadroot) (special |$miss|))
 (when (eq |$miss| t) (format t "getdatabase call: ~20a ~a~%" constructor key))
 (let (data table stream ignore struct)
  (declare (ignore ignore))
  (when (or (symbolp constructor)
          (and (eq key 'hascategory) (pairp constructor)))
  (case key
; note that abbreviation, constructorkind and cosig are heavy hitters
; thus they occur first in the list of things to check
   (abbreviation
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
      (setq data (database-abbreviation struct))))
   (constructorkind
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-constructorkind struct))))
   (cosig
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-cosig struct))))
   (operation
    (setq stream |$operation_stream|)
    (setq data (gethash constructor |$operation_hash|)))
   (constructormodemap
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-constructormodemap struct))))
   (constructorcategory
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-constructorcategory struct))
     (when (null data) ;domain or package then subfield of constructormodemap
      (setq data (cadar (getdatabase constructor 'constructormodemap))))))
   (operationalist
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-operationalist struct))))
   (modemaps
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-modemaps struct))))
   (hascategory
    (setq table  |$has_category_hash|)
    (setq stream |$category_stream|)
    (setq data (gethash constructor table)))
   (object
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-object struct))))
   (asharp?
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-object struct))))
   (niladic
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-niladic struct))))
   (constructor?
    (when (setq struct (get constructor 'database))
      (setq data (when (database-operationalist struct) t))))
   (superdomain ; only 2 superdomains in the world
    (case constructor
     (|NonNegativeInteger|
      (setq data '((|Integer|) (IF (< |#1| 0) |false| |true|))))
     (|PositiveInteger|
      (setq data '((|NonNegativeInteger|) (< 0 |#1|))))))
   (constructor
    (when (setq data (get constructor 'abbreviationfor))))
   (defaultdomain
    (setq data (cadr (assoc constructor |$defaultdomain_list|))))
   (ancestors
    (setq stream |$interp_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-ancestors struct))))
   (sourcefile
    (setq stream |$browse_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-sourcefile struct))))
   (constructorform
    (setq stream |$browse_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-constructorform struct))))
   (constructorargs
    (setq data (cdr (getdatabase constructor 'constructorform))))
   (predicates
    (setq stream |$browse_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-predicates struct))))
   (documentation
    (setq stream |$browse_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-documentation struct))))
   (parents
    (setq stream |$browse_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-parents struct))))
   (users
    (setq stream |$browse_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-users struct))))
   (dependents
    (setq stream |$browse_stream|)
    (when (setq struct (get constructor 'database))
     (setq data (database-dependents struct))))
   (otherwise  (warn "~%(GETDATABASE ~a ~a) failed~%" constructor key)))
  (when (numberp data)                 ;fetch the real data
   (when |$miss| (format t "getdatabase miss: ~20a ~a~%" constructor key))
   (file-position stream data)
   (setq data (unsqueeze (read stream)))
   (case key ; cache the result of the database read
    (operation           (setf (gethash constructor |$operation_hash|) data))
    (hascategory         (setf (gethash constructor |$has_category_hash|) data))
    (constructorkind     (setf (database-constructorkind struct) data))
    (cosig               (setf (database-cosig struct) data))
    (constructormodemap  (setf (database-constructormodemap struct) data))
    (constructorcategory (setf (database-constructorcategory struct) data))
    (operationalist      (setf (database-operationalist struct) data))
    (modemaps            (setf (database-modemaps struct) data))
    (object              (setf (database-object struct) data))
    (niladic             (setf (database-niladic struct) data))
    (abbreviation        (setf (database-abbreviation struct) data))
    (constructor         (setf (database-constructor struct) data))
    (ancestors           (setf (database-ancestors struct) data))
    (constructorform     (setf (database-constructorform struct) data))
    (predicates          (setf (database-predicates struct) data))
    (documentation       (setf (database-documentation struct) data))
    (parents             (setf (database-parents struct) data))
    (users               (setf (database-users struct) data))
    (dependents          (setf (database-dependents struct) data))
    (sourcefile          (setf (database-sourcefile struct) data))))
   (case key ; fixup the special cases
    (sourcefile
     (when (and data (string= (directory-namestring data) "")
             (string= (pathname-type data) "spad"))
      (setq data
       (concatenate 'string $spadroot "/../../src/algebra/" data))))
    (asharp?                               ; is this asharp code?
     (if (consp data)
      (setq data (cdr data))
      (setq data nil)))
    (object                                ; fix up system object pathname
     (if (consp data)
       (setq data
             (if (string= (directory-namestring (car data)) "")
                 (concatenate 'string $spadroot "/algebra/" (car data)
                                        "." |$lisp_bin_filetype|)
               (car data)))
      (when (and data (string= (directory-namestring data) ""))
       (setq data (concatenate 'string $spadroot "/algebra/" data
                                          "." |$lisp_bin_filetype|)))))))
  data))

; localdatabase tries to find files in the order of:
;  NRLIB/index.KAF
;  .asy
;  .ao, then asharp to .asy

(defun localdatabase (filelist options &optional (make-database? nil))
 "read a local filename and update the hash tables"
 (labels (
  (processOptions (options)
   (let (only dir noexpose)
    (when (setq only (assoc '|only| options))
     (setq options (delete only options :test #'equal))
     (setq only (cdr only)))
    (when (setq dir (assoc '|dir| options))
     (setq options (delete dir options :test #'equal))
     (setq dir (second dir))
     (when (null dir)
      (|sayKeyedMsg| 'S2IU0002 nil) ))
    (when (setq noexpose (assoc '|noexpose| options))
     (setq options (delete noexpose options :test #'equal))
     (setq noexpose 't) )
    (when options
     (format t "   Ignoring unknown )library option: ~a~%" options))
    (values only dir noexpose)))

  (processDir (dirarg thisdir)
      (let (nrlibdirs skipasos aos)

      (chdir (string dirarg))
      #-:GCL
      (setq nrlibdirs (directory "*.NRLIB/index.KAF"))

      #| directory in GCL (at least gcl-2.6.8) on Windows is buggy,
      it can not handle pathnames having wildcards in the middle,
      so we need a workaround.  |#
      #+:GCL
      (setq nrlibdirs
           (mapcar #'(lambda (f)
                          (concatenate 'string (namestring f)
                                         "/index.KAF"))
                   (directory "*.NRLIB")))

      (setq asys (directory "*.asy"))

      (setq skipasos (mapcan #'pathname-name asys))

      (chdir thisdir)
      (values
          nrlibdirs
          asys
          (mapcan #'(lambda (f)
              (when (and (string= (pathname-type f) "ao")
                         (not (member (pathname-name f)
                                       skipasos :test #'string=)))
                    (list (namestring f))))
           aos)
          nil
    ))))

 (let (thisdir nrlibs asos asys libs object only dir key
      (|$forceDatabaseUpdate| t) noexpose)
  (declare (special |$forceDatabaseUpdate|))
  ;;; FIXME: make this _really_ portable
  (setq thisdir (namestring (truename "./")))
  (setq noexpose nil)
  (multiple-value-setq (only dir noexpose) (processOptions options))
     ;don't force exposure during database build
  (if make-database? (setq noexpose t))
  (when dir (multiple-value-setq (nrlibs asys asos libs) (processDir dir thisdir)))
  (dolist (file filelist)
   (let ((filename (pathname-name (string file)))
         (namedir (directory-namestring (string file))))
    (unless namedir (setq thisdir (concatenate 'string thisdir "/")))
    (cond
     ((setq file (probe-file
       (concatenate 'string namedir filename ".NRLIB/"
                    |$index_filename|)))
      (push (namestring file) nrlibs))
     ((setq file (probe-file
       (concatenate 'string namedir filename ".asy")))
      (push (namestring file) asys))
     ((setq file (probe-file
       (concatenate 'string namedir filename ".ao")))
      (push (namestring file) asos))
     ('else (format t "   )library cannot find the file ~a.~%" filename)))))

  (dolist (file (nreverse nrlibs))
   (setq key (pathname-name (first (last (pathname-directory file)))))
   (setq object (concatenate 'string (directory-namestring file) key))
   (localnrlib key file object make-database? noexpose))
  (dolist (file (nreverse asys))
   (setq object
    (concatenate 'string (directory-namestring file) (pathname-name file)))
   (localasy (|astran| file) object only make-database? noexpose))
  (dolist (file (nreverse asos))
   (setq object
    (concatenate 'string (directory-namestring file) (pathname-name file)))
   (asharp file)
   (setq file (|astran| (concatenate 'string (pathname-name file) ".asy")))
   (localasy file object only make-database? noexpose))
  (|clearConstructorCaches|))
))

(defun localasy (asy object only make-database? noexpose)
 "given an alist from the asyfile and the objectfile update the database"
 (labels (
  (fetchdata (alist index)
     (cdr (assoc index alist :test #'string=))))
  (let (cname kind key alist (systemdir? nil) oldmaps asharp-name dbstruct abbrev)
   (set-file-getter object)  ; sets the autoload property for G-object
   (dolist (domain asy)
     (setq key (first domain))
     (setq alist (rest domain))
     (setq asharp-name
           (foam::axiomxl-global-name (pathname-name object) key
                                     (lassoc '|typeCode| alist)))
     (if (< (length alist) 4) ;we have a naked function object
         (let ((opname key)
               (modemap (car (LASSOC '|modemaps| alist))) )
           (setq oldmaps (getdatabase opname 'operation))
           (setf (gethash opname |$operation_hash|)
                 (adjoin (subst asharp-name opname (cdr modemap))
                         oldmaps :test #'equal))
           (asharpMkAutoloadFunction object asharp-name))
       (when (if (null only) (not (eq key '%%)) (member key only))
        (setq |$all_operations| nil)        ; force this to recompute
        (setq oldmaps (getdatabase key 'modemaps))
        (setq dbstruct (make-database))
        (setf (get key 'database) dbstruct)
        (setq |$all_constructors| (adjoin key |$all_constructors|))
        (setf (database-constructorform dbstruct)
         (fetchdata alist "constructorForm"))
        (setf (database-constructorkind dbstruct)
         (fetchdata alist "constructorKind"))
        (setf (database-constructormodemap dbstruct)
         (fetchdata alist "constructorModemap"))
        (unless (setf (database-abbreviation dbstruct)
                      (fetchdata alist "abbreviation"))
                (setf (database-abbreviation dbstruct) key)) ; default
        (setq abbrev (database-abbreviation dbstruct))
        (setf (get abbrev 'abbreviationfor) key)
        (setf (database-constructorcategory dbstruct)
         (fetchdata alist "constructorCategory"))
        (setf (database-sourcefile dbstruct)
         (fetchdata alist "sourceFile"))
        (setf (database-operationalist dbstruct)
         (fetchdata alist "operationAlist"))
        (setf (database-modemaps dbstruct)
         (fetchdata alist "modemaps"))
        (setf (database-documentation dbstruct)
         (fetchdata alist "documentation"))
        (setf (database-predicates dbstruct)
         (fetchdata alist "predicates"))
        (setf (database-niladic dbstruct)
         (fetchdata alist "NILADIC"))
        (addoperations key oldmaps)
        (setq cname  (|opOf| (database-constructorform dbstruct)))
        (setq kind (database-constructorkind dbstruct))
        (if (null noexpose) (|setExposeAddConstr| (cons cname nil)))
        (unless make-database?
         (|updateDatabase| cname) ;makes many hashtables???
         (|installConstructor| cname)
          ;; following can break category database build
         (if (eq kind '|category|)
             (setf (database-ancestors dbstruct)
                   (fetchdata alist "ancestors")))
         (if (eq kind '|domain|)
             (dolist (pair (cdr (assoc "ancestors" alist :test #'string=)))
                     (setf (gethash (cons cname (caar pair)) |$has_category_hash|)
                           (cdr pair))))
         (if |$InteractiveMode| (setq |$CategoryFrame| |$EmptyEnvironment|)))
        (setf (database-cosig dbstruct)
         (cons nil (mapcar #'|categoryForm?|
          (cddar (database-constructormodemap dbstruct)))))
        (setf (database-object dbstruct) (cons object asharp-name))
        (if (eq kind '|category|)
         (asharpMkAutoLoadCategory object cname asharp-name
          (database-cosig dbstruct))
         (asharpMkAutoLoadFunctor object cname asharp-name
          (database-cosig dbstruct)))
        (|sayKeyedMsg| 'S2IU0001 (list cname object))))))))

(defun localnrlib (key nrlib object make-database? noexpose)
 "given a string pathname of an index.KAF and the object update the database"
 (labels (
  (fetchdata (alist in index)
   (let (pos)
    (setq pos (third (assoc index alist :test #'string=)))
    (when pos
     (file-position in pos)
     (read in)))))
 (let (alist kind (systemdir? nil) pos constructorform oldmaps abbrev dbstruct)
  (with-open-file (in nrlib)
   (file-position in (read in))
   (setq alist (read in))
   (setq pos (third (assoc "constructorForm" alist :test #'string=)))
   (file-position in pos)
   (setq constructorform (read in))
   (setq key (car constructorform))
   (setq oldmaps (getdatabase key 'modemaps))
   (setq dbstruct (make-database))
   (setq |$all_constructors| (adjoin key |$all_constructors|))
   (setf (get key 'database) dbstruct) ; store the struct, side-effect it...
   (setf (database-constructorform dbstruct) constructorform)
   (setq |$all_operations| nil)   ; force this to recompute
   (setf (database-object dbstruct) object)
   (setq abbrev
     (intern (pathname-name (first (last (pathname-directory object))))))
   (setf (database-abbreviation dbstruct) abbrev)
   (setf (get abbrev 'abbreviationfor) key)
   (setf (database-operationalist dbstruct) nil)
   (setf (database-operationalist dbstruct)
    (fetchdata alist in "operationAlist"))
   (setf (database-constructormodemap dbstruct)
    (fetchdata alist in "constructorModemap"))
   (setf (database-modemaps dbstruct) (fetchdata alist in "modemaps"))
   (setf (database-sourcefile dbstruct) (fetchdata alist in "sourceFile"))
   (when make-database?
    (setf (database-sourcefile dbstruct)
     (file-namestring  (database-sourcefile dbstruct))))
   (setf (database-constructorkind dbstruct)
    (setq kind (fetchdata alist in "constructorKind")))
   (setf (database-constructorcategory dbstruct)
    (fetchdata alist in "constructorCategory"))
   (setf (database-documentation dbstruct)
    (fetchdata alist in "documentation"))
   (setf (database-predicates dbstruct)
    (fetchdata alist in "predicates"))
   (setf (database-niladic dbstruct)
    (when (fetchdata alist in "NILADIC") t))
  (addoperations key oldmaps)
  (unless make-database?
   (if (eq kind '|category|)
       (setf (database-ancestors dbstruct)
             (SUBLISLIS |$FormalMapVariableList| (cdr constructorform) (fetchdata alist in "ancestors"))))
   (|updateDatabase| key) ;makes many hashtables???
   (|installConstructor| key) ;used to be key cname ...
   (|updateCategoryTable| key kind)
   (if |$InteractiveMode| (setq |$CategoryFrame| |$EmptyEnvironment|)))
  (setf (database-cosig dbstruct)
    (cons nil (mapcar #'|categoryForm?|
     (cddar (database-constructormodemap dbstruct)))))
  (remprop key 'loaded)
  (if (null noexpose) (|setExposeAddConstr| (cons key nil)))
  (setf (symbol-function key) ; sets the autoload property for cname
    #'(lambda (&rest args)
     (unless (get key 'loaded)
      (|startTimingProcess| '|load|)
      (|loadLibNoUpdate| key key object)) ; used to be cname key
     (apply key args)))
  (|sayKeyedMsg| 'S2IU0001 (list key object))))))


; making new databases consists of:
;  1) reset all of the system hash tables
;  1a) set up Union, Record and Mapping
;  2) map )library across all of the system files (fills the databases)
;  3) loading some normally autoloaded files
;  4) making some database entries that are computed (like ancestors)
;  5) writing out the databases
; note that this process should be done in a clean image
; followed by a rebuild of the system image to include
; the new index pointers (e.g. $interp_stream_stamp)
; the system will work without a rebuild but it needs to
; re-read the databases on startup. rebuilding the system
; will cache the information into the image and the databases
; are opened but not read, saving considerable startup time.
; also note that the order the databases are written out is
; critical. interp.daase depends on prior computations and has
; to be written out last.

(defun make-databases (dirlist br_data)
 (labels (
    ;; these are types which have no library object associated with them.
    ;; we store some constructed data to make them perform like library
    ;; objects
  (withSpecialConstructors ()
   ; Category
   (setf (get '|Category| 'database)
     (make-database :operationalist nil :niladic t))
   (push '|Category| |$all_constructors|)
   ; UNION
   (setf (get '|Union| 'database)
     (make-database :operationalist nil :constructorkind '|domain|))
   (push '|Union| |$all_constructors|)
   ; RECORD
   (setf (get '|Record| 'database)
    (make-database :operationalist nil :constructorkind '|domain|))
   (push '|Record| |$all_constructors|)
   ; MAPPING
   (setf (get '|Mapping| 'database)
    (make-database :operationalist nil :constructorkind '|domain|))
   (push '|Mapping| |$all_constructors|)
   ; ENUMERATION
   (setf (get '|Enumeration| 'database)
    (make-database :operationalist nil :constructorkind '|domain|))
   (push '|Enumeration| |$all_constructors|)
   )
  (final-name (root)
    (format nil "~a.daase" root))
  )
 (let (d)
  (do-symbols (symbol)
   (when (get symbol 'database)
    (setf (get symbol 'database) nil)))
  (setq |$has_category_hash| (make-hash-table :test #'equal))
  (setq |$operation_hash| (make-hash-table))
  (setq |$all_constructors| nil)
  (setq |$compress_vector| nil)
  (setq |$all_operations| nil)
  (withSpecialConstructors)
  (localdatabase nil
     (list (list '|dir| (namestring (truename "./")) ))
     'make-database)
  (dolist (dir dirlist)
          (localdatabase nil
                         (list (list '|dir|
                                     (namestring (truename
                                                  (format nil "./~a"
                                                          dir)))))
                         'make-database))
;browse.daase
  (if br_data
      (|save_browser_data|))
  (write-compress)
  (if br_data
      (write-browsedb))
  (write-operationdb)
 ; note: genCategoryTable creates a new |$has_category_hash| table
 ; this smashes the existing table and regenerates it.
 ; write-categorydb does getdatabase calls to write the new information
  (write-categorydb)
  (dolist (con (|allConstructors|))
   (let (dbstruct)
     (when (setq dbstruct (get con 'database))
           (setf (database-cosig dbstruct)
                 (cons nil (mapcar #'|categoryForm?|
                                   (cddar (database-constructormodemap dbstruct)))))
           (when (and (|categoryForm?| con)
                      (= (length (setq d (|domainsOf| (list con) NIL))) 1))
                 (setq d (caar d))
                 (when (= (length d) (length (|getConstructorForm| con)))
                       (format t "   ~a has a default domain of ~a~%" con (car d))
                       (setf (database-defaultdomain dbstruct) (car d)))))))
          ; note: genCategoryTable creates |$ancestors_hash|. write-interpdb
          ; does gethash calls into it rather than doing a getdatabase call.
  (write-interpdb)
  (|createInitializers|)
  (when (probe-file (final-name "compress"))
        (delete-file (final-name "compress")))
  (rename-file "compress.build" (final-name "compress"))
  (when (probe-file (final-name "interp"))
        (delete-file (final-name "interp")))
  (rename-file "interp.build" (final-name "interp"))
  (when (probe-file (final-name "operation"))
        (delete-file (final-name "operation")))
  (rename-file "operation.build" (final-name "operation"))
  (when (probe-file (final-name "browse"))
        (delete-file (final-name "browse")))
  (if br_data
        (rename-file "browse.build" (final-name "browse")))
  (when (probe-file (final-name "category"))
        (delete-file (final-name "category")))
  (rename-file "category.build"
               (final-name "category")))))

(defun DaaseName (name)
 (let (daase filename)
  (declare (special $spadroot))
  (if (setq daase (|getEnv| "DAASE"))
   (progn
    (setq filename  (concatenate 'string daase "/algebra/" name))
    (format t "   Using local database ~a.." filename))
   (setq filename (concatenate 'string $spadroot "/algebra/" name)))
  filename))

;; The compress database is special. It contains a list of symbols.
;; The character string name of a symbol in the other databases is
;; represented by a negative number. To get the real symbol back you
;; take the absolute value of the number and use it as a byte index
;; into the compress database. In this way long symbol names become
;; short negative numbers.

(defun |compressOpen| ()
 (let (lst stamp pos)
  (setq |$compress_stream|
    (open (DaaseName "compress.daase") :direction :input))
  (setq stamp (read |$compress_stream|))
  (unless (equal stamp |$compress_stream_stamp|)
   (format t "   Re-reading compress.daase")
   (setq |$compress_stream_stamp| stamp)
   (setq pos (car stamp))
   (file-position |$compress_stream| pos)
   (setq lst (read |$compress_stream|))
   (setq |$compress_vector_length| (car lst))
   (setq |$compress_vector|
     (make-array (car lst) :initial-contents (cdr lst))))))

(defun write-compress ()
 (let (compresslist masterpos out)
  (close |$compress_stream|)
  (setq out (open "compress.build" :direction :output :if-exists :supersede))
  (princ "                              " out)
  #+:GCL (force-output out)
  (setq masterpos (file-position out))
  (setq compresslist
        (append (|allConstructors|) (|allOperations|)))
  (push "algebra" compresslist)
  (push "failed" compresslist)
  (push 'signature compresslist)
  (push '|ofType| compresslist)
  (push '|Join| compresslist)
  (push 'and compresslist)
  (push '|nobranch| compresslist)
  (push 'category compresslist)
  (push '|category| compresslist)
  (push '|domain| compresslist)
  (push '|package| compresslist)
  (push 'attribute compresslist)
  (push '|isDomain| compresslist)
  (push '|ofCategory| compresslist)
  (push '|Union| compresslist)
  (push '|Record| compresslist)
  (push '|Mapping| compresslist)
  (push '|Enumeration| compresslist)
  ;;; dummy zero element
  (push 0 compresslist)
  (setq |$compress_vector_length| (length compresslist))
  (setq |$compress_vector|
    (make-array |$compress_vector_length| :initial-contents compresslist))
  (print (cons (length compresslist) compresslist) out)
  #+:GCL (force-output out)
  (file-position out 0)
  (print (cons masterpos (get-universal-time)) out)
  #+:GCL (force-output out)
  (close out)))

(defun write-interpdb ()
 "build interp.daase from hash tables"
 (declare (special |$ancestors_hash|))
 (let (opalistpos modemapspos cmodemappos master masterpos obj (*print-pretty* t)
        concategory categorypos kind niladic cosig abbrev defaultdomain
        ancestors ancestorspos out)
  (print "building interp.daase")
  (setq out (open "interp.build" :direction :output :if-exists :supersede))

  ;;  We reserve some space at the top of the file for the key-time pair
  ;;  We will overwrite these spaces just before we close the file.
  (princ "                              " out)
  #+:GCL (force-output out)

  ;;  We loop across the list of constructors dumping the data and
  ;;  remembering the byte positions in a key-value pair table.
  (dolist (constructor (|allConstructors|))
   (let (struct)
    (setq struct (get constructor 'database))
    (setq opalistpos (file-position out))
    (print (squeeze (database-operationalist struct)) out)
    (setq cmodemappos (file-position out))
    (print (squeeze (database-constructormodemap struct)) out)
    (setq modemapspos (file-position out))
    (print (squeeze (database-modemaps struct)) out)
    (let ((dob (database-object struct)))
       (setq obj
             (cond
                 ((consp dob) ; if asharp code
                      (cons (pathname-name (car dob))
                            (cdr dob)))
                 (dob
                      (pathname-name
                          (first (last (pathname-directory dob)))))
                 (t "NIL")))
    )
    (setq concategory (squeeze (database-constructorcategory struct)))
    (if concategory  ; if category then write data else write nil
     (progn
      (setq categorypos (file-position out))
      (print concategory out))
     (setq categorypos nil))
    (setq niladic (database-niladic struct))
    (setq abbrev (database-abbreviation struct))
    (setq cosig (database-cosig struct))
    (setq kind (database-constructorkind struct))
    (setq defaultdomain (database-defaultdomain struct))
    (setq ancestors (squeeze (gethash constructor |$ancestors_hash|)))
    (if ancestors
     (progn
      (setq ancestorspos (file-position out))
      (print ancestors out))
     (setq ancestorspos nil))
    (push (list constructor opalistpos cmodemappos modemapspos
      obj categorypos niladic abbrev cosig kind defaultdomain
      ancestorspos) master)))
  #+:GCL (force-output out)
  (setq masterpos (file-position out))
  (print (mapcar #'squeeze master) out)
  #+:GCL (force-output out)

  ;; Write the timestamp
  (file-position out 0)
  (print (cons masterpos (get-universal-time)) out)
  #+:GCL (force-output out)

  (close out)))

(defun write-browsedb ()
 "make browse.daase from hash tables"
 (let (master masterpos src formpos docpos predpos (*print-pretty* t) out)
  (print "building browse.daase")
  (setq out (open "browse.build" :direction :output :if-exists :supersede))
  (princ "                              " out)
  #+:GCL (force-output out)
  (dolist (constructor (|allConstructors|))
   (let (struct)
    (setq struct (get constructor 'database))
     ; sourcefile is small. store the string directly
    (setq src (database-sourcefile struct))
    (setq formpos (file-position out))
    (print (squeeze (database-constructorform struct)) out)
    (setq docpos (file-position out))
    (print (database-documentation struct) out)
    (setq predpos (file-position out))
    (print (squeeze (database-predicates struct)) out)
    (push (list constructor src formpos docpos predpos) master)))
  #+:GCL (force-output out)
  (setq masterpos (file-position out))
  (print (mapcar #'squeeze master) out)
  #+:GCL (force-output out)
  (file-position out 0)
  (print (cons masterpos (get-universal-time)) out)
  #+:GCL (force-output out)
  (close out)))

(defun write-categorydb ()
 "make category.daase from scratch. contains the |$has_category_hash| table"
 (let (out master pos (*print-pretty* t))
  (print "building category.daase")
  (|genCategoryTable|)
  (setq out (open "category.build" :direction :output :if-exists :supersede))
  (princ "                              " out)
  #+:GCL (force-output out)
  (maphash #'(lambda (key value)
    (if (or (null value) (eq value t))
     (setq pos value)
     (progn
      (setq pos (file-position out))
      (print (squeeze value) out)))
     (push (list key pos) master))
     |$has_category_hash|)
  #+:GCL (force-output out)
  (setq pos (file-position out))
  (print (mapcar #'squeeze master) out)
  #+:GCL (force-output out)
  (file-position out 0)
  (print (cons pos (get-universal-time)) out)
  #+:GCL (force-output out)
  (close out)))


(defun unsqueeze (expr) expr)

(defun squeeze (expr) expr)

(defun write-operationdb ()
 (let (pos master out (*print-pretty* t))
  (print "building operation.daase")
  (setq out (open "operation.build" :direction :output :if-exists :supersede))
  (princ "                              " out)
  #+:GCL (force-output out)
  (maphash #'(lambda (key value)
   (setq pos (file-position out))
   (print (squeeze value) out)
   (push (cons key pos) master))
   |$operation_hash|)
  #+:GCL (force-output out)
  (setq pos (file-position out))
  (print (mapcar #'squeeze master) out)
  (file-position out 0)
  (print (cons pos (get-universal-time)) out)
  #+:GCL (force-output out)
  (close out)))

(defun |allConstructors| ()
 (declare (special |$all_constructors|))
 |$all_constructors|)

(defun |allOperations| ()
 (declare (special |$all_operations|))
 (unless |$all_operations|
  (maphash #'(lambda (k v) (declare (ignore v)) (push k |$all_operations|))
    |$operation_hash|))
 |$all_operations|)

; the variable NOPfuncall is a funcall-able object that is a dummy
; initializer for libfricas aldor domains.
(defvar NOPfuncall (cons 'identity nil))

(defun |createInitializers| ()
;; since libfricas is now built with -name=fricas following unnecessary
;; (dolist (con (|allConstructors|))
;;   (let ((sourcefile (getdatabase con 'sourcefile)))
;;     (if sourcefile
;;       (set (foam::axiomxl-file-init-name (pathname-name sourcefile))
;;             NOPfuncall))))
 (set (foam::axiomxl-file-init-name "fricas") NOPfuncall)
;; (set (foam::axiomxl-file-init-name "axclique") NOPfuncall)
 (set (foam::axiomxl-file-init-name "filecliq") NOPfuncall)
 (set (foam::axiomxl-file-init-name "attrib") NOPfuncall)
 (|createInitializers2|))

;; following needs to happen inside restart since $FRICAS may change
(defun |createInitializers2| ()
 (let ((asharprootlib (strconc (|getEnv| "FRICAS") "/aldor/lib/")))
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
;(defvar |$this_file| "no-file")

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
; (eval (cdr (assoc file-id (get name 'asharp-name) :test #'equal))))
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

(defun asharpMkAutoLoadFunctor (file cname asharp-name cosig)
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


(defun asharpMkAutoLoadCategory (file cname asharp-name cosig)
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

(defun asharpMkAutoLoadFunction (file asharpname)
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
         (filename (getdatabase cname 'object)))
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
                     (if (getdatabase bootname 'niladic)
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
