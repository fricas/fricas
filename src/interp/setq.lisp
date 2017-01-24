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


(setq copyrights '(
 "Copyright The Numerical Algorithms Group Limited 1991-94."
 "All rights reserved"
 "Certain derivative-work portions Copyright (C) 1998 by Leslie Lamport."
 "Portions (c) Copyright Taiichi Yuasa and Masami Hagiya, 1984."
 "All rights reserved"))

(in-package "BOOT")

(defvar |$standard| 't)
(defvar |$saturn| nil)

(setq |$printTimeIfTrue| nil)

(setq |nullstream| '|nullstream|)
(setq |nonnullstream| '|nonnullstream|)
(setq *print-escape* nil) ;; so stringimage doesn't escape idents?

;;; FIXME: do we need this?
#+(and :GCL :IEEE-FLOATING-POINT)
  (setq system:*print-nans* T)

(setq |$algebraOutputStream|
      (setq |$fortranOutputStream|
            (setq |$texOutputStream|
                  (setq |$formulaOutputStream|
                        (make-synonym-stream '*standard-output*)))))

(defvar |$frameMessages| nil)
(defvar |$displayStartMsgs| nil)

(setq |$localVars| ())  ;checked by isType

;; For the browser, used for building local databases when a user compiles
;; their own code.
(SETQ |$newConstructorList| nil)
(SETQ |$newConlist| nil)
(SETQ |$createLocalLibDb| nil)


;; These were originally in SPAD LISP

(setq |$interpOnly| nil)
(SETQ |$testingSystem| NIL)
(SETQ |$permitWhere| NIL)
(DEFPARAMETER |$bootStrapMode| NIL) ;; if true skip functor bodies
(SETQ |$bootstrapDomains| NIL)
(SETQ |$compileDontDefineFunctions| 'T)
(SETQ |$compileOnlyCertainItems| NIL)
(SETQ |$devaluateList| NIL)
(SETQ |$doNotCompressHashTableIfTrue| T)
(SETQ |$mutableDomains| NIL)     ; checked in DEFINE BOOT
(SETQ |$maxSignatureLineNumber| 0)
(SETQ |$functionLocations| NIL)
(SETQ |$functorLocalParameters| NIL) ; used in compSymbol
(SETQ |$insideCategoryPackageIfTrue| NIL)
(SETQ |$insideCompileBodyIfTrue| NIL)
(SETQ |$globalExposureGroupAlist| NIL)
(SETQ |$localExposureDataDefault|
  (VECTOR (LIST '|basic| '|categories|) NIL NIL))
(SETQ |$localExposureData|
  (VECTOR (LIST '|basic| '|categories|) NIL NIL))
(SETQ |$compilingInputFile| NIL)
(SETQ |$minivectorNames| NIL)
(setq |$ReadingFile| NIL)
(setq |$NonNullStream| "NonNullStream")
(setq |$NullStream| "NullStream")
(setq |$UninitializedStream| "UninitializedStream")
(setq |$domPvar| nil)
(defvar $dalymode nil "if true then leading paren implies lisp cmd")
(setq |$Newline| #\Newline)


(DEFPARAMETER $FUNNAME NIL)   ;; this and next used in COMP,TRAN,1
(DEFPARAMETER $FUNNAME_TAIL '(()))
(SETQ $SPAD_ERRORS (VECTOR 0 0 0))
(SETQ $OLDLINE NIL)  ;"used to output command lines"
(SETQ |$edit_file| NIL)
(DEFPARAMETER |$InteractiveMode| T)

(SETQ |$ruleSetsInitialized| NIL)

(SETQ |$returnNowhereFromGoGet| NIL)

(SETQ |$insideCanCoerceFrom| NIL)

(SETQ |$useCoerceOrCroak| T)

(SETQ |$abbreviateJoin| NIL)

(SETQ |$InterpreterMacroAlist|
      '((|%i| . (|complex| 0 1))
        (|%e| . (|exp| 1))
        (|%pi| . (|pi|))
        (|SF| . (|DoubleFloat|))
        (|%infinity| . (|infinity|))
        (|%plusInfinity| . (|plusInfinity|))
        (|%minusInfinity| . (|minusInfinity|))))

;; Common lisp control variables
;;(setq *load-verbose* nil)
(setq *print-array* nil)
(setq *print-pretty* t)
(setq *print-circle* nil)

;; $SYSCOMMANDS is now defined at the top of i-syscmd.boot

(SETQ |$systemCommands| '(
;;  COMMAND              USER LEVEL   - )set userlevel
   (|abbreviations|                  . |compiler|   )
   (|boot|                           . |development|)
   (|cd|                             . |interpreter|)
   (|clear|                          . |interpreter|)
   (|close|                          . |interpreter|)
   (|compiler|                       . |compiler|   )
   (|copyright|                      . |interpreter|)
   (|credits|                        . |interpreter|)
   (|display|                        . |interpreter|)
   (|edit|                           . |interpreter|)
   (|fin|                            . |development|)
   (|frame|                          . |interpreter|)
   (|help|                           . |interpreter|)
   (|history|                        . |interpreter|)
;; (|input|                          . |interpreter|)
   (|lisp|                           . |development|)
   (|library|                        . |interpreter|)
   (|load|                           . |interpreter|)
   (|ltrace|                         . |interpreter|)
   (|nopiles|                        . |interpreter|)
   (|piles|                          . |interpreter|)
   (|pquit|                          . |interpreter|)
   (|quit|                           . |interpreter|)
   (|read|                           . |interpreter|)
   (|set|                            . |interpreter|)
   (|show|                           . |interpreter|)
   (|spool|                          . |interpreter|)
   (|summary|                        . |interpreter|)
   (|synonym|                        . |interpreter|)
   (|system|                         . |interpreter|)
   (|trace|                          . |interpreter|)
   (|undo|                           . |interpreter|)
   (|what|                           . |interpreter|)
 ))

(SETQ |$noParseCommands| '(
    |boot|
    |copyright|
    |credits|
    |fin|
    |lisp|
    |piles|
    |pquit|
    |quit|
    |suspend|
    |synonym|
    |system|
    ))

(SETQ |$tokenCommands| '(
    |abbreviations|
    |cd|
    |clear|
    |close|
    |compiler|
    |depends|
    |display|
    |edit|
    |frame|
    |frame|
    |help|
    |history|
    |input|
    |library|
    |load|
    |ltrace|
    |nopiles|
    |read|
    |set|
    |spool|
    |undo|
    |what|
    |with|
    ))

;; following 2 variables are referenced by PREPARSE1

(defvar |$byConstructors| () "list of constructors to be compiled")
(defvar |$constructorsSeen| () "list of constructors found")

;; These are for the output routines in OUT BOOT

(SETQ $LINELENGTH 77)
(DEFPARAMETER $MARGIN 3)
(DEFCONST BLANK " ")
(DEFCONST UNDERBAR "_")
(SETQ |$fortranArrayStartingIndex| 0)

;; These were originally in INIT LISP

(SETQ |$forceDatabaseUpdate| NIL)  ;; see "load" function
(DEFPARAMETER |$functorForm| NIL)

(SETQ |$InitialCommandSynonymAlist| '(
       (|?|          . "what commands")
       (|apropos|    . "what things")
       (|cache|      . "set functions cache")
       (|cl|         . "clear")
       (|co|         . "compiler")
       (|d|          . "display")
       (|dep|        . "display dependents")
       (|dependents| . "display dependents")
       (|expose|     . "set expose add constructor")
       (|fortran|    . "set output fortran")
       (|h|          . "help")
       (|hd|         . "system hypertex &")
       (|kclam|      . "boot clearClams ( )")
       (|killcaches| . "boot clearConstructorAndLisplibCaches ( )")
       (|prompt|     . "set message prompt")
       (|recurrence| . "set functions recurrence")
       (|restore|    . "history )restore")
       (|save|       . "history )save")
       (|startGraphics|    .  "system $AXIOM/lib/viewman &")
       (|stopGraphics|     .  "lisp (|sockSendSignal| 2 15)")
       (|time|       . "set message time")
       (|type|       . "set message type")
       (|unexpose|   . "set expose drop constructor")
       (|version|    . "lisp (concat |$build_version| \" compiled at \" |$build_date|)")
       (|wc|         . "what categories")
       (|wd|         . "what domains")
       (|wp|         . "what packages")
       (|ws|         . "what synonyms")
))

(SETQ |$CommandSynonymAlist| (COPY |$InitialCommandSynonymAlist|))

(SETQ |$spadLibFT| 'NRLIB)

(SETQ |$updateCatTableIfTrue| NIL)

(DEFPARAMETER |$ConstructorCache| (MAKE-HASHTABLE 'ID))
(SETQ |$instantRecord| (MAKE-HASHTABLE 'ID))
(SETQ |$immediateDataSymbol| '|--immediateData--|)

(SETQ |$useIntegerSubdomain| 'T)

;; See CLAMMED BOOT for defs of following functions
(SETQ |$clamList| '(
  (|canCoerce| |hash| UEQUAL |count|)
  (|canCoerceFrom| |hash| UEQUAL |count|)
  (|coerceConvertMmSelection| |hash| UEQUAL |count|)
; (|getModemapsFromDatabase| |hash| UEQUAL |count|)
; (|getOperationAlistFromLisplib| |hash| UEQUAL |count|)
  (|isLegitimateMode| |hash| UEQUAL |count|)
  (|isValidType| |hash| UEQUAL |count|)
  (|resolveTT|   |hash| UEQUAL |count|)
  (|selectMms1| |hash| UEQUAL |count|)
  (|underDomainOf|   |hash| UEQUAL |count|)
  (|findRetractMms| |hash| UEQUAL |count|)
  (|getConstantFromDomain|  |hash| UEQUAL |count|)
  (|interpLookup| |hash| UEQUAL |count|)
;  (|isSubDomain|   |hash| UEQUAL |count|)
))

;; the following symbol holds the canonical "failed" value
(SETQ |$failed| "failed")

(SETQ |$constructorDataTable| NIL)

(SETQ |$univariateDomains| '(
    |UnivariatePolynomial|
    |UnivariateTaylorSeries|
    |UnivariateLaurentSeries|
    |UnivariatePuiseuxSeries|
    ))
(SETQ |$multivariateDomains| '(
    |MultivariatePolynomial|
    |DistributedMultivariatePolynomial|
    |HomogeneousDistributedMultivariatePolynomial|
    |GeneralDistributedMultivariatePolynomial|
    ))

(SETQ |$Primitives| '(|Union| |Mapping| |Record| |Enumeration|))

(SETQ |$DomainsWithoutLisplibs| '(
  CAPSULE |Union| |Record| |SubDomain| |Mapping| |Enumeration| |Domain| |Mode|))

(SETQ |$letAssoc| NIL)
        ;" used for trace of assignments in SPAD code -- see macro LETT"
(SETQ |$QuickCode| T)
         ;" controls generation of QREFELT etc."
(SETQ |$QuickLet| T)
         ;" controls generation of LET tracing."
(SETQ |$domainTraceNameAssoc| NIL)
        ;"alist of traced domains"
(SETQ |$tracedMapSignatures| ())
(SETQ |$highlightAllowed| 'T)
         ;" used in BRIGHTPRINT and is a )set variable"

(SETQ |$ConstructorNames| '(
  |SubDomain| |Union| |Record|
      ))
           ;" Used in isFunctor test, and compDefine "

(SETQ |$SpecialDomainNames| '(
  |add| CAPSULE |SubDomain| |Union| |Record|
      ))
                 ;" Used in isDomainForm, addEmptyCapsuleIfnecessary"

(SETQ |$CategoryNames| '(
   |Category| |CATEGORY| |RecordCategory| |Join| |EnumerationCategory|
   |UnionCategory|
      ))

(SETQ |$printStorageIfTrue| NIL) ;; storage info disabled in common lisp
(SETQ |$noEnv| NIL)
(SETQ |$evalDomain| NIL)

(SETQ |$SideEffectFreeFunctionList| '(
  |null| |case| |Zero| |One| \: |::| |has| |Mapping| |Record| |Union|
  |Enumeration| |elt| = |>| |>=| |<| |<=| MEMBER |is| |isnt| ATOM
))

(SETQ |$AnonymousFunction| '(|AnonymousFunction|))
(SETQ |$Any|   '(|Any|))
(SETQ |$BFtag| '|:BF:|)
(SETQ |$Boolean| '(|Boolean|))
(SETQ |$Category| '(|Category|))
(SETQ |$Exit|  '(|Exit|))

(SETQ |$OutputForm| '(|OutputForm|))
(SETQ |$Float| '(|Float|))
(SETQ |$DoubleFloat| '(|DoubleFloat|))

(SETQ |$Integer| '(|Integer|))
(SETQ |$ComplexInteger| (LIST '|Complex| |$Integer|))
(SETQ |$NegativeInteger| '(|NegativeInteger|))
(SETQ |$NonNegativeInteger| '(|NonNegativeInteger|))
(SETQ |$NonPositiveInteger| '(|NonPositiveInteger|))
(SETQ |$PositiveInteger| '(|PositiveInteger|))
(SETQ |$RationalNumber| '(|Fraction| (|Integer|)))
(SETQ |$String| '(|String|))
(SETQ |$Symbol| '(|Symbol|))
(SETQ |$Void|  '(|Void|))
(SETQ |$QuotientField| '|Fraction|)
(SETQ |$FunctionalExpression| '|Expression|)

;; Old names
(SETQ |$SmallInteger| '(|SingleInteger|))

;; New Names
(SETQ |$SingleFloat| '(|SingleFloat|))
(SETQ |$DoubleFloat| '(|DoubleFloat|))
(SETQ |$SingleInteger| '(|SingleInteger|))

(SETQ |$InteractiveFrame| (LIST (LIST NIL)))
(SETQ |$DomainsInScope| (LIST NIL))
(SETQ |$EmptyEnvironment| '((NIL)))
(SETQ |$EmptyMode| '|$EmptyMode|)
(SETQ |$NoValue| '|$NoValue|)
(SETQ |$NoValueMode| '|NoValueMode|)
(SETQ |$DummyFunctorNames| '(|Mapping|))
(SETQ |$form| NIL)
(SETQ |$EmptyVector| (VECTOR))
(SETQ |$Index| 0)
(SETQ |$true| ''T)
(SETQ |$false| NIL)
(DEFPARAMETER |$suffix| NIL)
(SETQ |$BasicPredicates| '(INTEGERP STRINGP FLOATP))
(DEFPARAMETER |$reportCompilation| NIL)
(DEFPARAMETER |$streamCount| 0)
(SETQ |$cacheAlist| NIL)
(SETQ |$cacheCount| 0)
(SETQ |$reportExitModeStack| NIL)
(DEFPARAMETER |$prefix| NIL)
(DEFPARAMETER |$formalArgList| ())
(SETQ |$FormalMapVariableList|
  '(|#1| |#2| |#3| |#4| |#5| |#6| |#7| |#8| |#9| |#10|
    |#11| |#12| |#13| |#14| |#15| |#16| |#17| |#18| |#19| |#20|
    |#21| |#22| |#23| |#24| |#25| |#26| |#27| |#28| |#29| |#30|
    |#31| |#32| |#33| |#34| |#35| |#36| |#37| |#38| |#39| |#40|
    |#41| |#42| |#43| |#44| |#45| |#46| |#47| |#48| |#49| |#50|
    ))
(SETQ |$PatternVariableList|
  '(*1 *2 *3 *4 *5 *6 *7 *8 *9 *10 *11 *12 *13 *14 *15 *16 *17 *18 *19 *20
  *21 *22 *23 *24 *25 *26 *27 *28 *29 *30 *31 *32 *33 *34 *35 *36 *37 *38 *39 *40
  *41 *42 *43 *44 *45 *46 *47 *48 *49 *50))
(SETQ |$ModeVariableList|
  '(dv$1 dv$2 dv$3 dv$4 dv$5 dv$6 dv$7 dv$8 dv$9 dv$10 dv$11 dv$12 dv$13 dv$14 dv$15
         dv$16 dv$17 dv$18 dv$19 dv$20))

(SETQ |$TriangleVariableList|
   '(|t#1| |t#2| |t#3| |t#4| |t#5| |t#6| |t#7| |t#8| |t#9| |t#10|
     |t#11| |t#12| |t#13| |t#14| |t#15| |t#16| |t#17| |t#18| |t#19| |t#20|
     |t#21| |t#22| |t#23| |t#24| |t#25| |t#26| |t#27| |t#28| |t#29| |t#30|
     |t#31| |t#32| |t#33| |t#34| |t#35| |t#36| |t#37| |t#38| |t#39| |t#40|
     |t#41| |t#42| |t#43| |t#44| |t#45| |t#46| |t#47| |t#48| |t#49| |t#50|))

(SETQ |$FormalFunctionParameterList|
   '(|##1| |##2| |##3| |##4| |##5| |##6| |##7| |##8| |##9| |##10|
     |##11| |##12| |##13| |##14| |##15|))

(SETQ |$PrimitiveDomainNames|
      '(|List| |Integer| |NonNegativeInteger| |PositiveInteger|
        |SingleInteger| |String| |Boolean|))
            ;" used in mkCategory to avoid generating vector slots"
            ;" for primitive domains "
            ;" also used by putInLocalDomainReferences and optCall"
(SETQ |$optimizableConstructorNames|
   '(|List| |Integer| |PositiveInteger| |NonNegativeInteger| |SingleInteger|
     |String| |Boolean| |Symbol| |DoubleFloat| |PrimitiveArray| |Vector|
     |Matrix| |OneDimensionalArray| |TwoDimensionalArray| |U32Vector|
     |U32Matrix| |U16Vector| |U16Matrix| |U8Vector| |U8Matrix|
     |DoubleFloatVector| |DoubleFloatMatrix| |ComplexDoubleFloatVector|
     |ComplexDoubleFloatMatrix| |Character| |SortedExponentVector|
     |HashState| ))
            ;" used by optCallSpecially"
(SETQ |$Zero| '(|Zero|))
(SETQ |$One| '(|One|))
(SETQ |$NonMentionableDomainNames|
      '($ |Rep| |Record| |Union| |Mapping| |Enumeration|))

;"  modemap:==  ( <map> (p e) (p e) ... (p e) )  "
;"  modemaplist:= ( modemap ... )  "

(SETQ |$CategoryFrame| '((NIL)))

(SETQ |$InitialDomainsInScope|
  '(|$EmptyMode| |$NoValueMode|))

(SETQ |$InitialModemapFrame| '((NIL)))

(SETQ |$NRTaddForm| NIL)
(SETQ |$NRTdeltaList| NIL)
(SETQ |$NRTdeltaListComp| NIL)
(SETQ |$NRTbase| 0)
(SETQ |$NRTdeltaLength| 0)
(SETQ |$NRTmonitorIfTrue| NIL)

(SETQ |$useConvertForCoercions| NIL)

;; ---- start of initial settings for variables used in test.boot

(SETQ |$testOutputLineFlag| NIL)   ;; referenced by charyTop, prnd
                                   ;; to stash lines
(SETQ |$runTestFlag| NIL)          ;; referenced by maPrin to stash
                                   ;; output by recordAndPrint to not
                                   ;; print type/time
(SETQ |$mkTestFlag| NIL)           ;; referenced by READLN to stash input
                                   ;; by maPrin to stash output
                                   ;; by recordAndPrint to write i/o
                                   ;; onto $testStream
(SETQ |$mkTestOutputStack| NIL)    ;; saves output for $testStream
                                   ;; (see maPrin)

;; ---- end of initial settings for variables used in test.boot


;; By default, don't generate info files with old compiler.
(setq |$profileCompiler| nil)

(setq credits '(
"An alphabetical listing of contributors to AXIOM (to October, 2006):"
"Cyril Alberga          Roy Adler              Christian Aistleitner"
"Richard Anderson       George Andrews"
"Henry Baker            Stephen Balzac         Yurij Baransky"
"David R. Barton        Gerald Baumgartner     Gilbert Baumslag"
"Fred Blair             Vladimir Bondarenko    Mark Botch"
"Alexandre Bouyer       Peter A. Broadbery     Martin Brock"
"Manuel Bronstein       Florian Bundschuh      Luanne Burns"
"William Burge"
"Quentin Carpent        Robert Caviness        Bruce Char"
"Cheekai Chin           David V. Chudnovsky    Gregory V. Chudnovsky"
"Josh Cohen             Christophe Conil       Don Coppersmith"
"George Corliss         Robert Corless         Gary Cornell"
"Meino Cramer           Claire Di Crescenzo"
"Timothy Daly Sr.       Timothy Daly Jr.       James H. Davenport"
"Jean Della Dora        Gabriel Dos Reis       Michael Dewar"
"Claire DiCrescendo     Sam Dooley             Lionel Ducos"
"Martin Dunstan         Brian Dupee            Dominique Duval"
"Robert Edwards         Heow Eide-Goodman      Lars Erickson"
"Richard Fateman        Bertfried Fauser       Stuart Feldman"
"Brian Ford             Albrecht Fortenbacher  George Frances"
"Constantine Frangos    Timothy Freeman        Korrinn Fu"
"Marc Gaetano           Rudiger Gebauer        Kathy Gerber"
"Patricia Gianni        Holger Gollan          Teresa Gomez-Diaz"
"Laureano Gonzalez-Vega Stephen Gortler        Johannes Grabmeier"
"Matt Grayson           James Griesmer         Vladimir Grinberg"
"Oswald Gschnitzer      Jocelyn Guidry"
"Steve Hague            Vilya Harvey           Satoshi Hamaguchi"
"Martin Hassner         Waldek Hebisch         Ralf Hemmecke"
"Henderson              Antoine Hersen"
"Pietro Iglio"
"Richard Jenks"
"Kai Kaminski           Grant Keady            Tony Kennedy"
"Paul Kosinski          Klaus Kusche           Bernhard Kutzler"
"Larry Lambe            Frederic Lehobey       Michel Levaud"
"Howard Levy            Rudiger Loos           Michael Lucks"
"Richard Luczak"
"Camm Maguire           Bob McElrath           Michael McGettrick"
"Ian Meikle             David Mentre           Victor S. Miller"
"Gerard Milmeister      Mohammed Mobarak       H. Michael Moeller"
"Michael Monagan        Marc Moreno-Maza       Scott Morrison"
"Mark Murray"
"William Naylor         C. Andrew Neff         John Nelder"
"Godfrey Nolan          Arthur Norman          Jinzhong Niu"
"Michael O'Connor       Kostas Oikonomou"
"Julian A. Padget       Bill Page              Susan Pelzel"
"Michel Petitot         Didier Pinchon         Jose Alfredo Portes"
"Claude Quitte"
"Norman Ramsey          Michael Richardson     Renaud Rioboo"
"Jean Rivlin            Nicolas Robidoux       Simon Robinson"
"Michael Rothstein      Martin Rubey"
"Philip Santas          Alfred Scheerhorn      William Schelter"
"Gerhard Schneider      Martin Schoenert       Marshall Schor"
"Frithjof Schulze       Fritz Schwarz          Nick Simicich"
"William Sit            Elena Smirnova         Jonathan Steinbach"
"Christine Sundaresan   Robert Sutor           Moss E. Sweedler"
"Eugene Surowitz"
"James Thatcher         Balbir Thomas          Mike Thomas"
"Dylan Thurston         Barry Trager           Themos T. Tsikas"
"Gregory Vanuxem"
"Bernhard Wall          Stephen Watt           Jaap Weel"
"Juergen Weiss          M. Weller              Mark Wegman"
"James Wen              Thorsten Werther       Michael Wester"
"John M. Wiley          Berhard Will           Clifton J. Williamson"
"Stephen Wilson         Shmuel Winograd        Robert Wisbauer"
"Sandra Wityak          Waldemar Wiwianka      Knut Wolf"
"Clifford Yapp          David Yun"
"Richard Zippel         Evelyn Zoernack        Bruno Zuercher"
"Dan Zwillinger"
))
