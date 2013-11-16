-----------------------------------------------------------------------------
----
---- lang.as: AXIOM-XL Language-defined types.
----
-----------------------------------------------------------------------------
---- Copyright (c) 1990-2007 Aldor Software Organization Ltd (Aldor.org).
-----------------------------------------------------------------------------

#assert AssertConfig

#include "axllib"

#assert UseArrow
#assert UsePackedArrow
#unassert UseMap
#unassert UsePackedMap

+++ Type is the type of all data type objects, including itself.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: type

Type: with == add;


+++ Category is the type of all type categories
+++ (i.e. the type of subtypes of `Type').
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: category type

Category: with == add;


+++ Map(A, R) creates a mapping type with arguments of type A and return
+++ type of type R.  Mappings of this type accept arguments and return
+++ results in a uniform data format.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: mapping type

#if UseMap
Map(A: Tuple Type, R: Tuple Type): with == add;
#endif

#if UseArrow
((A: Tuple Type) -> (R: Tuple Type)): with == add;
#endif


+++ PackedMap(A, R) creates a mapping type with arguments of type A and return
+++ type of type R.  Mappings of this type accept arguments and return
+++ results in type-specific data formats.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: mapping type

#if UsePackedMap
PackedMap(A: Tuple Type, R: Tuple Type): with == add;
#endif

#if UsePackedArrow
((A: Tuple Type) ->* (R: Tuple Type)): with == add;
#endif


+++ Tuple(T) is the type of n-ary, homogenous products of values in T.
+++ E.g.  `(), (1), (1, 2), (1,3,7,8)'	are all values of type Tuple Integer.
+++ Tuple values are not mutable.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: tuple, homogeneous product

Tuple(T: Type): with == add;


+++ Cross(T) is the n-ary type cross product former.
+++ Cross products may be cartesian or dependent.
+++ Cross product values are not mutable.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: cross product

Cross(T: Tuple Type): with == add;


+++ Record(T) is the n-ary record type former.
+++ Records may be cartesian or dependent.
+++ Record values are mutable.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: record

Record(T: Tuple Type): with == add;


+++ RawRecord(T) is the n-ary raw record type former.
+++ Raw records may be cartesian or dependent.
+++ Raw record values are mutable.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 2000
+++ Keywords: record

RawRecord(T:Tuple Type): with == add;


+++ Union(T) is the disjoint union type former.
+++ Union values are not mutable.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: union, disjoint union

Union(T: Tuple Type): with == add;


+++ Enumeration(T) forms a finite type consisting of the names given in T.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: enumeration

Enumeration(T: Tuple Type): with == add;


+++ TrailingArray(I, A) is the type of array whose first elements are
+++ as Record(I) followed by a sequence of Record(A).
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1997-8
+++ Keywords: TrailingArray.

TrailingArray(I: Tuple Type, A: Tuple Type): with == add;

+++ Join(T) forms a new category which has a union of all the exports
+++ of the categories in T.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: category, join, union

Join(T: Tuple Category): Category == with;


+++ Exit is a promise that an expression does not produce a result locally.
+++ For example, the expression `return x' has type Exit.
+++ Exit /\ T => T
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: exit, error

Exit: with == add;


+++ Generator(T) is a type which allows T values to be obtained serially
+++ in a `repeat' or `collect' form.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: generator, step

Generator(T: Type): with == add;


+++ Ref(T) is a type which represents references to identifiers of type T.
+++ Author: AXIOM-XL library
+++ Date Created: 1998
+++ Keywords: Ref, ref

Ref(T: Type): with == add;


+++ Literal is the type in which the source literals are made available
+++ to a progam.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: literal

Literal: with == add;


+++ Builtin is the type which provides builtin runtime operations
+++ to an AXIOM-XL program.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: builtin

Builtin: with == add;


+++ C is the type which provides operations implemented in C
+++ to an AXIOM-XL program.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: builtin

C: with == add;


+++ Fortran is the type which provides operations implemented in Fortran
+++ to an AXIOM-XL program.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-1997
+++ Keywords: builtin

Fortran: with == add;


+++ Lisp is the type which provides operations implemented in Lisp
+++ to an AXIOM-XL program.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: builtin

Lisp: with == add;


+++ Foreign is the type which provides operations implemented in the
+++ language of the runtime system (eg LISP or C).
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: Foreign

Foreign: with == add;


+++ Foreign(Lang) is the type which provides operations implemented in other
+++ programming languages to an AXIOM-XL program.
+++
+++ Author: AXIOM-XL library
+++ Date Created: 1992-94
+++ Keywords: Foreign

Foreign(T: Type): with == add;

+++ Elements of domains satisfying FortranLogical can be passed to Fortran
+++ as an object of type LOGICAL.
FortranLogical:Category == with;       -- Boolean/LOGICAL

+++ Elements of domains satisfying FortranCharacter can be passed to Fortran
+++ as an object of type CHARACTER.
FortranCharacter:Category == with;     -- Character/CHARACTER/CHARACTER*1

+++ Elements of domains satisfying FortranString can be passed to Fortran
+++ as an object of type CHARACTER(*).
FortranString:Category == with;        -- String/CHARACTER(*)

+++ Elements of domains satisfying FortranInteger can be passed to Fortran
+++ as an object of type INTEGER.
FortranInteger:Category == with;       -- SingleInteger/INTEGER

+++ Elements of domains satisfying FortranComplexReal can be passed to Fortran
+++ as an object of type COMPLEX REAL.
FortranComplexReal:Category == with;   -- Complex SF/COMPLEX REAL

+++ Elements of domains satisfying FortranComplexDouble can be passed to Fortran
+++ as an object of the (non-standard) type COMPLEX DOUBLE PRECISION.
FortranComplexDouble:Category == with; -- Complex DF/COMPLEX(KIND=KIND(0.D0))
