-- The whole file is handcrafted and serves the purpose of reducing
-- the size of Axiom cliques. Furthermore, it should help to bring
-- down the interdependencies of Axiom types so much as to be able
-- to compile axlit.as and axextend.as in a separate compilation step
-- after the compilation of the types they depend on.

-- Some initial stuff to get the compiler understand some identifiers.
-- These declarations are wrong, but all we need is to make the 
-- identifiers known for the export's that follow.
-- Actually, the declarations for 'Category', 'Type', 'Tuple', and '->'
-- are there to be able to compile from initlist.as to initlist.ap.
Category: with;
Type:     with;
Tuple:    with;
(->):     with;

-- These categories are only provided to make the stuff below compile.
Field:          Category;
IntegralDomain: Category;
Ring:           Category;
SetCategory:    Category;

-- The following list contains some basic domains and categories from
-- the Axiom algebra library.
-- Declarations are sufficient.
export Boolean:                    with;
export InputForm:                  with;
export NonNegativeInteger:         with;
export OpenMathDevice:             with;
export OutputForm:                 with;
export PositiveInteger:            with;
export SingleInteger:              with;
export String:                     with;

export DoubleFloat:                SetCategory;
export Float:                      SetCategory;
export Symbol:                     SetCategory;

export Integer:                    IntegralDomain;

export Equation:                   (T: Type)           -> with;
export List:                       (T: Type)           -> with;
export SegmentBinding:             (T: Type)           -> with;
export UniversalSegment:           (T: Type)           -> with;
export Vector:                     (T: Type)           -> with;

export Factored:                   (T: IntegralDomain) -> IntegralDomain;
export Fraction:                   (T: IntegralDomain) -> Field;
export Matrix:                     (T: Ring)           -> with;
export SparseUnivariatePolynomial: (T: Ring)           -> IntegralDomain;
