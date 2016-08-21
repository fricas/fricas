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
Type:     with;
Tuple:    with;
(->):     with;

-- The following list contains some basic domains and categories from
-- the Axiom algebra library.
-- Declarations are sufficient.

-- Note that because of the incomplete analysis of the dependencies,
-- it is necessary here to give for some domains more than just
-- 'with' as their initial type. Fine tuning could be done by starting
-- with 'with' for every domain and specializing to more concrete
-- categories according to the failure message in the compilation of
-- big cliques.

export InputForm:                  with;
export NonNegativeInteger:         with;
export OpenMathDevice:             with;
export OutputForm:                 with;
export PositiveInteger:            with;
export SingleInteger:              with;

export String:                     SetCategory;
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
export Matrix:                     (T: AbelianMonoid) -> with;
export SparseUnivariatePolynomial: (T: with {SemiRng;AbelianMonoid}) -> IntegralDomain;
