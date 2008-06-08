-- Some initial stuff to get the compiler understand some identifiers.
-- These declarations are wrong, but all we need is to make the 
-- identifiers known for the export's that follwo.
Type: with;
Tuple: with;
(->): with;

-- The following list contains some basic domains and categories from
-- the Axiom algebra library.
-- Declarations are sufficient.
export Boolean:                    with;
export Field:                      with;
export InputForm:                  with;
export IntegralDomain:             with;
export NonNegativeInteger:         with;
export OpenMathDevice:             with;
export OutputForm:                 with;
export PositiveInteger:            with;
export SetCategory:                with;
export SingleInteger:              with;
export Ring:                       with;
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
