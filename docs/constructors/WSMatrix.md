# WSMatrix

> **Kind**: Domain &nbsp;|&nbsp; \[[Source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L335)\] &nbsp;|&nbsp; **Group**: WS — Wolfram/MathLink

## Description

Julia Wolfram Symbolic matrices using the MathLink Julia package.

**WSMatrix(E: WSRing) is a domain constructor**  
**Abbreviation for WSMatrix is WSMAT**  
**This constructor is exposed in this frame.**

## Signatures

```fricas
--- Operations ---

 #? : % -> NonNegativeInteger if % has ATFINAG          #? : % -> WSInteger
 ?*? : (WSVector(E), %) -> %                            ?*? : (%, WSVector(E)) -> %
 ?*? : (%, E) -> %                                      ?*? : (E, %) -> %
 ?*? : (Integer, %) -> %                                ?*? : (%, %) -> %
 ?+? : (%, %) -> %                                      -? : % -> %
 ?-? : (%, %) -> %                                      ?/? : (%, E) -> % if E has FIELD
 ?=? : (%, %) -> Boolean                                Pfaffian : % -> E if E has COMRING
 ?^? : (%, NonNegativeInteger) -> %                     ?^? : (%, PositiveInteger) -> %
 ?^? : (%, Integer) -> % if E has FIELD                 accumulate : % -> % if WSVector(E) has WSNUM
 adjoint : % -> %                                       adjugate : % -> %
 antisymmetric? : % -> Boolean                          append : (%, WSVector(E)) -> %
 array2 : List(List(E)) -> %                            blockConcat : List(List(%)) -> %
 cholesky : % -> Record(l: %,u: %)                      coerce : WSVector(E) -> %
 coerce : List(WSVector(E)) -> %                        coerce : % -> WSExpression
 coerce : % -> JLObject                                 coerce : % -> OutputForm
 colSlice : % -> Segment(Integer)                       column : (%, Integer) -> %
 column : (%, Integer) -> WSVector(E)                   convert : % -> String
 copy : % -> %                                          delete : (%, WSList(WSInteger)) -> %
 determinant : % -> E if E has COMRING                  diagonal : % -> WSVector(E)
 diagonal? : % -> Boolean                               diagonalMatrix : WSList(E) -> %
 diagonalMatrix : List(E) -> %                          diagonalMatrix : List(%) -> %
 differences : % -> % if WSVector(E) has WSNUM          dimensions : % -> WSList(WSInteger)
 eigenvalues : % -> WSVector(WSExpression)              eigenvectors : % -> WSMatrix(WSExpression)
 elt : (%, Integer, Integer) -> E                       elt : (%, Integer, Integer, E) -> E
 elt : (%, Integer, List(Integer)) -> %                 elt : (%, List(Integer), Integer) -> %
 elt : (%, List(Integer), List(Integer)) -> %           elt : (%, List(Integer), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Integer)) -> %        elt : (%, Integer, List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Integer) -> %        elt : (%, Integer) -> WSVector(E)
 empty : () -> %                                        empty? : % -> Boolean
 eq? : (%, %) -> Boolean                                extract : (%, WSExpression) -> %
 fill! : (%, E) -> %                                    first : % -> WSVector(E)
 generalizedInverse : % -> %                            hash : % -> SingleInteger if E has HASHABL
 hermitian? : % -> Boolean                              hessenberg : % -> Record(p: %,h: %)
 horizConcat : (%, %) -> %                              horizConcat : List(%) -> %
 horizSplit : (%, PositiveInteger) -> List(%)           insert : (%, WSVector(E), WSInteger) -> %
 intersection : (%, %) -> %                             inverse : % -> %
 jWSAggregate : List(WSVector(E)) -> %                  jWSInterpret : (String, String, String) -> %
 jWSInterpret : (String, String) -> %                   jWSInterpret : String -> %
 jWSMatrix : String -> %                                jlAbout : % -> Void
 jlApply : (String, %, %, %, %, %) -> JLObject          jlApply : (String, %, %, %, %) -> JLObject
 jlApply : (String, %, %, %) -> JLObject                jlApply : (String, %, %) -> JLObject
 jlApply : (String, %) -> JLObject                      jlDisplay : % -> Void
 jlDump : JLObject -> Void                              jlEval : % -> %
 jlFieldNames : % -> JLObject                           jlGetField : (%, JLSymbol) -> JLObject
 jlGetJuliaIndex : % -> String                          jlGetProperty : (%, JLSymbol) -> JLObject
 jlHead : % -> WSSymbol                                 jlId : % -> JLInt64
 jlObject : () -> String                                jlPropertyNames : % -> JLObject
 jlRef : % -> SExpression                               jlSymbolic : % -> String
 jlText : (%, String) -> List(String)                   jlType : % -> Symbol
 jlimref : String -> %                                  jlref : String -> %
 join : (%, %) -> %                                     jordan : % -> Record(s: %,j: %)
 kroneckerProduct : (%, %) -> % if E has SRNG           kroneckerProduct : List(%) -> % if E has SRNG
 kroneckerSum : (%, %) -> %                             kroneckerSum : List(%) -> %
 last : % -> WSVector(E)                                latex : % -> String
 length : % -> WSInteger                                less? : (%, NonNegativeInteger) -> Boolean
 linearSolve : (%, %) -> %                              listOfLists : % -> List(List(E))
 lu : % -> Record(lu: %,p: %,c: %)                      map : ((E -> E), %) -> %
 map : (((E, E) -> E), %, %) -> %                       map : (((E, E) -> E), %, %, E) -> %
 map! : ((E -> E), %) -> %                              matrix : WSVector(E) -> %
 matrix : List(List(E)) -> %                            maxColIndex : % -> Integer
 maxRowIndex : % -> Integer                             members : % -> List(E) if % has ATFINAG
 minColIndex : % -> Integer                             minRowIndex : % -> Integer
 minordet : % -> E if E has COMRING                     minors : (%, WSInteger) -> %
 minors : % -> %                                        missing? : % -> Boolean
 more? : (%, NonNegativeInteger) -> Boolean             mutable? : % -> Boolean
 ncols : % -> NonNegativeInteger                        norm : (%, WSExpression) -> E
 norm : % -> E                                          nothing? : % -> Boolean
 nrows : % -> NonNegativeInteger                        nullSpace : % -> WSList(WSVector(E))
 numeric : (%, PositiveInteger) -> WSExpression         numeric : % -> WSExpression
 numeric? : % -> Boolean                                part : (%, WSInteger) -> WSVector(E)
 parts : % -> List(E)                                   permanent : % -> E
 positiveDefinite? : % -> Boolean                       prepend : (%, WSVector(E)) -> %
 qelt : (%, Integer, Integer) -> E                      qelt : (%, Integer) -> WSVector(E)
 qr : % -> Record(q: %,r: %)                            qsetelt : (%, Integer, Integer, E) -> %
 qsetelt : (%, Integer, WSVector(E)) -> %               qsetelt! : (%, Integer, Integer, E) -> E
 qsetelt! : (%, Integer, WSVector(E)) -> %              rank : % -> NonNegativeInteger if E has INTDOM
 removeDuplicates : % -> %                              replacePart : (%, %) -> %
 rest : % -> %                                          reverse : (%, WSList(WSInteger)) -> %
 reverse : (%, WSInteger) -> %                          reverse : % -> %
 riffle : (%, %, %) -> %                                riffle : (%, %) -> %
 row : (%, Integer) -> %                                row : (%, Integer) -> WSVector(E)
 rowEchelon : % -> % if E has EUCDOM                    rowSlice : % -> Segment(Integer)
 sample : () -> %                                       scalarMatrix : (NonNegativeInteger, E) -> %
 schur : % -> Record(q: %,t: %)                         setColumn! : (%, Integer, WSVector(E)) -> %
 setIntersection : (%, %) -> %                          setRow! : (%, Integer, WSVector(E)) -> %
 setelt : (%, Integer, Integer, E) -> %                 setelt : (%, Integer, WSVector(E)) -> %
 setelt! : (%, Integer, Integer, E) -> E                setelt! : (%, Integer, List(Integer), %) -> %
 setelt! : (%, List(Integer), Integer, %) -> %          setelt! : (%, Integer, WSVector(E)) -> %
 setsubMatrix! : (%, Integer, Integer, %) -> %          size? : (%, NonNegativeInteger) -> Boolean
 smaller? : (%, %) -> Boolean if E has COMPAR           sort : % -> %
 sorted? : % -> Boolean                                 square? : % -> Boolean
 squareTop : % -> %                                     string : % -> String
 svd : % -> Record(S: %,sv: %,V: %)                     svdvals : % -> WSList(E)
 swapColumns! : (%, Integer, Integer) -> %              swapRows! : (%, Integer, Integer) -> %
 symmetric? : % -> Boolean                              take : (%, WSList(WSInteger)) -> %
 take : (%, Integer) -> %                               toString : (%, WSExpression) -> String
 toString : % -> String                                 trace : % -> E
 transpose : % -> %                                     transpose : WSVector(E) -> %
 union : (%, %) -> %                                    vertConcat : (%, %) -> %
 vertConcat : List(%) -> %                              vertSplit : (%, PositiveInteger) -> List(%)
 zero? : % -> Boolean if E has ABELMON                  ?~=? : (%, %) -> Boolean
 ?*? : (%, WSVector(E)) -> WSVector(E) if E has SRNG
 ?*? : (WSVector(E), %) -> WSVector(E) if E has SRNG
 any? : ((E -> Boolean), %) -> Boolean if % has ATFINAG
 blockSplit : (%, PositiveInteger, PositiveInteger) -> List(List(%))
 blockSplit : (%, List(NonNegativeInteger), List(NonNegativeInteger)) -> List(List(%))
 columnSpace : % -> List(WSVector(E)) if E has EUCDOM
 count : ((E -> Boolean), %) -> NonNegativeInteger if % has ATFINAG
 count : (E, %) -> NonNegativeInteger if % has ATFINAG and E has BASTYPE
 eigenSystem : % -> Record(values: WSVector(WSExpression),vectors: WSMatrix(WSExpression))
 elt : (%, Segment(Integer), Segment(Integer)) -> %
 elt : (%, Segment(Integer), List(Segment(Integer))) -> %
 elt : (%, List(Segment(Integer)), Segment(Integer)) -> %
 elt : (%, List(Segment(Integer)), List(Segment(Integer))) -> %
 eval : (%, List(Equation(E))) -> % if E has EVALAB(E) and E has SETCAT
 eval : (%, Equation(E)) -> % if E has EVALAB(E) and E has SETCAT
 eval : (%, E, E) -> % if E has EVALAB(E) and E has SETCAT
 eval : (%, List(E), List(E)) -> % if E has EVALAB(E) and E has SETCAT
 every? : ((E -> Boolean), %) -> Boolean if % has ATFINAG
 ?exquo? : (%, E) -> Union(%,"failed") if E has INTDOM
 hashUpdate! : (HashState, %) -> HashState if E has HASHABL
 horizSplit : (%, List(NonNegativeInteger)) -> List(%)
 inverse : % -> Union(%,"failed") if E has FIELD
 invertIfCan : % -> Union(%,"failed") if E has INTDOM
 kronecker_prod1 : (%, Integer, List(List(NonNegativeInteger)), List(%), NonNegativeInteger, NonNegativeInteger, Union(E,one)) -> Void
 matrix : (NonNegativeInteger, NonNegativeInteger, ((Integer, Integer) -> E)) -> %
 matrixFunction : (WSExpression, %) -> WSMatrix(WSExpression)
 max : (((E, E) -> Boolean), %) -> E if % has ATFINAG
 max : % -> E if % has ATFINAG and E has ORDSET
 member? : (E, %) -> Boolean if % has ATFINAG and E has BASTYPE
 min : % -> E if % has ATFINAG and E has ORDSET
 new : (NonNegativeInteger, NonNegativeInteger, E) -> %
 nullSpace : % -> List(WSVector(E)) if E has INTDOM
 nullity : % -> NonNegativeInteger if E has INTDOM
 positivePower : (%, Integer) -> % if E has SRNG
 qnew : (NonNegativeInteger, NonNegativeInteger) -> %
 randomComplexMatrix : (WSList(WSComplex), WSList(WSInteger)) -> WSMatrix(WSComplex)
 randomRealMatrix : (WSList(WSReal), WSList(WSInteger)) -> WSMatrix(WSReal)
 setelt! : (%, Integer, List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Integer, %) -> %
 setelt! : (%, List(Integer), List(Integer), %) -> %
 setelt! : (%, Segment(Integer), Segment(Integer), %) -> %
 setelt! : (%, List(Integer), Segment(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Integer), %) -> %
 setelt! : (%, Segment(Integer), List(Segment(Integer)), %) -> %
 setelt! : (%, List(Segment(Integer)), Segment(Integer), %) -> %
 setelt! : (%, List(Segment(Integer)), List(Segment(Integer)), %) -> %
 subMatrix : (%, Integer, Integer, Integer, Integer) -> %
 total : % -> WSVector(E) if WSVector(E) has WSNUM
 vertSplit : (%, List(NonNegativeInteger)) -> List(%)
 zero : (NonNegativeInteger, NonNegativeInteger) -> %
```

## Operations added

### `adjoint` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L419)\]

adjoint(m) returns the adjoint of m, i.e. the conjugate transposition of m.

- **Signature**: `(%)->%`

### `adjugate` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L421)\]

adjugate(m) returns the adjugate of square m.

- **Signature**: `(%)->%`

### `antisymmetric?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L364)\]

antisymmetric?(m) returns true if the matrix m is square and antisymmetric (i.e. m[i, j] = -m[j, i] for all iand j) and false otherwise.

- **Signature**: `(%)->Boolean`

### `cholesky` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L479)\]

cholesky(m) computes the Cholesky decomposition of the positive definite/hermitian square matrix m.

- **Signature**: `(%)->Record(l:%,u:%)`

### `column` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L415)\]

column(mat,i) returns the i-th column.

- **Signature**: `(%,Integer)->%`

### `diagonal` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L434)\]

diagonal(m) returns the diagonal elements of m as a vector.

- **Signature**: `(%)->WSVector(E)`

### `diagonal?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L355)\]

diagonal?(m) returns true if the matrix m is square and diagonal (i.e. all entries of m not on the diagonal are zero) and false otherwise.

- **Signature**: `(%)->Boolean`

### `diagonalMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L502)\]

diagonalMatrix(l) returns a diagonal matrix with elements of l as diagonal elements.

- **Signature**: `(WSList(E))->%`

### `eigenSystem` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L461)\]

eigenSystem(m) computes the spectral decomposition of the square matrix m. It returns in a Record, the eigenvalues, selector 'values', and the eigenvectors selector 'vectors'.

- **Signature**: `(%)->Record(values:WSVector(WSExpression),vectors:WSMatrix(WSExpression))`

### `eigenvalues` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L457)\]

eigenvalues(m) returns the eigenvalues of the square matrix m.

- **Signature**: `(%)->WSVector(WSExpression)`

### `eigenvectors` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L459)\]

eigenvectors(m) returns the eigenvectors of the square matrix m.

- **Signature**: `(%)->WSMatrix(WSExpression)`

### `elt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L399)\]

elt(mat, m, n) returns the element (m,n) of the matrix mat.

- **Signature**: `(%,Integer,Integer)->E`

### `generalizedInverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L446)\]

generalizedInverse(m) computes the pseudo inverse of m also known as Moore-Penrose inverse.

- **Signature**: `(%)->%`

### `hermitian?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L370)\]

hermitian?(m) checks whether or not m is hermitian.

- **Signature**: `(%)->Boolean`

### `hessenberg` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L476)\]

hessenberg(m) computes the Hessenberg decomposition of the square matrix m.

- **Signature**: `(%)->Record(p:%,h:%)`

### `inverse` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L436)\]

inverse(m) computes the inverse of m. For example: 

**Example**:
```fricas
m:=jWSMatrix("{{a, b}, {b, a}}")@WSMAT(WSEXPR)
```

**Example**:
```fricas
inverse m
```

- **Signature**: `(%)->%`

### `invertIfCan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L442)\]

invertIfCan(m) returns the inverse of the matrix m. If the matrix is not invertible, "failed" is returned. Error: if the matrix is not square.

- **Signature**: `(%)->Union(%,"failed")`

### `jWSMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L510)\]

jWSMatrix(str) constructs str as a WSMatrix. str must be in the WS language (WS list of WS list(s)).

- **Signature**: `(String)->%`

### `jordan` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L474)\]

jordan(m) computes the Jordan decomposition of the square matrix m.

- **Signature**: `(%)->Record(s:%,j:%)`

### `linearSolve` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L449)\]

linearSolve(mat, b) finds x, solution of the equation mat * x = b.

- **Signature**: `(%,%)->%`

### `lu` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L482)\]

lu(m) computes the LU decomposition of the matrix m.

- **Signature**: `(%)->Record(lu:%,p:%,c:%)`

### `matrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L508)\]

matrix(vec) returns vec as a matrix (inplace coercion).

- **Signature**: `(WSVector(E))->%`

### `matrixFunction` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L484)\]

matrixFunction(func, m) applies the matrix function func, if available, to the matrix m. The Wolfram Languageoperator needs to be used. Note that it is not an element-wise operation. For example:

**Example**:
```fricas
m := jWSMatri x("{{1.2, 1.7},{1.12, -2.1}}")@WSMAT(WSREAL)
```

**Example**:
```fricas
matrixFunction("Sqrt", m)
```

- **Signature**: `(WSExpression,%)->WSMatrix(WSExpression)`

### `minors` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L453)\]

minors(m) returns the minors of m.

- **Signature**: `(%)->%`

minors(m,i) returns the i-th minors of m.

- **Signature**: `(%,WSInteger)->%`

### `norm` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L423)\]

norm(m) returns the norm of m, i.e. the value of the maximum singular values of m.

- **Signature**: `(%)->E`

norm(m, type) returns the type norm of m. For example, norm(m,2) returns the usual norm, norm(m, "Infinity") or norm(m, "Frobenius") returns their respective norms. See documentation for information.

- **Signature**: `(%,WSExpression)->E`

### `nullSpace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L451)\]

nullSpace(m) returns the null space of m as a list of vectors.

- **Signature**: `(%)->WSList(WSVector(E))`

### `permanent` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L432)\]

permanent(m) returns the permanent of m.

- **Signature**: `(%)->E`

### `positiveDefinite?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L368)\]

positiveDefinite?(m) checks whether or not m is positive definite.

- **Signature**: `(%)->Boolean`

### `qelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L401)\]

qelt(mat, m, n) returns the element (m,n) of the matrix mat. No checks are done at the FriCAS level.

- **Signature**: `(%,Integer,Integer)->E`

### `qr` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L472)\]

qr(m) computes the QR decomposition of the matrix m.

- **Signature**: `(%)->Record(q:%,r:%)`

### `qsetelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L407)\]

qsetelt(mat,n,m,elt) returns a new matrix with element (n,m) replaced by the element elt.

- **Signature**: `(%,Integer,Integer,E)->%`

### `qsetelt!` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L410)\]

qsetelt!(mat,n,m,elt) returns the element elt. The matrix mat is modified in place.

- **Signature**: `(%,Integer,Integer,E)->E`

### `randomComplexMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L496)\]

randomComplexMatrix(range, dims) returns a random matrix with random complex numbers in the range range and dims dimensions. 

**Example**:
```fricas
range:=jWSList("{1+I}")@WSLIST(WSCPLX)
```

**Example**:
```fricas
randomComplexMatrix(range,jWSList(" {3,3}")$WSLIST(WSINT))@WSMAT(WSCPLX)
```

- **Signature**: `(WSList(WSComplex),WSList(WSInteger))->WSMatrix(WSComplex)`

### `randomRealMatrix` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L490)\]

randomRealMatrix(range, dims) returns a random matrix with random numbers in the range range and dims dimensions. 

**Example**:
```fricas
range := jWSList("{-5,5}")$WSLIST(WSREAL)
```

**Example**:
```fricas
randomRealMatrix(range,jWSList("{3,3}")$WSL IST(WSINT))@WSMAT(WSREAL)
```

- **Signature**: `(WSList(WSReal),WSList(WSInteger))->WSMatrix(WSReal)`

### `row` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L413)\]

row(mat,i) returns the i-th row.

- **Signature**: `(%,Integer)->%`

### `schur` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L470)\]

schur(m) computes the Schur decomposition of the square matrix m.

- **Signature**: `(%)->Record(q:%,t:%)`

### `setelt` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L404)\]

setelt(mat,n,m,elt) returns a new matrix with element (n,m) replaced by the element elt.

- **Signature**: `(%,Integer,Integer,E)->%`

### `square?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L352)\]

square?(m) returns true if m is a square matrix (i.e. if m has the same number of rows as columns) and false otherwise.

- **Signature**: `(%)->Boolean`

### `svd` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L466)\]

svd(m) computes the singular value decomposition of the matrix m.

- **Signature**: `(%)->Record(S:%,sv:%,V:%)`

### `svdvals` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L468)\]

svdvals(m) returns the singular values of the matrix m.

- **Signature**: `(%)->WSList(E)`

### `symmetric?` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L359)\]

symmetric?(m) returns true if the matrix m is square and symmetric (i.e. m[i, j] = m[j, i] for all i and j) and false otherwise.

- **Signature**: `(%)->Boolean`

### `toString` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L505)\]

toString(mat, form) returns the string representation of mat with WS language format form.

- **Signature**: `(%,WSExpression)->String`

### `trace` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L430)\]

trace(m) returns the trace of m.

- **Signature**: `(%)->E`

### `transpose` &nbsp; \[[source](https://github.com/gvanuxem/jlfricas/blob/master/src/algebra/jwsagg.spad#L417)\]

transpose(m) returns the transposition of m.

- **Signature**: `(%)->%`
---
[Back to Index](../index.md)
