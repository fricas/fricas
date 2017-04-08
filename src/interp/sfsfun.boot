-- Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
--     - Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     - Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in
--       the documentation and/or other materials provided with the
--       distribution.
--
--     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
--       names of its contributors may be used to endorse or promote products
--       derived from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
-- IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
-- TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
-- PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
-- OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-- Used to be SPECFNSF
)package "BOOT"

FloatError(formatstring,arg) ==
--        ERROR(formatstring,arg)
        ERROR FORMAT([],formatstring,arg)

float(x) == FLOAT(x, 0.0)

fracpart(x) ==
        CADR(MULTIPLE_-VALUE_-LIST(FLOOR(x)))

intpart(x) ==
        first(MULTIPLE_-VALUE_-LIST(FLOOR(x)))

negintp(x) ==
        if ZEROP IMAGPART(x) and x<0.0 and ZEROP fracpart(x)
        then
                true
        else
                false

-- Lisp PI is a long float and causes type errors, here we give
-- enough digits to have double accuracy even after conversion
-- to binary
DEFCONSTANT(dfPi, 3.14159265358979323846264338328)

--- Small float implementation of Gamma function.  Valid for
--- real arguments.  Maximum error (relative) seems to be
--- 2-4 ulps for real x 2<x<9, and up to ten ulps for larger x
--- up to overflow.  See Hart & Cheney.
--- Bruce Char, April, 1990.

horner(l,x) ==
        result := 0
        for el in l repeat
                result := result *x + el
        return result

r_gamma (x) ==
        if COMPLEXP(x) then FloatError('"Gamma not implemented for complex value ~D",x)
        ZEROP (x-1.0) => 1.0
        if x>20 then gammaStirling(x) else gammaRatapprox(x)

r_lngamma (x) ==
        if x>20 then lnrgammaRatapprox(x) else LOG(gammaRatapprox(x))

cbeta(z,w) ==
        cgamma(z)*cgamma(w)/(cgamma(z+w))

gammaStirling(x) ==
       EXP(r_lngamma(x))

lnrgammaRatapprox(x) ==
       (x-.5)*LOG(x) - x + LOG(SQRT(2.0*dfPi)) + phiRatapprox(x)

phiRatapprox(x) ==
        arg := 1/(x^2)
        p := horner([.0666629070402007526,_
                     .6450730291289920251389,_
                     .670827838343321349614617,_
                     .12398282342474941538685913],arg);
        q := horner([1.0,7.996691123663644194772,_
                      8.09952718948975574728214,_
                      1.48779388109699298468156],arg);
        result := p/(x*q)
        result

gammaRatapprox (x) ==
        if (x>=2 and x<=3)
        then
                result := gammaRatkernel(x)
        else
                if x>3
                then
                     n := FLOOR(x)-2
                     a := x-n-2
                     reducedarg := 2+a
                     prod := */[reducedarg+i for i in 0..n-1]
                     result := prod* gammaRatapprox(reducedarg)
                else
                   if (2>x and x>0)
                   then
                     n := 2-FLOOR(x)
                     a := x-FLOOR(x)
                     reducedarg := 2+a
                     prod := */[x+i for i in 0..n-1]
                     result := gammaRatapprox(reducedarg)/prod
                 else
                        Pi := dfPi
                        lx := MULTIPLE_-VALUE_-LIST(FLOOR(x))
                        intpartx := first(lx)+1
                        restx := CADR(lx)
                        if ZEROP restx  -- case of negative non-integer value
                        then
                          FloatError ('"Gamma undefined for non-positive integers: ~D",x)
                        else
                          result := Pi/(gammaRatapprox(1.0-x)*(-1.0)^(intpartx+1)*SIN(restx*Pi))
        result

gammaRatkernel(x) ==
           p := horner(REVERSE([3786.01050348257245475108,_
                        2077.45979389418732098416,_
                        893.58180452374981423868,_
                        222.1123961680117948396,_
                        48.95434622790993805232,_
                        6.12606745033608429879,_
                        .778079585613300575867]),x-2)
           q := horner(REVERSE([3786.01050348257187258861,_
                        476.79386050368791516095,_
                        -867.23098753110299445707,_
                        83.55005866791976957459,_
                        50.78847532889540973716,_
                        -13.40041478578134826274,_
                        1]),x-2.0)
           p/q

-- cgamma(z) Gamma function for complex arguments.
--    Bruce Char    April-May, 1990.
--
-- Our text for complex gamma is H. Kuki's paper Complex Gamma
-- Function with Error Control", CACM vol. 15, no. 4, ppp. 262-267.
-- (April, 1972.)  It uses the reflection formula and the basic
-- z+1 recurrence to transform the argument into something that
-- Stirling's asymptotic formula can handle.
--
-- However along the way it does a few tricky things to reduce
-- problems due to roundoff/cancellation error for particular values.

-- cgammat is auxiliary "t" function (see p. 263 Kuki)
cgammat(x) ==
        MAX(0.1, MIN(10.0, 10.0*SQRT(2.0) - ABS(x)))

cgamma (z) ==
        z2 := IMAGPART(z)
        z1 := REALPART(z)       --- call real valued gamma if z is real
        if ZEROP z2
        then    result := r_gamma(z1)
        else
                result := clngamma(z1,z2,z)
                result := EXP(result)
        result

lncgamma(z) ==
   clngamma(REALPART z, IMAGPART z, z)

clngamma(z1,z2,z) ==
        --- conjugate of gamma is gamma of conjugate.  map 2nd and 4th quads
        --- to first and third quadrants
        if z1<0.0
        then if z2 > 0.0
                then result := CONJUGATE(clngammacase1(z1,-z2,COMPLEX(z1,-z2)))
                else result := clngammacase1(z1,z2,z)
        else if z2 < 0.0
                then result := CONJUGATE(clngammacase23(z1,-z2,_
                                COMPLEX(z1,-z2)))
                else result := clngammacase23(z1,z2,z)
        result

clngammacase1(z1,z2,z) ==
        result1 := PiMinusLogSinPi(z1,z2,z)
        result2 := clngamma(1.0-z1,-z2,1.0-z)
        result1-result2

PiMinusLogSinPi(z1,z2,z) ==
        cgammaG(z1,z2)  - logH(z1,z2,z)

cgammaG(z1,z2) ==
        LOG(2*dfPi) + dfPi*z2 - COMPLEX(0.0,1.0)*dfPi*(z1-.5)

logH(z1,z2,z) ==
        z1bar := CADR(MULTIPLE_-VALUE_-LIST(FLOOR(z1))) ---frac part of z1
        piz1bar := dfPi*z1bar
        piz2 := dfPi*z2
        twopiz2 := 2.0*piz2
        i := COMPLEX(0.0,1.0)
        part2 := EXP(twopiz2)*(2.0*SIN(piz1bar)^2 + SIN(2.0*piz1bar)*i)
        part1 := -TANH(piz2)*(1.0+EXP(twopiz2))
--- part1 is another way of saying 1 - exp(2*Pi*z1bar)
        LOG(part1+part2)

clngammacase23(z1,z2,z) ==
        tz2 := cgammat(z2)
        if (z1 < tz2)
        then result:= clngammacase2(z1,z2,tz2,z)
        else result:= clngammacase3(z)
        result

clngammacase2(z1,z2,tz2,z) ==
        n := float(CEILING(tz2-z1))
        zpn := z+n
        (z-.5)*LOG(zpn) - (zpn) + cgammaBernsum(zpn) - cgammaAdjust(logS(z1,z2,z,n,zpn))

logS(z1,z2,z,n,zpn) ==
        sum := 0.0
        for k in 0..(n-1) repeat
                if z1+k < 5.0 - 0.6*z2
                then sum := sum + LOG((z+k)/zpn)
                else sum := sum + LOG(1.0 - (n-k)/zpn)
        sum

--- on p. 265, Kuki, logS result should have its imaginary part
--- adjusted by 2 Pi if it is negative.
cgammaAdjust(z) ==
        if IMAGPART(z)<0.0
        then result := z + COMPLEX(0.0, 2.0*dfPi)
        else result := z
        result

clngammacase3(z) ==
        (z- .5)*LOG(z) - z + cgammaBernsum(z)

cgammaBernsum (z) ==
        sum := LOG(2.0*dfPi)/2.0
        zterm := z
        zsquaredinv := 1.0/(z*z)
        l:= [.083333333333333333333, -.0027777777777777777778,_
                .00079365079365079365079,  -.00059523809523809523810,_
                .00084175084175084175084, -.0019175269175269175269,_
                .0064102564102564102564]
        for m in 1..7 for el in l repeat
                zterm := zterm*zsquaredinv
                sum := sum + el*zterm
        sum




--- nth derivatives of ln gamma for real x, n = 0,1,....
--- requires files floatutils, rgamma
$PsiAsymptoticBern := VECTOR(0.0, 0.1666666666666667, -0.03333333333333333, 0.02380952380952381,_
              -0.03333333333333333, 0.07575757575757576, -0.2531135531135531, 1.166666666666667,_
              -7.092156862745098, 54.97117794486216, -529.1242424242424, 6192.123188405797,_
              -86580.25311355311, 1425517.166666667, -27298231.06781609, 601580873.9006424,_
              -15116315767.09216, 429614643061.1667, -13711655205088.33, 488332318973593.2,_
              -19296579341940070.0,  841693047573682600.0, -40338071854059460000.0)


PsiAsymptotic(n,x) ==
        xn := x^n
        xnp1 := xn*x
        xsq := x*x
        xterm := xsq
        factterm := r_gamma(n+2)/2.0/r_gamma(float(n+1))
        --- initialize to 1/n!
        sum := AREF($PsiAsymptoticBern,1)*factterm/xterm
        for k in 2..22 repeat
                xterm := xterm * xsq
                if n=0 then factterm := 1.0/float(2*k)
                else if n=1 then factterm := 1
                else factterm := factterm * float(2*k+n-1)*float(2*k+n-2)/(float(2*k)*float(2*k-1))
                sum := sum + AREF($PsiAsymptoticBern,k)*factterm/xterm
        PsiEps(n,x) + 1.0/(2.0*xnp1) + 1.0/xn * sum


PsiEps(n,x) ==
        if n = 0
        then
                result := -LOG(x)
        else
                result :=  1.0/(float(n)*(x^n))
        result

PsiAsymptoticOrder(n,x,nterms) ==
        sum := 0
        xterm := 1.0
        np1 := n+1
        for k in 0..nterms repeat
                xterm := (x+float(k))^np1
                sum := sum + 1.0/xterm
        sum


r_psi(n,x) ==
        if x<=0.0
        then
                if ZEROP fracpart(x)
                then FloatError('"singularity encountered at ~D",x)
                else
                        m := MOD(n,2)
                        sign := (-1)^m
                        if fracpart(x)=.5
                        then
                                skipit := 1
                        else
                                skipit := 0
                        sign*((dfPi^(n + 1))*cotdiffeval(n, dfPi*(-x), skipit)
                            + r_psi(n, 1.0 - x))
        else if n=0
        then
                - rPsiW(n,x)
        else
                r_gamma(float(n+1))*rPsiW(n,x)*(-1)^MOD(n+1,2)

---Amos' w function, with w(0,x) picked to be -psi(x) for x>0
rPsiW(n,x) ==
        if (x <=0 or n < 0)
        then
                HardError('"rPsiW not implemented for negative n or non-positive x")
        nd := 6         -- magic number for number of digits in a word?
        alpha := 3.5 + .40*nd
        beta := 0.21 + (.008677e-3)*(nd-3) + (.0006038e-4)*(nd-3)^2
        xmin := float(FLOOR(alpha + beta*n) + 1)
        if n>0
        then
                a := MIN(0,1.0/float(n)*LOG(DOUBLE_-FLOAT_-EPSILON/MIN(1.0,x)))
                c := EXP(a)
                if ABS(a) >= 0.001
                then
                        fln := x/c*(1.0-c)
                else
                        fln := -x*a/c
                bign := FLOOR(fln) + 1
--- Amos says to use alternative series for large order if ordinary
--- backwards recurrence too expensive
                if (bign < 15) and (xmin > 7.0+x)
                then
                        return PsiAsymptoticOrder(n,x,bign)
        if x>= xmin
        then
                return PsiAsymptotic(n,x)
---ordinary case -- use backwards recursion
        PsiBack(n,x,xmin)

PsiBack(n,x,xmin) ==
        xintpart := PsiIntpart(x)
        x0 := x-xintpart                ---frac part of x
        result := PsiAsymptotic(n,x0+xmin+1.0)
        for k in xmin..xintpart by -1 repeat
--- Why not decrement from x?   See Amos p. 498
                result := result + 1.0/((x0 + float(k))^(n+1))
        result


PsiIntpart(x) ==
        if x<0
        then
                result :=  -PsiInpart(-x)
        else
                result := FLOOR(x)
        return result


---Code for computation of derivatives of cot(z), necessary for
--- polygamma reflection formula.  If you want to compute n-th derivatives of
---cot(Pi*x), you have to multiply the result of cotdiffeval by Pi^n.

-- MCD: This is defined at the Lisp Level.
-- COT(z) ==
--         1.0/TAN(z)

cotdiffeval(n,z,skipit) ==
---skip=1 if arg z is known to be an exact multiple of Pi/2
        a := MAKE_-ARRAY(n+2)
        SETF(AREF(a,0),0.0)
        SETF(AREF(a,1),1.0)
        for i in 2..n repeat
                SETF(AREF(a,i),0.0)
        for l in 1..n repeat
                m := MOD(l+1,2)
                for k in m..l+1 by 2 repeat
                        if k<1
                        then
                                t1 := 0
                        else
                                t1 := -AREF(a,k-1)*(k-1)
                        if k>l
                        then
                                t2 := 0
                        else
                                t2 := -AREF(a,k+1)*(k+1)
                        SETF(AREF(a,k), t1+t2)
        --- evaluate d^N/dX^N cot(z) via Horner-like rule
        v := COT(z)
        sq := v^2
        s := AREF(a,n+1)
        for i in (n-1)..0 by -2 repeat
                s := s*sq + AREF(a,i)
        m := MOD(n,2)
        if m=0
        then
                s := s*v
        if skipit=1
        then
                if m=0
                then
                        return 0
                else
                        return AREF(a,0)
        else
                return s
--- nth derivatives of ln gamma for complex z, n=0,1,...
--- requires files rpsi (and dependents), floaterrors
--- currently defined only in right half plane until reflection formula
--- works

--- B. Char, June, 1990.

cPsi(n,z) ==
        x := REALPART(z)
        y := IMAGPART(z)
        if ZEROP y
        then    --- call real function if real
                return r_psi(n, x)
        if y<0.0
        then    -- if imagpart(z) negative, take conjugate of conjugate
                conjresult := cPsi(n,COMPLEX(x,-y))
                return COMPLEX(REALPART(conjresult),-IMAGPART(conjresult))
        nterms := 22
        bound := 10.0
        if x<0.0 --- and ABS(z)>bound and ABS(y)<bound
        then
                FloatError('"Psi implementation can't compute at ~S ",[n,z])
---             return cpsireflect(n,x,y,z)
        else if (x>0.0 and ABS(z)>bound ) --- or (x<0.0 and ABS(y)>bound)
        then
                return PsiXotic(n,PsiAsymptotic(n,z))
        else            --- use recursion formula
                m := CEILING(SQRT(bound*bound - y*y) - x + 1.0)
                result := COMPLEX(0.0,0.0)
                for k in 0..(m-1) repeat
                        result := result + 1.0/((z + float(k))^(n+1))
                return PsiXotic(n,result+PsiAsymptotic(n,z+m))

PsiXotic(n,result) ==
        r_gamma(float(n+1))*(-1)^MOD(n+1,2)*result

--- c parameter to 0F1, possibly complex
--- z argument to 0F1
--- Depends on files floaterror, floatutils

--- Program transcribed from Fortran,, p. 80 Luke 1977

chebf01 (c,z) ==
--- w scale factor so that 0<z/w<1
--- n    n+2 coefficients will be produced stored in an array
---  indexed from 0 to n+1.
--- See Luke's books for further explanation
        n := 75 --- ad hoc decision
---     if ABS(z)/ABS(c) > 200.0 and ABS(z)>10000.0
---     then
---             FloatError('"cheb0F1 not implemented for ~S < 1",[c,z])
        w := 2.0*z
--- arr will be used to store the Cheb. series coefficients
        four:= 4.0
        start := EXPT(10.0, -200)
        n1 := n+1
        n2 := n+2
        a3 := 0.0
        a2 := 0.0
        a1 := start     -- arbitrary starting value
        z1 := four/w
        ncount := n1
        arr := MAKE_-ARRAY(n2)
        SETF(AREF(arr,ncount) , start)  -- start off
        x1 := n2
        c1 := 1.0 - c
        for ncount in n..0 by -1 repeat
                divfac := 1.0/x1
                x1 := x1 -1.0
                SETF(AREF(arr,ncount) ,_
                        x1*((divfac+z1*(x1-c1))*a1 +_
                        (1.0/x1 + z1*(x1+c1+1.0))*a2-divfac*a3))
                a3 := a2
                a2 := a1
                a1 := AREF(arr,ncount)
        SETF(AREF(arr,0),AREF(arr,0)/2.0)
--  compute scale factor
        rho := AREF(arr,0)
        sum := rho
        p := 1.0
        for i in 1..n1 repeat
                rho := rho - p*AREF(arr,i)
                sum := sum+AREF(arr,i)
                p := -p
        for l in 0..n1 repeat
                SETF(AREF(arr,l), AREF(arr,l)/rho)
        sum := sum/rho
---     Now evaluate array at argument
        b := 0.0
        temp := 0.0
        for i in (n+1)..0 by -1 repeat
                cc := b
                b := temp
                temp := -cc + AREF(arr,i)
        temp


--- c parameter to 0F1
--- w scale factor so that 0<z/w<1
--- n    n+2 coefficients will be produced stored in an array
---  indexed from 0 to n+1.
--- See Luke's books for further explanation

--- Program transcribed from Fortran,, p. 80 Luke 1977
chebf01coefmake (c,w,n) ==
--- arr will be used to store the Cheb. series coefficients
        four:= 4.0
        start := EXPT(10.0, -200)
        n1 := n+1
        n2 := n+2
        a3 := 0.0
        a2 := 0.0
        a1 := start     -- arbitrary starting value
        z1 := four/w
        ncount := n1
        arr := MAKE_-ARRAY(n2)
        SETF(AREF(arr,ncount) , start)  -- start off
        x1 := n2
        c1 := 1.0 - c
        for ncount in n..0 by -1 repeat
                divfac := 1.0/x1
                x1 := x1 -1.0
                SETF(AREF(arr,ncount) ,_
                        x1*((divfac+z1*(x1-c1))*a1 +_
                        (1.0/x1 + z1*(x1+c1+1.0))*a2-divfac*a3))
                a3 := a2
                a2 := a1
                a1 := AREF(arr,ncount)
        SETF(AREF(arr,0),AREF(arr,0)/2.0)
--  compute scale factor
        rho := AREF(arr,0)
        sum := rho
        p := 1.0
        for i in 1..n1 repeat
                rho := rho - p*AREF(arr,i)
                sum := sum+AREF(arr,i)
                p := -p
        for l in 0..n1 repeat
                SETF(AREF(arr,l), AREF(arr,l)/rho)
        sum := sum/rho
        return([sum,arr])




---evaluation of Chebychev series of degree n at x, where the series's
---coefficients are given by the list in descending order (coef. of highest
---power first)

---May be numerically unstable for certain lists of coefficients;
--- could possibly reverse sequence of coefficients

--- Cheney and Hart p. 15.

--- B. Char, March 1990

--- If plist is a list of coefficients for the Chebychev approximation
--- of a function f(x), then chebderiveval computes the Chebychev approximation
--- of f'(x).  See Luke, "Special Functions and their approximations, vol. 1
--- Academic Press 1969., p. 329 (from Clenshaw and Cooper)

--- < definition to be supplied>

--- chebstareval(plist,n) computes a Chebychev approximation from a
--- coefficient list, using shifted Chebychev polynomials of the first kind
--- The defining relation is that T*(n,x) = T(n,2*x-1).  Thus the interval
--- [0,1] of T*n is the interval [-1,1] of Tn.

chebstarevalarr(coefarr,x,n) ==          -- evaluation of sum(C(n)*T*(n,x))

        b := 0
        temp := 0
        y := 2*(2*x-1)

        for i in (n+1)..0 by -1 repeat
                c := b
                b := temp
                temp := y*b -c + AREF(coefarr,i)
        temp - y*b/2

--Float definitions for Bessel functions I and J.
--External references:  cgamma, rgamma, chebf01coefmake, chebevalstarsf
-- floatutils

---BesselJ works for complex and real values of v and z
BesselJ(v,z) ==
---Ad hoc boundaries for approximation
        B1:= 10
        B2:= 10
        n := 50         --- number of terms in Chebychev series.
        --- tests for negative integer order
        (FLOATP(v) and ZEROP fracpart(v) and (v<0)) or (COMPLEXP(v) and ZEROP IMAGPART(v) and ZEROP fracpart(REALPART(v)) and REALPART(v)<0.0) =>
             --- odd or even according to v (9.1.5 A&S)
             --- $J_{-n}(z)=(-1)^{n} J_{n}(z)$
             BesselJ(-v,z)*EXPT(-1.0,v)
        (FLOATP(z) and  (z<0)) or (COMPLEXP(z) and REALPART(z)<0.0) =>
          --- negative argument (9.1.35 A&S)
          --- $J_{\nu}(z e^{m \pi i}) = e^{m \nu \pi i} J_{\nu}(z)$
             BesselJ(v,-z)*EXPT(-1.0,v)
        ZEROP z and ((FLOATP(v) and (v>=0.0)) or (COMPLEXP(v) and
           ZEROP IMAGPART(v) and REALPART(v)>=0.0)) =>  --- zero arg, pos. real order
            ZEROP v => 1.0  --- J(0,0)=1
            0.0  --- J(v,0)=0 for real v>0
        rv := ABS(v)
        rz := ABS(z)
        (rz>B1) and (rz > B2*rv) =>  --- asymptotic argument
            BesselJAsympt(v,z)
        (rv>B1) and (rv > B2*rz) => --- asymptotic order
            BesselJAsymptOrder(v,z)
        (rz< B1) and (rv<B1) =>       --- small order and argument
                 arg := -(z*z)/4.0
                 w := 2.0*arg
                 vp1 := v+1.0
                 [sum,arr] := chebf01coefmake(vp1,w,n)
                 ---if we get NaNs then half n
                 while not _=(sum,sum) repeat
                        n:=FLOOR(n/2)
                        [sum,arr] := chebf01coefmake(vp1,w,n)
                 ---now n is safe, can we increase it (we know that 2*n is bad)?
                 chebstarevalarr(arr,arg/w,n)/cgamma(vp1)*EXPT(z/2.0,v)
        true => BesselJRecur(v,z)
        FloatError('"BesselJ not implemented for ~S", [v,z])

BesselJRecur(v,z) ==
        -- boost order
        --Numerical.Recipes. suggest so:=v+sqrt(n.s.f.^2*v)
        so:=15.0*z
        -- reduce order until non-zero
        while ZEROP ABS(BesselJAsymptOrder(so,z)) repeat so:=so/2.0
        if ABS(so)<ABS(z) then so:=v+18.*SQRT(v)
        m:= FLOOR(ABS(so-v))+1
        w:=MAKE_-ARRAY(m)
        SETF(AREF(w,m-1),BesselJAsymptOrder(v+m-1,z))
        SETF(AREF(w,m-2),BesselJAsymptOrder(v+m-2,z))
        for i in m-3 .. 0 by -1 repeat
          SETF(AREF(w,i), 2.0 * (v+i+1.0) * AREF(w,i+1) /z -AREF(w,i+2))
        AREF(w,0)

BesselI(v,z) ==
        B1 := 15.0
        B2 := 10.0
        ZEROP(z) and FLOATP(v) and (v>=0.0) =>  --- zero arg, pos. real order
            ZEROP(v) => 1.0  --- I(0,0)=1
            0.0             --- I(v,0)=0 for real v>0
--- Transformations for negative integer orders
        FLOATP(v) and ZEROP(fracpart(v)) and (v<0) => BesselI(-v,z)
--- Halfplane transformations for Re(z)<0
        REALPART(z)<0.0 => BesselI(v,-z)*EXPT(-1.0,v)
--- Conjugation for complex order and real argument
        REALPART(v)<0.0 and not ZEROP IMAGPART(v) and FLOATP(z) =>
              CONJUGATE(BesselI(CONJUGATE(v),z))
---We now know that Re(z)>= 0.0
        ABS(z) > B1 =>    --- asymptotic argument case
                                FloatError('"BesselI not implemented for ~S",[v,z])
        ABS(v) > B1 =>
                                FloatError('"BesselI not implemented for ~S",[v,z])
---     case of small argument and order
        REALPART(v)>= 0.0 =>  besselIback(v,z)
        REALPART(v)< 0.0 =>
                        chebterms := 50
                        besselIcheb(z,v,chebterms)
        FloatError('"BesselI not implemented for ~S",[v,z])

--- Compute n terms of the chebychev series for f01
besselIcheb(z,v,n) ==
        arg := (z*z)/4.0
        w := 2.0*arg;
        vp1 := v+1.0;
        [sum,arr] := chebf01coefmake(vp1,w,n)
        result := chebstarevalarr(arr,arg/w,n)/cgamma(vp1)*EXPT(z/2.0,v)

besselIback(v,z) ==
        ipv := IMAGPART(v)
        rpv := REALPART(v)
        lm := MULTIPLE_-VALUE_-LIST(FLOOR(rpv))
        m := first(lm)    --- floor of real part of v
        n := 2*MAX(20,m+10)  --- how large the back recurrence should be
        tv := CADR(lm)+(v-rpv) ---  fractional part of real part of v
                        --- plus imaginary part of v
        vp1 := tv+1.0;
        result := BesselIBackRecur(v,m,tv,z,'"I",n)
        result := result/cgamma(vp1)*EXPT(z/2.0,tv)

--- Backward recurrence for Bessel functions.  Luke (1975), p. 247.
--- works for -Pi< arg z <= Pi and  -Pi < arg v <= Pi
BesselIBackRecur(largev,argm,v,z,type,n) ==
--- v + m = largev
        one := 1.0
        two := 2.0
        zero := 0.0
        start := EXPT(10.0,-40)
        z2 := two/z
        m2 := n+3
        w:=MAKE_-ARRAY(m2+1)
        SETF(AREF(w,m2), zero) --- start off
        if type = '"I"
        then
                val := one
        else
                val := -one
        m1 := n+2
        SETF(AREF(w,m1), start)
        m := n+1
        xm := float(m)
        ct1 := z2*(xm+v)
        --- initialize
        for m in (n+1)..1 by -1 repeat
                SETF(AREF(w,m), AREF(w,m+1)*ct1 + val*AREF(w,m+2))
                ct1 := ct1 - z2
        m := 1 + FLOOR(n/2)
        m2 := m + m -1
        if (v=0)
        then
                pn := AREF(w, m2 + 2)
                for m2 in (2*m-1)..3 by -2 repeat
                        pn := AREF(w, m2) - val *pn
                pn := AREF(w,1) - val*(pn+pn)
        else
                v1 := v-one
                xm := float(m)
                ct1 := v + xm + xm
                pn := ct1*AREF(w, m2 + 2)
                for m2 in (m+m -1)..3 by -2 repeat
                        ct1 := ct1 - two
                        pn := ct1*AREF(w,m2) - val*pn/xm*(v1+xm)
                        xm := xm - one
                pn := AREF(w,1) - val * pn
        m1 := n+2
        for m in 1..m1 repeat
                SETF(AREF(w,m), AREF(w,m)/pn)
        AREF(w,argm+1)




---Asymptotic functions for large values of z.  See p. 204 Luke 1969 vol. 1.

--- mu is 4*v^2
--- zsqr is z^2
--- zfth is z^4

BesselasymptA(mu,zsqr,zfth) ==
        (mu -1)/(16.0*zsqr) * (1 + (mu - 13.0)/(8.0*zsqr) + _
                (mu^2 - 53.0*mu + 412.0)/(48.0*zfth))

BesselasymptB(mu,z,zsqr,zfth) ==
        musqr := mu*mu
        z + (mu-1.0)/(8.0*z) *(1.0 + (mu - 25.0)/(48.0*zsqr) + _
                (musqr - 114.0*mu + 1073.0)/(640.0*zfth) +_
                (5.0*mu*musqr - 1535.0*musqr + 54703.0*mu - 375733.0)/(128.0*zsqr*zfth))

--- Asymptotic series only works when |v| < |z|.

BesselJAsympt (v,z) ==
        pi := dfPi
        mu := 4.0*v*v
        zsqr := z*z
        zfth := zsqr*zsqr
        SQRT(2.0/(pi*z))*EXP(BesselasymptA(mu,zsqr,zfth))*_
                COS(BesselasymptB(mu,z,zsqr,zfth) - pi*v/2.0 - pi/4.0)


---Asymptotic series for I.  See Whittaker, p. 373.
--- valid for -3/2 Pi < arg z < 1/2 Pi

BesselIAsympt(v,z,n) ==
        i := COMPLEX(0.0, 1.0)
        if (REALPART(z) = 0.0)
        then return EXPT(i,v)*BesselJ(v,-IMAGPART(z))
        sum1 := 0.0
        sum2 := 0.0
        fourvsq := 4.0*v^2
        two := 2.0
        eight := 8.0
        term1 := 1.0
---             sum1, sum2, fourvsq,two,i,eight,term1])
        for r in 1..n repeat
                term1 := -term1 *(fourvsq-(two*float(r)-1.0)^2)/_
                        (float(r)*eight*z)
                sum1 := sum1 + term1
                sum2 := sum2 + ABS(term1)
        sqrttwopiz := SQRT(two*dfPi*z)
        EXP(z)/sqrttwopiz*(1.0 + sum1 ) +_
                EXP(-(float(n)+.5)*dfPi*i)*EXP(-z)/sqrttwopiz*(1.0+ sum2)


---Asymptotic formula for BesselJ when order is large comes from
---Debye (1909).  See Olver, Asymptotics and Special Functions, p. 134.
---Expansion good for 0<=phase(v)<Pi
---A&S recommend "uniform expansion" with complicated coefficients and Airy function.
---Debye's Formula is in 9.3.7,9.3.9,9.3.10 of A&S
---FriCAS recurrence for u_{k}
---f(0)==1::EXPR INT
---f(n)== (t^2)*(1-t^2)*D(f(n-1),t)/2 + (1/8)*integrate( (1-5*t^2)*f(n-1),t)
BesselJAsymptOrder(v,z) ==
        sechalpha := z/v
        alpha := ACOSH(1.0/sechalpha)
        tanhalpha := SQRT(1.0-(sechalpha*sechalpha))
    --  cothalpha := 1.0/tanhalpha
        ca := 1.0/tanhalpha

        Pi := dfPi
        ca2:=ca*ca
        ca4:=ca2*ca2
        ca8:=ca4*ca4
        EXP(-v*(alpha-tanhalpha))/SQRT(2.0*Pi*v*tanhalpha)*_
        (1.0+_
        horner([              -5.0,                3.0],_
                                                                ca2)*ca/(v*24.0)+_
        horner([             385.0,             -462.0,              81.0],_
                                                                ca2)*ca2/(1152.0*v*v)+_
        horner([         -425425.0,           765765.0,         -369603.0,             30375.0],_
                                                                ca2)*ca2*ca/(414720.0*v*v*v)+_
        horner([       185910725.0,       -446185740.0,       349922430.0,        -94121676.0,         4465125.0],_
                                                                ca2)*ca4/(39813120.0*v*v*v*v)+_
        horner([   -188699385875.0,     566098157625.0,   -614135872350.0,     284499769554.0,    -49286948607.0,      1519035525.0],_
                                                                ca2)*ca4*ca/(6688604160.0*v*v*v*v*v)+_
        horner([1023694168371875.0,-3685299006138750.0,5104696716244125.0,-3369032068261860.0,1050760774457901.0,-127577298354750.0,2757049477875.0],_
                                                                ca2)*ca4*ca2/(4815794995200.0*v*v*v*v*v*v))


---  See Olver, p. 376-382.
BesselIAsymptOrder(v,vz) ==
        z := vz/v
        Pi := dfPi
---     Use reflection formula (Atlas, p. 492)  if v not in right half plane;  Is this always accurate?
        if REALPART(v)<0.0
        then return BesselIAsymptOrder(-v,vz) + 2.0/Pi*SIN(-v*Pi)*BesselKAsymptOrder(-v,vz)
---     Use the reflection formula (Atlas, p. 496) if z not in right half plane;
        if REALPART(vz) < 0.0
        then return EXPT(-1.0,v)*BesselIAsymptOrder(v,-vz)
        vinv := 1.0/v
        opzsqroh := SQRT(1.0+z*z)
        eta := opzsqroh + LOG(z/(1.0+opzsqroh))
        p := 1.0/opzsqroh
        p2 := p*p
        p4 := p2*p2
        u0p := 1.
        u1p := 1.0/8.0*p-5.0/24.0*p*p2
        u2p := (9.0/128.0+(-77.0/192.0+385.0/1152.0*p2)*p2)*p2
        u3p := (75.0/1024.0+(-4563.0/5120.0+(17017.0/9216.0-85085.0/82944.0*p2)_
                *p2)*p2)*p2*p
        u4p := (3675.0/32768.0+(-96833.0/40960.0+(144001.0/16384.0+(-7436429.0/663552.0+37182145.0/7962624.0*p2)*p2)*p2)*p2)*p4
        u5p := (59535.0/262144.0+(-67608983.0/9175040.0+(250881631.0/5898240.0+(-108313205.0/1179648.0+(5391411025.0/63700992.0-5391411025.0/191102976.0*p2)*p2)*p2)*p2)*p2)*p4*p
        hornerresult := horner([u5p,u4p,u3p,u2p,u1p,u0p],vinv)
        EXP(v*eta)/(SQRT(2.0*Pi*v)*SQRT(opzsqroh))*hornerresult


---See also Olver, pp. 376-382
BesselKAsymptOrder (v,vz) ==
        z := vz/v
        vinv := 1.0/v
        opzsqroh := SQRT(1.0+z*z)
        eta := opzsqroh + LOG(z/(1.0+opzsqroh))
        p := 1.0/opzsqroh
        p2 := p^2
        p4 := p2^2
        u0p := 1.
        u1p := (1.0/8.0*p-5.0/24.0*p^3)*(-1.0)
        u2p := (9.0/128.0+(-77.0/192.0+385.0/1152.0*p2)*p2)*p2
        u3p := ((75.0/1024.0+(-4563.0/5120.0+(17017.0/9216.0-85085.0/82944.0*p2)_
                *p2)*p2)*p2*p)*(-1.0)
        u4p := (3675.0/32768.0+(-96833.0/40960.0+(144001.0/16384.0+(-7436429.0/663552.0+37182145.0/7962624.0*p2)*p2)*p2)*p2)*p4
        u5p := ((59535.0/262144.0+(-67608983.0/9175040.0+(250881631.0/5898240.0+(-108313205.0/1179648.0+(5391411025.0/63700992.0-5391411025.0/191102976.0*p2)*p2)*p2)*p2)*p2)*p4*p)*(-1.0)
        hornerresult := horner([u5p,u4p,u3p,u2p,u1p,u0p],vinv)
        SQRT(dfPi/(2.0*v))*EXP(-v*eta)/(SQRT(opzsqroh))*hornerresult


-- Conversion between spad and lisp complex representations
s_to_c(c) == COMPLEX(first c, CDR c)
c_to_s(c) == CONS(REALPART c, IMAGPART c)
c_to_r(c) ==
    r := REALPART c
    i := IMAGPART c
    if ZEROP(i) or (ABS(i) <  1.0E-10*(ABS r)) then
        r
    else
        error "Result is not real."

c_to_rf(c) == COERCE(c_to_r(c), 'DOUBLE_-FLOAT)

-- Wrappers for functions in the special function package
c_lngamma(z) ==  c_to_s(lncgamma(s_to_c z))

c_gamma(z) ==  c_to_s(cgamma (s_to_c z))

c_psi(n, z) == c_to_s(cPsi(n, s_to_c(z)))

r_besselj(n, x) == c_to_r(BesselJ(n, x))
c_besselj(v, z) == c_to_s(BesselJ(s_to_c(v), s_to_c(z)))

r_besseli(n, x) == c_to_r(BesselI(n, x))
c_besseli(v, z) == c_to_s(BesselI(s_to_c(v), s_to_c(z)))

c_hyper0f1(a, z) == c_to_s(chebf01(s_to_c(a), s_to_c(z)))
