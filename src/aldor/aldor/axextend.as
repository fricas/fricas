-----------------------------------------------------------------------------
----
---- axextend.as: Extensions to domains from the Axiom library.
----
-----------------------------------------------------------------------------
---- Copyright (c) 1990-2007 Aldor Software Organization Ltd (Aldor.org).
-----------------------------------------------------------------------------

-- This file extends some Axiom types for general use from AXIOM-XL programs.

import from AxiomLib;
inline from AxiomLib;

macro {
        rep x == x @ % pretend Rep;
        per r == r @ Rep pretend %;

	Bit	== Boolean;
	Str	== String;
	SI	== SingleInteger;
	I	== Integer;
	NNI	== NonNegativeInteger;
	PI	== PositiveInteger;
	BVal	== BuiltinValue;
	BArr	== BuiltinArray;
	SEG	== Segment;
	UNISEG	== UniversalSegment;
}

extend Symbol : with {
	string:		Literal -> %;
}
== add {
	string (l: Literal) : % == string(l)$Str::%;
}

extend SingleInteger : with {
	integer: Literal -> %;

	coerce:  Integer -> %;

	+: (%, %) -> %;
	-: (%, %) -> %;
	*: (%, %) -> %;

	=:  (%, %) -> Bit;
	~=: (%, %) -> Bit;

	>=: (%, %) -> Bit;
	<=: (%, %) -> Bit;
	>:  (%, %) -> Bit;
	<:  (%, %) -> Bit;

	zero?: % -> Bit;
	export from UNISEG %;

	1: %;
	0: %;
} == add {
	Rep ==> SInt;
	asBool(x) ==> x pretend Boolean;
	import from Machine, String, Integer;
	import {
		SIntPlus: (SInt, SInt) -> SInt;
		SIntMinus: (SInt, SInt) -> SInt;
		SIntTimes: (SInt, SInt) -> SInt;

		SIntEQ:        (SInt, SInt) -> Bool;
		SIntNE:        (SInt, SInt) -> Bool;
		SIntLT:        (SInt, SInt) -> Bool;
		SIntLE:        (SInt, SInt) -> Bool;
		ArrToSInt:     (Arr) 	    -> SInt;

		SInt1:		() -> SInt;
		SInt0:		() -> SInt;

		SIntMin:	() -> SInt;
		SIntMax:	() -> SInt;

		BIntToSInt:	BInt -> SInt;
		SIntToBInt:	SInt -> BInt;
		BIntLE:		(BInt, BInt) -> Bool;
	} from Builtin;

	integer(l: Literal): % == per ArrToSInt(l pretend Arr);

	(a: %) + (b: %): % == per(SIntPlus(rep a, rep b));
	(a: %) - (b: %): % == per(SIntMinus(rep a, rep b));
	(a: %) * (b: %): % == per(SIntTimes(rep a, rep b));

	(a: %) =  (b: %): Bit == asBool(SIntEQ(rep a, rep b));
	(a: %) ~= (b: %): Bit == asBool(SIntNE(rep a, rep b));
	(a: %) <  (b: %): Bit == asBool(SIntLT(rep a, rep b));
	(a: %) <= (b: %): Bit == asBool(SIntLE(rep a, rep b));
	(a: %) >  (b: %): Bit == not(a<=b);
	(a: %) >= (b: %): Bit == not(a<b);

	one(): 	%   	== per SInt1();
	zero():	%   	== per SInt0();

	1: % == per SInt1();
	0: % == per SInt0();

	zero?(n: %): Bit == n = zero();

	coerce(n: Integer): % == {
--		nn := n pretend BInt;
--		if asBool(nn <= SIntToBInt(SIntMax())) and  asBool(SIntToBInt(SIntMin()) <= nn)  then
--			per(BIntToSInt(nn));
--		else
--			error "Integer too big to fit in a SingleInteger";
		if (n > per(SIntMax())::Integer or n < per(SIntMin())::Integer)	then
			error "Integer too big to fit in a SingleInteger";
		else
			per(BIntToSInt(n pretend BInt));
	}
}

extend Integer : with {
	integer: Literal -> %;

	-:		     % -> %;
	+:		(%, %) -> %;
	-:		(%, %) -> %;
	*:		(%, %) -> %;

	=:		(%, %) -> Bit;
	<:		(%, %) -> Bit;
	<=:		(%, %) -> Bit;
	>:		(%, %) -> Bit;
	>=:		(%, %) -> Bit;

	coerce:		SI -> %;
	coerce:		Integer -> %;
	coerce:		% -> Integer;
	coerce:		% -> %;

	one: 		() -> %;
	zero:		() -> %;
	zero?:		%  -> Bit;

	1: %;
	0: %;

	quo: (%, %) -> %;
	rem: (%, %) -> %;

	export from UNISEG %;
}
== add {
	Rep ==> BInt;
	asBool(x) ==> x pretend Boolean;

	import from Machine;
	import {
		BIntNegate:	BInt -> BInt;

		BIntPlus:  (BInt, BInt) -> BInt;
		BIntMinus: (BInt, BInt) -> BInt;
		BIntTimes: (BInt, BInt) -> BInt;

		BIntEQ:        (BInt, BInt) -> Bool;
		BIntNE:        (BInt, BInt) -> Bool;
		BIntLT:        (BInt, BInt) -> Bool;
		BIntLE:        (BInt, BInt) -> Bool;

		BIntQuo:	(BInt, BInt) -> BInt;
		BIntRem:	(BInt, BInt) -> BInt;

		ArrToBInt:     (Arr) 	    -> BInt;

		BInt1:		() -> BInt;
		BInt0:		() -> BInt;

		SIntToBInt:	SInt -> BInt;

	} from Builtin;

	integer(l: Literal): % == per ArrToBInt(l pretend Arr);

	-(a: %): % == per BIntNegate(rep(a));

	(a: %) + (b: %): % == per(BIntPlus(rep a, rep b));
	(a: %) - (b: %): % == per(BIntMinus(rep a, rep b));
	(a: %) * (b: %): % == per(BIntTimes(rep a, rep b));

	(a: %) =  (b: %): Bit == asBool(BIntEQ(rep a, rep b));
	(a: %) ~= (b: %): Bit == asBool(BIntNE(rep a, rep b));

	(a: %) <  (b: %): Bit == asBool(BIntLT(rep a, rep b));
	(a: %) <= (b: %): Bit == asBool(BIntLE(rep a, rep b));
	(a: %) >  (b: %): Bit == not(a<=b);
	(a: %) >= (b: %): Bit == not(a<b);

	-- see SI for coerce: I -> SI
	coerce (n: SI) : % == per(SIntToBInt(n pretend SInt));
	coerce (i: Integer): % == i;
	coerce (i: %): Integer == i;
	coerce (i: %): %       == i;

	one():  % == per BInt1();
	zero(): % == per BInt0();

	1: % == per BInt1();
	0: % == per BInt0();

	zero?(x: %): Bit == x = zero();

	(quo)(a: %, b: %): % == per(BIntQuo(rep(a), rep(b)));
	(rem)(a: %, b: %): % == per(BIntRem(rep(a), rep(b)));
}

extend NonNegativeInteger : with {
	coerce:		% -> SI;
	coerce:		SI -> %;

	coerce:		% -> I;
	coerce:		I -> %;

	one:		() -> %;
	zero:		() -> %;

	1: %;
	0: %;
	export from UNISEG %;

	+:		(%, %) -> %;
	-:		(%, %) -> %;
	*:		(%, %) -> %;

	=:		(%, %) -> Bit;
	<:		(%, %) -> Bit;
	<=:		(%, %) -> Bit;
	>:		(%, %) -> Bit;
	>=:		(%, %) -> Bit;

	quo: (%, %) -> %;
	rem: (%, %) -> %;
}
== add {
	Rep ==> Integer;
	import from Rep;

	import {
		AXL_-IntegerIsNonNegative:	Integer -> Bit;
	} from Foreign Lisp;

	coerce (n: %) : SI == n::I::SI;
	coerce (n: SI) : % == n::I::%;

	coerce (n: %) : I == rep n;
	coerce (i: I):  % == {
		import from String;
		if AXL_-IntegerIsNonNegative i then
			per i
		else
			error "Need a non-negative integer"
	}

	-- see axlit.as for coerce: I -> NNI

	one():		% == per one();
	zero():		% == per zero();
	1: % == per 1;
	0: % == per 0;

	zero?(n: %):  Bit == zero?(rep n);

	(a: %) + (b: %): % == per(rep(a) + rep(b));
	(a: %) * (b: %): % == per(rep(a) * rep(b));
	(a: %) - (b: %): % == (rep(a) - rep(b))::%;

	(a: %) =  (b: %): Bit == rep(a) =$Integer  rep(b);

	(a: %) <  (b: %): Bit == rep(a) <  rep(b);
	(a: %) >  (b: %): Bit == rep(a) >  rep(b);
	(a: %) <= (b: %): Bit == rep(a) <= rep(b);
	(a: %) >= (b: %): Bit == rep(a) >= rep(b);

	(quo)(a: %, b: %): % == per(rep(a) quo rep(b));
	(rem)(a: %, b: %): % == per(rep(a) rem rep(b));
	(mod)(a: %, b: %): % == per(rep(a) rem rep(b));
}

extend PositiveInteger : with {
	coerce:		% -> SI;
	coerce:		SI -> %;

	coerce:		% -> I;
	coerce:		I -> %;

	coerce:		% -> NNI;
	coerce:		NNI -> %;

	one: 		() -> %;

	1: %;
	export from UNISEG %;

	+:		(%, %) -> %;
	-:		(%, %) -> %;
	*:		(%, %) -> %;

	=:		(%, %) -> Bit;
	<:		(%, %) -> Bit;
	<=:		(%, %) -> Bit;
	>:		(%, %) -> Bit;
	>=:		(%, %) -> Bit;

	quo: (%, %) -> %;
	rem: (%, %) -> %;
	mod: (%, %) -> %;
}
== add {
	Rep ==> Integer;
	import from Rep;

	import {
		AXL_-IntegerIsPositive:		Integer -> Bit;
	} from Foreign Lisp;

	coerce (n: %) : SI == n::I::SI;
	coerce (n: SI) : % == n::I::%;

	coerce (n: %) : I == rep n;
	coerce (i: Integer) : % == {
		import from String;
		if AXL_-IntegerIsPositive i then
			per i
		else
			error "Need a positive integer"
	}

	coerce (n: %) : NNI == n::I::NNI;
	coerce (i: NNI) : % == i::I::%;

	one(): % == per one();
	1: % == per 1;

	(a: %) + (b: %): % == per(rep(a) + rep(b));
	(a: %) * (b: %): % == per(rep(a) * rep(b));
	(a: %) - (b: %): % == (rep(a) - rep(b))::%;

	(a: %) =  (b: %): Bit == rep(a) =$Integer  rep(b);

	(a: %) <  (b: %): Bit == rep(a) <  rep(b);
	(a: %) >  (b: %): Bit == rep(a) >  rep(b);
	(a: %) <= (b: %): Bit == rep(a) <= rep(b);
	(a: %) >= (b: %): Bit == rep(a) >= rep(b);

	(quo)(a: %, b: %): % == per(rep(a) quo rep(b));
	(rem)(a: %, b: %): % == per(rep(a) rem rep(b));
	(mod)(a: %, b: %): % == a rem b;
}

extend List(S: Type): with {
        #:		% -> NNI;
        reverse!:	% -> %;
	sort:		((S, S) -> Bit, %) -> %;

	generator:	% -> Generator S;
	bracket:	Generator S -> %;

	export from S;
	export from 'first';
	export from 'rest';
	export from 'last';
	export from 'value';
} == add {
        macro Rep == P;
        macro R   == Record(first: S, rest: Rep);

	--!! Remove when cascaded imports can be inferred in the correct order.
	import from S, 'first', 'rest', String;

        -- This local domain gives an untagged union of
        -- Records and Nil.
        P: with {
                nil?:    % -> Boolean;
                nilptr:  %;
                recptr:  R -> %;
                value:   % -> R;
        } == add {
                import from Machine;
		import {
			PtrIsNil: Ptr -> Bool;
			PtrNil:    () -> Ptr;
		} from Builtin;
                macro  Rep == Ptr$Machine;
                import from Rep;
                nil? (p: %): Boolean == (PtrIsNil rep p) pretend Boolean;
                nilptr: %        == per(PtrNil());
                recptr(r: R): %  == r pretend %;
                value(p: %): R   == p pretend R;
        }

        import from R;

        default n: SingleInteger;

        -- non-empty % as a Record
        rec(x: %): R == value rep x;

        empty?(l: %): Boolean  == nil? rep l;
        #(l: %): SingleInteger == { n:=0; for i in l repeat n:=n+1; n }
        nil: %     == per nilptr;
        empty():%  == nil;
        cons(a: S, l: %): % == per recptr [a, rep l];
	test	 (x: %): Boolean	 == not empty? x;

        first(l: %): S == {
		empty? l => error "Cannot select `first' of empty list.";
		rec(l).first;
	}
        rest(l: %): % == {
		empty? l => l;
		per rec(l).rest;
	}
        setFirst!(l: %, a: S): S == {
		empty? l => error "Cannot set `first' of empty list.";
		rec(l).first := a
	}
        setRest! (l: %, t: %): % == {
		empty? l => error "Cannot set `rest' of empty list.";
		per (rec(l).rest := rep t)
	}


	reverse! (x: %) : % == {
		empty? x => x;
		empty?(y := rest x) => x;
		setrest!(x, empty());
		while not empty? y repeat {
			z := rest y;
			setrest!(y, x);
			x := y;
			y := z;
		}
		x;
	}

	# (l: %) : NNI == {
		k: NNI := 0;
		for x in l repeat k := k + 1;
		k;
	}

	generator (l: %) : Generator S == generate {
		while l repeat {
			yield first l;
			l := rest l;
		}
	}

	[g: Generator S]: % == {
		--!! Remove the local when we can use the export.
		local nil: % := empty();

		h := l := nil;
		for a in g repeat {
			t := l;
			l := cons(a, nil);
			empty? t => h := l;
			setrest!(t, l);
		}
		h;
	}
}

extend Vector(S: Type): with {
#if VectorConditionals
        if S has SetCategory then SetCategory;
        if S has OrderedSet then OrderedSet;
#endif

	generator:	% -> Generator S;
	bracket:	Generator S -> %;
	bracket:	Tuple S     -> %;

	new:		(SI, S) -> %;
	#:		% -> SI;
	apply:		(%, SI) -> S;
	set!:		(%, SI, S) -> ();
}
== add {
	Rep ==> BArr;
	import from Rep, SI, S;

#if VectorConditionals
        if S has SetCategory then {
		(x: %) = (y: %) : Bit == {
			(#x ~= #y)$SI => false;
			for xx in x for yy in y repeat
				xx ~= yy => return false;
			true;
		}

		--coerce(l1: %) : OutputForm == l1 pretend OutputForm;
        }
	if S has OrderedSet then {
		(a: %) < (b: %) : Bit == {
			for aa in a for bb in b repeat
				aa ~= bb => return aa < bb;
			(#a < #b)$SI;
		}

	}
#endif
	[g: Generator S]: % == {
		import from List S;
		l := empty()@List(S);
		n := zero();
		for e in g repeat {
			l := cons(e, l);
			n := inc n;
		}
		v := per new n;
		while l repeat {
			v.n := first l;
			l := rest l;
			n := dec n;
		}
		v
	}

	[t: Tuple S]: % == {
		n := length t;
		v := per new n;
		while leq(one(), n) repeat {
			v.n := element(t, n);
			n := dec n;
		}
		v
	}

	new(n: SI, x: S): % == {
		v := per new n;
		while leq(one(), n) repeat {
			v.n := x;
			n := dec n;
		}
		v
	}

	generator(v: %): Generator S == {
		n := (#v)@SI;
		i := one();
		generate while leq(i, n) repeat {
			yield v.i;
			i := inc i;
		}
	}

	#(v: %): SI ==
		# rep v;

	apply(x: %, i: SI): S ==
		(rep x).(dec i) pretend S;

	set!(x: %, n: SI, v: S): () ==
		(rep x).(dec n) := v pretend BVal;
}

extend Matrix(R: Ring): with {
	apply:		(%, Integer, Integer)    -> R;
	set!: 		(%, Integer, Integer, R) -> R;
	qelt:		(%, Integer, Integer)    -> R;
	qsetelt!:	(%, Integer, Integer, R) -> R;
} == add {
	Rep ==> BArr;
	import from Rep;
	asR(x) ==> x pretend R;

	local apply(m: %, i: SI): BArr == rep(m).i pretend BArr;
	local dec2si(x: Integer): SingleInteger == (x-1) pretend SI;
	apply(m: %, i: I, j: I): R == asR((m).(dec2si i).(dec2si j));
	set!(m: %, i: I, j: I, v: R): R == {
		(set!((m).(dec2si i),(dec2si j), v pretend BVal));
		v
	}

	qelt(m: %, i: I, j: I): R == asR((m).(dec2si i).(dec2si j));
	qsetelt!(m: %, i: I, j: I, v: R): R == {
		(set!((m).(dec2si i),(dec2si j), v pretend BVal));
		v
	}
}

ShouldHaveSegmentGenerator(S) ==>
	S has OrderedSet and S has AbelianSemiGroup
	and S has with { coerce: I -> %; }

define GeneratorCategory(S: Type): Category == with {
	generator:	% -> Generator S;
	by:		(%, I) -> %;
}


extend Segment (S: Type) : with {
	if ShouldHaveSegmentGenerator(S) then
		GeneratorCategory S;
}
== add {
	import from Machine;
	import {
		BInt1:		() -> BInt;
		BInt0:		() -> BInt;
	} from Builtin;

--	local one(): Integer == BInt1() pretend Integer;
--	local zero(): Integer == BInt1() pretend Integer;

	if ShouldHaveSegmentGenerator(S) then {
		Rep ==> Record(low: S, high: S, incrz: I);
		import from Rep, Bit, SingleInteger;
		-- ONE is to allow us to environment-merge segments
		ONE ==> one();
		ZERO ==> zero();

		(s: %) by (n: I) : % == { rep(s).incrz := n; s }
		low(x: %): S  == rep(x).low;
		high(x: %): S == rep(x).high;
		incrx(x: %): I == rep(x).incrz;

		(a: S) .. (b: S): % == per[a, b, ONE];

		import from S;
		generator(x: %): Generator S == generate {
			l: S := low x;
			h: S := high x;
			inc: S := incrx(x)::S;
			if incrx(x) >= ZERO then
				while l <= h repeat {
					yield l;
					l := l + inc;
				}
			else if incrx(x) < ZERO then
				while l >= h repeat {
					yield l;
					l := l + inc;
			}
		}
	}
}

extend UniversalSegment (S: Type) : with {
	if ShouldHaveSegmentGenerator(S) then {
		GeneratorCategory S;
	}
}
== add {
	import from Machine;
	import {
		BInt1:		() -> BInt;
		BInt0:		() -> BInt;
	} from Builtin;

	local lone(): Integer == BInt1() pretend Integer;
	local lzero(): Integer == BInt1() pretend Integer;

	if ShouldHaveSegmentGenerator(S) then {
	    	Rec  ==> Record(low: S, high: S, incr: I);
	    	Rec2 ==> Record(low: S, incr: I);
	    	Rep ==> Union(unbdd: Rec2, bdd: Rec);
		import from Rep, Bit, Integer;
		-- need the pretend 'cos of compiler problems
		ONE  ==> lone();
		ZERO ==> lzero();

		(a: S) .. (b: S): % == per[[a,b,ONE]];
		(a: S) .. :	  % == per[[a,ONE]];

		hasHi(x: %): Boolean == rep(x) case bdd;

		low(x: %): S  == {
			rep(x) case bdd => rep(x).bdd.low;
			rep(x).unbdd.low;
		}
		-- this will produce an error msg if nec.
		high(x: %): S == rep(x).bdd.high;

		incr(x: %): I == {
			rep(x) case bdd => rep(x).bdd.incr;
			rep(x).unbdd.incr;
		}
		(x: %) by (s: I) : % == {
			rep(x) case bdd => { rep(x).bdd.incr := s; x }
			rep(x).unbdd.incr := s; x
		}

		import from S;
		generator(x: %): Generator S == generate {
			rep(x) case unbdd => {
				l := rep(x).unbdd.low;
				inc := (rep(x).unbdd.incr)::S;
				repeat {
					yield l;
					l := l + inc;
				}
			}
			h: S := rep(x).bdd.high;
			l := rep(x).bdd.low;
			inc := (rep(x).bdd.incr)::S;
			if incr(x) >= ZERO then
				while l <= h repeat {
					yield l;
					l := l + inc;
				}
			else if incr(x) < ZERO then
				while l >= h repeat {
					yield l;
					l := l + inc;
			}
		}
	}
}
