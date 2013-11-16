-----------------------------------------------------------------------------
----
---- axlit.as: Function definitions needed by the Axiom library.
----
-----------------------------------------------------------------------------
---- Copyright (c) 1990-2007 Aldor Software Organization Ltd (Aldor.org).
-----------------------------------------------------------------------------

-- This file extends some Axiom types provide literal formers and other
-- functions for compiling Axiom-generated .ax files.

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

import {
	AXL_-error:			String  -> Exit;
} from Foreign Lisp;

error	(s: String) : Exit == AXL_-error s;
integer	(l: Literal) : Literal == l;

--- Builtin value type.  Used to store data values which fit in a single word.
BuiltinValue : with == add;

--- Builtin array type.  0-based indexing.
BuiltinArray : with {
	new:		SI -> %;
	#:		% -> SI;
	apply:		(%, SI) -> BVal;
	set!:		(%, SI, BVal) -> ();
}
== add {
	import {
		AXL_-arrayNew:		SI -> %;
		AXL_-arraySize:		% -> SI;
		AXL_-arrayRef:		(%, SI) -> BVal;
		AXL_-arraySet:		(%, SI, BVal) -> ();
	} from Foreign Lisp;

	new	(n: SI) : % == AXL_-arrayNew n;
	#	(x: %) : SI == AXL_-arraySize x;

	apply	(x: %, n: SI) : BVal ==
		AXL_-arrayRef(x, n);

	set!	(x: %, n: SI, v: BVal) : () ==
		AXL_-arraySet(x, n, v);
}

extend String : with {
	string:		Literal -> %;
}
== add {
	import {
		AXL_-LiteralToString:	Literal -> %;
	} from Foreign Lisp;

	string (l: Literal) : % == AXL_-LiteralToString l;
}

extend Symbol : with {
	string:		Literal -> %;
}
== add {
	string (l: Literal) : % == string(l)$String::%;
}

extend SingleInteger : with {
	integer:	Literal -> %;
	coerce:		I -> %;

	zero:		() -> %;
	one:		() -> %;
	inc:		% -> %;
	dec:		% -> %;
	leq:		(%, %) -> Bit;
	spit:		% -> ();
}
== add {
	Rep ==> Integer;

	import {
		AXL_-LiteralToSingleInteger:   	Literal -> %;
		AXL_-zerofnSingleInteger:	() -> %;
		AXL_-onefnSingleInteger:		() -> %;
		AXL_-incSingleInteger:		% -> %;
		AXL_-decSingleInteger:		% -> %;
		AXL_-leSingleInteger:		(%, %) -> Bit;
		AXL_-spitSInt:			% -> ();
	} from Foreign Lisp;

	integer	(l: Literal) : %	== AXL_-LiteralToSingleInteger l;
	coerce	(n: I) : %		== per n;

	zero	() : %			== AXL_-zerofnSingleInteger();
	one	() : %			== AXL_-onefnSingleInteger();
	inc	(n: %) : %		== AXL_-incSingleInteger n;
	dec	(n: %) : %		== AXL_-decSingleInteger n;
	leq	(x: %, y: %) : Bit	== AXL_-leSingleInteger(x, y);
	spit	(x: %) : ()		== AXL_-spitSInt x;
}

extend Integer : with {
	integer:	Literal -> %;
}
== add {
	import {
		AXL_-LiteralToInteger:   	Literal -> %;
	} from Foreign Lisp;

	integer (l: Literal) : % == AXL_-LiteralToInteger l;
}

extend NonNegativeInteger : with {
	integer:	Literal -> %;
	coerce: 	Integer -> %;
}
== add {
	import {
		AXL_-IntegerIsNonNegative:	Integer -> Bit;
	} from Foreign Lisp;
	Rep ==> Integer;
	import from Rep, String;

	integer (l: Literal) : % == integer(l)$Integer::%;

	coerce (i: Integer) : % == {
		if AXL_-IntegerIsNonNegative i then
			per i
		else
			error "Need a non-negative integer"
	}
}

extend PositiveInteger : with {
	integer:	Literal -> %;
	coerce: 	Integer -> %;
}
== add {
	import {
		AXL_-IntegerIsPositive:		Integer -> Bit;
	} from Foreign Lisp;
	Rep ==> Integer;
	import from Rep, String;
	integer (l: Literal) : % == integer(l)$Integer::%;
	coerce (i: Integer) : % == {
		if AXL_-IntegerIsPositive i then
			per i
		else
			error "Need a positive integer"
	}

}

extend DoubleFloat: with {
	float:		Literal -> %;
}
== add {
	import {
		AXL_-LiteralToDoubleFloat:	Literal -> %;
	} from Foreign Lisp;

	float (l: Literal) : % == AXL_-LiteralToDoubleFloat l;
}

extend Float: with {
	float:		Literal -> %;
}
== add {
	import {
		AXL_-StringToFloat:		String  -> %;
	} from Foreign Lisp;

	import from String;
	float (l: Literal) : % == AXL_-StringToFloat string l;
}

extend Tuple (T: Type) : with {
	length:		% -> SI;
	element:	(%, SI) -> T;

	export from T;
}
== add {
	Rep ==> Record(sz: SI, values: BArr);
	import from Rep;

	length (t: %) : SI == rep(t).sz;
	element(t: %, n: SI): T == (rep(t).values.(dec n)) pretend T;
}

extend List (S: Type) : with {
	bracket:	Tuple S -> %;

	nil:		%;
	first:		% -> S;
	rest:		% -> %;
	cons:		(S, %) -> %;

	empty:		() -> %;
	empty?:		% -> Bit;
	test:		% -> Bit;

	setfirst!:	(%, S) -> S;
	setrest!:	(%, %) -> %;
}
== add {
	import {
		AXL_-nilfn:		() -> %;
		AXL_-car:		% -> S;
		AXL_-cdr:		% -> %;
		AXL_-cons:		(S, %) -> %;
		AXL_-rplaca:		(%, S) -> S;
		AXL_-rplacd:		(%, %) -> %;
		AXL_-null?:		% -> Bit;
	} from Foreign Lisp;

	[t: Tuple S]: % == {
		import {
			one:	() -> %;
			dec:	% -> %;
			leq:	(%, %) -> Bit;
		} from SI;

		--!! Remove the local when we can use the export.
		local nil: % := empty();

		l := nil;
		i := length t;
		while leq(one(), i) repeat {
			l := cons(element(t, i), l);
			i := dec i;
		}
		l;
	}

	-- Redefine a selection of List operations for efficiency.

	nil	 : %		 == AXL_-nilfn();
	first	 (x: %): S	 == AXL_-car x;
	rest	 (x: %): %	 == AXL_-cdr x;
	cons	 (x: S, y: %): % == AXL_-cons(x, y);
	setfirst!(x: %, y: S): S == AXL_-rplaca(x, y);
	setrest! (x: %, y: %): % == AXL_-rplacd(x, y);

	empty	 (): %		 == AXL_-nilfn();
	empty?	 (x: %): Bit	 == AXL_-null? x;
	test	 (x: %): Bit	 == not empty? x;
}
