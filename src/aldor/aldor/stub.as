-----------------------------------------------------------------------------
----
---- stub.as:  Stub domain/category definitions for the Axiom library.
----
-----------------------------------------------------------------------------
---- Copyright (c) 1990-2007 Aldor Software Organization Ltd (Aldor.org).
-----------------------------------------------------------------------------

import from AxiomLib;
inline from AxiomLib;

macro {
	StubDomain X == X: with == add;
}

StubDomain Boolean;
StubDomain InputForm;
StubDomain NonNegativeInteger;
StubDomain PositiveInteger;
StubDomain SingleInteger;
StubDomain String;

StubDomain Equation(T: Type);
StubDomain List(T: Type);
StubDomain SegmentBinding(T: Type);
StubDomain UniversalSegment(T: Type);
StubDomain Vector(T: Type);

SubsetCategory (C: Category, D: with) : Category == C with {
	coerce:		% -> D;
	retract:	D -> %;
}
