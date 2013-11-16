

import from AxiomLib;inline from AxiomLib;

Byte ==> XByte;  --!! Needed to disambiguate

+++ `Machine' provides machine-level types and operations.
Machine: with {
	-- Types
	Bool:       Type;
	Byte:       Type;
	HInt:       Type;
	SInt:       Type;
	BInt:       Type;
	Char:       Type;
	SFlo:       Type;
	DFlo:       Type;
	Nil:        Type;
	Arr:        Type;
	Rec:        Type;
	Ptr:        Type;
	Word:       Type;
} == add {
	Bool:  Type == add;
	Byte:  Type == add;
	HInt:  Type == add;
	SInt:  Type == add;
	BInt:  Type == add;

	Char:  Type == add;

	SFlo:  Type == add;
	DFlo:  Type == add;

	Nil:   Type == add;
	Arr:   Type == add;
	Rec:   Type == add;
	Ptr:   Type == add;

	Word:  Type == add;

}
