-- Standard include file for using or extending Axiom library via AXIOM-XL

#library AxiomLib "axiom"
import from AxiomLib;
inline from AxiomLib;

macro {
        rep x == x @ % pretend Rep;
        per r == r @ Rep pretend %;
}

--import from Integer, PositiveInteger, NonNegativeInteger, SingleInteger;
--import from Float, DoubleFloat;

import { true: %; false: % } from Boolean;
import {
	string:		Literal -> %;
} from String;
