-- Standard include file for using or extending FriCAS library via Aldor

#library FriCASLib "fricas"
import from FriCASLib;
inline from FriCASLib;

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
