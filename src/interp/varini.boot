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


)package "BOOT"

 
-- Files used by the compiler.
$erLocMsgDatabaseName     := pathname '(co_-eng msgs a)
$erGlbMsgDatabaseName     := pathname '(co_-eng msgs i)

-- Variables to control phases and their output
 
$ncmParse :=            NIL
$ncmMacro :=            NIL
 
-- note flags to control the error message facility must have
-- the prefix $ncm, since catExcpts (in ncsetvar boot) strips the
-- prefix and uses the name.  ie. $ncmWarning ==> "Warning"
$ncmPhase :=      NIL
$compBugPrefix :=      '"Bug!"
$compErrorPrefix :=    '"Error"

-- Modes
$NoValueMode :=       'NoValueMode
 
--error message facility
$nopos   := ['noposition]
$showKeyNum   :=        NIL
 
-- Miscellaneous nonsense.
$newcompErrorCount :=           0
 
-- Items from STATS BOOT
-- $timerTicksPerSecond := INTERNAL_-TIME_-UNITS_-PER_-SECOND
$LINELENGTH := 80
 
-- Items from MSG BOOT I
$preLength := 11
$LOGLENGTH := $LINELENGTH - 6
$specificMsgTags := []
 
$imPrTagGuys := ['unimple, 'bug, 'debug, 'say, 'warn]
$toWhereGuys := ['fileOnly, 'screenOnly ]
$imPrGuys    := ['imPr]
$repGuys     := ['noRep, 'rep]
$attrCats    := ['$imPrGuys, '$toWhereGuys, '$repGuys]
 
 
$ncMsgList := nil

--## Bug in RIOS version of KCL
NeedAtLeastOneFunctionInThisFile(x) == x
