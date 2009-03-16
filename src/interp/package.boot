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


mkList u ==
  u => ["LIST",:u]
  nil
 
mkOperatorEntry(opSig is [op,sig,:flag],pred,count) ==
  null flag => [opSig,pred,["ELT","$",count]]
  first flag="constant" => [[op,sig],pred,["CONST","$",count]]
  systemError ["unknown variable mode: ",flag]
 
--% Code for encoding function names inside package or domain
 
encodeFunctionName(fun,package is [packageName,:arglist],signature,sep,count)
   ==
    signature':= substitute("$",package,signature)
    reducedSig:= mkRepititionAssoc [:rest signature',first signature']
    encodedSig:=
      ("STRCONC"/[encodedPair for [n,:x] in reducedSig]) where
        encodedPair() ==
          n=1 => encodeItem x
          STRCONC(STRINGIMAGE n,encodeItem x)
    encodedName:= INTERNL(getAbbreviation(packageName,#arglist),";",
        encodeItem fun,";",encodedSig, sep,STRINGIMAGE count)
    if $LISPLIB then
      $lisplibSignatureAlist:=
        [[encodedName,:signature'],:$lisplibSignatureAlist]
    encodedName
 
splitEncodedFunctionName(encodedName, sep) ==
    -- [encodedPackage, encodedItem, encodedSig, sequenceNo] or NIL
    -- sep0 is the separator used in "encodeFunctionName".
    sep0 := '";"
    if not STRINGP encodedName then
        encodedName := STRINGIMAGE encodedName
    null (p1 := STRPOS(sep0, encodedName, 0,    '"*")) => nil
    null (p2 := STRPOS(sep0, encodedName, p1+1, '"*")) => 'inner
--  This is picked up in compile for inner functions in partial compilation
    null (p3 := STRPOS(sep,  encodedName, p2+1, '"*")) => nil
    s1 := SUBSTRING(encodedName, 0,    p1)
    s2 := SUBSTRING(encodedName, p1+1, p2-p1-1)
    s3 := SUBSTRING(encodedName, p2+1, p3-p2-1)
    s4 := SUBSTRING(encodedName, p3+1, nil)
    [s1, s2, s3, s4]
 
mkRepititionAssoc l ==
  mkRepfun(l,1) where
    mkRepfun(l,n) ==
      null l => nil
      l is [x] => [[n,:x]]
      l is [x, =x,:l'] => mkRepfun(rest l,n+1)
      [[n,:first l],:mkRepfun(rest l,1)]
 
encodeItem x ==
  x is [op,:argl] => getCaps op
  IDENTP x => PNAME x
  STRINGIMAGE x
 
getCaps x ==
  s:= STRINGIMAGE x
  clist:= [c for i in 0..MAXINDEX s | UPPER_-CASE_-P (c:= s.i)]
  null clist => '"__"
  "STRCONC"/[first clist,:[DOWNCASE u for u in rest clist]]
 
--% abbreviation code

DEFPARAMETER($abbreviationTable, '())
 
getAbbreviation(name,c) ==
  --returns abbreviation of name with c arguments
  x := constructor? name
  X := ASSQ(x,$abbreviationTable) =>
    N:= ASSQ(name,rest X) =>
      C:= ASSQ(c,rest N) => rest C --already there
      newAbbreviation:= mkAbbrev(X,x)
      RPLAC(rest N,[[c,:newAbbreviation],:rest N])
      newAbbreviation
    newAbbreviation:= mkAbbrev(X,x)
    RPLAC(rest X,[[name,[c,:newAbbreviation]],:rest X])
    newAbbreviation
  $abbreviationTable:= [[x,[name,[c,:x]]],:$abbreviationTable]
  x
 
mkAbbrev(X,x) == addSuffix(alistSize rest X,x)
 
alistSize c ==
  count(c,1) where
    count(x,level) ==
      level=2 => #x
      null x => 0
      count(CDAR x,level+1)+count(rest x,level)
 
addSuffix(n,u) ==
  ALPHA_-CHAR_-P (s:= STRINGIMAGE u).(MAXINDEX s) => INTERN STRCONC(s,STRINGIMAGE n)
  INTERNL STRCONC(s,STRINGIMAGE ";",STRINGIMAGE n)
 

