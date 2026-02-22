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


--% Functions to handle specific errors (mostly syntax)

)package "BOOT"

syGeneralErrorHere() ==
   pos := tokPosn($tok)
   ncSoftError(pos, 'S2CY0002, [])

syIgnoredFromTo(pos1, pos2) ==
  if pfGlobalLinePosn pos1 = pfGlobalLinePosn pos2 then
      ncSoftError(position_from_to(pos1, pos2), 'S2CY0005, [])
  else
      ncSoftError(position_from(pos1), 'S2CY0003, [])
      ncSoftError(position_to(pos2), 'S2CY0004, [])

npMissingMate(close,open)==
   ncSoftError(tokPosn open, 'S2CY0008, [])
   npMissing close

npMissing s==
   ncSoftError(tokPosn $stok,'S2CY0007, [PNAME s])
   THROW("TRAPPOINT","TRAPPED")

npCompMissing s == npEqKey s or npMissing s

pfSourceStok x==
       if pfLeaf? x
       then x
       else if null pfParts x
            then 'NoToken
            else pfSourceStok pfFirst x

npTrapForm(x)==
   a:=pfSourceStok x
   EQ(a,'NoToken)=>
         syGeneralErrorHere()
         THROW("TRAPPOINT","TRAPPED")
   ncSoftError(tokPosn a, 'S2CY0002, [])
   THROW("TRAPPOINT","TRAPPED")

npTrap()==
   ncSoftError(tokPosn $stok,'S2CY0002,[])
   THROW("TRAPPOINT","TRAPPED")

npRecoverTrap()==
  npFirstTok()
  pos1 := tokPosn $stok
  npMoveTo 0
  pos2 := tokPosn $stok
  syIgnoredFromTo(pos1, pos2)
  npPush [pfWrong(pfDocument ['"pile syntax error"],pfListOf [])]


npListAndRecover(f)==
   a:=$stack
   b:=nil
   $stack:=nil
   done:=false
   c:=$inputStream
   while not done repeat
     found:=CATCH("TRAPPOINT",APPLY(f,nil))
     if found="TRAPPED"
     then
        $inputStream:=c
        npRecoverTrap()
     else if not found
          then
            $inputStream:=c
            syGeneralErrorHere()
            npRecoverTrap()
     if npEqKey "BACKSET"
     then
        c:=$inputStream
     else if npEqPeek "BACKTAB"
          then
             done:=true
          else
            $inputStream:=c
            syGeneralErrorHere()
            npRecoverTrap()
            if npEqPeek "BACKTAB"
            then done:=true
            else
                npNext()
                c:=$inputStream
     b:=cons(npPop1(),b)
   $stack:=a
   npPush NREVERSE b

npMoveTo n==
      if null $inputStream
      then true
      else
           if npEqPeek "BACKTAB"
           then if n=0
                then true
                else (npNext();npMoveTo(n-1))
           else if npEqPeek "BACKSET"
                then if n=0
                     then true
                     else (npNext();npMoveTo n)
                 else if npEqKey "SETTAB"
                      then npMoveTo(n+1)
                      else (npNext();npMoveTo n)
