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

-- Type hasher for old compiler style type names which produces a hash code
-- compatible with the asharp compiler.  Takes a hard error if the type
-- is parameterized, but has no constructor modemap.
getDomainHash dom == SPADCALL(CDR dom, (CAR dom).4)

hashType(type, percentHash) ==
        SYMBOLP type  =>
           type = "%" => percentHash
           hashString SYMBOL_-NAME type
        STRINGP type  => hashCombine(hashString type,
                                        hashString('"Enumeration"))
        type is ['QUOTE, val] => hashType(val, percentHash)
        type is [dom] => hashString SYMBOL_-NAME dom
        type is ['_:, ., type2] => hashType(type2, percentHash)
        isDomain type => getDomainHash type
        [op, :args] := type
        hash := hashString SYMBOL_-NAME op
        op = 'Mapping =>
                hash := hashString '"->"
                [retType, :mapArgs] := args
                for arg in mapArgs repeat
                        hash := hashCombine(hashType(arg, percentHash), hash)
                retCode := hashType(retType, percentHash)
                EQL(retCode, $VoidHash) => hashCombine(32236, hash)
                hashCombine(retCode, hashCombine(32236,hash))
        op = 'Enumeration =>
                for arg in args repeat
                        hash := hashCombine(hashString(STRING arg), hash)
                hash
        op in $DomainsWithoutLisplibs =>
                for arg in args repeat
                        hash := hashCombine(hashType(arg, percentHash), hash)
                hash

        cmm :=   CDDAR getConstructorModemap(op)
        cosig := rest GETDATABASE(op, 'COSIG)
        for arg in args for c in cosig for ct in cmm repeat
                if c then
                        hash := hashCombine(hashType(arg, percentHash), hash)
                else
                        hash := hashCombine(7, hash)
--           !!!   If/when asharp hashes values using their type, use instead
--                      ctt := EQSUBSTLIST(args, $FormalMapVariableList, ct)
--                      hash := hashCombine(hashType(ctt, percentHash), hash)


        hash

--The following are in cfuns.lisp
$hashModulus := 1073741789                      -- largest 30-bit prime

-- Produce a 30-bit hash code.  This function must produce the same codes
-- as the asharp string hasher in src/strops.c
hashString str ==
        h := 0
        for i in 0..#str-1 repeat
                j := CHAR_-CODE char str.i
                h := LOGXOR(h, ASH(h, 8))
                h := h + j + 200041
                h := LOGAND(h, 1073741823)      -- 0x3FFFFFFF
        REM(h, $hashModulus)

-- Combine two hash codes to make a new one.  Must be the same as in
-- the hashCombine function in aslib/runtime.as in asharp.

-- 419AC241: 1100661313
-- 5577F8E1: 1433925857
-- 440BADFC05072367: 4903203917250634599


$hashZ1 := 1100661313
$hashZ2 := 1433925857
$hashZZ := 4903203917250634599


hashCombine(hash1, hash2) ==
         h1 := LOGAND(hash1, ASH(1, 32) - 1)
         h2 := LOGAND(hash2, ASH(1, 32) - 1)
         LOGAND(ASH((h1*$hashZ1 + h2*$hashZ2) * $hashZZ, -32), 1073741823)

$VoidHash := hashString '"Void"
