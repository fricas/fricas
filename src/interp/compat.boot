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

-- some functions that may need to be changed on different lisp
-- systems.

-- tests if x is an identifier beginning with #

isSharpVar x ==
  IDENTP x and SCHAR(SYMBOL_-NAME x,0) = char "#"

isSharpVarWithNum x ==
  null isSharpVar x => nil
  (n := QCSIZE(p := PNAME x)) < 2 => nil
  ok := true
  c := 0
  for i in 1..(n-1) while ok repeat
    d := ELT(p,i)
    ok := DIGITP d => c := 10*c + DIG2FIX d
  if ok then c else nil

rread(key, rstream) ==
  if IDENTP key then key := PNAME key
  rread0(key, rstream)

rwrite(key,val,stream) ==
  if IDENTP key then key := PNAME key
  rwrite0(key,val,stream)


editFile file ==
  MEMQ(INTERN('"WIN32",FIND_-PACKAGE("KEYWORD")),_*FEATURES_*) =>
    OBEY STRCONC('"notepad ", namestring pathname file)
  OBEY STRCONC('"$AXIOM/lib/SPADEDIT ",namestring pathname file)

-- Reads a line,
READLINE(s) ==
  read_-line(s)
