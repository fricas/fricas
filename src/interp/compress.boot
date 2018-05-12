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

-- This one is not currently in general use, but can be applied
-- to various situations are required

minimalise x ==
  $hash : local := MAKE_HASHTABLE('UEQUAL)
  min x where
    min x ==
      y:=HGET($hash,x)
      y => y
      PAIRP x =>
        x = '(QUOTE T) => '(QUOTE T)
        -- copes with a particular Lucid-ism, God knows why
        -- This circular way of doing things is an attempt to deal with Lucid
        -- Who may place quoted cells in read-only memory
        z := min first x
        if not EQ(z, first x) then RPLACA(x, z)
        z:=min CDR x
        if not EQ(z,CDR x) then RPLACD(x,z)
        HashCheck x
      REFVECP x =>
        for i in 0..MAXINDEX x repeat
          x.i:=min (x.i)
        HashCheck x
      STRINGP x => HashCheck x
      x
    HashCheck x ==
      y:=HGET($hash,x)
      y => y
      HPUT($hash,x,x)
      x
  x
