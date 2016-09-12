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

--% Common Lisp Pathname Functions

pathname p ==
  if SYMBOLP(p) then p := SYMBOL_-NAME(p)
  PATHNAMEP p => p
  not PAIRP p => PATHNAME p
  p is [fn] => PATHNAME make_filename(fn)
  p is [fn, ft] => PATHNAME make_filename0(fn, ft)
  error '"Strange argument to pathname"
namestring p == NAMESTRING pathname p

pathnameName p == PATHNAME_-NAME pathname p

pathnameType p == PATHNAME_-TYPE pathname p

pathnameTypeId p == UPCASE object2Identifier pathnameType p

pathnameDirectory p ==
    NAMESTRING MAKE_-PATHNAME(INTERN('"DIRECTORY", '"KEYWORD"),_
                               PATHNAME_-DIRECTORY pathname p)

deleteFile f == DELETE_-FILE f

isExistingFile f ==
  if make_input_filename(f)
    then
      true
    else false

--% Scratchpad II File Name Functions

makePathname(name,type) ==
  -- Common Lisp version of this will have to be written
  -- using MAKE-PATHNAME and the optional args.
  pathname [object2String name,object2String type]

isSystemDirectory dir == EVERY(function CHAR_=,$SPADROOT,dir)
