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

$primitiveHtCommands := '(
  ("\ContinueButton"     . 1)
  ("\andexample"         . 1)
  ("\autobutt" .    0)
  ("\autobuttons".  0)
  ("\begin"  .      1)
  ("\beginscroll".  0)
  ("\bound"  .      1)
  ("\fbox"    .      1)
  ("\centerline" .      1)
  ("\downlink" .    2)
  ("\em"     .      0)
  ("\end"    .      1)
  ("\endscroll"  .  0)
  ("\example"            . 1)
  ("\free"   .      1)
  ("\graphpaste" .  1)
  ("\helppage" .    1)
  ("\htbmdir"            . 0)
  ("\htbmfile"   .  1)
  ("\indent" .      1)
  ("\inputbitmap"        . 1)
  ("\inputstring" . 3)
  ("\item"   .      0)
  ("\keyword"            . 1)
  ("\link"               . 2)
  ("\lispdownlink"       . 2)
  ("\lispmemolink"       . 2)
  ("\lispwindowlink"     . 2)
  ("\menudownlink"       . 2)
  ("\menuitemstyle"      . 1)
  ("\menulink"           . 2)
  ("\menulispdownlink"   . 2)
  ("\menulispmemolink"   . 2)
  ("\menulispwindowlink" . 2)
  ("\menumemolink"       . 2)
  ("\menuwindowlink"     . 2)
  ("\newline" .     0)
  ("\radioboxes" .  3)
  ("\space"  .      1)
  ("\spadcommand" . 1)
  ("\stringvalue" . 1)
  ("\tab"    .      1)
  ("\table"              . 1)
  ("\vspace" .      1)
  ("\windowlink"         . 2))

buildHtMacroTable() ==
  $htMacroTable := MAKE_HASHTABLE('UEQUAL)
  fn := CONCAT(getEnv '"AXIOM", '"/share/hypertex/pages/util.ht")
  if PROBE_-FILE(fn) then
    instream := MAKE_INSTREAM(fn)
    while not EOFP instream repeat
      line := read_line instream
      getHtMacroItem line is [string,:numOfArgs] =>
        HPUT($htMacroTable,string,numOfArgs)
    for [s,:n] in $primitiveHtCommands repeat HPUT($htMacroTable,s,n)
    SHUT instream
  else
    sayBrightly '"Warning: HyperTeX macro table not found"
  $htMacroTable

getHtMacroItem line ==
  null stringPrefix?('"\newcommand{",line) => nil
  k := charPosition(char '_},line,11)
  command := SUBSTRING(line,12,k - 12)
  numOfArgs :=
    m := #line
    i := charPosition(char '_[,line,k)
    i = m => 0
    j := charPosition(char '_],line,i + 1)
    digitString := SUBSTRING(line,i + 1,j - i - 1)
    and/[DIGITP digitString.i for i in 0..MAXINDEX digitString]
      => PARSE_-INTEGER digitString
    return nil
  [command,:numOfArgs]

spadSysChoose(tree,form) ==     --tree is ((word . tree) ..)
  null form => true
  null tree => false
  lookupOn :=
    form is [key,arg] => key
    form
  newTree := LASSOC(lookupOn,tree) => spadSysBranch(newTree,IFCAR IFCDR form)
  false

spadSysBranch(tree,arg) ==  --tree is (msg kind TREEorSomethingElse ...)
  null arg => true
  kind := tree.2
  kind = 'TREE => spadSysChoose(tree.4,arg)
  kind = 'LITERALS => member(arg,tree.4)
  kind = 'INTEGER  => INTEGERP arg
  kind = 'FUNCTION => atom arg
  systemError '"unknown tree branch"

buildHtMacroTable()
