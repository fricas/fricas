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

)if false
                        Usage

)bo inputFile2RecordFile('"<idir>fn.input",'"<odir>a.b")
  converts input file "fn" to a record file stored at "<odir>fn.record".
  If you give one argument, <idir> is used for <odir>

)bo printRecordFile('"<idir>fn.record") to display results recorded

)bo verifyRecordFile('"<idir>fn.record") to verfiy that same output
   results from running original fn.input file
)endif

--=======================================================================
--                      Global Variables
--=======================================================================
$backslash := char '_\
$runTestFlag := nil          -- referenced by maPrin to stash output
                             -- by recordAndPrint to not print type/time
$mkTestFlag := nil           -- referenced by READLN to stash input
                             -- by maPrin to stash output
                             -- by recordAndPrint to write i/o onto $testStream
$mkTestOutputStack := nil    -- saves output for $testStream (see maPrin)
$mkTestOutputType := nil     -- saves the type for $testStream

--=======================================================================
--                Function for Creating a `record' file
--=======================================================================
inputFile2RecordFile(pathname,:option) ==
  ifn := PATHNAME_-NAME pathname
  not isExistingFile pathname => throwKeyedMsg("S2IL0003",[namestring ifn])
  opath := IFCAR option or pathname
  odirect := pathnameDirectory opath
  opathname := htMkPath(odirect,ifn,'"rec")
  -- logically _*PRINT_-ARRAY_* should be local, but Common Lisp
  -- forces us to omit it.
  _*PRINT_-ARRAY_* := true
  $mkTestFlag: local := true
  $runTestFlag: local := false
  $mkTestOutputStack: local := nil
  $mkTestOutputType: local := nil
  $currentLine: local := nil
  if isExistingFile opathname then DELETE_-FILE opathname
  $testStream := MAKE_OUTSTREAM(opathname)
  CATCH('SPAD_READER, do_read(pathname, nil))
  --for trailing system commands
  if not null $currentLine then recordAndPrintTest '(ForSystemCommands)
  SHUT $testStream
  opathname
--=======================================================================
--                Function for Displaying a `record' file
--=======================================================================
printRecordFile(pathname,:option) ==
  $LINELENGTH : local := IFCAR option or 76
  $printTimeIfTrue: local := nil
  $printTypeIfTrue: local := true
  stream := MAKE_INSTREAM(pathname)
  repeat
    NULL (PEEK_-CHAR ( true, stream , nil, nil )) => return nil
    [i,t,:o] := dewritify VMREAD stream
    sayNewLine()
    for x in i repeat sayBrightly x
    sayNewLine()
    for x in o repeat maPrin x
    if t~= '(Void) then printTypeAndTime(nil,t)

--=======================================================================
--                Function for Verifying a `record' file
--=======================================================================
verifyRecordFile(pathname) ==
  ifn := PATHNAME_-NAME pathname
  sayBrightly ['"Verifying",:bright ifn]
  not isExistingFile pathname => throwKeyedMsg("S2IL0003",[namestring ifn])
  stream := MAKE_INSTREAM(pathname)
  clearCmdAll()
  result := 'ok
  for j in 1.. repeat
    NULL (PEEK_-CHAR ( true, stream ,nil,nil ))=>return nil
    [i,t,:o] := dewritify VMREAD stream
    null i => return nil
    t = 'ForSystemCommands =>
      return testInput2Output(i,nil)
        --read trailing system commands
    [typ,:output] := testInput2Output(i,j)
    typ = t =>
      output = o => 'ok
      result := 'error
      wasIs(o,output)
    result := 'error
    wasIs(o,output,t,typ)
  suffix := (result = 'ok => '"is ok"; '"has errors")
  sayBrightly [:bright ifn,suffix]

testInput2Output(lines,n) ==
  $mkTestOutputStack: local := nil
  $mkTestOutputType: local := nil
  $mkTestFlag: local := nil
  $runTestFlag: local := true
  $testOutput: local := nil
  evaluateLines lines
  null n => nil     --return from reading trailing system commands
  typ := $mkTestOutputType
  output := NREVERSE $mkTestOutputStack
  [prefix2String typ,:output]

evaluateLines lines ==
  file := MAKE_OUTSTREAM('"/tmp/temp.input")
  for line in lines repeat
--  stringPrefix?('")read ",line) => 'skip
    stringPrefix?('")r",line) => 'skip
    stringPrefix?('")undo )redo",line) => 'skip
    PRINTEXP(line, file)
    TERPRI file
  SHUT file
  $edit_file : fluid := '"/tmp/temp.input"
  read_or_compile(false, false)
    -- can't use $edit_file since it might be reset
  DELETE_-FILE '"/tmp/temp.input"


wasIs(old,new,:typePart) ==
  sayBrightly '"*************************************************************"
  if old ~= new then
    sayBrightly '"Was ----------> "
    for x in old repeat maPrin x
    sayBrightly '"Is -----------> "
    for x in new repeat maPrin x
  typePart is [oldtype,newtype] and oldtype ~= newtype =>
    sayBrightlyNT ['" Type was ---> ",oldtype]
    pp old
    sayBrightlyNT ['" Type is  ---> ",newtype]
    pp new

--=======================================================================
--              Creating Input Files from HT Files
--=======================================================================

htMkPath(directory,name,typ) ==
  nameType := STRCONC(name,'".",typ)
  null directory => nameType
  STRCONC(directory,nameType)

--=======================================================================
--           Function to record and print values into $testStream
--=======================================================================
recordAndPrintTest md ==  --called by recordAndPrint
  input :=
    STRINGP $currentLine => [$currentLine]
    fn $currentLine where fn x ==
      x is [y,:r] =>
        y.(k := MAXINDEX y) = char '__ =>
          u := fn r
          [STRCONC(SUBSTRING(y,0,k),'" ",first u),:rest u]
        [y,:fn r]
      x
  output := NREVERSE $mkTestOutputStack -- set by maPrin
  PRINT(writify [input,prefix2String md,:output],$testStream)
  $mkTestOutputStack := nil
