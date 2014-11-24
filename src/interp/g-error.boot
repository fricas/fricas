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

-- This file contains the error printing code used in BOOT and SPAD.
-- While SPAD only calls "error" (which is then labeled as an algebra
-- error, BOOT calls "userError" and "systemError" when a problem is
-- found.
--
-- The variable $BreakMode is set using the system command )set breakmode
-- and can have one of the values:
--   break    -- always enter a lisp break when an error is signalled
--   nobreak  -- do not enter lisp break mode
--   query    -- ask the user if break mode should be entered
--   quit     -- quit on error with exit status 1

DEFPARAMETER($SystemError, 'SystemError)
DEFPARAMETER($UserError, 'UserError)
DEFPARAMETER($AlgebraError, 'AlgebraError)

DEFVAR($timedNameStack)

BUMPCOMPERRORCOUNT() == nil

argumentDataError(argnum, condit, funname) ==
  msg := ['"The test",:bright pred2English condit,'"evaluates to",
    :bright '"false",'%l,'"   for argument",:bright argnum,_
    '"to the function",:bright funname,'"and this indicates",'%l,_
    '"   that the argument is not appropriate."]
  errorSupervisor($AlgebraError,msg)

queryUser msg ==
  -- display message and return reply
  sayBrightly msg
  read_-line _*TERMINAL_-IO_*

-- errorSupervisor is the old style error message trapper

errorSupervisor(errorType,errorMsg) ==
  $BreakMode = 'trapSpadErrors => THROW('trapSpadErrors, $numericFailure)
  errorSupervisor1(errorType,errorMsg,$BreakMode)

errorSupervisor1(errorType,errorMsg,$BreakMode) ==
  BUMPCOMPERRORCOUNT()
  errorLabel :=
      errorType = $SystemError  => '"System error"
      errorType = $UserError    => '"Apparent user error"
      errorType = $AlgebraError =>
        '"Error detected within library code"
      STRINGP errorType         => errorType
      '"Error with unknown classification"
  msg :=
    errorMsg is ['mathprint, :.] => errorMsg
    not PAIRP errorMsg => ['"   ", errorMsg]
    splitmsg := true
    if member('%b,errorMsg) then splitmsg := nil
      else if member('%d,errorMsg) then splitmsg := nil
           else if member('%l,errorMsg) then splitmsg := nil
    splitmsg => rest [:['%l, '"   ", u] for u in errorMsg]
    ['"   ",:errorMsg]
  sayErrorly(errorLabel, msg)
  handleLispBreakLoop($BreakMode)

handleLispBreakLoop($BreakMode) ==
  TERPRI()
  -- The next line is to try to deal with some reported cases of unwanted
  -- backtraces appearing, MCD.
  ENABLE_-BACKTRACE(nil)
  $BreakMode = 'break =>
    sayBrightly '" "
    BREAK()
  $BreakMode = 'query =>
    gotIt := nil
    while not gotIt repeat
      gotIt := true
      msgQ :=
       ['%l,'"   You have three options. Enter:",'%l,_
        '"    ",:bright '"continue",'"  to continue processing,",'%l,_
        '"    ",:bright '"top     ",'"  to return to top level, or",'%l,_
        '"    ",:bright '"break   ",'"  to enter a LISP break loop.",'%l,_
        '%l,'"   Please enter your choice now:"]
      x := STRING2ID_-N(queryUser msgQ,1)
      x :=
        selectOptionLC(x,'(top break continue),NIL)
      null x =>
        sayBrightly bright '"  That was not one of your choices!"
        gotIt := NIL
      x = 'top => returnToTopLevel()
      x = 'break =>
        $BreakMode := 'break
        sayBrightly ['"   Enter",:bright '":C",
            '"when you are ready to continue processing where you ",'%l,_
            '"   interrupted the system, enter",:bright '"(TOP)",_
            '"when you wish to return",'%l,'"   to top level.",'%l,'%l]
        BREAK()
      sayBrightly
        '"   Processing will continue where it was interrupted."
      THROW('SPAD_READER, nil)
  $BreakMode = 'resume =>
    returnToReader()
  $BreakMode = 'quit =>
    EXIT_-WITH_-STATUS(1)
  returnToTopLevel()

TOP() == returnToTopLevel()

returnToTopLevel() ==
  SETQ(CHR, "ENDOFLINECHR")
  SETQ(TOK, 'END_UNIT)
  TOPLEVEL()

TOPLEVEL() ==
    THROW('top_level, 'restart)

returnToReader() ==
  not $ReadingFile => returnToTopLevel()
  sayBrightly ['"   Continuing to read the file...", '%l]
  THROW('SPAD_READER, nil)

sayErrorly(errorLabel, msg) ==
  $saturn => saturnSayErrorly(errorLabel, msg)
  sayErrorly1(errorLabel, msg)

saturnSayErrorly(errorLabel, msg) ==
  _*STANDARD_-OUTPUT_* : fluid := $texOutputStream
  old := pushSatOutput("line")
  sayString '"\bgroup\color{red}"
  sayString '"\begin{verbatim}"
  sayErrorly1(errorLabel, msg)
  sayString '"\end{verbatim}"
  sayString '"\egroup"
  popSatOutput(old)

sayErrorly1(errorLabel, msg) ==
  sayBrightly '" "
  if $testingSystem then sayMSG $testingErrorPrefix
  sayBrightly ['"   >> ",errorLabel,'":"]
  m := msg
  msg is ['mathprint, mathexpr] =>
    mathprint mathexpr
  sayBrightly msg

-- systemError is being phased out. Please use keyedSystemError.
systemError(x) == errorSupervisor($SystemError, x)

userError x == errorSupervisor($UserError,x)

error(x) == errorSupervisor($AlgebraError,x)

nice_failure_msg(val, branch, umode) ==
    uname := devaluate(umode)
    of1 := coerceUn2E(val, uname);
    str1 := prefix2String(of1);
    STRCONC(str1,
            '" of mode ", outputDomainConstructor(umode),
              '" cannot be coerced to mode ",
                outputDomainConstructor(branch))

check_union_failure_msg(val, branch, umode) ==
    got_str1 := false
    CATCH('top_level, CATCH('SPAD_READER, (
           str1 := nice_failure_msg(val, branch, umode);
           got_str1 := true)))
    got_str1 => str1
    str1 := MAKE_-REASONABLE(STRINGIMAGE val)
    STRCONC(str1,
            '" of mode ", STRINGIMAGE(devaluate(umode)),
              '" cannot be coerced to mode ",
                STRINGIMAGE(devaluate(branch)))

coerce_failure_msg(val, submode, mode) ==
    check_union_failure_msg(val, submode, mode)

IdentityError(op) ==
    error(["No identity element for reduce of empty list using operation",op])

throwMessage(:msg) ==
  if $compilingMap then clearCache $mapName
  msg' := mkMessage concatList msg
  sayMSG msg'
  if $printMsgsToFile then sayMSG2File msg'
  spadThrow()
