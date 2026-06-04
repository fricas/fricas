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


-- HyperTeX Utilities for generating basic Command pages

)package "BOOT"

htSay2(page, x) == bcHt2(page, x)

htSayList1(page, lx) ==
  for x in lx repeat bcHt2(page, x)

bcHt2(page, line) == ht_add_item(page, line)

htShowPage1(page) ==
-- show the page which has been computed
   ht_add_string(page, '"\endscroll")
   ht_show_page(page)

-- start defining a HyperTeX page

htInitPage(title, propList) ==
    page := ht_new_page(propList)
    bcHt2(page, ['"\begin{page}{", htpName page, '"}{"])
    htSay2(page, title)
    ht_add_string(page, '"} \beginscroll ")
    page

-- List of currently active window named
$activePageList := nil

htpName htPage ==
-- GENSYM whose value is the page
  ELT(htPage, 0)

htpRadioButtonAlist htPage ==
-- alist of radio button group names and labels
  ELT(htPage, 4)

htpButtonValue(htPage, groupName) ==
  for buttonName in LASSOC(groupName, htpRadioButtonAlist htPage) repeat
    (stripSpaces htpLabelInputString(htPage, buttonName)) = '"t" =>
      return buttonName

htpInputAreaAlist htPage ==
-- Alist of input-area labels, and default values
  ELT(htPage, 5)

htpPropertyList htPage ==
-- Association list of user-defined properties
  ELT(htPage, 6)

htpProperty(htPage, propName) ==
  LASSOC(propName, ELT(htPage, 6))

htpSetProperty(htPage, propName, val) ==
  pair := assoc(propName, ELT(htPage, 6))
  pair => RPLACD(pair, val)
  SETELT(htPage, 6, [[propName, :val], :ELT(htPage, 6)])

htpLabelInputString(htPage, label) ==
-- value user typed as input string on page
  props := LASSOC(label, htpInputAreaAlist htPage)
  props and STRINGP (s := ELT(props,0)) =>
    s = '"" => s
    trimString s
  nil

stringize s ==
  STRINGP s => s
  PRINC_-TO_-STRING s

mkCurryFun(fun, val) ==
  name := GENTEMP()
  code :=
    ['DEFUN, name, '(arg), ['APPLY, MKQ fun, ['CONS, 'arg, MKQ val]]]
  EVAL code
  name

htMakeDoneButton(page, message, func) ==
    ht_add_to_page(page, [['doneButton, message, func]])

htMakeDoitButton(page, label, command) ==
    ht_add_to_page(page, [['doitButton, label, command]])

doDoitButton(htPage, command) ==
  executeInterpreterCommand command

executeInterpreterCommand command ==
  PRINC command
  TERPRI()
  setCurrentLine(command)
  CATCH('SPAD_READER, parseAndInterpret command)
--  MRX I'm not sure whether I should call ioHook("startPrompt")/ioHook("endOfPrompt") here
  princPrompt()
  FORCE_-OUTPUT()

-- Called via lisplink
htDoneButton(func, htPage) ==
  NULL FBOUNDP func =>
    systemError ['"unknown function", func]
  FUNCALL(SYMBOL_-FUNCTION func, htPage)

-- predefined filter strings

quoteString string == CONCAT('"_"", string, '"_"")

$funnyQuote := char 127
$funnyBacks := char 128

htEscapeString str ==
  str := SUBSTITUTE($funnyQuote, char '_", str)
  SUBSTITUTE($funnyBacks, char '_\, str)

unescapeStringsInForm form ==
  STRINGP form =>
    str := NSUBSTITUTE(char '_", $funnyQuote, form)
    NSUBSTITUTE(char '_\, $funnyBacks, str)
  CONSP form =>
    unescapeStringsInForm first form
    unescapeStringsInForm rest form
    form
  form

bcBlankLine(page) ==
    ht_add_string(page, '"\vspace{1}\newline ")

errorPage([heading, kind, :info]) ==
  kind = 'invalidType => BREAK()
  page :=
      heading = 'error => htInitPage('"Error", nil)
      htInitPage(heading, nil)
  bcBlankLine(page)
  for x in info repeat htSay2(page, x)
  htShowPage1(page)

 -- from bc-util

bcFinish(name,arg,:args) == bcGen bcMkFunction(name,arg,args)

bcMkFunction(name,arg,args) ==
  args := [x for x in args | x]
  STRCONC(name,'"(",arg,"STRCONC"/[STRCONC('",", x) for x in args],'")")

bcFindString(s,i,n,char) ==  or/[j for j in i..n | s.j = char]

bcGen command ==
  page := htInitPage('"Basic Command",nil)
  string :=
    #command < 50 => STRCONC('"{\centerline{\tt ",command,'" }}")
    STRCONC('"{\tt ",command,'" }")
  ht_add_to_page(page, [
     '(text
        "{Here is the FriCAS command you could have issued to compute this result:}"
            "\vspace{2}\newline "),
      ['text,:string]])
  htMakeDoitButton(page, '"Do It", command)
  htShowPage1(page)

bcString2WordList s == fn(s,0,MAXINDEX s) where
  fn(s,i,n) ==
    i > n => nil
    k := or/[j for j in i..n | s.j ~= char '_  ]
    null INTEGERP k => nil
    l := bcFindString(s,k + 1,n,char '_  )
    null INTEGERP l => [SUBSTRING(s,k,nil)]
    [SUBSTRING(s,k,l-k),:fn(s,l + 1,n)]

bcwords2liststring u ==
  null u => nil
  STRCONC('"[",first u,fn rest u) where
    fn(u) ==
      null u => '"]"
      STRCONC('", ",first u,fn rest u)

bcVectorGen vec == bcwords2liststring vec

bcError string ==
  sayBrightlyNT '"NOTE: "
  sayBrightly string

htStringPad(n,w) ==
  s := STRINGIMAGE n
  ws := #s
  STRCONC('"\space{",STRINGIMAGE (w - ws + 1),'"}",s)
