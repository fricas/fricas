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


SPACE_CHAR       := STR_ELT('"    ", 0)
-- Hardcode ASCII code to avoid editors messing up control code
PAGE_CTL    := 12
ESCAPE      := STR_ELT('"__  ", 0)
STRING_CHAR := STR_ELT('"_"  ", 0)
PLUSCOMMENT := STR_ELT('"+   ", 0)
MINUSCOMMENT:= STR_ELT('"-   ", 0)
RADIX_CHAR  := STR_ELT('"r   ", 0)
DOT         := STR_ELT('".   ", 0)
EXPONENT1   := STR_ELT('"E   ", 0)
EXPONENT2   := STR_ELT('"e   ", 0)
CLOSEPAREN  := STR_ELT('")   ", 0)
CLOSEANGLE  := STR_ELT('">   ", 0)
QUESTION    := STR_ELT('"?   ", 0)

scanKeyWords := [ _
           ['"add",      "add"], _
           ['"and",      "and"], _
           ['"break",   "break"], _
           ['"by",        "by"], _
           ['"case",     "case"], _
           ['"catch",  "catch"], _
           ['"default",  "DEFAULT" ],_
           ['"define",  "DEFN" ],_
           ['"do",        "DO"],_
           ['"else",    "else"], _
           ['"exquo",   "exquo"], _
           ['"export","EXPORT" ],_
           ['"finally", "finally"], _
           ['"for",      "for"], _
           ['"free",    "FREE" ],_
           ['"from",    "from"], _
           ['"generate", "generate"], _
           ['"goto",    "goto"], _
           ['"has",      "has"], _
           ['"if",       "if"], _
           ['"import", "import"], _
           ['"in", "in"], _
           ['"inline", "INLINE" ],_
           ['"is", "is"], _
           ['"isnt", "isnt"], _
           ['"iterate", "ITERATE"],_
           ['"local", "local"], _
           ['"macro", "MACRO" ],_
           ['"mod", "mod"], _
           ['"not", "not"], _
           ['"or", "or"], _
           ['"pretend", "pretend"], _
           ['"quo", "quo"], _
           ['"rem", "rem"], _
           ['"repeat", "repeat"],_
           ['"return", "return"],_
           ['"rule","RULE" ],_
           ['"then", "then"],_
           ['"try", "try"], _
           ['"until", "until"], _
           ['"where", "where"], _
           ['"while", "while"],_
           ['"with", "with"], _
           ['"yield", "yield"], _
           ['"|",  "|"], _
           ['".",  "."], _
           ['"::", "::"], _
           ['":",  ":"], _
           ['":-","COLONDASH" ],_
           ['"@",  "@"], _
           ['"@@","ATAT" ],_
           ['",", ","],_
           ['";",  ";"],_
           ['"**", "**"], _
           ['"*",  "*"],_
           ['"+",  "+"], _
           ['"-",  "-"], _
           ['"<",  "<"], _
           ['">",  ">"], _
           ['"<=", "<="], _
           ['">=", ">="], _
           ['"=",  "="], _
           ['"~=", "~="], _
           ['"~", "~"], _
           ['"^",  "^" ], _
           ['"..","SEG" ],_
           ['"#","#" ],_
           ['"#1", "#1" ],_
           ['"&","AMPERSAND" ],_
           ['"$","$" ],_
           ['"/",  "/"], _
           ['"\",  "\"], _
           ['"//","SLASHSLASH" ],_
           ['"\\","BACKSLASHBACKSLASH" ],_
           ['"/\", "/\"], _
           ['"\/", "\/"], _
           ['"=>", "=>"], _
           ['":=", ":="], _
           ['"==", "=="], _
           ['"==>", "==>"],_
           ['"->","ARROW" ],_
           ['"<-","LARROW" ],_
           ['"+->", "+->"], _
           ['"(","(" ],_
           ['")",")" ],_
           ['"(|","(|" ],_
           ['"|)","|)" ],_
           ['"[","[" ],_
           ['"]","]" ],_
           ['"[__]","[]" ],_
           ['"{","{" ],_
           ['"}","}" ],_
           ['"{__}","{}" ],_
           ['"[|","[|" ],_
           ['"|]","|]" ],_
           ['"[|__|]","[||]" ],_
           ['"{|","{|" ],_
           ['"|}","|}" ],_
           ['"{|__|}","{||}" ],_
           ['"<<", "<<"], _
           ['">>", ">>"], _
           ['"'", "'" ],_
           ['"`", "BACKQUOTE" ]_
                          ]

scanKeyTableCons()==
   KeyTable:=MAKE_-HASHTABLE("CVEC")
   for st in scanKeyWords repeat
      HPUT(KeyTable, first st, CADR st)
   KeyTable

scanInsert(s,d) ==
      l := #s
      h := STR_ELT(s, 0)
      u := ELT(d,h)
      n := #u
      k:=0
      while l <= #(ELT(u,k)) repeat
          k:=k+1
      v := MAKE_-VEC(n+1)
      for i in 0..k-1 repeat QSETVELT(v, i, ELT(u, i))
      QSETVELT(v, k, s)
      for i in k..n-1 repeat QSETVELT(v, i + 1, ELT(u, i))
      QSETVELT(d, h, v)
      s

scanDictCons()==
      l:= HKEYS scanKeyTable
      d :=
          a:=MAKE_-VEC(256)
          b:=MAKE_-VEC(1)
          QSETVELT(b, 0, make_full_CVEC(0, '" "))
          for i in 0..255 repeat QSETVELT(a, i, b)
          a
      for s in l repeat scanInsert(s,d)
      d

scanPunCons()==
    listing := HKEYS scanKeyTable
    a := make_BVEC(256, 0)
    for i in 0..255 repeat SETELT_BVEC(a, i, 0)
    for k in listing repeat
       if not startsId? k.0 then
           SETELT_BVEC(a, STR_ELT(k, 0), 1)
    a

scanKeyTable:=scanKeyTableCons()

scanDict:=scanDictCons()

scanPun:=scanPunCons()

for i in   [ _
   ["=",   "="], _
   ["*",   "*"], _
   ["has",      "has"], _
   ["case",     "case"], _
   ["exquo",    "exquo"], _
   ["rem",      "rem"], _
   ["mod", "mod"], _
   ["quo",      "quo"], _
   ["/",   "/"], _
   ["\",   "\"], _
   ["SLASHSLASH"    ,"//"], _
   ["BACKSLASHBACKSLASH","\\"], _
   ["/\",  "/\"], _
   ["\/",  "\/"], _
   ["**",  "**"], _
   ["^",   "^"], _
   ["+",   "+"], _
   ["-",   "-"], _
   ["<",   "<"], _
   [">",   ">"], _
   ["<<",  "<<"], _
   [">>",  ">>"], _
   ["<=",  "<="], _
   [">=",  ">="], _
   ["~=",  "~="], _
   ["by",       "by"], _
   ["ARROW"       ,"->"], _
   ["LARROW"       ,"<-"], _
   ["|",   "|"], _
   ["SEG"       ,".."] _
    ] repeat MAKEPROP(first i, 'INFGENERIC, CADR i)

-- Scanner

is_white?(c) == c = SPACE_CHAR or c = PAGE_CTL

skip_whitespace(ln, n) ==
    l := #ln
    while n < l and is_white?(STR_ELT(ln, n)) repeat
        n := n + 1
    n

DEFVAR($f)
DEFVAR($floatok)
DEFVAR($linepos)
DEFVAR($ln)
DEFVAR($n)
DEFVAR($r)
DEFVAR($sz)
DEFPARAMETER($was_nonblank, false)

DEFVAR($comment_indent, 0)
DEFVAR($current_comment_block, nil)
DEFVAR($comment_line)
DEFVAR($last_nonempty_linepos, nil)
DEFVAR($spad_scanner, false)

finish_comment() ==
    NULL($current_comment_block) => nil
    pos :=
        $comment_indent = 0 => $comment_line
        first(rest(rest($last_nonempty_linepos)))
    PUSH([pos, :NREVERSE($current_comment_block)], $COMBLOCKLIST)
    $current_comment_block := nil

--  lineoftoks  bites off a token-dq from a line-stream
--  returning the token-dq and the rest of the line-stream

scanIgnoreLine(ln,n)==
    if n = $sz then
        false
    else
       fst := STR_ELT(ln, 0)
       if EQ(fst, CLOSEPAREN) and ($sz > 1) and
           not(is_white?(STR_ELT(ln, 1)))
       then if incPrefix?('"command",1,ln)
            then true
            else nil
       else n

nextline(s)==
     if npNull s
     then false
     else
       $f:= first s
       $r:= rest s
       $ln := rest $f
       $linepos:=CAAR $f
       $n := skip_whitespace($ln, 0) -- spaces at beginning
       $sz :=# $ln
       true


lineoftoks(s)==
   $f: local:=nil
   $r:local :=nil
   $ln:local :=nil
   $linepos:local:=nil
   $n:local:=nil
   $sz:local := nil
   $floatok:local:=true
   $was_nonblank := false
   not nextline s => CONS(nil,nil)
   if null scanIgnoreLine($ln,$n) -- line of spaces or starts ) or >
   then cons(nil,$r)
   else
      toks:=[]
      a:= incPrefix?('"command",1,$ln)
      a =>
           $ln := SUBSTRING($ln, 8, nil)
           b := dqUnit constoken($ln, $linepos, ["command", $ln], 0)
           cons([[b, s]], $r)

      while $n<$sz repeat
          tok := scanToken()
          if tok and $spad_scanner then finish_comment()
          toks:=dqAppend(toks, tok)
      if null toks
      then cons([],$r)
      else
          $last_nonempty_linepos := $linepos
          cons([[toks,s]],$r)


scanToken () ==
      ln:=$ln
      c := STR_ELT($ln, $n)
      linepos:=$linepos
      n:=$n
      ch:=$ln.$n
      b:=
            startsComment?()          =>
                           scanComment()
                           []
            startsNegComment?()       =>
                           scanNegComment()
                           []
            c= QUESTION               =>
                               $n:=$n+1
                               lfid '"?"
            punctuation? c            => scanPunct ()
            startsId? ch              => scanWord  (false)
            is_white?(c)              =>
                           scanSpace ()
                           $was_nonblank := false
                           []
            c = STRING_CHAR           => scanString ()
            digit? ch                 => scanNumber ()
            c=ESCAPE                  => scanEscape()
            scanError ()
      null b => nil
      nb := $was_nonblank and b.0 = "key" and b.1 = "("
      $was_nonblank := true
      dqUnit constoken1(ln, linepos, b, n + lnExtraBlanks linepos, nb)

-- to pair badge and badgee

DEFPARAMETER($boot_package, FIND_-PACKAGE('"BOOT"))
lfid x== ["id", INTERN(x, $boot_package)]

lfkey x==["key",keyword x]

lfinteger x == ["integer", x]

lfrinteger (r,x)==["integer",CONCAT (r,CONCAT('"r",x))]
--lfrfloat(a,w,v)==["rfloat",CONCAT(a,'"r.",v)]
lffloat(a, w, e) == ["float", [a, w, e]]
lfstring x==if #x=1 then ["char",x] else ["string",x]
lfcomment (n, lp, x) == ["comment", x]
lfnegcomment x== ["negcomment", x]
lferror x==["error",x]
lfspaces x==["spaces",x]

constoken1(ln, lp, b, n, nb) ==
--  [b.0,b.1,cons(lp,n)]
       a:=cons(b.0,b.1)
       if nb then ncPutQ(a, "nonblank", true)
       ncPutQ(a,"posn",cons(lp,n))
       a

constoken(ln, lp, b, n) == constoken1(ln, lp, b, n, false)

scanEscape()==
         $n:=$n+1
         a:=scanEsc()
         if a then scanWord true else nil

scanEsc()==
     if $n>=$sz
     then if nextline($r)
          then
             $n := 0
             false
          else false
     else
         true

checkEsc()==
    if STR_ELT($ln, $sz - 1) = ESCAPE then scanEsc()

startsComment?()==
    if $n<$sz
    then
         if STR_ELT($ln, $n) = PLUSCOMMENT then
            www:=$n+1
            if www>=$sz
            then false
            else STR_ELT($ln, www) = PLUSCOMMENT
         else false
    else false

startsNegComment?()==
    if $n< $sz
    then
         if STR_ELT($ln, $n) = MINUSCOMMENT then
            www:=$n+1
            if www>=$sz
            then false
            else STR_ELT($ln, www) = MINUSCOMMENT
         else false
    else false

scanNegComment()==
      n:=$n
      $n:=$sz
      res := lfnegcomment SUBSTRING($ln,n,nil)
      checkEsc()
      res

scanComment()==
      n:=$n
      $n:=$sz
      c_str := SUBSTRING($ln,n,nil)
      if $spad_scanner then
          if not(n = $comment_indent) then
              finish_comment()
          $comment_line := first(rest(rest($linepos)))
          $comment_indent := n
          PUSH(CONCAT(make_full_CVEC(n, '" "), c_str), $current_comment_block)
      res := lfcomment(n, $linepos, c_str)
      checkEsc()
      res


scanPunct()==
            sss:=subMatch($ln,$n)
            a:= # sss
            if a=0
            then
               scanError()
            else
               $n:=$n+a
               scanKeyTr sss

scanKeyTr w==
       if EQ(keyword w, ".")
       then if $floatok
            then scanPossFloat(w)
            else lfkey w
       else
            $floatok:=not scanCloser? w
            lfkey w

scanPossFloat (w)==
     if $n>=$sz or not digit? $ln.$n
     then lfkey w
     else
       w:=spleI(function digit?)
       scanExponent('"0",w)

scanCloser:=[")","}","]","|)","|}","|]"]

scanCloser? w== MEMQ(keyword w,scanCloser)

scanSpace()==
           n:=$n
           $n := skip_whitespace($ln, $n)
           $floatok:=true
           lfspaces ($n-n)

e_concat(s1, s2) ==
    #s2 = 0 => s1
    idChar?(s2.0) => CONCAT(s1, "__", s2)
    CONCAT(s1, s2)

scanString()==
            $n:=$n+1
            $floatok:=false
            lfstring scanS ()

scanS()==
   if $n>=$sz
   then
     ncSoftError(cons($linepos,lnExtraBlanks $linepos+$n),"S2CN0001",[])
     '""
   else
           n:=$n
           strsym :=STRPOS ('"_"",$ln,$n,nil) or $sz
           escsym:=STRPOS ('"__"
                          ,$ln,$n,nil)  or $sz
           mn:=MIN(strsym,escsym)
           if mn=$sz
           then
                 $n:=$sz
                 ncSoftError(cons($linepos,lnExtraBlanks $linepos+$n),
                         "S2CN0001",[])
                 SUBSTRING($ln,n,nil)
           else if mn=strsym
                then
                   $n:=mn+1
                   SUBSTRING($ln,n,mn-n)
                else     --escape is found first
                  str:=SUBSTRING($ln,n,mn-n)-- before escape
                  $n:=mn+1
                  a:=scanEsc() -- case of end of line when false
                  not(a) => CONCAT(str, scanS())
                  ec := $ln.$n
                  $n := $n + 1
                  e_concat(str, CONCAT(ec, scanS()))

--idChar? x== scanLetter x or DIGITP x or MEMQ(x,'(_? _%))

--scanLetter x==
--   if not CHARP x
--   then false
--   else STRPOSL(scanTrTable,x,0,NIL)

posend(line,n)==
     while n<#line and idChar? line.n repeat n:=n+1
     n

--numend(line,n)==
--     while n<#line and digit? line.n repeat n:=n+1
--     n

--startsId? x==  scanLetter x or MEMQ(x,'(_? _%))
digit? x== DIGITP x

scanW(b)==             -- starts pointing to first char
       n1:=$n         -- store starting character position
       $n := inc_SI($n)          -- the first character is not tested
       l:=$sz
       endid:=posend($ln,$n)
       if endid = l or STR_ELT($ln, endid) ~= ESCAPE then
           -- not escaped
           $n:=endid
           [b, SUBSTRING($ln, n1, sub_SI(endid, n1))] -- l overflows
       else -- escape and endid~=l
           str:=SUBSTRING($ln,n1,endid-n1)
           $n:=endid+1
           a:=scanEsc()
           bb:=if a -- escape nonspace
               then scanW(true)
               else
                  if $n>=$sz
                  then [b,'""]
                  else
                    if idChar?($ln.$n)
                    then scanW(b)
                    else [b,'""]
           [bb.0 or b, e_concat(str, bb.1)]

scanWord(esp) ==
          aaa:=scanW(false)
          w:=aaa.1
          $floatok:=false
          if esp or aaa.0
          then lfid w
          else if (keyword? w and ($spad_scanner or w ~= '"not"))
               then
                  $floatok:=true
                  lfkey w
               else lfid  w



spleI(dig)==spleI1(dig,false)
spleI1(dig,zro) ==
       n:=$n
       l:= $sz
       while $n<l and FUNCALL(dig,($ln.$n)) repeat $n:=$n+1
       if $n = l or STR_ELT($ln, $n) ~= ESCAPE
       then if n=$n and zro
            then '"0"
            else SUBSTRING($ln,n,$n-n)
       else  -- escaped
             str:=SUBSTRING($ln,n,$n-n)
             $n:=$n+1
             a:=scanEsc()
             bb:=spleI1(dig,zro)-- escape, anyno spaces are ignored
             CONCAT(str,bb)

scanCheckRadix(r,w)==
       ns:=#w
       ns = 0 =>
            ncSoftError([$linepos, :lnExtraBlanks $linepos+$n], "S2CN0004", [])
       done:=false
       for i in 0..ns-1  repeat
         a:=rdigit? w.i
         if null a or a>=r
         then  ncSoftError(cons($linepos,lnExtraBlanks $linepos+$n-ns+i),
                    "S2CN0002", [w.i])

scanNumber() ==
       a := spleI(function digit?)
       if $n>=$sz
       then lfinteger a
       else
         if STR_ELT($ln, $n) ~= RADIX_CHAR then
           if $floatok and STR_ELT($ln, $n) = DOT then
             n:=$n
             $n:=$n+1
             if  $n<$sz and STR_ELT($ln, $n) = DOT then
               $n:=n
               lfinteger a
             else
               w:=spleI1(function digit?,true)
               scanExponent(a,w)
           else lfinteger a
         else
             $n:=$n+1
             w:=spleI1(function rdigit?, false)
             scanCheckRadix(PARSE_-INTEGER a,w)
             if $n>=$sz
             then
                lfrinteger(a,w)
             else if STR_ELT($ln, $n) = DOT then
                    n:=$n
                    $n:=$n+1
                    if  $n < $sz and STR_ELT($ln, $n) = DOT then
                       $n:=n
                       lfrinteger(a,w)
                    else
                    --$n:=$n+1
                      v:=spleI1(function rdigit?, false)
                      scanCheckRadix(PARSE_-INTEGER a,v)
                      scanExponent(CONCAT(a,'"r",w),v)
                  else lfrinteger(a,w)

scanExponent(a,w)==
     if $n>=$sz
     then lffloat(a,w,'"0")
     else
        n:=$n
        c := STR_ELT($ln, $n)
        if c=EXPONENT1 or c=EXPONENT2
        then
           $n:=$n+1
           if $n>=$sz
           then
             $n:=n
             lffloat(a,w,'"0")
           else if digit?($ln.$n)
                then
                  e:=spleI(function digit?)
                  lffloat(a,w,e)
                else
                  c1 := STR_ELT($ln, $n)
                  if c1=PLUSCOMMENT or c1=MINUSCOMMENT
                  then
                    $n:=$n+1
                    if $n>=$sz
                    then
                      $n:=n
                      lffloat(a,w,'"0")
                    else
                      if digit?($ln.$n)
                      then
                        e:=spleI(function digit?)
                        lffloat(a,w,
                          (if c1=MINUSCOMMENT then CONCAT('"-",e)else e))
                      else
                        $n:=n
                        lffloat(a,w,'"0")
        else lffloat(a,w,'"0")

rdigit? x==
   STRPOS(x,'"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",0,nil)

scanError()==
      n:=$n
      $n:=$n+1
      ncSoftError(cons($linepos,lnExtraBlanks $linepos+$n),
         "S2CN0003",[$ln.n])
      lferror ($ln.n)


keyword st   == HGET(scanKeyTable,st)

keyword? st  ==  not null HGET(scanKeyTable,st)

subMatch(l,i)==substringMatch(l,scanDict,i)

substringMatch (l,d,i)==
       h := STR_ELT(l, i)
       u:=ELT(d,h)
       ll:=SIZE l
       done:=false
       s1:='""
       for j in 0.. SIZE u - 1 while not done repeat
          s:=ELT(u,j)
          ls:=SIZE s
          done:=if ls+i > ll
                then false
                else
                 eql:= true
                 for k in 1..ls-1 while eql repeat
                    eql := EQL(STR_ELT(s, k), STR_ELT(l, k + i))
                 if eql
                 then
                   s1:=s
                   true
                 else false
       s1

punctuation? c == c < 256 and ELT_BVEC(scanPun, c) = 1
