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

poNoPosition()    == $nopos
pfNoPosition() == poNoPosition()

poNoPosition? pos == EQCAR(pos,'noposition)
pfNoPosition? pos == poNoPosition? pos

poIsPos? pos ==
    PAIRP pos and PAIRP first pos and LENGTH first pos = 5

lnCreate(extBl, st, gNo, :optFileStuff) ==
    lNo :=
        num := IFCAR optFileStuff => num
        0
    fN  := IFCAR IFCDR optFileStuff
    [extBl, st, gNo, lNo, fN]

lnString lineObject ==
    lineObject.1

lnExtraBlanks lineObject ==
    lineObject.0

lnGlobalNum lineObject   ==
    lineObject.2

lnSetGlobalNum(lineObject, num) ==
    lineObject.2 := num

lnLocalNum lineObject    ==
    lineObject.3

lnFileName lineObject    ==
    (fN := lnFileName? lineObject)  => fN
    ncBug('"there is no file name in %1", [lineObject] )

lnFileName? lineObject   ==
    NOT PAIRP (fN := lineObject.4)  => NIL
    fN

lnPlaceOfOrigin lineObject ==
    lineObject.4

lnImmediate? lineObject  ==
    not lnFileName? lineObject

poGetLineObject posn ==
    first posn

pfPosn pf == pfSourcePosition pf

pfSourcePosition form ==
    --null form => pfNoPosition()
    pfLeaf? form => pfLeafPosition form
    parts := pfParts form
    pos := $nopos
    for p in parts while poNoPosition? pos repeat
        pos := pfSourcePosition p
    pos

pfSourcePositions form ==
    if pfLeaf? form
    then
     a:=tokPosn form
     if null a
     then nil
     else [a]
    else  pfSourcePositionlist pfParts form

pfSourcePositionlist x==
      if null x
      then nil
      else APPEND(pfSourcePositions first x,pfSourcePositionlist rest x)


poCharPosn posn == rest posn
pfCharPosn posn == poCharPosn posn

poLinePosn posn       ==
    posn => lnLocalNum  poGetLineObject posn  --VECP posn =>
    CDAR posn
pfLinePosn posn == poLinePosn posn

poGlobalLinePosn posn ==
    posn => lnGlobalNum poGetLineObject posn
    ncBug('"old style pos objects have no global positions",[])
pfGlobalLinePosn posn == poGlobalLinePosn posn

poFileName posn       ==
    posn => lnFileName poGetLineObject posn
    CAAR posn
pfFileName posn == poFileName posn

poPlaceOfOrigin posn ==
    lnPlaceOfOrigin poGetLineObject posn

poNopos? posn ==
    posn = ['noposition]

poPosImmediate? txp==
    poNopos? txp => NIL
    lnImmediate? poGetLineObject txp

poImmediate? txp==
    lnImmediate? poGetLineObject txp
pfImmediate? txp == poImmediate? txp


compareposns(a,b)==
 c:=poGlobalLinePosn a
 d:=poGlobalLinePosn b
 if c=d then poCharPosn a>=poCharPosn b else c>=d

pfPrintSrcLines(pf) ==
  lines := pfSourcePositions pf
  lno := 0
  for l in lines repeat
    line := car l
    if lno < lnGlobalNum(line) then
      FORMAT(true, '"   ~A~%",  lnString line)
      lno := lnGlobalNum(line)

-- Source positions of the nodes of the Spad parse tree.  The parser
-- records the position of the first token of each node, and the later
-- stages copy it to the nodes they build from it.  The positions are kept
-- in a table, so the tree itself is not changed.

DEFPARAMETER($parse_positions, MAKE_HASHTABLE('EQ))
-- The positions of names: a symbol occurs in many places, so the position
-- of an occurrence is kept for the list cell that holds it.
DEFPARAMETER($name_positions, MAKE_HASHTABLE('EQ))

clear_parse_positions() ==
    CLRHASH($parse_positions)
    CLRHASH($name_positions)

get_pos(x) ==
    CONSP(x) => HGET($parse_positions, x)
    nil

earlier_pos(p1, p2) ==
    null(p1) => p2
    null(p2) => p1
    compareposns(p1, p2) => p2
    p1

-- give x the position pos, unless it already has an earlier one
note_pos(x, pos) ==
    if CONSP(x) and pos then
        HPUT($parse_positions, x, earlier_pos(get_pos(x), pos))

-- give y, which was built from x, the position of x, and to the names
-- in y the positions of the same names in x; returns y
copy_pos(x, y) ==
    carry_name_pos(x, y)
    if CONSP(y) and not(get_pos(y)) and (pos := get_pos(x)) then
        HPUT($parse_positions, y, pos)
    y

-- copy the positions of the nodes and names of x to the corresponding
-- ones of y, which has the shape of x (e.g. the result of SUBLIS);
-- returns y
copy_pos_tree(x, y) ==
    EQ(x, y) => y
    CONSP(x) and CONSP(y) =>
        if not(get_pos(y)) and (pos := get_pos(x)) then
            HPUT($parse_positions, y, pos)
        if not(get_name_pos(y)) and (pos := get_name_pos(x)) then
            HPUT($name_positions, y, pos)
        copy_pos_tree(first x, first y)
        copy_pos_tree(rest x, rest y)
        y
    y

-- give the nodes of x without a position, down to the nodes that have
-- one, the position pos; returns x
note_new_pos(x, pos) ==
    pos and CONSP(x) and not(get_pos(x)) =>
        HPUT($parse_positions, x, pos)
        y := x
        while CONSP(y) repeat
            note_new_pos(first y, pos)
            y := rest y
        x
    x

-- the position of the name in the list cell c
get_name_pos(c) == HGET($name_positions, c)

name_entry_before?(a, b) == not(compareposns(rest a, rest b))

-- give the names in the list y the positions in l, a list of
-- [name, :position]: each one once, in source order; returns y
note_names(y, l) ==
    if rest l then l := SORT(l, function name_entry_before?)
    c := y
    while l and CONSP(c) repeat
        s := first c
        if s and atom(s) and not(get_name_pos(c)) and (hit := ASSQ(s, l)) then
            HPUT($name_positions, c, rest hit)
            l := [d for d in l | not(EQ(d, hit))]
        c := rest c
    y

-- give the names in the list y, which was built from x, the positions of
-- the same names in x: in the list x or in the lists directly in it,
-- except those that are kept as elements of y; returns y
carry_name_pos(x, y) ==
    not(CONSP(x)) or not(CONSP(y)) or EQ(x, y) => y
    -- usually a name stays at its place in the list
    missing := nil
    cx := x
    cy := y
    while CONSP(cy) repeat
        s := first cy
        if s and atom(s) and not(get_name_pos(cy)) then
            if CONSP(cx) and EQ(first cx, s) and (p := get_name_pos(cx)) then
                HPUT($name_positions, cy, p)
            else missing := [s, :missing]
        if CONSP(cx) then cx := rest cx
        cy := rest cy
    null(missing) => y
    l := nil
    c := x
    while CONSP(c) repeat
        el := first c
        if MEMQ(el, missing) and (p := get_name_pos(c)) then l := [[el, :p], :l]
        if CONSP(el) and not(MEMQ(el, y)) then
            d := el
            while CONSP(d) repeat
                if MEMQ(first d, missing) and (p := get_name_pos(d)) then
                    l := [[first d, :p], :l]
                d := rest d
        c := rest c
    l => note_names(y, l)
    y

-- a new list cell holding the element of the cell c, with its position
keep_name_cell(c) ==
    cell := [first c]
    if (p := get_name_pos(c)) then HPUT($name_positions, cell, p)
    cell

-- the position of x, a part of the node n: its own, or for a name the
-- earliest position of it in n
item_pos(n, x) ==
    CONSP(x) => get_pos(x)
    name_pos_in(x, n, true)

-- the earliest position of the name s in the list x, or with deep? also
-- in the lists inside it
name_pos_in(s, x, deep?) ==
    pos := nil
    c := x
    while CONSP(c) repeat
        if EQ(first c, s) then pos := earlier_pos(pos, get_name_pos(c))
        if deep? and CONSP(first c) then
            pos := earlier_pos(pos, name_pos_in(s, first c, true))
        c := rest c
    pos

-- the text "file:line:column: " for a position, or nil
pos_prefix(pos) ==
    null(pos) => nil
    fn := lnPlaceOfOrigin(poGetLineObject pos)
    ln := STRINGIMAGE(poLinePosn pos)
    col := poCharPosn pos
    if INTEGERP(col) then ln := CONCAT(ln, '":", STRINGIMAGE(col + 1))
    STRINGP(fn) => CONCAT(fn, '":", ln, '": ")
    CONCAT('"line ", ln, '": ")
