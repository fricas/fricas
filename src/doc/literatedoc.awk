#!/usr/bin/awk
###################################################################
#
# Copyright (C) 2012, 2014, 2015  Ralf Hemmecke <ralf@hemmecke.org>
#
###################################################################
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above
# copyright notice, this list of conditions and the following
# disclaimer in the documentation and/or other materials provided
# with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived
# from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
# INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
# STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
# OF THE POSSIBILITY OF SUCH DAMAGE.
###################################################################
#
# Usage:
#     awk -f src/doc/literatedoc.awk file.spad > file.tex
#     TEXINPUTS=src/doc/:$TEXINPUTS pdflatex file.tex
#
# It is assumed that file.spad is nearly a LaTeX file.
# There should be
#   )if LiterateDoc
# and
#   )endif
# around the LaTeX part.
# To turn the .spad file into a .tex file, one basically has to replace
# ")if LiterateDoc" by "\end{spadcode}" and ")endif" by "\begin{spadcode}".
#
# The following awk code does exactly that. For simplicity, every line
# before the first ")if LiterateDoc" is copied to the output, but with
# a percent sign in front of it.
#
# Furthermore, empty lines after ")endif" and empty lines before
# ")if LiterateDoc" are moved from the code into the documentation part.
#
# The LaTeX part is supposed to contain a line
#   \usepackage{literatedoc}
# which defines a spadcode and verbcode environment.
#
###################################################################

/^[ \t]*$/ {
    if (open_literal) {
        print
    } else {
        emptyline++
    }
    next
}

/^)if LiterateDoc/ {
    if (open_code) {
        print "\\end{spadcode}";
        open_code=0 # non-code section
    }
    while (emptyline>0) {print ""; emptyline--}
    open_literal=1 # LiterateDoc is active
    literate=1 # We have seen at least one ')if LiterateDoc'.
    next
}

/^)endif/ {
    if (!open_literal) {
        print;
        open_code=1 # non-empty spad line seen
    }
    open_literal=0
    next
}

open_literal {print; next}

open_code==0 { # We know this is not an empty line.
    #-- If there is some stuff before the first ')if LiterateDoc', it
    #-- is considered and treated as comments.
    if (!literate) {print "%" $0; next}

    # Print pending empty lines.
    while (emptyline>0) {print ""; emptyline--}
    print "\\begin{spadcode}"
    open_code=1
    print
    next
}

# Arriving here we have open_code==1 and open_literal==0.
{
    while (emptyline>0) {print ""; emptyline--}
    print
}
