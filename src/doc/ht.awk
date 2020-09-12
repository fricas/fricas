# Copyright The Numerical Algorithms Group Limited 1992-94.
#
# This is the main file for processing htex files and creating ht files
# It accepts one argument or four arguments. After processing the
# arguments, ARGC is reset to 2, if necessary.
#
# One argument case:
#   - argument 1 is a ugXX.htex Users Guide chapter file where XX
#     is the chapter number.
#   - \head{chapter}... tag is included
#
# Four argument case:
#   - this is an example file for an exposed constructor of the form
#     abbr.htex where abbr is the constructor abbreviation. This file
#     name is the first argument
#   - 2nd argument = directory of .menu files
#   - 3rd argument = full constructor name
#   - 4th argument = the users guide chapter number for the examples
#     chapter
#   - 5th argument = the section number to be used for the constructor

BEGIN           {
        globalPagePrefix = "NOT-ASSIGNED-YET"
        globalTitle      = "NOT-ASSIGNED-YET"
        globalNumber     = "NOT-ASSIGNED-YET"

        n=split(ARGV[1],a,"/")
        fn = a[n]
        n=split(fn,a,".")
        fn=a[1]
        menusdir = ARGV[2]
        if (ARGC > 3) { # example file, not chapter file
                conName = ARGV[3]
                example = 1
                chapNum = ARGV[4]
                secNum  = ARGV[5]
                inSect = 1
        }
        else {  # chapter file, not example file
                # figure out what the chapter number is. we assume the
                # filename is of the form ugXX.htex
                chapNum = 0 + substr(fn,3,2)
                secNum = 0
                example = 0
                inSect = 0
        }
        ARGC = 2
        xmpStepCounter = 0
        inChap = 0
        inSubSect = 0
        inCenter = 0
        indent = 0
        inItem = 0
        inEnum = 0
        subSecNum = 0
        comsep = "% ====================================================================="
        print "% Copyright The Numerical Algorithms Group Limited 1992-94. All rights reserved."
        print "% !! DO NOT MODIFY THIS FILE BY HAND !! Created by ht.awk."
        if (example) {
                num = chapNum"."secNum
                unConName = unnumber(conName)
                addStdDefs(unConName"Xmp",conName,num)
                printf "\\begin{page}{%sXmpPage}{%s %s}\n",unConName,num,conName
                print comsep
                print "\\beginscroll"
        }
}

# just kill full line comments that have a blank in col 2
/^% /            {
        next
}

# delete index entries
/^\\index/ || /^\\exptypeindex/ || /^\\syscmdindex/ {
        printf("%%-%% \\HD%s{%sPage}{%s}{%s}\n",substr($0,2), globalPagePrefix, globalNumber, globalTitle)
        next
}

# delete lines between \begin{texonly} and \end{texonly}
/^\\begin{texonly}/,/^\\end{texonly}/ {next}

# HyperDoc should never see stuff that is not intended for it, i.e. we
# translate \texht{T}{H} into H. We assume that \texht with arguments
# appears on a single line.
/\\texht{/ {
        cmd = "\\texht{"
        line = $0
        p = index(line,cmd)
        while (p > 0) {
            pref = (p == 1) ? "" : substr(line,1,p-1)
            line = substr(line,p)
            n = endMacroIndex(line,2)
            if (n < 0) {
                print ARGV[1] ":" NR ": ERROR: texht not on ended on the same line"
                print $0
                next
            }
            line = extractArg(line,2) substr(line,n+1)
            if (p != 1) {line = pref line}
            p = index(line,cmd)
        }
        $0 = line
}

# delete lines between \begin{inputonly} and \end{inputonly}
/^\\begin{inputonly}/,/^\\end{inputonly}/ {next}

# handle discard stuff (delete the lines)
/^\\begin{discard}/,/^\\end{discard}/ {next}

# delete \begin{htonly} and \end{htonly} lines (leaving what is in between)
/^\\begin{htonly}/ || /^\\end{htonly}/  {next}


# handle various xmpLines and figXmpLines environments

/^\\begin{xmpLinesNoReset}/ {
        inXmpLines = 1
        print "\\beginImportant"
        print "  "
        print "\\noindent"
        next
}
/^\\end{xmpLinesNoReset}/ {
        print "\\endImportant"
        inXmpLines = 0
        next
}
/^\\begin{xmpLinesNoResetPlain}/ || /^\\end{xmpLinesNoResetPlain}/{
        if(/^\\begin/) {inXmpLines=1} else {inXmpLines=0}
        print "  "
        print "\\noindent"
        next
}
/^\\begin{xmpLinesPlain}/ || /^\\end{xmpLinesPlain}/{
        if(/^\\begin/) {inXmpLines=1} else {inXmpLines=0}
        xmpStepCounter = 0
        print "  "
        print "\\noindent"
        next
}
/^\\begin{xmpLines}/ || /^\\begin{figXmpLines}/ {
        inXmpLines = 1
        print "\\beginImportant"
        print "  "
        print "\\noindent"
        xmpStepCounter = 0
        # We can have an optional argument of the form
        # [caption={...}, label={...}]
        # which must all appear on one line.
        # We'll remove [ and ] and change "caption=" into "\caption"
        # and ", *label=" into "\label".
        if(/\\begin{figXmpLines}\[/){
            opts=substr($0,21)
            sub(/] *$/, "",opts)
            sub(/caption=/, "\\caption", opts)
            sub(/, *label=/, "\\label", opts)
        } else {
            opts=""
        }
        next
}
/^\\end{xmpLines}/ || /^\\end{figXmpLines}/ {
        if(opts != "") {print opts}
        print "\\endImportant"
        xmpStepCounter = 0
        inXmpLines = 0
        next
}

inXmpLines==1 {
        if(/^%%%/){print substr($0,4);  next}
        xmpStepCounter++
        sc = xmpStepCounter"."
        scl = length(sc)
        while (scl < 4) {
          sc = sc"\\ "    # adding two characters
          scl++
        }
        sub(/ *--.*/, "") # throw away comments
        gsub(/\\/, "\\bs{}")
        gsub(/{/, "\\{")
        gsub(/}/, "\\}")
        gsub(/\\bs\\{\\}/, "\\bs{}")
        gsub(/ /, "\\ ")
        gsub(/&/, "\\\\&")
        gsub(/\#/, "\\#")
        gsub(/\$/, "\\$")
        gsub(/%/, "\\%")
        gsub(/_/, "\\_")
        gsub(/~=/, "\\notequal{}")
        printf "{\\tt %s\\ %s}\\newline\n",sc,$0
        next
}


/^\\begin{simpleList}/ {
        print "\\begin{items}"
        next
}

/^\\end{simpleList}/ {
        print "\\end{items}"
        next
}

# pass through lines in spadsrc environment (piles) removing backslashes
/^\\begin{spadsrc}/     {
        print
        getline
        while ($1 != "\\end{spadsrc}") {
                gsub(/\\/,"")
                print
                getline
        }
        print
        next
}

/^\\spadcommand/        {
        gsub(/\\_/,"_")
        print "\\spadpaste"substr($0,13)
        next
}
/^\\spadgraph/          {
        gsub(/\\_/,"_")
        print "\\graphpaste"substr($0,11)
        next
}

# do some translations

# Escape TeX special characters inside arguments of
# \spad, \spadop, \spadopFrom, \spadfun, \spadfunFrom, \spadtype, \spadSyntax.
# texSpecialChars = "\\{ }$&#^_%~"
#/\\userfun{/     {$0 = escapeArgs($0, "\\userfun{",     1)}
#/\\pspadtype{/   {$0 = escapeArgs($0, "\\pspadtype{",   1)}
/\\spad{/        {$0 = escapeArgs($0, "\\spad{",        1)}
/\\spadop{/      {$0 = escapeArgs($0, "\\spadop{",      1)}
/\\spadfun{/     {$0 = escapeArgs($0, "\\spadfun{",     1)}
/\\spadtype{/    {$0 = escapeArgs($0, "\\spadtype{",    1)}
/\\spadopFrom{/  {$0 = escapeArgs($0, "\\spadopFrom{",  2)}
/\\spadfunFrom{/ {$0 = escapeArgs($0, "\\spadfunFrom{", 2)}
/\\spadSyntax{/  {$0 = escapeArgs($0, "\\spadSyntax{",  1)}

# handle cross references
/\\spadref/             {
        while (i = match($0,/\\spadref{/)) {
          lab = substr($0,i+9) # front of label
          lab = substr(lab,1,-1 + index(lab,"}"))
          lab = unnumber(lab)
          str = "\\downlink{``\\" lab "Title''}{" lab "Page} in Section \\" lab "Number\\ignore"
          sub(/\\spadref/,str)
        }
}
/\\xmpref/             {
        while (i = match($0,/\\xmpref{/)) {
          lab = substr($0,i+8) # front of label
          lab = substr(lab,1,-1 + index(lab,"}"))
          str = "\\downlink{`" lab "'}{" unnumber(lab) "XmpPage}\\ignore"
          sub(/\\xmpref/,str)
        }
}
/\\menuxmpref/             {
        while (i = match($0,/\\xmpref{/)) {
          lab = substr($0,i+12) # front of label
          lab = substr(lab,1,-1 + index(lab,"}"))
          str = "\\menudownlink{`" lab "'}{" unnumber(lab) "XmpPage}\\ignore"
          sub(/\\menuxmpref/,str)
        }
}
/\\chapref/             {
        while (i = match($0,/\\chapref{/)) {
          lab = substr($0,i+9) # front of label
          lab = substr(lab,1,-1 + index(lab,"}"))
          lab = unnumber(lab)
          str = "\\downlink{``\\" lab "Title''}{" lab "Page} in Chapter \\" lab "Number\\ignore"
          sub(/\\chapref/,str)
        }
}
/\\appxref/             {
        while (i = match($0,/\\appxref{/)) {
          lab = substr($0,i+9) # front of label
          lab = substr(lab,1,-1 + index(lab,"}"))
          lab = unnumber(lab)
          str = "\\downlink{``\\" lab "Title''}{" lab "Page} in Appendix \\" lab "Number\\ignore"
          sub(/\\appxref/,str)
        }
}
/^\\menuspadref/ || /^\\titledspadref/       {
        # we want to skip the first argument and get label
        n = length($0)
        while (substr($0,n,1) != "}")
          --n
        lastpos = --n
        while (substr($0,n,1) != "{")
          --n
        firstpos = ++n
        lab = substr($0,firstpos,lastpos-firstpos+1)
        lab = unnumber(lab)
        if (substr($1,2,1) == "m")
          printf "\\menudownlink{``\\" lab "Title''}{" lab "Page} in section \\" lab "Number\n"
        else
          printf "\\downlink{``\\" lab "Title''}{" lab "Page} in section \\" lab "Number\n"
        next
}

/^\\begin{center}/      {
        inCenter = 1
        next
}

/^\\end{center}/      {
        inCenter = 0
        next
}

(inCenter == 1)         {
        sub("\\\\\\\\$","") # seems like 4 should suffice here
#       sub("/\\\\/","")
        printf "\\centerline{{%s}}\n",$0
        next
}

/^\\head{chapter}/      {
        inChap = 1
        inSubSect = 0
        subSecNum = 0
        startPage(16)
        next
}

/^\\head{section}/      {
        handleSection(16)
        next
}
/^\\head{subsection}/      {
        handleSubSection(19)
        next
}

/^\\begin{description}/   {
        startList(0)
        next
}
/^\\end{description}/   {
        stopList(0)
        next
}

/^\\begin{enumerate}/   {
        inEnum = 1
        startList(4)
        next
}
/^\\end{enumerate}/   {
        inEnum = 0
        stopList(4)
        next
}

/^\\begin{itemize}/   {
        inItem = 1
        startList(4)
        next
}
/^\\end{itemize}/     {
        inItem = 0
        stopList(4)
        next
}

($1 == "\\item")      {
        if (inItem)             {
                printf "\\item[-] "
                for (i = 2; i < NF; i++)
                        printf "%s ",$i
                printf "%s\n",$i
        }
        else if (inEnum)        {
                printf "\\item[%d. ] ",inEnum
                for (i = 2; i < NF; i++)
                        printf "%s ",$i
                printf "%s\n",$i
                inEnum = inEnum + 1
        }
        else
                print
        next
}

# otherwise
                {
        print $0
        next
}

END             {
        if (inChap) {
          printSectionMenu()
          endPage()
        }
        if (inSect || inSubSect)
          endPage()
        else if (example)
          endPage()
}

function startList(i) {
        indent = indent + i
        printf "\\indent{%d}\n",indent
        print "\\beginitems"
}

function stopList(i) {
        print "\\enditems"
        indent = indent - i
        printf "\\indent{%d}\n",indent
}

function extractTitleAndLabel(type,line,  et,pagePrefix) {
        # return title in slot 1, label in slot 2
        et = index(line,"}{")
        if (et == 0) {
          printf "%! ERROR: badly formed \\head form.\n"
          return "\\begin{page}{ERRORPage}{ERROR}\n"
        }
        else {
          if (chapNum > 15) {
                chap = substr("ABCDEFGHIJKLMNOPQRSTUVWXYZ",chapNum-14,1)
          }
          else
                chap = chapNum
          if (type == 1) # chapter
                num = chap"."
          else if (type == 2) # section
                num = chap"."secNum"."
          else if (type == 3) # section
                num = chap"."secNum"."subSecNum"."
          conName = substr(line,1,et-1)
          pagePrefix = substr(line,et+2,length(line)-et-2)
          pagePrefix = unnumber(pagePrefix)
          addStdDefs(pagePrefix,conName,num)
          return "\\begin{page}{" pagePrefix "Page}{" num " " conName "}\n"
        }
}

function startPage(preflen) {
        printf "%%\n"
        if (match(substr($0,1,preflen),/chapter/))
                type = 1
        else if (match(substr($0,1,preflen),/subsection/))
                type = 3
        else
                type = 2
        printf extractTitleAndLabel(type,substr($0,preflen)) # skip prefix
        print comsep
        print "\\beginscroll"
}

function endPage() {
        printf "\\endscroll\n\\autobuttons\n\\end{page}\n%%\n"
}

function printSectionMenu() {
        system("cat "menusdir"/"fn".menu")
}

function printSubSectionMenu() {
        system("cat "menusdir"/ug"chapNum"."secNum".menu")
}

function handleSection(prefLen) {
        if (inChap) {
          printSectionMenu()
          endPage()
          inChap = 0
        }
        else if (inSect || inSubSect)
          endPage()
        inSect = 1
        inSubSect = 0
        subSecNum = 0
        secNum++
        startPage(prefLen)
}

function handleSubSection(prefLen) {
        if (inSubSect)
          endPage()
        else if (inSect) {
          printSubSectionMenu()
          endPage()
          inSect = 0
        }
        inSubSect = 1
        subSecNum++
        startPage(prefLen)
}

function addStdDefs(pagePrefix,conName,num) {
        globalPagePrefix = pagePrefix
        globalTitle      = conName
        globalNumber     = num

        printf "\\newcommand{\\%sTitle}{%s}\n",pagePrefix,conName
        printf "\\newcommand{\\%sNumber}{%s}\n",pagePrefix,num
        print  "%"
        print comsep
}

function unnumber(s) {
        gsub(/1/,"One",s)
        gsub(/2/,"Two",s)
        gsub(/3/,"Three",s)
        gsub(/4/,"Four",s)
        gsub(/5/,"Five",s)
        gsub(/6/,"Six",s)
        gsub(/7/,"Seven",s)
        gsub(/8/,"Eight",s)
        gsub(/9/,"Nine",s)
        gsub(/0/,"Zero",s)
        return s
}

# Escape special characters by a backslash. Replace "\" by "\bs{}"
function escapeArg(arg) {
        gsub(/\\/, "\\bs{}", arg)
        gsub(/{/, "\\{", arg)
        gsub(/}/, "\\}", arg)
        gsub(/\\bs\\{\\}/, "\\bs{}", arg)
        gsub(/\$/, "\\$", arg)
        gsub(/&/, "\\\\&", arg)
        gsub(/\#/, "\\#", arg)
        gsub(/_/, "\\_", arg)
        gsub(/%/, "\\%", arg)
#        gsub(/^/, "\\^", arg) # not necessary
        gsub(/~=/, "\\notequal{}", arg)
        gsub(/~/, "\\~", arg)
        return arg
}

function escapeArgs(line, cmd, params,    p, n, arg, result) {
        p = index(line,cmd)
        result = ""
        while (p > 0) {
          if (p > 1) {result = result substr(line,1,p-1)}
          result = result cmd
          line = substr(line,p)
          n = endMacroIndex(line,params)
          if (n < 0) {
              print
              print "!!!"
              print "!!!" ARGV[1] ":" NR ":", cmd "<<no end>>}"
              print "!!!"
              exit 1
          }
          arg = escapeArg(extractArg(line,1))
          result = result arg "}"
          for (param = 2; param <= params; param++) {
              arg = extractArg(line,param)
              result = result "{" arg "}"
          }
          line = substr(line,n+1)
          p = index(line,cmd)
        }
        return result line
}


function endMacroIndex(line,parms,    pp,x,bc,cc,len,found) {
# assumes start of line is a macro call and returns position of final "}"
        x = 0
        found = -1
        pp = index(line,"{")
        len = length(line)
        if (pp != 0) {
          bc = 1
          for (x = pp+1; x<=len; x++) {
            cc = substr(line,x,1)
            if (cc == "{")
              bc++

            else if (cc == "}") {
              bc--
              if (bc == 0) {
                parms--
                if (parms == 0) {
                    found = 1
                    break
                }
              }
            }
          }
          x = x * found # negative if not found
        }
        return x
}

function extractArg(line,num,   p,arg) {
# assumes line is a macro call and extracts the num-th arg
        arg = ""
        p = index(line,"{")
        if (p != 0) {
          line = substr(line,p)

          if (num > 1) {
            p = endMacroIndex(line,num-1)
            line = substr(line,p+1)
          }
          p = endMacroIndex(line,1)
          arg = substr(line,2,p-2)
        }
        return arg
}
