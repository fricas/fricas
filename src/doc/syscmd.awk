# Copyright The Numerical Algorithms Group Limited 1992-94.
#
# This awk script is meant to be using in the following way:
#
#      awk -f syscmd.awk ug16.htex
#
# with, perhaps, appropriate path information added.  This script
# reads Chapter 16 of the FriCAS User Guide and generates individual
# ASCII files that describe each of the system commands.  It handles
# tex macros and flows lines together.  Should ug16.htex get updated,
# this script should be checked to make sure it is still processing
# all the tex macros.
#
# Author: Robert S. Sutor (IBM Research)

BEGIN   {
        inFile   = 0
        fileName = ""
        secNum = 0
        chapNum = "A"
        indent = 0
        seenUnind = 1
        inDescriptionItem = 0
        flow = 1
        flowBuffer = ""
        lastLineWasBlank = 0
        debug = 0
        debugIndent = ""
        screenWidth = 78
}

# lines to discard
/^%/ || /^\\syscmdindex/ || /^\\index/ || /^\\head{chapter}{/     {
        next
}

/^\\head{section}{/     {
        flushFlowBuffer()
        line   = substr($0,16)
        syscmd = substr(line,1,index(line,"}")-1)
        startFile(syscmd)
        next
}

(inFile == 0)           {
        next
}

/^\\begin{enumerate}/ || /^\\begin{itemize}/  || /^\\begin{description}/ || /^\\begin{simpleList}/ {
        flushFlowBuffer()
        ind +=2
        printBlankLine()
        next
}

/^\\end{enumerate}/ || /^\\end{itemize}/ || /^\\end{simpleList}/ {
        flushFlowBuffer()
        ind -=2
        printBlankLine()
        next
}

/^\\begin{center}/   {
# assume each line is to be centered
        flushFlowBuffer()
        printBlankLine()
        getline
        while ($1 != "\\end{center}") {
          gsub(/\\\\/,"")
          $0 = cleanLine($0)
          l = length($0)
          if (l >= screenWidth)
            print $0 > fileName
          else {
            for (i = 0; i < (screenWidth - l) / 2; i++)
              printf(" ") > fileName
            print $0 > fileName
          }
          lastLineWasBlank = 0
          getline
        }
        printBlankLine()
        next
}

/^\\end{description}/   {
        flushFlowBuffer()
        ind -=2
        if (inDescriptionItem) {
          inDescriptionItem = 0
          ind -=2
        }
        printBlankLine()
        next
}

/^\\beginmenu/          {
        flushFlowBuffer()
        ind +=2
        next
}

/^\\endmenu/          {
        flushFlowBuffer()
        ind -=2
        next
}

/^\\beginImportant/ || /^\\endImportant/        {
        flushFlowBuffer()
        print "------------------------------------------------------------------------------" > fileName
        next
}

/^\\par\\noindent{\\bf / {
        flushFlowBuffer()
        line = substr($0,19)
        gsub(/}/," ",line)
        printIndented(line)
        next
}

/^\\begin{verbatim}/ {
        flushFlowBuffer()
        flow = 0
        printBlankLine()
        next
}

/^\\end{verbatim}/        {
        flow = 1
        printBlankLine()
        next
}

/^\\item\\menuitemstyle{/       {
        flushFlowBuffer()
        line = substr($0,21)
        gsub(/}/," ",line)
        printIndented("o "line)
        next
}

/^\\item\[/     { # description item
        flushFlowBuffer()
        if (inDescriptionItem) {
          ind -=2
        }
        inDescriptionItem = 1
        gsub(/\\item\[/,"")
        gsub(/\]/,"")
        printIndented(cleanLine($0))
        ind +=2
        next
}

/^\\item/       {
        flushFlowBuffer()
        printIndented("- "substr(cleanLine($0),6))
        next
}

/^\\ind/        {
        flushFlowBuffer()
        if (seenUnind) {
          ind +=2
          seenUnind = 0
        }
        gsub(/\\ind/,"")
        # now fall through
}

/^\\menuspadref{/ {
        flushFlowBuffer()
        $0 = extractArg($0,1)
        printIndented("o "cleanLine($0))
        next
}

/^\\titledspadref{/ {
        flushFlowBuffer()
        $0 = extractArg($0,1)
        printIndented("o "cleanLine($0))
        next
}

# a blank line
($1 == "") {
        if (! lastNameWasBlank) {
                if (flow)
                        flushFlowBuffer()
                printBlankLine()
        }
}

# a regular line
        {
        ui = index($0,"\\unind")
        if (ui) {
          gsub(/\\unind/,"")
          seenUnind = 1
        }
        if (flow) {
          if (flowBuffer == "")
                flowBuffer = $0
          else
                flowBuffer = flowBuffer " " $0
        }
        else
           printIndented($0)
        if (ui) {
          flushFlowBuffer()
          ind -=2
        }
}

END     {
        flushFlowBuffer()
        if (inFile) {
          close(fileName)
        }
        system("cat Introduction.help >> help.help")
        system("rm Introduction.help")
}

# -- functions -----------------------------------------------------------

function startFile(syscmd)        {
        if (inFile) {
          close(fileName)
        }

        print "Processing " syscmd

        if (substr(syscmd,1,1) == ")")
                fileName = substr(syscmd,2)".help"
        else
                fileName = syscmd".help"
        inFile = 1
        ++secNum

        print "====================================================================" > fileName
        printf("%s.%s.  %s\n",chapNum,secNum,syscmd) > fileName
        print "====================================================================" > fileName
        print " " > fileName
}

function printIndented(s,    i)  {
        lastLineWasBlank = 0
        for (i = 0; i < ind; i += 2)
                printf("  ") > fileName
        print s > fileName
}

function cleanLine(line,     p,pref,n,a,p2,pref2,n2,a2) {
debugEnter("cleanLine")

        gsub(/\\tt /,"",line)
        gsub(/\\it /,"",line)

        # first handle some macros

        p = index(line,"\\texht")
        while (p > 0) {
          pref = (p == 1) ? ""  : substr(line,1,p-1)
          line = substr(line,p)
          n = endMacroIndex(line,2)
          a = extractArg(line,2)
          # Transform "\lispmemolink{X}{Y}" to "[X]".
          p2 = index(a,"\\lispmemolink")
          if (p2 > 0) {
            pref2 = (p2 == 1) ? ""  : substr(a,1,p2-1)
            a = substr(a,p2)
            n2 = endMacroIndex(a,2)
            a2 = "[" extractArg(a,1) "]"
            a = a2 " " substr(a,n2+1)
            if (p2 != 1)
              a = pref2 " " a
          }
          line = a " " substr(line,n+1)
          if (p != 1)
            line = pref " " line
          p = index(line,"\\texht")
        }
        p = index(line,"\\spadref")
        while (p > 0) {
          pref = (p == 1) ? ""  : substr(line,1,p-1)
          line = substr(line,p)
          n = endMacroIndex(line,1)
          a = extractArg(line,1)
          if ("ugSysCmd" == substr(a,1,8))
            a = "description of command )" substr(a,9)
          else
            a = "the FriCAS User Guide index"
          line = a " " substr(line,n+1)
          if (p != 1)
            line = pref " " line
          p = index(line,"\\spadref")
        }
        p = index(line,"\\spadgloss")
        while (p > 0) {
          pref = (p == 1) ? ""  : substr(line,1,p-1)
          line = substr(line,p)
          n = endMacroIndex(line,1)
          a = extractArg(line,1)
          line = a " " substr(line,n+1)
          if (p != 1)
            line = pref " " line
          p = index(line,"\\spadgloss")
        }
        p = index(line,"\\footnote")
        while (p > 0) {
          pref = (p == 1) ? ""  : substr(line,1,p-1)
          line = substr(line,p)
          n = endMacroIndex(line,1)
          a = extractArg(line,1)
          line = "(" a ") " substr(line,n+1)
          if (p != 1)
            line = pref " " line
          p = index(line,"\\footnote")
        }
        p = index(line,"\\eth")
        while (p > 0) {
          pref = (p == 1) ? ""  : substr(line,1,p-1)
          line = substr(line,p)
          n = endMacroIndex(line,1)
          a = extractArg(line,1)
          line = a"th" substr(line,n+1)
          if (p != 1)
            line = pref " " line
          p = index(line,"\\eth")
        }
        p = index(line,"\\lispwindowlink")
        while (p > 0) {
          pref = (p == 1) ? ""  : substr(line,1,p-1)
          line = substr(line,p)
          n = endMacroIndex(line,2)
          a = extractArg(line,1)
          line = "[" a "] " substr(line,n+1)
          if (p != 1)
            line = pref " " line
          p = index(line,"\\lispwindowlink")
        }

        # now do some global substitutions

        gsub(/\\%/,"%",line)
        gsub(/\\_/,"_",line)
        gsub(/\\\$/,"$",line)
        gsub(/\\Language/,"FriCAS",line)
        gsub(/\\HyperName/,"HyperDoc",line)
        gsub(/\\axiomxl/,"Aldor",line)
        gsub(/\\Browse/,"Browse",line)
        gsub(/\\lanb/,"[",line)
        gsub(/\\ranb/,"]",line)
        gsub(/\\vertline/,"|",line)
        gsub(/\{\\tt /,"",line)
        gsub(/\{\\it /,"",line)
        gsub(/\{\\it/,"",line)
        gsub(/\{\\bf /,"",line)
        gsub(/\{\\sf /,"",line)
        gsub(/\\Lisp\{/,"Lisp",line)
        gsub(/\\noindent /,"",line)
        gsub(/\\noindent/,"",line)
        gsub(/\\spad\{/,"",line)
        gsub(/\\smath\{/,"",line)
        gsub(/\\spadcmd/,"",line)
        gsub(/\\spadsys/,"",line)
        gsub(/\\spadSyntax/,"",line)
        gsub(/\\spadFileExt/,"",line)
        gsub(/\\axiomType/,"",line)
        gsub(/\\spadtype/,"",line)
        gsub(/\\userfun/,"",line)
        gsub(/\\spadfun/,"",line)
        gsub(/\\beginenumerate/,"",line)
        gsub(/\\endenumerate/,"",line)
        gsub(/A\\#/,"AXIOM-XL",line)
        gsub(/\{/,"",line)
        gsub(/\}/,"",line)

debugExit("cleanLine","%s",line)
        return line
}

function debugEnter(name) {
        if (debug) {
          printf("%sEntering  %s\n",debugIndent,name)
          debugIndent = debugIndent "  "
        }
}

function debugExit(name,format,retval) {
        if (debug) {
          debugIndent = substr(debugIndent,3)
          if (format != "") {
            printf("%sExiting:  %s ("format")\n",debugIndent,name,retval)
          }
          else
            printf("%sExiting:  %s\n",debugIndent,name)
        }
}

function endMacroIndex(line,parms,    pp,x,bc,cc) {
# assumes start of line is a macro call and returns position of final "}"
debugEnter("endMacroIndex")
        x = 0
        pp = index(line,"{")
        if (pp != 0) {
          bc = 1
          for (x = pp+1; ; x++) {
            cc = substr(line,x,1)
            if (cc == "{")
              bc++
            else if (cc == "}") {
              bc--
              if (bc == 0) {
                parms--
                if (parms == 0)
                  break
              }
            }
          }
        }
debugExit("endMacroIndex","%d",x)
        return x
}

function extractArg(line,num,   p,arg) {
# assumes line is a macro call and extracts the num-th arg
debugEnter("extractArg")
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
debugExit("extractArg","%s",arg)
        return arg
}

function printBlankLine() {
        if (lastLineWasBlank == 0) {
          print " " > fileName
          lastLineWasBlank = 1
        }
}

function flushFlowBuffer(   ii) {
debugEnter("flushFlowBuffer")
        if (inFile && (flowBuffer != "")) {
                flowBuffer = cleanLine(flowBuffer)
                gsub(/  /," ",flowBuffer)
                while (ind + length(flowBuffer) > screenWidth ) {
                  for (ii = screenWidth - ind; ii > 0; ii--) {
                    if (substr(flowBuffer,ii,1) == " ")
                      break;
                  }
                  if (ii == 0)
                    break;
                  printIndented(substr(flowBuffer,1,ii-1))
                  flowBuffer = substr(flowBuffer,ii+1)
                }
                if (flowBuffer != "")
                  printIndented(flowBuffer)
                flowBuffer = ""
        }
debugExit("flushFlowBuffer")
}
