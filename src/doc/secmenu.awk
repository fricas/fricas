# Copyright The Numerical Algorithms Group Limited 1992-94.
#
# This awk program creates two kinds of menus for a ht page.
#
# If there is one argument only, we assume it is a file of the
# form ugXX.htex where XX is the chapter number. This will create
# on /tmp a file with the name ugXX.menu containing a menu of
# section titles and possibly files named ugXX.Y.menu containing a
# menu of subsection titles for section number Y.
#
# If there are three arguments, the first argument is a file with name
# abbr.htex which is an example file for the constructor with abbreviation
# abbr. The second argument is the chapter number for the examples chapter
# and the third argument is the section number for this particular
# constructor.

# Only subsection menus go to a specified file (menuFile). All else
# goes to stdout.  Chapter 16 is the system command chapter and we
# make a table for those sections instead of a standard menu.

BEGIN                   {
        sysCmdChap = 16
        subSecNum = 0
        inSubSect = 0
        chapNum = 0 + ARGV[2]
        secNum  = ARGV[3]
        menusdir = ARGV[4]
        ARGC = 2
        # for the system command chapter we produce
        # a table of entries
        if (chapNum == sysCmdChap)
          print "\\table{"
        else
          print "\\beginmenu"
}

/^\\head{section}/      {
        sectionHandler($0,16,"")
        next
}
/^\\head{subsection}/      {
        subSectionHandler($0,19,"")
        next
}
# otherwise
                {
        next
}

END                     {
        if (chapNum == sysCmdChap)
          print "}"
        else
          print "\\endmenu"
        if (inSubSect)
                finalizeMenu()
}

function sectionHandler(line,prefLen,font) {
        if (inSubSect)
                finalizeMenu()
        inSubSect = 0
        subSecNum = 0
        secNum++
        # extract title
        line = substr(line,prefLen)            # skip prefix
        et = index(line,"}{")
        if (et == 0)
          printf "%! ERROR: badly formed \\head form.\n"
        else {
          title = substr(line,1,et-1)
          label = substr(line,et+2,length(line)-et-2)
          if (chapNum == sysCmdChap)
            printf("  { \\downlink{\\menuitemstyle{%s%s.%d. %s}}{%sPage} }\n",font,chap(chapNum),secNum,title,label)
          else
            printf("    \\menudownlink{{%s%s.%d. %s}}{%sPage}\n",font,chap(chapNum),secNum,title,label)
        }
}

function subSectionHandler(line,prefLen,font) {
        subSecNum++
        if (! inSubSect)
                startMenu()
        inSubSect = 1
        # extract title
        line = substr(line,prefLen)            # skip prefix
        et = index(line,"}{")
        if (et == 0)
          printf "%! ERROR: badly formed \\head form.\n"
        else {
          title = substr(line,1,et-1)
          label = substr(line,et+2,length(line)-et-2)
          printf("    \\menudownlink{{%s%s.%d.%d. %s}}{%sPage}\n",font,chap(chapNum),secNum,subSecNum,title,label) >> menuFile
        }
}

function startMenu() {
        menuFile = menusdir"/ug"chapNum"."secNum".menu"
        print "\\beginmenu" > menuFile
}

function finalizeMenu() {
        print "\\endmenu" >> menuFile
}

function chap(num) {
        if (num > 15)
              return substr("ABCDEFGHIJKLMNOPQRSTUVWXYZ",num-15,1)
        return num""
}
