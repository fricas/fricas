# Spool output has lines with and without "-- ".
# There are special lines that come from ioHook that look like
## -- \begin{TeXOutput}
## .. \end{TeXOutput}
## -- \begin{MessageOutput}
## .. \end{MessageOutput}
## -- \begin{SysCmdOutput}
## .. \end{SysCmdOutput}
# Lines inside these environments are treated in a special way.
# These environments appear inside lines that start with
## -- \begin{xtc}
# and end with
## -- \end{xtc}
# or "noOutputXtc" instead of "xtc".
# All lines outside the xtc stuff that have no "-- " in front of it
# will not make it into the output.
# Output of commands appears inside
## \begin{TeXOutput}
## \begin{fricasmath}{STEPNUMBER}
## \end{fricasmath}
## \end{TeXOutput}

# If the first word of a MessageOutput is "Type:" then that
# environment describes the type of the output and the whole environment
# will be replaced by
## \formatResultType{TYPE}
# where TYPE is the text after "Type: " and is different from "Void".
# If the type is "Void", nothing is output.

BEGIN {
    inxtc=0
    print "% !! DO NOT MODIFY THIS FILE BY HAND !! Created by spool2tex.awk."
}

# Discard \begin{inputonly} ... \end{inputonly}.
/^-- \\begin{inputonly}/,/^-- \\end{inputonly}/ {next}

# start of xtc
/^-- \\begin{xtc}/ || /^-- \begin{noOutputXtc}/ {
    inxtc=1
    sub(/^-- /, "")
    print $0
    next
}

# everything outside xtc environments...
inxtc==0 {
    if (substr($0,1,3) == "-- ") {
        printf("%s\n",substr($0,4))
    }
    next
}

# For everything below xtc>0.

# end of xtc
/^-- \end{xtc}$/ || /^-- \end{noOutputXtc}$/ {
    printf("%s\n",substr($0,4))
    inxtc=0
    next
}

/^-- \\begin{SysCmdOutput}/ {
    getline
    if ($0 != "-- \\end{SysCmdOutput}") {
        print "\\begin{SysCmdOutput}"
        if ($0 != "-- \\begin{MessageOutput}") {print $0}
        inSysCmdOutput=1
    }
    next
}

/^-- \\begin{MessageOutput}/ {
    if(inSysCmdOutput==1) {next} # skip inside SysCmdOutput
    getline
    if ($1 == "Type:") {
        # Treat a line whose first non-space characters are "Type:".
        if ($2 != "Void") {
            gsub(/%/,"\\%")
            gsub(/,/,", ")
            $1=""          # remove "Type:"
            gsub(/^ */,"") # and the blanks following it
            print "\\formatResultType{" $0 "}"
        }
        # read until \\end{MessageOutput} and discard
        while (substr($0,1,22) != "-- \\end{MessageOutput}") {
            getline
        }
        next
    }
    if ($0 != "-- \\end{MessageOutput}") {
        print "\\begin{MessageOutput}"
        print $0
        inMessageOutput=1
    }
    next
}

/^-- \\end{MessageOutput}/ {
    if(inSysCmdOutput==1) {next}
    printf("%s\n",substr($0,4))
    inMessageOutput=0
    next
}

/^-- \\end{SysCmdOutput}/ {
    printf("%s\n",substr($0,4))
    inSysCmdOutput=0
    next
}

(inMessageOutput==1 || inSysCmdOutput==1) {
    print $0
    next
}

/^-- \\begin{TeXOutput}/ {
    print "\\begin{TeXOutput}"
    inTeXOutput=1
    next
}

/^-- \\end{TeXOutput}/ {
    print "\\end{TeXOutput}"
    inTeXOutput=0
    next
}

# Treat math lines (ignore empty lines)
inTeXOutput==1 {
    if ($1 != "") {print $0}
}

# Print lines inside xtc that start with "-- ".
/^-- / {printf("%s\n",substr($0,4))}
