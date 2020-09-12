# This script creates the examples chapter.
# At stdin it requires the examples.list file.
# Output is written to stdout.

BEGIN   {
        print "% !! DO NOT MODIFY THIS FILE BY HAND !! Created by ugexamples.awk"
        print "% ====================================================================="
        print "%"
        print "\\chapter{Some Examples of Domains and Packages}\\label{ExamplesExposed}"
        print "%"
        print "In this chapter we show examples of many of the most commonly"
        print "used \\Language{} domains and packages."
        print "The sections are organized by constructor names."
        print "%"
}

# In examples.list we have lines in the form
## SHORTNAME LongName
        {
            printf "\\head{section}{%s}{%sXmp}\n",hyphen($2),$2
            printf "\\exptypeindex{%s}\n",$2
            printf "\\input{%s}\n",$1
            print "%"
        }

# add discretionary hyphens to constructor names
function hyphen(name) {
    gsub(/[A-Z]/,"\\-&", name)
    sub(/^\\-/,"",name)
    return name
}
