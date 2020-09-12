/:\\head/ {
    sub(/.*{/,"")
    sub(/}/,"")
    view=$0 "Page"
    n=0
}

(/:\\\psXtc/ || /:\\xtc/ || /:\\noOutputXtc/ || /:\\nullXtc/) {n++}

/:\\spadgraph/ {spadgraph=1}

/:\\epsffile/ {
    sub(/.*{/,"")
    gsub(/}/,"")
    sub(/.*\//,"")
    if (spadgraph==1) {
        print "pics: " $0
    } else {
        print "no-pics: " $0
    }
    print $0 ": " view n ".VIEW/image.xpm"
    print "\tconvert $< $@"
    spadgraph=0
}
