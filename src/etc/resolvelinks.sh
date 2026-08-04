# Taken from https://github.com/passagemath/passagemath/blob/main/sage#L29
#
# Resolve all symbolic links in a filename.  This more or less behaves
# like "readlink -f" except that it does not convert the filename to an
# absolute path (a relative path remains relative), nor does it treat
# "." or ".." specially.
resolvelinks() {
    # $in is what still needs to be converted (normally has no starting slash)
    in="$1"
    # $out is the part which is converted (normally ends with trailing slash)
    out="./"

    # Move stuff from $in to $out
    while [ -n "$in" ]; do
        # Normalize $in by replacing consecutive slashes by one slash
        in=$(echo "${in}" | sed 's://*:/:g')

        # If $in starts with a slash, remove it and set $out to the root
        in_without_slash=${in#/}
        if [ "$in" != "$in_without_slash" ]; then
            in=$in_without_slash
            out="/"
            continue
        fi

        # Check that the directory $out exists by trying to cd to it.
        # If this fails, then cd will show an error message (unlike
        # test -d "$out"), so no need to be more verbose.
        ( cd "$out" ) || return $?


        # Get the first component of $in
        f=${in%%/*}

        # If it is not a symbolic link, simply move it to $out
        if [ ! -L "$out$f" ]; then
            in=${in#"$f"}
            out="$out$f"

            # If the new $in starts with a slash, move it to $out
            in_without_slash=${in#/}
            if [ "$in" != "$in_without_slash" ]; then
                in=$in_without_slash
                out="$out/"
            fi
            continue
        fi

        # Now resolve the symbolic link "$f"
        f_resolved=`readlink -n "$out$f" 2>/dev/null`
        status=$?
        # status 127 means readlink could not be found.
        if [ $status -eq 127 ]; then
            # We don't have "readlink", try a stupid "ls" hack instead.
            # This will fail if we have filenames like "a -> b".
            fls=`ls -l "$out$f" 2>/dev/null`
            status=$?
            f_resolved=${fls##*-> }

            # If $fls equals $f_resolved, then certainly
            # something is wrong
            if [ $status -eq 0 -a "$fls" = "$f_resolved" ]; then
                echo >&2 "Cannot parse output from ls -l '$out$f'"
                return 1
            fi
        fi
        if [ $status -ne 0 ]; then
            echo >&2 "Cannot read symbolic link '$out$f'"
            return $status
        fi

        # In $in, replace $f by $f_resolved (leave $out alone)
        in="${in#${f}}"
        in="${f_resolved}${in}"
    done

    # Return $out
    echo "$out"
}
