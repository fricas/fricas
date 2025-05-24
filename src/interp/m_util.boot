)package "BOOT"

DEFPARAMETER($error_mark, GENSYM())

get_io_index_table(stream, io?) ==
    pos := READ(stream, nil, nil)
    NUMBERP(pos) =>
        FILE_-POSITION(stream, pos)
        res := READ(stream)
        FILE_-POSITION(stream, pos)
        res
    io? =>
        FILE_-POSITION(stream, 0)
        PRINC('"                    ", stream)
        nil
    nil

kaf_open(name, io?) ==
    full_name :=
        io? => make_full_namestring(name)
        make_input_filename(name)
    if io? then
        kind := file_kind(full_name)
        if kind = -1 then
            makedir(full_name)
        else if kind = 0 then
            ERROR(FORMAT(nil, '"~s is an existing file, not a library",
                  full_name))
    stream := get_io_index_stream(full_name, io?)
    make_kaf((io? => 'output; 'input), full_name,
             get_io_index_table(stream, io?), stream)

kaf_close(kaf) ==
    istr := LIBSTREAM_-INDEXSTREAM(kaf)
    if LIBSTREAM_-MODE(kaf) = 'output then
        write_indextable(istr, LIBSTREAM_-INDEXTABLE(kaf))
    CLOSE(istr)

kaf_read(kaf, key, sv) ==
    not(LIBSTREAM_-MODE(kaf) = 'input) =>
        ERROR('"not input stream")
    NULL(entry := find_key(LIBSTREAM_-INDEXTABLE(kaf), key)) =>
        sv = $error_mark =>
            ERROR(FORMAT(nil, '"key ~a not found", key))
        sv
    pos := CADR(entry)
    NULL(pos) => CDDR(entry)
    NUMBERP(pos) =>
        stream := LIBSTREAM_-INDEXSTREAM(kaf)
        FILE_-POSITION(stream, pos)
        READ(stream)
    BREAK()

kaf_read_list(kaf, key) ==
    IDENTP(key) => kaf_read(kaf, PNAME(key), [])
    BREAK()

make_entry(kaf, key, pos) ==
    entry := find_key(LIBSTREAM_-INDEXTABLE(kaf), key)
    NULL(entry) =>
        kaf_set_indextable(kaf,
                           CONS([key, :pos], LIBSTREAM_-INDEXTABLE(kaf)))
    SETF(CDR(entry), pos)

kaf_write(kaf, key, val) ==
    not(LIBSTREAM_-MODE(kaf) = 'output) =>
        ERROR('"not output stream")
    stream := LIBSTREAM_-INDEXSTREAM(kaf)
    pos :=
        NULL(val) => CONS(nil, val)
        [FILE_-POSITION(stream)]
    make_entry(kaf, key, pos)
    if NUMBERP(CAR(pos)) then write_to_stream(val, stream)

kaf_write0(kaf, key, val) ==
    IDENTP(key) => kaf_write(kaf, PNAME(key), val)
    BREAK()

kaf_remove(kaf, key) ==
    itable := LIBSTREAM_-INDEXTABLE(kaf)
    itable := assoc_delete_equal(itable, key)
    kaf_set_indextable(kaf, itable)

rkeys2(kaf) ==
    MAPCAR(function CAR, LIBSTREAM_-INDEXTABLE(kaf))

rkeys(name) ==
    kaf := kaf_open(name, false)
    res := rkeys2(kaf)
    kaf_close(kaf)
    res

get_directory_list(ft) ==
    cd := get_current_directory()
    member(ft, ['"NRLIB", '"DAASE"]) =>
        $UserLevel = 'development => cons(cd, $library_directory_list)
        $library_directory_list
    [cd, get_home_dir(), :$directory_list]

object2String2(x) ==
    STRINGP(x) => x
    IDENTP(x)  => x
    WRITE_-TO_-STRING(x)

filler_chars(n, char_str) ==
    not(STRINGP(char_str)) => BREAK()
    n <= 0 => '""
    make_string0(n, char_str.0)

filler_spaces(n) == filler_chars(n, '" ")
