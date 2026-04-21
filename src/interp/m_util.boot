)package "BOOT"

)if false

The KAF file consists of header, entries and index.  Header is just
byte offset to the index.  Index is a Lisp association list
containing keys (strings) and byte offsets of corresponding entries.

As an optimization, if the data is a simple thing (currently only
Lisp NIL is considered simple enough), then the entry byte offset
is replaced by immediate data.

When opening a KAF file we read index into memory and keep there.
Index is written back only when KAF is closed (higher level code
frequently closes KAF files to force storing of the index).

)endif

DEFPARAMETER($error_mark, GENSYM())

$mode_off := 0
$dir_name_off := 1
$index_table_off := 2
$index_stream_off := 3

make_kaf(mode, dir_name, index_table, index_stream) ==
    kaf := GETREFV(4)
    kaf.$mode_off := mode
    kaf.$dir_name_off := dir_name
    kaf.$index_table_off := index_table
    kaf.$index_stream_off := index_stream
    kaf

kaf_mode(kaf) == kaf.$mode_off
kaf_dir_name(kaf) == kaf.$dir_name_off
kaf_index_table(kaf) == kaf.$index_table_off
kaf_set_indextable(kaf, index_table) ==
    kaf.$index_table_off := index_table
kaf_index_stream(kaf) == kaf.$index_stream_off

$index_filename := "index.KAF"

get_io_index_stream(dir_name, io?) ==
    ds := (io? => 'io; 'input)
    ind_name := CONCAT(dir_name, '"/", $index_filename)
    open_stream(ind_name, ds, false)

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
        io? => name
        make_input_filename1(name)
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
    istr := kaf_index_stream(kaf)
    if kaf_mode(kaf) = 'output then
        write_indextable(istr, kaf_index_table(kaf))
    CLOSE(istr)

kaf_read(kaf, key, sv) ==
    not(kaf_mode(kaf) = 'input) =>
        ERROR('"not input stream")
    NULL(entry := find_key(kaf_index_table(kaf), key)) =>
        sv = $error_mark =>
            ERROR(FORMAT(nil, '"key ~a not found", key))
        sv
    pos := CADR(entry)
    NULL(pos) => CDDR(entry)
    NUMBERP(pos) =>
        stream := kaf_index_stream(kaf)
        FILE_-POSITION(stream, pos)
        READ(stream)
    BREAK()

kaf_read_list(kaf, key) ==
    IDENTP(key) => kaf_read(kaf, PNAME(key), [])
    BREAK()

make_entry(kaf, key, pos) ==
    entry := find_key(kaf_index_table(kaf), key)
    NULL(entry) =>
        kaf_set_indextable(kaf,
                           CONS([key, :pos], kaf_index_table(kaf)))
    SETF(CDR(entry), pos)

kaf_write(kaf, key, val) ==
    not(kaf_mode(kaf) = 'output) =>
        ERROR('"not output stream")
    stream := kaf_index_stream(kaf)
    pos :=
        NULL(val) => CONS(nil, val)
        [FILE_-POSITION(stream)]
    make_entry(kaf, key, pos)
    if NUMBERP(CAR(pos)) then write_to_stream(val, stream)

kaf_write0(kaf, key, val) ==
    IDENTP(key) => kaf_write(kaf, PNAME(key), val)
    BREAK()

kaf_remove(kaf, key) ==
    itable := kaf_index_table(kaf)
    itable := assoc_delete_equal(itable, key)
    kaf_set_indextable(kaf, itable)

rkeys2(kaf) ==
    MAPCAR(function CAR, kaf_index_table(kaf))

rkeys(name) ==
    kaf := kaf_open(name, false)
    res := rkeys2(kaf)
    kaf_close(kaf)
    res

  -- File operations
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

SNAME(s) ==
    not(SYMBOLP(s)) => BREAK()
    SYMBOL_-NAME(s)

ext_position(n) ==
    k := #n - 1
    dot := '".".0
    while k >= 0 and not(n.k = dot) repeat
        is_dir_sepatator?(n.k) => k := -1
        k := k - 1
    k

file_extention(n) ==
    k := ext_position(n)
    k < 0 => '""
    SUBSTRING(n, k + 1, #n - 1 - k)

drop_extention(n) ==
    k := ext_position(n)
    k < 0 => n
    SUBSTRING(n, 0, k)

has_extention?(s, e) ==
    not((m := #e) < (n := #s)) => false
    l := n - m
    not(s.(l - 1) = '".".0) => false
    res := true
    for i in 0..(m - 1) while res repeat
        res := s.(l + i) = e.i
    res

last_dir_separator(n) ==
    k := #n - 1
    while k >=0 and not(is_dir_sepatator?(n.k)) repeat
        k := k - 1
    k

drop_directory_part(n) ==
    k := last_dir_separator(n) + 1
    SUBSTRING(n, k, #n - k)

file_basename(n) ==
    k := last_dir_separator(n) + 1
    l := ext_position(n)
    l < 0 => SUBSTRING(n, k, #n - k)
    SUBSTRING(n, k, l - k)

file_directory(n) ==
    k := #n - 1
    while k >=0 and not(is_dir_sepatator?(n.k)) repeat
        k := k - 1
    k < 0 => '""
    k = 0 => "/"
    SUBSTRING(n, 0, k)

make_filename2(n, e) ==
    has_extention?(n, e) => n
    CONCAT(n, '".", e)

probe_name(name) ==
    fricas_probe_file(name) => name
    nil

make_input_filename2(n, e) ==
    make_input_filename1(make_filename2(n, e))

make_input_filename1(name) ==
    is_absolute_name?(name) =>
        probe_name(name)
    ext := file_extention(name)
    d_lst := get_directory_list(ext)
    found := false
    for d in d_lst while(not(found)) repeat
        n1 := CONCAT(d, '"/", name)
        found := fricas_probe_file(n1)
    found => n1
    probe_name(name)

find_file(name, ftl) ==
    res := nil
    for ft in ftl while(not(res)) repeat
        res := make_input_filename2(name, ft)
    res

erase_lib0(n, e) == erase_lib(make_filename2(n, e))

make_fname(d, n, e) ==
    n = '"" => throwMessage '"name part can not be empty"
    n :=
        e = '"" => n
        CONCAT(n, '".", e)
    d = '"" => n
    n :=
        is_dir_sepatator?(n.0) => SUBSTRING(n, 1, #n - 1)
        n
    is_dir_sepatator?(d.(#d - 1)) => CONCAT(d, n)
    CONCAT(d, "/", n)

new_fname(d, n, e) ==
    good := false
    res := nil
    while not(good) repeat
        res := make_fname(d, PNAME(GENSYM(n)), e)
        good := not(fricas_probe_file(res))
    res

is_system_path?(n) ==
    #n < #(sr := $spadroot) => false
    res := true
    for i in 0..(#sr - 1) while res repeat
        res := n.i = sr.i
    res

delete_file(f) == DELETE_-FILE(f)

MAKE_INSTREAM(name) ==
    open_stream(make_input_filename1(name), 'input, false)

MAKE_OUTSTREAM(name) == open_stream(name, 'output, false)

make_out_stream(name) == cons(true, MAKE_OUTSTREAM(name))

make_append_stream(name) == cons(true, open_stream(name, 'output, true))

make_std_out_stream() == cons(false, get_lisp_std_out())

make_compiler_output_name(dir_name, name) ==
    CONCAT(dir_name, '"/", name, '".lsp")

make_compiler_output_stream(lib, name) ==
    open_stream(make_compiler_output_name(kaf_dir_name(lib), name),
                'output, false)

compile_lib(dir_name) ==
    name := file_basename(dir_name)
    compile_lib_file(make_compiler_output_name(dir_name, name))
