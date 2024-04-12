)package "BOOT"

$all_constructors := []

$operation_hash := nil
$has_category_hash := nil

allConstructors() == $all_constructors

do_operation1(key, val) ==
    PUSH(key, $all_operations)

allOperations() ==
    $all_operations => $all_operations
    MAPHASH(function do_operation1, $operation_hash)
    $all_operations

spad_set_autoload(c_name) ==
    SETF(SYMBOL_-FUNCTION(c_name), mkAutoLoad(c_name))

-- Default domain.  This in principle could be computed, but currently
-- we use hardcoded list.

$defaultdomain_list := '(
  (AlgebraicallyClosedFunctionSpace Expression)
  (AssociationListAggregate AssociationList)
  (BiCPO FiniteBiCPO)                  (ComplexCategory Complex)
  (DequeueAggregate Dequeue)           (DistributionCategory Distribution)
  (FileNameCategory FileName)          (FreeLieAlgebra LiePolynomial)
  (FunctionSpace Expression)           (IntervalCategory Interval)
  (JetBundleBaseFunctionCategory JetBundleXExpression)
  (LazyStreamAggregate Stream)         (MonoidRingCategory MonoidRing)
  (MultisetAggregate Multiset)         (OctonionCategory Octonion)
  (PermutationCategory Permutation)    (PlottableSpaceCurveCategory Plot3D)
  (PointCategory Point)                (PriorityQueueAggregate Heap)
  (QuaternionCategory Quaternion)
  (RealRootCharacterizationCategory RightOpenIntervalRootCharacterization)
  (SequenceCategory Sequence)
  (StringCategory String)              (ThreeSpaceCategory ThreeSpace)
  )

initial_get_database(display_messages) == nil

-- Streams for databases.
$browse_stream := nil
$category_stream := nil
$interp_stream := nil
$operation_stream := nil

-- Stamps for databases.  Database files contain
-- timestamps, when timestamp in the file is the same as
-- in core timestamp we use cached data in core, otherwise
-- we need to re-read all data.  Stamp has form (position . time)
-- where postion is byte offset of main directory and time is
-- timestamp proper.  We initialise to nil to avoid confusion
-- with valid stamp
$browse_stream_stamp := nil
$category_stream_stamp := nil
$interp_stream_stamp := nil
$operation_stream_stamp := nil

handle_daase_file2(stream, name, o_stream, o_stamp, display_messages, fun) ==
    stamp := READ(stream)
    stamp = o_stamp => [stream, o_stamp]
    if display_messages then
        FORMAT(true, '"   Re-reading ~a.daase", name)
    FILE_-POSITION(stream, first(stamp))
    FUNCALL(fun, stream)
    FORMAT(true, '"~&")
    [stream, stamp]

handle_daase_file(name, fun, display_messages, o_stream, o_stamp) ==
    f_name := full_database_name(STRCONC(name, '".daase"))
    stream := OPEN(f_name)
    stamp := READ(stream)
    stamp = o_stamp => [stream, o_stamp]
    if display_messages then
        FORMAT(true, '"   Re-reading ~a.daase", name)
    FILE_-POSITION(stream, first(stamp))
    FUNCALL(fun, stream)
    FORMAT(true, '"~&")
    [stream, stamp]

write_daase_file2(stream, fun) ==
    -- Reseves space for stamp
    PRINC('"                              ", stream)
    master := FUNCALL(fun, stream)
    write_stamp(master, stream)

write_daase_file(name, fun) ==
    FORMAT(true, '"building ~a.daase", name)
    handle_output_file(STRCONC(name, '".build"), function write_daase_file2,
                       [fun])

full_database_name(name) ==
    if env_v := getEnv('"DAASE") then
        fname := STRCONC(env_v, '"/algebra/", name)
        FORMAT(true, '"   Using local database ~a..", fname)
        fname
    else
        STRCONC($spadroot, '"/algebra/", name)

move_database(name, condition) ==
    fin_name := STRCONC(name, '".daase")
    maybe_delete_file(fin_name)
    if condition then
        RENAME_-FILE(STRCONC(name, '".build"), fin_name)

-- cleanup, done before saving core (for ECL on startup)
reset_hash_tables(display_messages) ==
    $has_category_hash := MAKE_HASHTABLE('EQUAL)
    $operation_hash := MAKE_HASHTABLE('EQL)
    $all_constructors := []
    $interp_stream_stamp := [0, :0]
    open_interp_db(display_messages)
    $operation_stream_stamp := [0, :0]
    open_operation_db(display_messages)
    $browse_stream_stamp := [0, :0]
    open_browse_db(display_messages)
    $category_stream_stamp := [0, :0]
    open_category_db(display_messages)
    initial_get_database(display_messages)
    CLOSE($interp_stream)
    CLOSE($operation_stream)
    CLOSE($category_stream)
    CLOSE($browse_stream)
    maybe_gc()

-- constructor_ind and defaultdomain_ind are really unused
$abbreviation_ind := 0
$ancestors_ind := 1
$operationalist_ind := 2
$constructorcategory_ind := 3
$constructorkind_ind := 4
$constructormodemap_ind := 5
$cosig_ind := 6
$object_ind := 7
$modemaps_ind := 8
$niladic_ind := 9
$users_ind := 10
$dependents_ind := 11
$documentation_ind := 12
$constructorform_ind := 13
$predicates_ind := 14
$sourcefile_ind := 15
$parents_ind := 16

make_dbstruct() == GETREFV(17)

-- Order of assignment below defines structure of interp.daase:
-- (constructor, operationalist, constructormodemap, modemaps,
--  object, constructorcategory, niladic, abbreviation,
--  cosig, constructorkind, ancestors)
init_dbstruct1(con, db_data) ==
    dbstruct := make_dbstruct()
    MAKEPROP(con, 'DATABASE, dbstruct)
    dbstruct.$operationalist_ind := POP(db_data)
    dbstruct.$constructormodemap_ind := POP(db_data)
    dbstruct.$modemaps_ind := POP(db_data)
    dbstruct.$object_ind := POP(db_data)
    dbstruct.$constructorcategory_ind := POP(db_data)
    dbstruct.$niladic_ind := POP(db_data)
    dbstruct.$abbreviation_ind := abbr := POP(db_data)
    MAKEPROP(abbr, 'ABBREVIATIONFOR, con)
    dbstruct.$cosig_ind := POP(db_data)
    dbstruct.$constructorkind_ind := POP(db_data)
    dbstruct.$ancestors_ind := POP(db_data)

interp_open2(stream) ==
    clean_symbols()
    $all_constructors := []
    constructors := READ(stream)
    for db_data in constructors repeat
        con := POP(db_data)
        $all_constructors := ADJOIN(con, $all_constructors)
        init_dbstruct1(con, db_data)


open_interp_db(display_messages) ==
    res := handle_daase_file('"interp", function interp_open2,
                             display_messages, $interp_stream,
                             $interp_stream_stamp)
    $interp_stream := first(res)
    $interp_stream_stamp := first(rest(res))

-- Order of assignment below defines structure of browse.daase:
-- (constructor, sourcefile, constructorform, documentation,
--  predicates, parents)
init_dbstruct2(db_data) ==
    con := first(db_data)
    dbstruct := GET(con, 'DATABASE)
    if not(dbstruct) then
        FORMAT(true, "browseOpen:~%")
        FORMAT(true, "the browse database contains a data ~a~%", db_data)
        FORMAT(true, "for which we cannot get the database structure.~%")
        FORMAT(true, "Creating a new one~%")
        dbstruct := make_dbstruct()
        MAKEPROP(con, 'DATABASE, dbstruct)
        $all_constructors := ADJOIN(con, $all_constructors)
    db_data := rest(db_data)
    dbstruct.$sourcefile_ind := POP(db_data)
    dbstruct.$constructorform_ind := POP(db_data)
    dbstruct.$documentation_ind := POP(db_data)
    dbstruct.$predicates_ind := POP(db_data)
    dbstruct.$parents_ind := POP(db_data)

browse_open2(stream) ==
    constructors := READ(stream)
    for db_data in constructors repeat
        init_dbstruct2(db_data)

open_browse_db(display_messages) ==
    res := handle_daase_file('"browse", function browse_open2,
                             display_messages, $browse_stream,
                             $browse_stream_stamp)
    $browse_stream := first(res)
    $browse_stream_stamp := first(rest(res))

category_open2(stream) ==
    $has_category_hash := MAKE_HASHTABLE('EQUAL)
    keys := READ(stream)
    for key_val in keys repeat
        HPUT($has_category_hash, first(key_val), first(rest(key_val)))

open_category_db(display_messages) ==
    res := handle_daase_file('"category", function category_open2,
                             display_messages, $category_stream,
                             $category_stream_stamp)
    $category_stream := first(res)
    $category_stream_stamp := first(rest(res))

operation_open2(stream) ==
    $all_operations := []
    $operation_hash := MAKE_HASHTABLE('EQL)
    keys := READ(stream)
    for key_val in keys repeat
        HPUT($operation_hash, first(key_val), rest(key_val))

open_operation_db(display_messages) ==
    res := handle_daase_file('"operation", function operation_open2,
                             display_messages, $operation_stream,
                             $operation_stream_stamp)
    $operation_stream := first(res)
    $operation_stream_stamp := first(rest(res))


SHOWDATABASE(con) ==
    f_descs := [['n, 'CONSTRUCTORKIND], ['n, 'COSIG], _
                ['n, 'OPERATION], ['p, 'CONSTRUCTORMODEMAP], _
                ['p, 'CONSTRUCTORCATEGORY], ['p, 'OPERATIONALIST], _
                ['p, 'MODEMAPS], ['n, 'HASCATEGORY], _
                ['n, 'OBJECT], ['n, 'NILADIC], _
                ['n, 'ABBREVIATION], ['n, 'CONSTRUCTOR], _
                ['n, 'DEFAULTDOMAIN], ['n, 'ANCESTORS], _
                ['n, 'SOURCEFILE], ['n, 'CONSTRUCTORFORM], _
                ['n, 'CONSTRUCTORARGS], ['p, 'PREDICATES], _
                ['n, 'DOCUMENTATION], ['n, 'PARENTS]]
    for f_desc in f_descs repeat
        ft := first(f_desc)
        fn := first(rest(f_desc))
        ft = 'n =>
            FORMAT(true, '"~&~a: ~a~%", fn, get_database(con, fn))
        ft = 'p =>
            FORMAT(true, '"~&~a: ", fn)
            PPRINT(get_database(con, fn))
        BREAK()

SETDATABASE(con, key, val) ==
    if SYMBOLP(con) then
        dbstruct := GET(con, 'DATABASE)
        if not(dbstruct) then
            dbstruct := make_dbstruct()
            MAKEPROP(con, 'DATABASE, dbstruct)
        key = 'ABBREVIATION =>
            dbstruct.$abbreviation_ind := val
            if SYMBOLP(val) then
                MAKEPROP(val, 'ABBREVIATIONFOR, con)
        key = 'NILADIC =>
            dbstruct.$niladic_ind := val
        key = 'COSIG =>
            dbstruct.$cosig_ind := val
        key = 'CONSTRUCTORMODEMAP =>
            dbstruct.$constructormodemap_ind := val
        key = 'CONSTRUCTORCATEGORY =>
            dbstruct.$constructorcategory_ind := val
        key = 'CONSTRUCTORKIND =>
            dbstruct.$constructorkind_ind := val

DELDATABASE(con, key) ==
    if SYMBOLP(con) then
        if key = 'ABBREVIATION then
            MAKEPROP(con, 'ABBREVIATIONFOR, nil)

-- Print info about get_database calls
$miss := false

get_data_from_file(con, key, data, stream) ==
    if $miss then
        FORMAT(true, '"get_database miss: ~20a ~a~%", con, key)
    FILE_-POSITION(stream, data)
    READ(stream)

get_database2(con, key, ind, stream) ==
    dbstruct := GET(con, 'DATABASE)
    NULL(dbstruct) => nil
    data := dbstruct.ind
    if NULL(data) and key = 'CONSTRUCTORCATEGORY then
        data := first(rest(first(
                    get_database(con, 'CONSTRUCTORMODEMAP))))
    if NUMBERP(data) then
        data := get_data_from_file(con, key, data, stream)
        dbstruct.ind := data
    NULL(data) => nil
    if key = 'SOURCEFILE then
        if NULL(PATHNAME_-DIRECTORY(data)) and
           PATHNAME_-TYPE(data) = '"spad" then
            data := STRCONC($spadroot, '"/../../src/algebra/", data)
    if key = 'OBJECT then
        if CONSP(data) then
            data := first(data)
        if NULL(PATHNAME_-DIRECTORY(data)) then
            data := STRCONC($spadroot, '"/algebra/", data,
                           '".", $lisp_bin_filetype)
    data

get_database3(con, key, hash, stream) ==
    data := HGET(hash, con)
    NULL(data) => nil
    if NUMBERP(data) then
        data := get_data_from_file(con, key, data, stream)
        HPUT(hash, con, data)
    data

get_database(con, key) ==
    if $miss then
        FORMAT(true, '"get_database call: ~20a ~a~%", con, key)
    key = 'HASCATEGORY and PAIRP(con) =>
        get_database3(con, key, $has_category_hash, $category_stream)
    key = 'HASCATEGORY => nil
    not(SYMBOLP(con)) => nil
    key = 'ABBREVIATION =>
        get_database2(con, key, $abbreviation_ind, $interp_stream)
    key = 'CONSTRUCTORKIND =>
        get_database2(con, key, $constructorkind_ind, $interp_stream)
    key = 'COSIG =>
        get_database2(con, key, $cosig_ind, $interp_stream)
    key = 'OPERATION =>
        get_database3(con, key, $operation_hash, $operation_stream)
    key = 'CONSTRUCTORMODEMAP =>
        get_database2(con, key, $constructormodemap_ind, $interp_stream)
    key = 'CONSTRUCTORCATEGORY =>
        get_database2(con, key, $constructorcategory_ind, $interp_stream)
    key = 'OPERATIONALIST =>
        get_database2(con, key, $operationalist_ind, $interp_stream)
    key = 'MODEMAPS =>
        get_database2(con, key, $modemaps_ind, $interp_stream)
    key = 'OBJECT =>
        get_database2(con, key, $object_ind, $interp_stream)
    key = 'ASHARP? =>
        dbstruct := GET(con, 'DATABASE)
        NULL(dbstruct) => nil
        data := dbstruct.$object_ind
        if NUMBERP(data) then
            data := get_data_from_file(con, key, data, $interp_stream)
        CONSP(data) => rest(data)
        nil
    key = 'NILADIC =>
        get_database2(con, key, $niladic_ind, $interp_stream)
    key = 'SUPERDOMAIN =>
        con = "NonNegativeInteger" =>
            [["Integer"],
              ["IF", ["<", "#1", 0], "false", "true"]]
        con = "PositiveInteger" => [["NonNegativeInteger"], ["<", 0, "#1"]]
        nil
    key = 'CONSTRUCTOR =>
        GET(con, 'ABBREVIATIONFOR)
    key = 'DEFAULTDOMAIN =>
        first(rest(ASSOC(con, $defaultdomain_list)))
    key = 'ANCESTORS =>
        get_database2(con, key, $ancestors_ind, $interp_stream)
    key = 'SOURCEFILE =>
        get_database2(con, key, $sourcefile_ind, $browse_stream)
    key = 'CONSTRUCTORFORM =>
        get_database2(con, key, $constructorform_ind, $browse_stream)
    key = 'CONSTRUCTORARGS =>
        rest(get_database(con, 'CONSTRUCTORFORM))
    key = 'PREDICATES =>
        get_database2(con, key, $predicates_ind, $browse_stream)
    key = 'DOCUMENTATION =>
        get_database2(con, key, $documentation_ind, $browse_stream)
    key = 'PARENTS =>
        get_database2(con, key, $parents_ind, $browse_stream)
    key = 'USERS =>
        get_database2(con, key, $users_ind, $browse_stream)
    key = 'DEPENDENTS =>
        get_database2(con, key, $dependents_ind, $browse_stream)
    WARN('"~%(get_database ~a ~a) failed~%", con, key)
    nil

compute_cosig(con_modemap) ==
    [NIL, :MAPCAR(FUNCTION categoryForm?, rest(rest(first(con_modemap))))]

set_dbstruct(dbstruct, fetch_data, ds, strip_path?, constructorform,
             abbrev, object) ==
    dbstruct.$constructorform_ind := constructorform
    dbstruct.$abbreviation_ind := abbrev
    dbstruct.$operationalist_ind :=
        FUNCALL(fetch_data, ds, '"operationAlist")
    dbstruct.$constructormodemap_ind :=
        FUNCALL(fetch_data, ds, '"constructorModemap")
    dbstruct.$modemaps_ind :=
        FUNCALL(fetch_data, ds, "modemaps")
    source_file := FUNCALL(fetch_data, ds, "sourceFile")
    if strip_path? then
        source_file := FILE_-NAMESTRING(source_file)
    dbstruct.$sourcefile_ind := source_file
    kind := FUNCALL(fetch_data, ds, '"constructorKind")
    dbstruct.$constructorkind_ind := kind
    dbstruct.$constructorcategory_ind :=
        FUNCALL(fetch_data, ds, '"constructorCategory")
    dbstruct.$documentation_ind :=
        FUNCALL(fetch_data, ds, '"documentation")
    dbstruct.$predicates_ind :=
        FUNCALL(fetch_data, ds, '"predicates")
    dbstruct.$niladic_ind :=
        FUNCALL(fetch_data, ds, '"NILADIC")
    dbstruct.$cosig_ind :=
        compute_cosig(dbstruct.$constructormodemap_ind)
    dbstruct.$object_ind := object
    kind

make_special_constructor1(con, niladic, kind) ==
    dbstruct := make_dbstruct()
    MAKEPROP(con, 'DATABASE, dbstruct)
    dbstruct.$operationalist_ind := []
    dbstruct.$niladic_ind := niladic
    dbstruct.$constructorkind_ind := kind
    PUSH(con, $all_constructors)

make_special_constructors() ==
    make_special_constructor1('Category, true, 'category)
    make_special_constructor1('Record, false, 'domain)
    make_special_constructor1('Union, false, 'domain)
    make_special_constructor1('Mapping, false, 'domain)
    make_special_constructor1('Enumeration, false, 'domain)

finish_con_dbstruct(con) ==
    dbstruct := GET(con, 'DATABASE)
    NULL(dbstruct) => nil
    dbstruct.$cosig_ind := compute_cosig(dbstruct.$constructormodemap_ind)

print_db_item(stream, item) ==
    res := FILE_-POSITION(stream)
    PRINT(item, stream)
    res

store_con_data1(con, stream) ==
    dbstruct := GET(con, 'DATABASE)
    opalistpos := print_db_item(stream, dbstruct.$operationalist_ind)
    cmodemappos := print_db_item(stream, dbstruct.$constructormodemap_ind)
    modemapspos := print_db_item(stream, dbstruct.$modemaps_ind)
    dob := dbstruct.$object_ind
    obj :=
        CONSP(dob) => -- asharp code
            [PATHNAME_-NAME(first(dob)), :rest(dob)]
        not(NULL(dob)) =>
            PATHNAME_-NAME(last(PATHNAME_-DIRECTORY(dob)))
        '"NIL"
    concategory := dbstruct.$constructorcategory_ind
    if concategory then
        categorypos := print_db_item(stream, concategory)
    else
        categorypos := nil
    niladic := dbstruct.$niladic_ind
    abbrev := dbstruct.$abbreviation_ind
    cosig := dbstruct.$cosig_ind
    kind := dbstruct.$constructorkind_ind
    ancestors := HGET($ancestors_hash, con)
    if ancestors then
        ancestorspos := print_db_item(stream, ancestors)
    else
        ancestorspos := nil
    [con, opalistpos, cmodemappos, modemapspos, obj, categorypos,
      niladic, abbrev, cosig, kind, ancestorspos]

store_interp_data(stream) ==
    master := []
    for con in allConstructors() repeat
        master := [store_con_data1(con, stream), :master]
    print_db_item(stream, master)

write_interpdb() ==
    write_daase_file('"interp", function store_interp_data)

store_con_data2(con, stream) ==
    dbstruct := GET(con, 'DATABASE)
    src := dbstruct.$sourcefile_ind
    formpos := print_db_item(stream, dbstruct.$constructorform_ind)
    docpos := print_db_item(stream, dbstruct.$documentation_ind)
    predpos := print_db_item(stream, dbstruct.$predicates_ind)
    [con, src, formpos, docpos, predpos]

store_browse_data(stream) ==
    master := []
    for con in allConstructors() repeat
        master := [store_con_data2(con, stream), :master]
    print_db_item(stream, master)

write_browsedb() ==
    write_daase_file('"browse", function store_browse_data)


store_category_data(stream) ==
    genCategoryTable()
    $store_category_data_mater := []
    for key_val in nreverse(H_KEY_VALS($has_category_hash)) repeat
        key := first(key_val)
        val := rest(key_val)
        posv :=
            NULL(val) or val = true => val
            print_db_item(stream, val)
        master := [[key, posv], :master]
    print_db_item(stream, master)

write_categorydb() ==
    write_daase_file('"category", function store_category_data)


store_operation_data(stream) ==
    for key_val in nreverse(H_KEY_VALS($operation_hash)) repeat
        key := first(key_val)
        val := rest(key_val)
        pos := print_db_item(stream, val)
        master := [[key, :pos], :master]
    print_db_item(stream, master)

write_operationdb() ==
    write_daase_file('"operation", function store_operation_data)


true_name(p) == NAMESTRING(TRUENAME(p))

check_for_ext(fn, ext) ==
    fricas_probe_file(STRCONC(fn, ext))

merge_info_from_objects(files, options, make_database?) ==
    dir := nil
    only := []
    expose := true
    noquiet := true

    for opt in options repeat
        op1 := first(opt)
        op1 = 'only =>
           only := rest opt
        op1 = 'dir =>
            dir := first(rest(opt))
            if NULL(dir) then
                sayKeyedMsg('S2IU0002, nil)
        op1 = 'noexpose =>
            expose := false
        op1 = 'quiet =>
            noquiet := false
        FORMAT(true, '"   Ignoring unknown )library option: ~a~%", opt)

    -- IXME: make this _really_ portable
    thisdir := true_name('"./")
    if make_database? then
        expose := false

    nrlibs := []
    asys := []
    asos := []
    if dir then
        CHDIR(STRING(dir))
        nrlibs := DIRECTORY('"*.NRLIB/index.KAF")
        asys := DIRECTORY('"*.asy")
        skipasos := MAPCAN(function PATHNAME_-NAME, asys)
        -- asos := DIRECTORY('"*.ao)
        CHDIR(thisdir)

    nr_ext := STRCONC('".NRLIB/", $index_filename)
    for file in files repeat
        fname := STRING(file)
        (nf := check_for_ext(fname, nr_ext)) =>
            nrlibs := [nf, :nrlibs]
        (nf := check_for_ext(fname, '".asy")) =>
            asys := [nf, :asys]
        (nf := check_for_ext(fname, '".ao")) =>
            asos := [nf, :asos]
        FORMAT(true, '"   )library cannot find the file ~a.~%", fname)

    for fname in nreverse(nrlibs) repeat
        key := PATHNAME_-NAME(last(PATHNAME_-DIRECTORY(fname)))
        object := STRCONC(DIRECTORY_-NAMESTRING(fname), key)
        merge_info_from_nrlib(key, fname, object, make_database?, expose,
                              noquiet)
    for fname in nreverse(asys) repeat
        object := STRCONC(DIRECTORY_-NAMESTRING(fname), PATHNAME_-NAME(fname))
        merge_info_from_asy(astran(fname), object, only, make_database?,
                            expose, noquiet)
    for fname in nreverse(asos) repeat
        object := STRCONC(DIRECTORY_-NAMESTRING(fname), PATHNAME_-NAME(fname))
        ASHARP(fname)
        file := astran(STRCONC(PATHNAME_-NAME(fname), ".asy"))
        merge_info_from_asy(file, object, only, make-database?, expose,
                            noquiet)
    clearConstructorCaches()

make_databases(dir_lst, br_data) ==
    clean_symbols2()
    $has_category_hash := MAKE_HASHTABLE('EQUAL)
    $operation_hash := MAKE_HASHTABLE('EQL)
    $all_constructors := []
    $all_operations := []
    make_special_constructors()
    merge_info_from_objects([], [['dir, true_name('"./")]], true)
    for dir in dir_lst repeat
        merge_info_from_objects([], [['dir, true_name(
                                      STRCONC('"./~a", dir))]], true)
    if br_data then
        save_browser_data()
        write_browsedb()
    write_operationdb()
    write_categorydb()
    for con in allConstructors() repeat
        finish_con_dbstruct(con)
    write_interpdb()
    createInitializers()
    move_database('"interp", true)
    move_database('"operation", true)
    move_database('"browse", br_data)
    move_database('"category", true)

add_operations(con, old_maps) ==
    for map in old_maps repeat
        op := first(map)
        old_map := get_database(op, 'OPERATION)
        old_map := NREMOVE(old_map, rest(map))
        HPUT($operation_hash, op, old_map)
    for map in get_database(con, 'MODEMAPS) repeat
        op := first(map)
        old_map := get_database(op, 'OPERATION)
        HPUT($operation_hash, op, [rest(map), :old_map])

merge_info_from_asy(asy, object, only, make_database?, expose,
                    noquiet) ==
    SET_-FILE_-GETTER(object)
    for domain in asy repeat
        key := first(domain)
        alist := rest(domain)
        asharp_name := asharp_global_name(PATHNAME_-NAME(object), key,
                                          LASSOC('typeCode, alist))
        #alist < 4 =>
            -- handle toplevel function object
            opname := key
            modemap := CAR(LASSOC('modemaps, alist))
            oldmaps := get_database(opname, 'OPERATION)
            HPUT($operation_hash, opname, adjoin_equal(
                SUBST(asharp_name, opname, CDR(modemap)), oldmaps))
            set_asharp_autoload_function(object, asharp_name)
        if (null(only) => key ~= '%%; MEMBER(key, only)) then
            $all_operations := []       -- force this to recompute
            oldmaps := get_database(key, 'modemaps)
            dbstruct := make_dbstruct()
            PUT(key, 'DATABASE, dbstruct)
            $all_constructors := ADJOIN(key, $all_constructors)
            abbrev := fetch_data_from_alist(alist, '"abbreviation") or key
            kind := set_dbstruct(dbstruct, FUNCTION(fetch_data_from_alist),
                      alist, false,
                        fetch_data_from_alist(alist, '"constructorForm"),
                          abbrev, CONS(object, asharp_name))
            PUT(abbrev, 'ABBREVIATIONFOR, key)
            add_operations(key, oldmaps)
            cname := opOf(dbstruct.$constructorform_ind)
            if expose then setExposeAddConstr2([cname], noquiet)
            if not(make_database?) then
                installConstructor(cname)
                updateDatabase(cname)
                -- following can break category database build
                if kind = 'category then
                    dbstruct.$ancestors_ind :=
                         fetch_data_from_alist(alist, '"ancestors")
                if kind = 'domain then
                    for pair in fetch_data_from_alist(alist, '"ancestors")
                      repeat
                        HPUT($has_category_hash, CONS(cname, CAAR(pair)),
                             CDR(pair))
                if $InteractiveMode then $CategoryFrame := $EmptyEnvironment
            if kind = 'category then
                set_asharp_autoload_category(object, cname, asharp_name,
                                             dbstruct.$cosig_ind)
            else
                set_asharp_autoload_functor(object, cname, asharp_name,
                                            dbstruct.$cosig_ind)
            if noquiet then
                sayKeyedMsg('S2IU0001i, [cname, object])

merge_info_from_nrlib1(in_f, key, object, make_database?, expose,
                       noquiet) ==
    FILE_-POSITION(in_f, READ(in_f))
    alist := READ(in_f)
    -- (setq pos (third (assoc "constructorForm" alist :test #'string=)))
    pos := first(rest(fetch_data_from_alist(alist, '"constructorForm")))
    FILE_-POSITION(in_f, pos)
    constructorform := READ(in_f)
    key := first(constructorform)
    oldmaps := get_database(key, 'MODEMAPS)
    dbstruct := make_dbstruct()
    PUT(key, 'DATABASE, dbstruct)
    $all_constructors := ADJOIN(key, $all_constructors)
    abbrev := INTERN(PATHNAME_-NAME(last(PATHNAME_-DIRECTORY(object))))
    ds := [alist, in_f]
    kind :=
        set_dbstruct(dbstruct, FUNCTION(fetch_data_from_file), ds,
                     make_database?, constructorform, abbrev, object)
    $all_operations := []  -- force this to recompute
    PUT(abbrev, 'ABBREVIATIONFOR, key)
    add_operations(key, oldmaps)
    if not(make_database?) then
        installConstructor(key)
        if kind = 'category then
            dbstruct.$ancestors_ind :=
                 SUBLISLIS($FormalMapVariableList, rest(constructorform),
                           fetch_data_from_file(ds, '"ancestors"))
        updateDatabase(key)
        updateCategoryTable(key, kind)
        if $InteractiveMode  then $CategoryFrame := $EmptyEnvironment
    REMPROP(key, 'LOADED)
    if expose then setExposeAddConstr2([key], noquiet)
    spad_set_autoload(key)
    if noquiet then
        sayKeyedMsg('S2IU0001, [key, object])

merge_info_from_nrlib(key, nrlib, object, make_database?, expose,
                      noquiet) ==
    handle_input_file(nrlib, FUNCTION(merge_info_from_nrlib1),
                      [key, object, make_database?, expose, noquiet])
