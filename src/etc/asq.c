#if 0
\documentclass{article}
\usepackage{axiom}
\begin{document}
\title{\$SPAD/etc asq.c}
\author{Timothy Daly and Waldek Hebisch}
\maketitle
\begin{abstract}
[[asq]] is a mini-browser for Axiom databases.  It understands
structure of databases and can retrieve information about
constructors.  To do this it implements (limited) Lisp-like
S-expressions.
\end{abstract}
\eject
\tableofcontents
\eject
\section{S-expressions}

Data in Axiom databases is stored as S-expressions.  One could
try to handle them with purely string based methods.  But such
approach would be inflexible and awkward. Namely, Axiom perform
various transformations before writing S-expressions to files.
Such transformations are natural and easy on S-expressions,
but complicated (require parsing and multiple string substitutions)
on string level.  So [[asq]]
needs S-expression support.  Axiom uses only very special forms
of S-expression: pairs, lists, strings, symbols and integers.
We implement lists, strings, symbols and small integers.  Actually,
since in C it is easier to work with arrays than with genuine lists,
we implement adjustable vectors and treat them as lists (so our
terminology is closer to Perl than to Lisp).  We cheat reading
pairs: we read them as a three element list, the middle element
being the symbol ``.'' (dot).


\section{Database format}

Below we try to explain essential properties of Axiom databases,
for longer description (including many low level details) look
at [[daase.lisp.pamphlet]].
General format of Axiom databases is as follows
\begin{verbatim}
stamp
indirect hunks
main data list
\end{verbatim}
[[stamp]] above is a Lisp pair, its first element is an integer
giving byte offset to [[main data list]].  Elements of main
data list are themselves lists, which for given database have
fixed number (and order) of fields.  For [[interp.daase]] the
fields are:
\begin{verbatim}
constructor name (symbol)
operation list 
constructor modemap
modemaps (of added operations)
object file (name of the file containing compiled code
                of the constructor)
constructorcategory
niladic (boolean field telling if the constructor takes arguments)
abbreviation
cosig
constructorkind
defaultdomain
ancestors
\end{verbatim}
For [[browse.daase]] the fields are:
\begin{verbatim}
constructor name
sourcefile
constructorform
documentation
attributes
predicates
\end{verbatim}
Only simple fields are stored directly, most values are stored in indirect
way: given value is replace by an integer giving position (byte offset
with respect to the start of the file) of actual value.  Additionally,
fields may be compressed.  Compression uses extra file ([[compress.daase]])
which contains a single list of values.  Values from other files may
be put in the [[compress.daase]] and replaced by minus their index
on the [[compress.daase]] list.

For example, expanded form of S-expression describing Axiom [[Type]]
(one of the simplest categories) is:
\begin{verbatim}
 (|Type| NIL (((|Type|) (|Category|)) (T |Type|)) NIL "TYPE"
    (|Join| (CATEGORY |package| (ATTRIBUTE |nil|)))
    T TYPE (NIL) |category| NIL NIL)
\end{verbatim}
After replacing composite values by file offsets we get something
like: 
\begin{verbatim}
 (|Type| 3013278 3013283 3013314 "TYPE" 3013319 T TYPE (NIL)
    |category| NIL NIL)
\end{verbatim}
Compression turns this into the following (assuming that position
9 in [[compress.daase]] contains symbol [[|category|]] and position
1106 in [[compress.daase]] contains symbol [[|Type|]]):
\begin{verbatim}
 (-1106 3013278 3013283 3013314 "TYPE" 3013319 T TYPE (NIL) -9 NIL NIL)
\end{verbatim}
Remark: When compression is active indirect values are also compressed,
so the list at offset 3013283:
\begin{verbatim}
(((|Type|) (|Category|)) (T |Type|))
\end{verbatim}
is stored as (assuming that position 1182 in [[compress.daase]] contains
symbol [[|Category|]]):
\begin{verbatim}
(((-1106) (-1182)) (T -1106))
\end{verbatim}
#endif

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

void
fatal(const char * message, ...)
{
    va_list ap;
    va_start(ap, message);
    vfprintf(stderr, message, ap);
    va_end(ap);
    exit(1);
}

void *
my_malloc(size_t n)
{
    void * p = malloc(n);
    if (!n) {
        fatal("Out of memory\n");
    }
    return p;
}

/* Dynamic data structures, Perl/Lisp like. */

typedef struct item {
    int tag;
} item;

#define TAG_int 1
#define TAG_string 2
#define TAG_symbol 3
#define TAG_list 4

int
get_tag(const item * it)
{
    return *((int *)it);
}

void
assert_tag(const void * it, int tag)
{
    if (get_tag(it) != tag) {
        fatal("Unexpected tag %d (expected %d)\n", get_tag(it), tag);
    }
}

typedef struct int_item {
    int tag;
    long val;
} int_item;

/* 
   Counted strings with fill pointer.  Not necessarily null terminated.
*/

typedef struct counted_string {
   int tag;
   char * buff;
   int size;
   int pos;
} counted_string;

void
add_char(counted_string * s, char c)
{
    if (s->pos >= s->size) {
        if (s->size < INT_MAX/2) {
            s->size *= 2;
            s->buff = realloc(s->buff, s->size);
            if (!s->buff) {
                fatal("Out of memory\n");
            }
        } else {
            fatal("Item too long\n");
        }
    }
    s->buff[s->pos] = c;
    s->pos++;
}

#define COUNTED_STRING_INITIAL_SIZE 80
counted_string *
empty_string(void)
{
    counted_string * s = my_malloc(sizeof(*s));
    s->buff = my_malloc(COUNTED_STRING_INITIAL_SIZE);
    s->tag = TAG_string;
    s->buff[0] = 0;
    s->size = COUNTED_STRING_INITIAL_SIZE;
    s->pos = 0;
    return s;
}

void
free_counted_string(counted_string * s)
{
    free(s->buff);
    free(s);
}


/*
   Item lists.
*/

typedef struct item_list {
    int tag;
    item * * buff;
    int size;
    int pos;
} item_list;

void
add_item(item_list * sl, item * s)
{
    if (sl->pos >= sl->size) {
        if (sl->size < INT_MAX/(2*sizeof(item *))) {
            sl->size *= 2;
            sl->buff = realloc(sl->buff, 
                         sizeof(item *)*sl->size);
            if (!sl->buff) {
                fatal("Out of memory\n");
            }
        } else {
            fatal("List too long\n");
        }
    }
    sl->buff[sl->pos] = s;
    sl->pos++;
}

/* 12 allows reading interp.daase entries without reallocation */
#define ITEM_LIST_INITIAL_SIZE 12

item_list *
empty_list(void)
{
    item_list * sl = my_malloc(sizeof(*sl));
    sl->buff = my_malloc(ITEM_LIST_INITIAL_SIZE*sizeof(item *));
    sl->buff[0] = 0;
    sl->tag = TAG_list;
    sl->size = ITEM_LIST_INITIAL_SIZE;
    sl->pos = 0;
    return sl;
}

void
free_item_list(item_list * sl);

void
free_item(item * it)
{
    switch (get_tag(it)) {
       case TAG_int:
           free(it);
           break;
       case TAG_string:
       case TAG_symbol:
           free_counted_string((counted_string *) it);
           break;
       case TAG_list:
           free_item_list((item_list *) it);
           break;
       default:
           fatal("Unknown tag %d in free_item\n", get_tag(it));
           break;
    }
}

void
free_item_list(item_list * sl)
{
    int i;
    for (i = 0; i < sl->pos; i++) {
        free_item(sl->buff[i]);
    }
    free(sl->buff);
    free(sl);
}


char *
string_val(item * it)
{
    int tag = get_tag(it);
    if (tag != TAG_string && tag != TAG_symbol) {
        fatal("Accessing string value of something not a string or symbol");
    }
    return ((counted_string *)it)->buff;
}

/*
   Below we implement a simple pretty-printer.  We want nicely
   indented lists, but to conserve space we try to print
   multiple items on a single line.  More precisely, when printing
   list we try first to print the whole list in a single line (starting
   at given indentation level).  If list does not fit (is too large) we
   then switch to multiline mode, where we try to fit as many items as
   possible on a single line.  To support this we fist print to a line
   sized buffer, and if the result fits we print the buffered content.
   Items which can not be printed on single line are printed "directly".
   In direct mode printing routine has to print spaces to indent output,
   while when printing to the buffer printing routine prints just content,
   checking that it fits into the buffer.  To handle indentation in
   we just subtract indentation offset form the buffer size, passing
   lower limit to printing routines.  Later upper routine prints 
   indentating spaces followed by buffer content.

   Unfortunately, this approach means that there is significant duplication
   in printing code, since various modes and cases are quite similar,
   (but different enough to inhibit sharing).
*/

#define LINE_LENGTH 76
int
print_item1(item * it, char * buff, int lim)
{
    int tag = get_tag(it);
    int len;
    if (tag == TAG_int) {
        long val = ((int_item *)it)->val;
        if (buff) {
            len = snprintf(buff, lim, "%ld", val);
            if (len > 0 && len < lim) {
                return len;
            } else {
                return -1;
            }
        }
        printf("%ld", ((int_item *)it)->val);
        return -1;
    } else {
        char * str = string_val(it);
        char * s1 = "";
        int l1 = 0;
        if (tag == TAG_string) {
            l1 = 1;
            s1 = "\"";
        }
        if (buff) {
            int l2 = strlen(str);
            if (l2 + 2*l1 < lim) {
                sprintf(buff, "%s%s%s", s1, str, s1);
                return l2 + 2*l1;
            } else {
                return -1;
            }
        } else {
            printf("%s%s%s", s1, str, s1);
            return -1;
        }
    }
}

void
indent(int offset)
{
    int i;
    for(i = 0; i < offset; i++) {
        putchar(' ');
    }
}

int
print_items1(item * it, int offset, char * buff, int lim)
{
    int tag = get_tag(it);
    int i;
    if (buff) {
         if (tag == TAG_list) {
             item_list * il = (item_list *)it;
             int pos = 1;
             int i = 0;
             if (pos >= lim - 1) {
                 return -1;
             }
             buff[0] = '(';
             for (i = 0 ; i < il->pos; i++) {
                 int l1;
                 if (pos >= lim - 1) {
                     return -1;
                 }
                 l1 = print_items1(il->buff[i], offset+3, 
                                         buff+pos, lim - pos - 1);
                 if (l1 < 0) {
                     return -1;
                 }
                 pos += l1;
                 buff[pos] = ' ';
                 pos++;
             }
             if (pos >= lim) {
                 return -1;
             }
             buff[pos - 1] = ')';
             buff[pos] = 0;
             return pos;
        } else {
             return print_item1(it, buff, lim);
        }
    }
    indent(offset);
    if (tag == TAG_list) {
       char buff1[LINE_LENGTH];
       int pos = 0;
       int lim1 = LINE_LENGTH - offset - 2;
       item_list * il = (item_list *)it;
       buff1[0] = 0;
       for(i = 0; i < il->pos; i++) {
           int l1 = print_items1(il->buff[i], offset+3,
                                   buff1 + pos, lim1 - pos - 1);
           if (l1 < 0) {
               goto multiline;
           }
           pos += l1;
           buff1[pos] = ' ';
           pos++;
       }
       if (pos>0) {
           pos--;
           buff1[pos] = 0;
       }
       printf("(%s)", buff1);
       return -1;
   multiline:
       printf("(\n");
       pos = 0;
       lim1 = LINE_LENGTH - offset - 3;
       for(i = 0; i < il->pos; i++) {
           int l1 = print_items1(il->buff[i], offset + 3,
                                   buff1+pos, lim1 - pos - 1);
           if (l1 < 0) {
               if (pos > 0) {
                   indent(offset+3);
                   pos--;
                   buff1[pos] = 0;
                   printf("%s\n", buff1);
                   pos = 0;
                   l1 = print_items1(il->buff[i], offset + 3, buff1, lim1-1);
                   if (l1 < 0) {
                        print_items1(il->buff[i], offset + 3, 0, lim1);
                        putchar('\n');
                   } else {
                        pos = l1;
                        buff1[pos] = ' ';
                        pos++;
                   }
               } else {
                   print_items1(il->buff[i], offset+3, 0, lim1);
                   putchar('\n');
               }
           } else {
               pos += l1;
               buff1[pos] = ' ';
               pos++;
           }
       }
       if (pos > 0) {
           pos--;
           buff1[pos] = 0;
           indent(offset+3);
           printf("%s\n", buff1);
       }
       indent(offset);
       putchar(')');
       return -1;
    } else {
        return print_item1(it, 0, lim);
    }
}

void
print_item(item * it)
{
    print_items1(it, 0, 0, LINE_LENGTH);
}

void
print_cars(item * it)
{
    const item_list * il = (item_list *)it;
    int i;
    int tag = get_tag(it);
    if (tag == TAG_symbol) {
        counted_string * sy = (counted_string *) it;
        if (!strcmp(sy->buff, "NIL")) {
            printf("NIL");
            return;
        }
    }
    assert_tag(il, TAG_list);
    for (i = 0; i< il->pos; i++) {
        item_list * el = (item_list *)(il->buff[i]);
        assert_tag(el, TAG_list);
        if (el->pos < 1) {
           fatal("print_cars trying to print car of empty list\n");
        }
        print_items1(el->buff[0], 3, 0, LINE_LENGTH);
        putchar('\n');
    }
}

/* 
   Utilities to read S-expression from files.  Support only things
   which appear in Axiom databases.
*/

/* Precondition: File is positioned before item.
   Postcondition: File is positioned just after the item.
*/

counted_string *
read_string(FILE * file)
{
    int c;
    counted_string * s = empty_string();
    while(isspace(c = fgetc(file)))
        ;
    if (c != '"') {
        fatal("String must begin with '\"'\n");
    }
    while (1) {
        c = fgetc(file);
        if (c == EOF) {
            fatal("Unexpected EOF\n");
        }
        if (c == '"') {
            add_char(s, 0);
            return s;
        } else if (c == '\\') {
            c = fgetc(file);
        }
        add_char(s, c);
    }
}

counted_string *
read_symbol(FILE * file)
{
    int c;
    enum {normal, in_ticks} state = normal;
    counted_string * s = empty_string();
    s->tag = TAG_symbol;
    while(isspace(c = fgetc(file)))
        ;
    ungetc(c, file);
    while (1) {
        c = fgetc(file);
        if (c == EOF) {
            fatal("Unexpected EOF\n");
        }
        if (state == normal) {
            if (c == ')' || c == '(' || isspace(c)) {
                ungetc(c, file);
                add_char(s, 0);
                return s;
            } else if (c == '|') {
                state = in_ticks;
            } else {
                add_char(s, c);
            }
        } else if (state == in_ticks) {
            if (c == '|') {
                state = normal;
            } else if (c == '\\') {
                c = fgetc(file);
                add_char(s, c);
            } else {
                add_char(s, c);
            }
        }
    }
}

item *
read_item(FILE * file)
{
    int c;
    while(isspace(c = fgetc(file)))
        ;
    if (c == ')') {
        fatal("Unexpected ')'\n");
    }
    if (c == '(') {
        item_list * res = empty_list();
        while (1) {
            while(isspace(c = fgetc(file)))
                ;
            if (c == ')') {
                return (item *) res;
            }
            ungetc(c, file);
            add_item(res, read_item(file));
        }
    }
    if (c == '\'') {
        item_list * res = empty_list();
        counted_string * s = empty_string();
        s->tag = TAG_symbol;
        char * ss = "QUOTE";
        while (*ss) {
            add_char(s, *ss);
            ss++;
        }
        add_item(res, (item *)s);
        add_item(res, read_item(file));
        return (item *) res;
    }
    if (c == '"') {
        counted_string * res = (ungetc(c, file), read_string(file));
        return (item *) res;
    }
    if (c == '-' || isdigit(c)) {
        int_item * res = my_malloc(sizeof(*res));
        char nbuff[25];
        int i = 0;
        nbuff[0] = c;
        i++;
        c = getc(file);
        while(isdigit(c)) {
            if (i + 1 >= sizeof(nbuff)) {
                fatal("Integer value too large\n");
            }
            nbuff[i] = c;
            i++;
            c = getc(file);
       }
       nbuff[i] = 0;
       ungetc(c, file);
       res->val = atol(nbuff);
       res->tag = TAG_int;
       return (item *) res;
   }
   {
       counted_string * res = (ungetc(c, file), read_symbol(file));
       return (item *) res;
   }
}

/* Main program */

char * AXIOM;
FILE *
open_file(const char * name)
{
    FILE * file;
    long offset;
    item_list * stamp;
    char * file_path;
    if (AXIOM != NULL) {
        file_path = my_malloc(strlen(AXIOM)+strlen("%s/algebra/%s.daase")
                              +strlen(name));
        sprintf(file_path, "%s/algebra/%s.daase", AXIOM, name);
    } else {
        file_path = my_malloc(strlen("%s.daase")+strlen(name));
        sprintf(file_path, "%s.daase", name);
    }
    file = fopen(file_path, "rb");
    if (!file) {
        fatal("unable to find the file %s\n", file_path);
    }
    /* Read main offset */
    stamp = (item_list *)read_item(file);
    assert_tag(stamp, TAG_list);
    if (stamp->pos != 3) {
       fatal("Bad stamp, have %d entries (should have 3)\n", stamp->pos);
    }
    assert_tag(stamp->buff[0], TAG_int);
    offset = ((int_item *)stamp->buff[0])->val;
    if (offset <= 0) {
        fatal("Bad main offset %ld\n", offset);
    }
    free_item((item *)stamp);
    fseek(file, offset, SEEK_SET);
    return file;
}

item *
decode(item * s, item_list * ct)
{
    int tag = get_tag(s);
    if (tag == TAG_int) {
        long n = ((int_item *)s)->val;
        if (n > 0 || 1 - n >= ct->pos) {
            return s;
        } else {
            return ct->buff[1 - n];
        }
    }
    if (tag == TAG_list) {
        item_list * res = empty_list();
        item_list * il = (item_list *)s;
        int i;
        for (i = 0; i < il->pos; i++) {
            add_item(res, decode(il->buff[i], ct));
        }
        return (item *) res;
    }
    return s;
}

item_list *
scan_file(FILE * file, char * fname, char * key,
            item_list * ct, int alt_pos)
{
    int c;
    while(isspace(c = fgetc(file)))
         ;
    if (c != '(') {
        fatal("Main %s.daase list begins with %c\n", fname, c);
    }
    while (1) {
        while(isspace(c = fgetc(file)))
            ;
        if (c == '(') {
            item_list * iml;
            item * it;
            ungetc(c, file);
            it = read_item(file);
            assert_tag(it, TAG_list);
            iml = (item_list *)it;
#if 0
            /* Debugging printout */
            {
                int i;
                for (i = 0; i < iml->pos; i++) {
                      /* printf("%s\n", decode (iml->buff[i], ct)); */
                      printf("%s\n", iml->buff[i]->buff);
                }
                putchar('\n');
             }
#endif
             if (!strcmp(key,
                    string_val(decode(iml->buff[0], ct)))) {
                 return iml;
             } else if (alt_pos && !strcmp(key,
                           string_val(decode(iml->buff[alt_pos], ct)))) {
                 return iml;
             }
             free_item(it);
        } else if (c != ')') {
             fatal("Unexpected char %c in interp.daase\n", c);
        } else {
             return 0;
        }
    }
}

struct info_slot {char * name; char *abbr; int slot; void (*handler)(item *);};
#define BROWSE_SHIFT 100
struct info_slot ot[] = {
{"constructor", "con", 0, 0},
{"sourcefile", "so", 101, 0},
{"constructorkind", "ckind", 9, 0},
{"niladic", "nilad", 6, 0},
{"abbreviation", "abbr", 7, 0},
{"defaultdomain", "ddom", 10, 0},
{"ancestors", "anc", 11, 0},
{"operations", "op", 1, &print_cars},
{"cosig", "cos", 8, 0},
{"constructorform", "cform", 102, 0},
{"constructormodemap", "cmod", 2, 0},
{"modemaps", "mod", 3, 0},
{"constructorcategory", "ccat", 5, 0},
{"documentation", "doc", 103, 0},
{"predicates", "pre", 104, 0},
};

void
printinfo(item_list * iml, item_list * bml, const char * propname,
          item_list * ct, FILE * interp_file, FILE * browse_file)
{
    int i;
    int all = !strcmp(propname, "all");
    const int nopts = sizeof(ot)/sizeof(struct info_slot);
    for (i=0; i < nopts; i++) {
        int slot = ot[i].slot;
        item * slot_data;
        FILE * file;
        if (!all && strcmp(propname, ot[i].name)) {
            continue;
        }
        printf("%s:\n", ot[i].name);
        if (slot >= BROWSE_SHIFT) {
            slot_data = bml->buff[slot - BROWSE_SHIFT];
            file = browse_file;
        } else {
            slot_data = iml->buff[slot];
            file = interp_file;
        }
        slot_data = decode(slot_data, ct);
        if (get_tag(slot_data) == TAG_int) {
            fseek(file, ((int_item *)slot_data)->val, SEEK_SET);
            slot_data = decode(read_item (file), ct);
        }
        if (ot[i].handler) {
            (*(ot[i].handler))(slot_data);
        } else {
            print_item(slot_data);
        }
        putchar('\n');
    }
}

void
usage(char * prog)
{
    int i;
    const int nopts = sizeof(ot)/sizeof(struct info_slot);
    printf("Usage:\n"
           "  %s key\n", prog);
    printf("or\n"
           "  %s -property key\n", prog);
    printf("where key is the name (or abbreviation) and property\n"
           "is one of the following:\n"
           "  (al) all\n");
    for (i=0; i < nopts; i++) {
        printf("  (%s) %s\n", ot[i].abbr, ot[i].name);
    }
    printf("You can give full property name or use abbreviated form\n"
           "given in parenthesis above.\n");
    printf("The first form prints all properties.\n");
    exit(1);
}

char *
unabbreviate_property(char * propname)
{
    int i;
    int j = strlen(propname);
    const int nopts = sizeof(ot)/sizeof(struct info_slot);
    if (j < 2) {
        j = 2;
    }
    if (!strncmp(propname, "all", j)) {
        return "all";
    }
    for (i=0; i < nopts; i++) {
        char * s = ot[i].name;
        if (!strcmp(propname, ot[i].abbr)
            || !strcmp(propname, s)) {
            return s;
        }
    }
    return 0;
}
int
main(int argc, char * * argv)
{
    FILE * compress_file;
    FILE * interp_file;
    FILE * browse_file;
    item_list * ct;
    item_list * iml;
    item_list * bml;
    char * cname;
    char * propname = "all";
    long ct_n;
    AXIOM=(char *)getenv("AXIOM");
    
    if (AXIOM == 0)
        fprintf(stderr, "AXIOM shell variable has no value. "
                           "using current directory\n");
    if (argc < 2 || argc > 3) {
        usage(argv[0]);
    }
    if (argc == 3 && argv[1][0] == '-') {
        cname = argv[2];
        propname = unabbreviate_property(argv[1]+1);
        if (!propname) {
           usage(argv[0]);
        }
    } else {
        cname = argv[1];
    }
    compress_file = open_file("compress");
    ct = (item_list *)read_item(compress_file);
    assert_tag(ct, TAG_list);
    if (ct->pos < 1) {
        fatal("Bad compress_file, no count\n");
    }
    {
        item * ct_n_item = ct->buff[0];
        assert_tag(ct_n_item, TAG_int);
        ct_n = ((int_item *)ct->buff[0])->val;
        if (ct->pos - 1 != ct_n) {
            fatal("Bad compress_file, count %d, (stored %ld)\n",
                    ct->pos - 1, ct_n);
        }
    }
    interp_file = open_file("interp");
    iml = scan_file(interp_file, "interp", cname, ct, 7);
    if (!iml) {
        printf("Not found!\n");
        exit(1);
    }
    browse_file = open_file("browse");
    bml = scan_file(browse_file, "browse",
                      string_val(decode(iml->buff[0], ct)), ct, 0);
    if (!bml) {
        printf("Not found!\n");
        exit(1);
    }
    printinfo(iml, bml, propname, ct, interp_file, browse_file);
    return 0;
}
