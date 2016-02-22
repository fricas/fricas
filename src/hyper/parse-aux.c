/*
Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    - Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    - Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the
      distribution.

    - Neither the name of The Numerical ALgorithms Group Ltd. nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "fricas_c_macros.h"
#include "debug.h"

#include "parse.h"
#include "lex.h"
#include "hyper.h"

#include "all_hyper_proto.H1"

static void read_ht_file(HashTable * page_hash, HashTable * macro_hash,
                    HashTable * patch_hash, FILE * db_fp, char * db_file);

extern FILE *cfile;
extern int make_input_file;
extern int gverify_dates;

InputBox *rb_list;
InputBox *end_rb_list;

HashTable ht_gFileHashTable;

#define htfhSize 100

/* Hash functions for active link windows */

int
window_equal(Window *w1, Window *w2)
{
    return *w1 == *w2;
}

/* hash code for a window */

int
window_code(Window *w, int size)
{
    return (*w) % size;
}

char *
window_id(Window w)
{
    char *ret;
    char buff[32];
    int length;

    sprintf(buff, "%ld", w);
    length = strlen(buff);
    ret = (char *) halloc(length * sizeof(char) + 1, "windowid");
    strcpy(ret, buff);
    return (ret);
}

/*
 * This procedure reads the ht database. It makes repeated calls to
 * db_file_open, and while the returned pointer is not null, it continues to
 * read the presented data base files
 */
void
read_ht_db(HashTable *page_hash, HashTable *macro_hash, HashTable *patch_hash)
{
    FILE *db_fp;
    char db_file[2048];
    int i = 0;

    gDatabasePath = NULL;

    hash_init(
              page_hash,
              PageHashSize,
              (EqualFunction) string_equal,
              (HashcodeFunction) string_hash);
    hash_init(
              macro_hash,
              MacroHashSize,
              (EqualFunction) string_equal,
              (HashcodeFunction) string_hash);
    hash_init(
              patch_hash,
              PatchHashSize,
              (EqualFunction) string_equal,
              (HashcodeFunction) string_hash);

    /* Lets initialize the FileHashTable         */
    hash_init(
              &ht_gFileHashTable,
              htfhSize,
              (EqualFunction) string_equal,
              (HashcodeFunction) string_hash);

    while ((db_fp = db_file_open(db_file)) != NULL) {
        i++;
        read_ht_file(page_hash, macro_hash, patch_hash, db_fp, db_file);
        fclose(db_fp);
    }

    if (!i) {
        fprintf(stderr,
          "(HyperDoc) read_ht_db: No %s file found\n", db_file_name);
        exit(-1);
    }

    free_hash(&ht_gFileHashTable, (FreeFunction)free_string);
}

/*
 * This procedure reads a single HyperDoc database file. It is passed an already
 * initilaized file pointer. It reads the whole file, updating the
 * page hash, or the macro hash only when a previous entry with the same name
 * is not found
 */

static void
read_ht_file(HashTable *page_hash, HashTable *macro_hash,
             HashTable *patch_hash, FILE *db_fp, char *db_file)
{
    char filename[2048];
    char *fullname = filename;
    UnloadedPage *page;
    MacroStore *macro;
    PatchStore *patch;
    int pages = 0, c, mtime, ret_val;
    struct stat fstats;
    /*short time_ok = 1;*/
/*    fprintf(stderr,"parse_aux:read_ht_file: dp_file=%s\n",db_file);*/
    cfile = db_fp;
    init_scanner();
    ret_val = strlen(db_file) - 1;
    for (; ret_val >= 0; ret_val--)
        if (db_file[ret_val] == '/') {
            db_file[ret_val] = '\0';
            break;
        }
    c = getc(db_fp);
    do {
        if (c == '\t') {
            get_filename();
            fullname = alloc_string(token.id);
            if (fullname[0] != '/') {
                strcpy(filename, db_file);
                strcat(filename, "/");
                strcat(filename, fullname);
                free(fullname);
                fullname = alloc_string(filename);
            }

            /*
             * Until I get a filename that I have not seen before, just keep
             * reading
             */
            while (hash_find(&ht_gFileHashTable, fullname) != NULL) {
                do {
                    c = getc(db_fp);
                } while ((c != EOF) && (c != '\t'));
                if (c == EOF)
                    return;
                get_filename();
                fullname = alloc_string(token.id);
                if (fullname[0] != '/') {
                    strcpy(filename, db_file);
                    strcat(filename, "/");
                    strcat(filename, fullname);
                    free(fullname);
                    fullname = alloc_string(filename);
                }
            }
/*          fprintf(stderr,"parse_aux:read_ht_file: fullname=%s\n",fullname);*/
            /* If I got here, then I must have a good filename  */
            hash_insert(&ht_gFileHashTable, fullname, fullname);

            ret_val = stat(fullname, &fstats);
            if (ret_val == -1) {
                char buffer[3000];

                sprintf(buffer, "(HyperDoc) read_ht_db: Unable To Open %s :", fullname);
                perror(buffer);
                exit(-1);
            }
            get_token();
            mtime = atoi(token.id);
            if (gverify_dates & (fstats.st_mtime > mtime)) {
                fprintf(stderr, "(HyperDoc) read_ht_file: HyperDoc file %s has been updated\n",

                        fullname);
                fprintf(stderr, "(HyperDoc) Issue htadd %s to update database\n", fullname);
                exit(-1);
            }
            while ((c = getc(db_fp)) != EOF) {
                if (c == '\t')
                    break;
                ungetc(c, db_fp);
                get_token();
                switch (token.type) {
                  case Page:
                    get_token();

                    /*
                     * now check to see if the page has already been
                     * loaded
                     */
                    page = (UnloadedPage *) halloc(sizeof(UnloadedPage),
                                                   "UnloadedPage");
                    page->fpos.name = alloc_string(fullname);
                    page->name = alloc_string(token.id);
                    get_token();
                    if (hash_find(page_hash, page->name) != NULL) {
                        fprintf(stderr, "(HyperDoc) Page name %s  occurred twice\n", page->name);
                        fprintf(stderr, "(HyperDoc) The Version in %s is being ignored \n",
                                page->fpos.name);
                        free(page);
                        get_token();
                        break;
                    }
                    page->fpos.pos = atoi(token.id);
                    get_token();
                    page->fpos.ln = atoi(token.id);
                    page->type = UnloadedPageType;
                    hash_insert(page_hash, (char *)page, page->name);
                    pages++;
                    break;
                  case NewCommand:
                    get_token();
                    macro = (MacroStore *) halloc(sizeof(MacroStore), "MacroStore");
                    macro->fpos.name = alloc_string(fullname);
                    macro->name = alloc_string(token.id);
                    macro->macro_string = NULL;
                    get_token();
                    if (hash_find(macro_hash, macro->name) != NULL) {
                        if (strcmp(macro->name, "localinfo") != 0) {
                            fprintf(stderr, "(HyperDoc) Macro name %s  occurred twice\n",
                                    macro->name);
                            fprintf(stderr, "(HyperDoc) The Version in %s is being ignored \n",
                                    macro->fpos.name);
                        }
                        get_token();
                        free(macro);
                        break;
                    }
                    macro->fpos.pos = atoi(token.id);
                    get_token();
                    macro->fpos.ln = atoi(token.id);
                    macro->loaded = 0;
                    hash_insert(macro_hash, (char *)macro, macro->name);
                    break;
                  case Patch:
                    get_token();
                    patch = (PatchStore *) alloc_patchstore();
                    patch->fpos.name = alloc_string(fullname);
                    patch->name = alloc_string(token.id);
                    get_token();
                    patch->fpos.pos = atoi(token.id);
                    get_token();
                    patch->fpos.ln = atoi(token.id);
                    if (hash_find(patch_hash, patch->name) != NULL) {
                        fprintf(stderr, "(HyperDoc) Patch name %s  occurred twice\n", patch->name);
                        fprintf(stderr, "(HyperDoc) The version in %s is being ignored \n",
                                patch->fpos.name);
                        free_patch(patch);
                        break;
                    }
                    hash_insert(patch_hash, (char *)patch, patch->name);
                    break;
                  default:
                    fprintf(stderr, "(HyperDoc) read_ht_db: Unknown type %s in ht.db\n", token.id);
                    exit(-1);
                    break;
                }
            }
        }
        else
            c = getc(db_fp);
    } while (c != EOF);
/*    fprintf(stderr,
     "parse_aux:read_ht_file:read %d pages from database\n", pages); */
}


/* create an unmapped input-only window for an active screen area */

HyperLink *
make_link_window(TextNode *link_node, int type, int isSubWin)
{
    HyperLink *link;
    XSetWindowAttributes at;

    if (make_input_file)
        switch (type) {
          case Downlink:
          case Memolink:
          case Windowlink:{
                char *name;
                HyperDocPage *p;

                name = print_to_string(link_node);
                p = (HyperDocPage *) hash_find(gWindow->fPageHashTable, name);
                if (!p)
                    printf("undefined link to %s\n", name);
                break;
            }
        }
    else {
        link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");
        if (link == NULL) {
            fprintf(stderr, "(HyperDoc) Ran out of memory allocating a hypertext link!\n");
            exit(-1);
        }
        at.cursor = gActiveCursor;
        at.event_mask = ButtonPress;
        if (isSubWin)
            link->win = XCreateWindow(gXDisplay, gWindow->fDisplayedWindow, 0, 0, 100, 100, 0,
                                      0, InputOnly, CopyFromParent,
                                      CWEventMask | CWCursor, &at);
        else
            link->win = 0;
        link->type = type;
        link->x = link->y = 0;
        link->reference.node = link_node;
        hash_insert(gLinkHashTable, (char *)link,(char *)&link->win);
        return link;
    }
    return 0;
}

HyperLink *
make_paste_window(PasteNode *paste)
{
    HyperLink *link;
    XSetWindowAttributes at;

    if (!make_input_file) {
        link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");
        if (link == NULL) {
            fprintf(stderr, "(HyperDoc) Ran out of memory allocating a hypertext link!\n");
            exit(-1);
        }
        at.cursor = gActiveCursor;
        at.event_mask = ButtonPress;
        link->win = XCreateWindow(gXDisplay, gWindow->fDisplayedWindow,
                                  0, 0, 100, 100, 0,
                                  0, InputOnly, CopyFromParent,
                                  CWEventMask | CWCursor, &at);
        link->type = Pastebutton;
        link->x = link->y = 0;
        link->reference.paste = paste;
        hash_insert(gLinkHashTable, (char *)link,(char *) &link->win);
        return link;
    }
    return 0;
}



/* create a HyperDoc page structure with the given type and name */

static HyperDocPage *
make_special_page(int type, char *name)
{
    HyperDocPage *page = alloc_page(name);

    if (page == NULL) {
        fprintf(stderr, "(HyperDoc) Ran out of memory allocating page.\n");
        exit(-1);
    }
    page->type = type;
    free(page->fLinkHashTable);
    page->fLinkHashTable = NULL;
    return page;
}


/* insert the special button page types into the page hash table */

void
make_special_pages(HashTable *pageHashTable)
{
    hash_insert(pageHashTable, (char *)make_special_page(Quitbutton, "QuitPage"),
                "QuitPage");
    hash_insert(pageHashTable, (char *)make_special_page(Returnbutton, "ReturnPage"),
                "ReturnPage");
    hash_insert(pageHashTable, (char *)make_special_page(Upbutton, "UpPage"),
                "UpPage");
}


/* Here is where I put the item into the pages linked list */

/* Parse the \bound{varlist} command, and add vars to dependency table */

void
add_dependencies(void)
{
    TextNode *bound_node = curr_node;
    TextNode *node;
    SpadcomDepend *depend;

    if (cur_spadcom == NULL) {
        fprintf(stderr, "(HyperDoc) \\bound occuring outside a \\spadcom\n");
        print_page_and_filename();
        exit(-1);
    }
    curr_node->type = Bound;
    curr_node->data.node = alloc_node();
    curr_node = curr_node->data.node;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    curr_node->type = Endarg;
    curr_node = bound_node;

    if (gPageBeingParsed->depend_hash == NULL) {
        gPageBeingParsed->depend_hash =
            (HashTable *) halloc(sizeof(HashTable), "Hash Table");
        hash_init(
                  gPageBeingParsed->depend_hash,
                  DependHashSize,
                  (EqualFunction) string_equal,
                  (HashcodeFunction) string_hash);
    }
    for (node = bound_node->data.node; node->type != Endarg; node = node->next) {
        if (node->type == Word) {
            depend = (SpadcomDepend *) halloc(sizeof(SpadcomDepend), "SpadcomDepend");
            depend->label = alloc_string(node->data.text);
            depend->spadcom = cur_spadcom;
            depend->executed = 0;
            hash_insert(gPageBeingParsed->depend_hash, (char *)depend, depend->label);
        }
    }
}

/* Returns true iff the TextNode contains a single integer */

int
is_number(char * str)
{
    char *s;

    for (s = str; *s != '\0'; s++) {
        if (!(isdigit(*s) || *s == '-'))
            return 0;
    }
    return 1;
}
void
parser_error(char *str)
{
    /** this procedure is called by the parser when an error occurs. It prints
      the error message, followed by the next 10 tokens to ease finding the
      error for the user.                                               *****/

    int i, v;

    fprintf(stderr, " %s\n", str);
    fprintf(stderr, "Here are the next 10 tokens:\n");
    for (i = 0; i < 10; i++) {
        v = get_token();
        if (v == EOF)
            break;
        print_token();
    }
    fprintf(stderr, "\n");
    exit(-1);
}


#define whitespace(c) ((c) == ' ' || (c) == '\t' || (c) == '\n')
#define delim(c) \
  (whitespace(c))


/* advance token to the next token in the input stream.  */
int
get_filename(void)
{
    int c, ws;
    static int seen_white = 0; /*UNUSED */
    static char buffer[2048];
    char *buf = buffer;

    if (last_token) {
        last_token = 0;
        return 0;
    }
    do {
        keyword_fpos = fpos;
        c = get_char();
        ws = whitespace(c);
        if (ws)
            seen_white = 1;
    } while (ws);
    switch (c) {
      case EOF:
        fprintf(stderr, "(HyperDoc) Error trying to read %s, unexpected end-of-file.\n",db_file_name);
        exit(-1);
      case '%':
      case '\\':
      case '{':
      case '}':
        fprintf(stderr, "(HyperDoc) Error unexpected character %c.\n",c);
        exit(-1);
      default:
        do {
            *buf++ = c;
        } while ((c = get_char()) != EOF && !delim(c));
        unget_char(c);
        *buf = '\0';
        token.type = Word;
        token.id = buffer;
        seen_white = 0;
        break;
    }
    return 1;
}

char *
get_input_string(void)
{
    char *string;
    TextNode *string_node,*save_node;

    save_node = curr_node;
    /* Get the nodes that make up the string */
    string_node = alloc_node();
    curr_node = string_node;
    parse_HyperDoc();
    curr_node->type = Endarg;

    /* Once here we print to string to get the actual name */
    string = print_to_string(string_node);
    free_node(string_node, 0);
    curr_node=save_node;
    return string;
}

/*
 * tries to determine if there is an optional argument for where I should be
 * parsing from. If so it then tries to determine which
 */
int
get_where(void)
{
    int tw;

    get_token();
    if (token.type != Word)
        return -1;

    /* Now try to determine if it is a good type */
    if (!strcmp(token.id, "lisp")) {
        tw = FromSpadSocket;
    }
    else if (!strcmp(token.id, "unix")) {
        tw = FromUnixFD;
    }
    else if (!strcmp(token.id, "ht")) {
        tw = FromFile;
    }
    else {
        return -1;
    }

    /* now check to see if I got a closing square brace */
    get_token();
    if (token.type != Rsquarebrace)
        return -1;

    return tw;
}


FILE *
find_fp(FilePosition fp)
{
    FILE *lfile;
    char fullname[2048], addname[2048];
    int ret_val;

    /* find the source file in the file hash table, if not there, open it */
    lfile = (FILE *) hash_find(&gFileHashTable, fp.name);
    if (lfile == NULL) {
        lfile = ht_file_open(fullname, addname, fp.name);
        hash_insert(&gFileHashTable, (char *)lfile, fp.name);
    }

    /* seek to beginning fp.pos */
    ret_val = fseek(lfile, fp.pos, 0);
    if (ret_val == -1) {
        perror("fseeking to a page");
        longjmp(jmpbuf, 1);
    }

    /* now set some global values */
    page_start_fpos = fp.pos;
    line_number = fp.ln;
    return lfile;
}
