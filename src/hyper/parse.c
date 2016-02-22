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

#include "debug.h"

#include "parse.h"
#include "parse-types.h"
#include "lex.h"
#include "hyper.h"
#include "extent.h"

#include "all_hyper_proto.H1"

static void end_a_page(void);
static HyperDocPage * format_page(UnloadedPage * ulpage);
static void parse_page(HyperDocPage * page);
static void parse_replacepage(void);
static void start_footer(void);
static void start_scrolling(void);

TextNode *curr_node;            /* current node being parsed. It is to be the
                                 * next one filled   */
HashTable *gLinkHashTable;           /* the hash table of active link windows   */
TextNode *cur_spadcom;          /* The current FRICAS command   */

short int gParserMode;           /* Parser mode flag */
short int gParserRegion;         /* Parser Region flag scrolling etc */
short int gStringValueOk;        /* is a string or box value ok */
boolean gEndedPage;

extern int example_number;             /* sequence example number */


int ret_val;                    /* The return value from get_token */

HyperDocPage *cur_page;

char *replace_page;             /* true if dynamic page is link to static one */

/*
 * These routines are used for storing an restoring the parser mode. When
 * I start to parse from string, or from a macro, I need to restore the
 * parser mode and region once done. These routines do that
 *
 */

typedef struct mr_stack {
    /** The structure for storing parser mode and region **/
    short int fParserMode;
    short int fParserRegion;
    struct mr_stack *fNext;
}   MR_Stack;

MR_Stack *top_mr_stack = NULL;  /** Declaration for the stack  **/

static void
Push_MR(void)
{
    MR_Stack *newStackItem = (MR_Stack *) halloc(sizeof(MR_Stack), "Mode Region Stack");

    newStackItem->fParserMode = gParserMode;
    newStackItem->fParserRegion = gParserRegion;
    newStackItem->fNext = top_mr_stack;
    top_mr_stack = newStackItem;
}

static void
Pop_MR(void)
{
    MR_Stack *old = top_mr_stack;

    if (old == NULL) {
        fprintf(stderr, "(HyperDoc) Parser Error: Tried to pop empty MR Stack\n");
        exit(-1);
    }
    else {
        gParserMode = old->fParserMode;
        gParserRegion = old->fParserRegion;
        top_mr_stack = old->fNext;
        free(old);
    }
}

void
load_page(HyperDocPage *page)
{
    if (page->type == UnloadedPageType) {
        HyperDocPage *new_page;
        init_scanner();
        new_page = format_page((UnloadedPage *)page);
        gWindow->page = new_page;
        /* free(page); */
        page = new_page;
    }
}

HyperDocPage *formatpage;

/* Display a HyperDoc page with the given name, parsing it if needed */

void
display_page(HyperDocPage *page)
{
    HyperDocPage *new_page;

    XUnmapSubwindows(gXDisplay, gWindow->fMainWindow);
    XUnmapSubwindows(gXDisplay, gWindow->fScrollWindow);
    XFlush(gXDisplay);

    if (setjmp(jmpbuf)) {

        /*
         * since I did not finish formatting the page, let me get rid of what
         * I had
         */
        free_page(formatpage);
        /* Replace the buggy page with what I started with */
        hash_replace(gWindow->fPageHashTable, (char *)page, formatpage->name);
        if (!strcmp(formatpage->name, "ErrorPage")) {
            fprintf(stderr, "(HyperDoc) Oops the error page is buggy\n");
            exit(-1);
        }
        gWindow->page = page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ErrorPage");
        if (page == NULL) {
            fprintf(stderr, "(HyperDoc) No error page found, exiting\n");
            exit(-1);
        }
        reset_connection();
    }
    if (page->type == UnloadedPageType || page->type == ErrorPage) {
        /* Gack! (page should be a union!) */
        init_scanner();
        new_page = format_page((UnloadedPage *)page);
        gWindow->page = new_page;
        /* free(page); */
        page = new_page;
    }
    show_page(page);
}


/* Parse a given HyperDoc Page, from the top */

static HyperDocPage *
format_page(UnloadedPage *ulpage)
{
    /*int ret_val;*/
    HyperDocPage *page = alloc_page(ulpage->name);

    /*
     * In case of an error I will have to get at this page so I can free the
     * waisted memory
     */
    formatpage = page;
    page->type = Normal;
    hash_replace(gWindow->fPageHashTable, (char *)page, ulpage->name);

    cfile = find_fp(ulpage->fpos);


    page->filename = alloc_string(ulpage->fpos.name);
    parse_page(page);
    return page;
}

/* parse the HyperDoc statements in the given string */

void
parse_from_string(char *str)
{
    save_scanner_state();
    last_ch = NoChar;
    last_token = 0;
    input_string = str;
    input_type = FromString;
    parse_HyperDoc();
    restore_scanner_state();
}

static void
parse_title(HyperDocPage *page)
{
    TextNode *node;

    Push_MR();
    gParserRegion = Title;
    get_expected_token(Lbrace);
    node = alloc_node();
    page->title = node;
    node->type = Titlenode;
    node->next = alloc_node();
    node = node->next;
    node->type = Center;
    node->next = alloc_node();
    curr_node = node->next;
    parse_HyperDoc();
    curr_node->type = Endcenter;
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    curr_node->type = Endtitle;
    curr_node->next = NULL;
    if (gNeedIconName) {
        char *title = print_to_string(page->title);

        XSetIconName(gXDisplay, gWindow->fMainWindow, title);
        gNeedIconName = 0;
    }
    if (token.type != Rbrace) {
        fprintf(stderr, "(HyperDoc) Parse title was expecting a closing brace\n");
        print_page_and_filename();
        jump();
    }
    linkTitleBarWindows();
    Pop_MR();
}

static void
parse_header(HyperDocPage *page)
{
    TextNode *node;

    Push_MR();
    gParserRegion = Header;
    node = alloc_node();
    page->header = node;
    node->type = Headernode;
    node->next = alloc_node();
    curr_node = node->next;
    parse_HyperDoc();
}

/*
 * parse a page from the top level
 */

static void
init_parse_page(HyperDocPage *page)
{
    gEndedPage = gInDesc = gStringValueOk = gInIf =
        gInButton = gInOptional = gInVerbatim = gInPaste = gInItems =
        gInSpadsrc = FALSE;
    example_number = 1;
    cur_page = page;
    gParserMode = AllMode;
    /* Now I should set the input list to be null */
    free_input_list(page->input_list);
    page->input_list = page->current_item = NULL;

    init_top_group();
    clear_be_stack();

    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    hash_init(
              gLinkHashTable,
              LinkHashSize,
              (EqualFunction) window_equal,
              (HashcodeFunction) window_code);
    gPageBeingParsed = page;

}

void
init_parse_patch(HyperDocPage *page)
{
    gEndedPage = gInDesc = gStringValueOk = gInIf =
        gInButton = gInOptional = gInVerbatim = gInPaste = gInItems =
        gInSpadsrc = FALSE;
    gParserMode = AllMode;
    gParserRegion = Scrolling;

    init_top_group();
    clear_be_stack();

    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    gPageBeingParsed = page;
}

#define end_page(t) ((t == Page || t == NewCommand ||t == Endpage)?1:0)

static void
parse_page(HyperDocPage *page)
{
    init_parse_page(page);

    /* Get the name of the page */

    get_expected_token(Page);
    get_expected_token(Lbrace);
    get_expected_token(Word);
    if (page->name == NULL)
        page->name = alloc_string(token.id);
    get_expected_token(Rbrace);
    /* parse the title */
    gWindow->fDisplayedWindow = gWindow->fMainWindow;
    parse_title(page);

    /*
     * Now start parsing the header region
     */
    parse_header(page);
}

char *ExpectedBeginScroll =
"Parser Error: Unexpected new page, expecting a begin scroll\n", *ExpectedEndScroll =
"Parser Error: Unexpected new page, expected an end scroll\n";

/*
 * The general HyperDoc parsing function.  expects to see anything. This
 * function will parse until it sees either: 1) A new page starting 2) An end
 * of file 3) a closing bracket "}"
 */

void
parse_HyperDoc(void)
{
    TextNode *node = NULL /*, *save_node = NULL, *arg_node = NULL*/ ;

    for(;;) {
        ret_val = get_token();

        if (ret_val == EOF)
            return;

        switch (token.type) {
          case Spadsrc:
            parse_spadsrc(curr_node);
            break;
          case Helppage:
            parse_help();
            break;
          case Endpatch:
          case Endpaste:
          case Rbrace:
            return;
          case Paste:
            parse_paste();
            break;
          case Pastebutton:
            parse_pastebutton();
            break;
          case Endpage:
          case NewCommand:
          case Page:
            end_a_page();
            return;
          case EndScroll:
            token.type = Endscroll;
          case Endscroll:
            start_footer();
            break;
          case Beginscroll:
            start_scrolling();
            break;
          case Thispage:        /* it really is just a word */
            curr_node->type = Word;
            curr_node->data.text = alloc_string(gPageBeingParsed->name);
            break;
          case Icorrection:
            node->type = Noop;
            break;
          case Newcond:
            parse_newcond();
            break;
          case Setcond:
            parse_setcond();
            break;
          case Dollar:
            parse_verbatim(Math);
            break;
          case Verbatim:
            parse_verbatim(Verbatim);
            break;
          case Ifcond:
            parse_ifcond();
            break;
          case Fi:
            if (gInIf)
                return;
            else {
                curr_node->type = Noop;
                /* Oops I had a problem parsing this puppy */
                fprintf(stderr, "(HyperDoc) \\fi found without macthing if?\n");
                longjmp(jmpbuf, 1);
                fprintf(stderr, "(HyperDoc) Longjmp failed -- Exiting \n");
                exit(-1);
            }
          case Else:
            if (gInIf)
                return;
            else {
                /* Oops I had a problem parsing this puppy */
                curr_node->type = Noop;
                fprintf(stderr, "(HyperDoc) \\else found without macthing if?\n");
                longjmp(jmpbuf, 1);
                fprintf(stderr, "(HyperDoc) Longjmp failed -- Exiting \n");
                exit(-1);
            }
          case Macro:
            parse_macro();
            break;
          case Env:
            /** In this case, get the environment value, and make it a word **/
            parse_env(curr_node);
            break;
          case WindowId:
            curr_node->type = WindowId;
            curr_node->space = token.id[-1];
            curr_node->data.text = window_id(gWindow->fMainWindow);
            break;
          case Punctuation:
          case Word:
          case Lsquarebrace:
          case Dash:
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            curr_node->data.text = alloc_string(token.id);
            break;
          case Pagename:
            {
                char *str;

                curr_node->type = Word;
                curr_node->space = 0;
                str = halloc(strlen(cur_page->name) + 1, "parse");
                sprintf(str, "%s", cur_page->name);
                curr_node->data.text = alloc_string(str);
                break;
            }
          case Examplenumber:
            {
                char *str;

                curr_node->type = Word;
                curr_node->space = 0;
                str = halloc(5, "parse");
                sprintf(str, "%d", example_number);
                curr_node->data.text = alloc_string(str);
                break;
            }
          case Rsquarebrace:
            if (gInOptional)
                return;
            else {
                curr_node->type = token.type;
                curr_node->space = token.id[-1];
                curr_node->data.text = alloc_string(token.id);
            }
            break;
          case EndTitems:
            token.type = Endtitems;
          case Endtitems:
            if (gParserMode != AllMode) {
                curr_node->type = Noop;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[token.type]);
                longjmp(jmpbuf, 1);
            }
            else {
                curr_node->type = token.type;
                break;
            }
          case EndItems:
            token.type = Enditems;
          case Enditems:
            gInItems--;
          case Horizontalline:
          case Par:
          case Newline:
          case Titem:
            if (gParserMode != AllMode) {
                curr_node->type = Noop;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[token.type]);
                longjmp(jmpbuf, 1);
            }
            else {
                curr_node->type = token.type;
                break;
            }
          case Begintitems:
          case Beginitems:
            if (gParserMode != AllMode) {
                curr_node->type = Noop;
                fprintf(stderr, "(HyperDoc) Found a bad token %s\n", token_table[token.type]);
                longjmp(jmpbuf, 1);
            }
            else {
                parse_begin_items();
                break;
            }
          case Item:
            parse_item();
            break;
          case Mitem:
            parse_mitem();
            break;
          case VSpace:
          case Tab:
          case HSpace:
          case Indent:
          case Indentrel:
            parse_value1();
            break;
          case Space:
            parse_value2();
            break;
          case Lbrace:
            curr_node->type = Group;
            curr_node->space = token.id[-1];
            push_group_stack();
            node = alloc_node();
            curr_node->next = node;
            curr_node = curr_node->next;
            parse_HyperDoc();
            curr_node->type = Endgroup;
            pop_group_stack();
            break;
          case Upbutton:
          case Returnbutton:
          case Link:
          case Downlink:
          case Memolink:
          case Windowlink:
            parse_button();
            break;
          case Unixlink:
          case LispMemoLink:
          case LispDownLink:
          case Lisplink:
          case Lispcommand:
          case Lispcommandquit:
          case Spadlink:
          case Spaddownlink:
          case Spadmemolink:
          case Unixcommand:
          case Spadcall:
          case Spadcallquit:
          case Qspadcall:
          case Qspadcallquit:
          case Lispwindowlink:
            parse_command();
            break;
          case Controlbitmap:
          case Inputbitmap:
          case Inputpixmap:
          case Inputimage:
            parse_input_pix();
            break;
          case Box:
            parse_box();
            break;
          case Mbox:
            parse_mbox();
            break;
          case Free:
            parse_free();
            break;
          case Center:
            parse_centerline();
            break;
          case Bound:
            add_dependencies();
            break;
          case Spadcommand:
          case Spadgraph:
            parse_spadcommand(curr_node);
            break;
          case Table:
            parse_table();
            break;
          case Beep:
          case Emphasize:
          case BoldFace:
          case Rm:
          case It:
          case Tt:
          case Sl:
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            break;
          case Inputstring:
            parse_inputstring();
            break;
          case SimpleBox:
            parse_simplebox();
            break;
          case BoxValue:
          case StringValue:
            if (!gStringValueOk) {
                strcpy(ebuffer,"(HyperDoc): Unexpected Value Command:");
                strcat(ebuffer, token.id);

                parser_error(ebuffer);
                curr_node->type = Noop;
                longjmp(jmpbuf, 1);
            }
            curr_node->type = token.type;
            curr_node->space = token.id[-1];
            get_expected_token(Lbrace);
            get_expected_token(Word);
            curr_node->data.text = alloc_string(token.id);
            get_expected_token(Rbrace);
            break;
          case NoLines:
            gPageBeingParsed->page_flags |= NOLINES;
            break;
          case Pound:
            curr_node->type = Pound;
            curr_node->space = token.id[-1];
            curr_node->next = alloc_node();
            curr_node = curr_node->next;
            parse_parameters();
            break;
          case Radiobox:
            parse_radiobox();
            break;
          case Radioboxes:
            parse_radioboxes();
            break;
          case Replacepage:
            parse_replacepage();
            break;
          default:
            fprintf(stderr, "(HyperDoc) Keyword not currently supported: %s\n", token.id);
            print_page_and_filename();
            curr_node->type = Noop;
            break;
        }
        if (gEndedPage)
            return;
        if (curr_node->type != Noop) {
            node = alloc_node();
            curr_node->next = node;
            curr_node = node;
        }
    }
}


/* parse a page from a socket source */

HyperDocPage *
parse_page_from_socket(void)
{
    HyperDocPage *page = alloc_page((char *) NULL);
    HyperDocPage *hpage;

    init_scanner();
    input_type = FromSpadSocket;
    input_string = "";
    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    hash_init(
              gLinkHashTable,
              LinkHashSize,
              (EqualFunction) window_equal,
              (HashcodeFunction) window_code);
    gPageBeingParsed = page;
    replace_page = NULL;
    if (setjmp(jmpbuf)) {
        /* Ooops, somewhere I had an error */
        free_page(page);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ErrorPage");
        reset_connection();
    }
    else {
        parse_page(page);
        page->type = SpadGen;
        page->filename = NULL;
        /* just for kicks, let me add this thing to the hash file */
        hpage = (HyperDocPage *) hash_find(gWindow->fPageHashTable, page->name);
        if (hpage)
            hash_replace(gWindow->fPageHashTable, (char *)page, page->name);
        else {
            hash_insert(gWindow->fPageHashTable, (char *)page, page->name);
        }
    }
    if (replace_page != NULL) {
        free_page(page);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, replace_page);
        if (page == NULL)
            fprintf(stderr, "(HyperDoc) Unknown page: %s\n", replace_page);
    }
    return page;
}

HyperDocPage *
parse_page_from_unixfd(void)
{
    HyperDocPage *page = alloc_page((char *) NULL);

    init_scanner();
    input_type = FromUnixFD;
    cur_spadcom = NULL;
    gLinkHashTable = page->fLinkHashTable;
    hash_init(
              gLinkHashTable,
              LinkHashSize,
              (EqualFunction) window_equal,
              (HashcodeFunction) window_code);
    gPageBeingParsed = page;
    if (setjmp(jmpbuf)) {
        /* Ooops, somewhere I had an error */
        free_page(page);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ErrorPage");
        reset_connection();
    }
    else {
        parse_page(page);
        page->type = Unixfd;
        page->filename = NULL;
    }
    return page;

}

static void
start_scrolling(void)
{

    /*
     * if I am here than I had a begin scroll. This means I should end the
     * header, and then start parsing the footer
     */

    if (gParserRegion != Header) {
        curr_node->type = Noop;
        fprintf(stderr, "(HyperDoc) Parser Error: Unexpected BeginScrollFound\n");
        longjmp(jmpbuf, 1);
        fprintf(stderr, "(HyperDoc) Longjump failed exiting\n");
    }
    curr_node->type = Endheader;
    curr_node->next = NULL;
    Pop_MR();

    Push_MR();
    gParserRegion = Scrolling;
    gWindow->fDisplayedWindow = gWindow->fScrollWindow;
    curr_node = alloc_node();
    gPageBeingParsed->scrolling = curr_node;
    curr_node->type = Scrollingnode;
}

static void
start_footer(void)
{
    /*
     * This ends the parsing of the scrolling region, and then starts to
     * parse the footer
     */

    if (gParserRegion != Scrolling) {
        curr_node->type = Noop;
        fprintf(stderr, "(HyperDoc) Parser Error: Unexpected Endscroll Found\n");
        print_page_and_filename();
        longjmp(jmpbuf, 1);
        fprintf(stderr, "(HyperDoc) Longjump failed exiting\n");
    }

    curr_node->type = Endscrolling;
    curr_node->next = NULL;
    Pop_MR();
    linkScrollBars();

    Push_MR();
    gParserRegion = Footer;
    curr_node = alloc_node();
    curr_node->type = Footernode;
    gPageBeingParsed->footer = curr_node;
    gWindow->fDisplayedWindow = gWindow->fMainWindow;
}

static void
end_a_page(void)
{
    if (gParserRegion == Scrolling) {
        fprintf(stderr, "%s\n",
                "(HyperDoc) end_a_page: Unexpected End of Page occurred \
                   inside a \beginscroll");
        print_page_and_filename();
        jump();
    }
    gEndedPage = TRUE;
    if (gParserRegion == Footer) {
        /* the person had all the regions, I basically just have to leave */
        curr_node->type = Endscrolling;
        curr_node->next = NULL;
        Pop_MR();
    }
    else if (gParserRegion == Header) {
        /* person had a header. So just end it and return */
        curr_node->type = Endheader;
        curr_node->next = NULL;
        Pop_MR();
        gPageBeingParsed->scrolling = NULL;
        gPageBeingParsed->footer = NULL;
    }
}

static void
parse_replacepage(void)
{
    get_expected_token(Lbrace);
    get_token();
    replace_page = alloc_string(token.id);
    get_expected_token(Rbrace);
}
