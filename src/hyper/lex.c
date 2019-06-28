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

/*
 * Lexical analyzer stuff. Exported functions: parser_init()       --
 * initialize the parser tables with keywords init_scanner()       --
 * initialize scanner for reading a new page get_token()                   --
 * sets the "token" variable to be the next -- token in the current input
 * stream save_scanner_state(   )  -- save the current state of scanner so
 * that -- the scanner input mode may be switched restore_scanner_state() --
 * undo the saved state
 *
 * Note: The scanner reads from three separate input locations depending on the
 * value of the variable "input_type".  If this variable is:
 *
 * FromFile       -- it read from the file pointed to by "cfile". FromString
 * -- It reads from the string "input_string". FromSpadSocket -- It reads
 * from the socket pointed to by spad_socket FromFD       -- It reads from a
 * file descriptor
 *
 *
 * New variable useAscii -- tells us if we we should translate
 * graphics characters on the fly
 * initialised in init_scanner
 *
 */

#include "fricas_c_macros.h"
#include "debug.h"

int useAscii;

#define PARSER 1

#include "hyper.h"
#include "hterror.h"
#include "lex.h"

#include "all_hyper_proto.H1"
#include "sockio-c.H1"


#include <ctype.h>
#include <setjmp.h>

static int get_char1(void);
static void spad_error_handler(void);
static int keyword_type(void);

extern int gTtFontIs850;


StateNode *top_state_node;
HyperDocPage *gPageBeingParsed;      /* page currently being parsed    */
extern jmp_buf jmpbuf;
extern char ebuffer[];
short int gInSpadsrc = 0;
short int gInVerbatim;

/* Parser variables */
long fpos;                      /* Position of pointer in file in characters */
long page_start_fpos;           /* where the current pages fpos started      */
long keyword_fpos;              /* fpos of beginning of most recent keyword */
Token token;                    /* most recently read token */
int last_token;                 /* most recently read token for unget_token */
int input_type;                 /* indicates where to read input */
char *input_string;             /* input string read when from_string is true */
int last_ch;                    /* last character read, for unget_char */
int last_command;               /* the last socket command */
int keyword;                    /* the last command was a keyword, or a group */
int cfd;                        /* current file descriptor */
FILE *cfile;                    /* currently active file pointer */
FILE *unixfd;
int line_number;

char sock_buf[1024];            /* buffer for socket input */

#define TokenHashSize   100

static HashTable tokenHashTable;           /* hash table of parser tokens */

void
dumpToken(char *caller, Token t)
{ fprintf(stderr,"%s:dumpToken type=%s id=%s\n",
    caller,token_table[t.type],t.id);
}


/* initialize the parser keyword hash table */
void
parser_init(void)
{
    int i;
    Token *toke;

    /* First I initialize the hash table for the tokens */

    hash_init(
              &tokenHashTable,
              TokenHashSize,
              (EqualFunction)string_equal,
              (HashcodeFunction)string_hash);
    for (i = 2; i <= NumberUserTokens; i++) {
        toke = (Token *) halloc(sizeof(Token), "Token");
        toke->type = i;
        toke->id = token_table[i];
        hash_insert(&tokenHashTable, (char *)toke, toke->id);
    }

}

/* initialize the lexical scanner to read from a file */
void
init_scanner(void)
{
    if (getenv("HTASCII")) {
        useAscii = (strcmp(getenv("HTASCII"), "yes") == 0);
    }
    else {
        if(gTtFontIs850==1) useAscii = 0;
        else useAscii = 1;
    }
    keyword = 0;
    last_ch = NoChar;
    last_token = 0;
    input_type = FromFile;
    fpos = 0;
    keyword_fpos = 0;
    last_command = -1;
    line_number = 1;
}

/*
 * variables to save current state of scanner.  Currently only one level of
 * saving is allowed.  In the future we should allow nested saves
 */

/* save the current state of the scanner */
void
save_scanner_state(void)
{
    StateNode *new_item = (StateNode *) halloc((sizeof(StateNode)), "StateNode");

    new_item->page_start_fpos = page_start_fpos;
    new_item->fpos = fpos;
    new_item->keyword_fpos = keyword_fpos;
    new_item->last_ch = last_ch;
    new_item->last_token = last_token;
    new_item->token = token;
    new_item->input_type = input_type;
    new_item->input_string = input_string;
    new_item->cfile = cfile;
    new_item->next = top_state_node;
    new_item->keyword = keyword;
    top_state_node = new_item;
}

/* restore the saved scanner state */
void
restore_scanner_state(void)
{
    StateNode *x = top_state_node;

    if (top_state_node == NULL) {
        fprintf(stderr, "Restore Scanner State: State empty\n");
        exit(-1);
    }
    top_state_node = top_state_node->next;
    page_start_fpos = x->page_start_fpos;
    fpos = x->fpos;
    keyword_fpos = x->keyword_fpos;
    last_ch = x->last_ch;
    last_token = x->last_token;
    token = x->token;
    input_type = x->input_type;
    input_string = x->input_string;
    cfile = x->cfile;
    keyword = x->keyword;
    if (cfile != NULL)
        fseek(cfile, fpos + page_start_fpos, 0);
    /** Once that is done, lets throw away some memory **/
    free(x);
}

/* return the character to the input stream. */
void
unget_char(int c)
{
    if (c == '\n')
        line_number--;
    last_ch = c;
}

int
get_char(void)
{
    int c;

    c = get_char1();
    if (useAscii) {
        switch (c) {
          case '�':
            c = '-';
            break;
          case '�':
            c = '+';
            break;
          case '�':
            c = '[';
            break;
          case '�':
            c = '+';
            break;
          case '�':
            c = '-';
            break;
          case '�':
            c = '+';
            break;
          case '�':
            c = '-';
            break;
          case '�':
            c = '+';
            break;
          case '�':
            c = ']';
            break;
          case '�':
            c = '+';
            break;
          case '�':
            c = '|';
            break;
          default:
            break;
        }
    }
    return c;
}

char * read_again = 0;

/* return the next character in the input stream */
static int
get_char1(void)
{
    int c;
    int cmd;

    if (last_ch != NoChar) {
        c = last_ch;
        last_ch = NoChar;
        if (c == '\n')
            line_number++;
        return c;
    }
    switch (input_type) {
      case FromUnixFD:
        c = getc(unixfd);
        if (c == '\n')
            line_number++;
        return c;
      case FromString:
        c = (*input_string ? *input_string++ : EOF);
        if (c == '\n')
            line_number++;
        return c;
      case FromFile:
        c = getc(cfile);
        fpos++;
        if (c == '\n')
            line_number++;
        return c;
      case FromSpadSocket:
AGAIN:
        if (*input_string) {
            /* this should never happen for the first character */
            c = *input_string++;
            if (c == '\n')
                line_number++;
            return c;
        }
        if (last_command == EndOfPage)
            return EOF;
        if (read_again == NULL) {
            last_command = cmd = get_int(spad_socket);
            if (cmd == EndOfPage)
                return EOF;
#ifndef HTADD
            if (cmd == SpadError)
                spad_error_handler();
#endif
        }
        read_again = get_string_buf(spad_socket, sock_buf, 1023);
        /* this will be null if this is the last time*/
        input_string = sock_buf;
        goto AGAIN;
      default:
        fprintf(stderr, "Get Char: Unknown type of input: %d\n", input_type);
        return -1;
    }
}


#define special(c) ((c) == '{' || (c) == '}' || (c) == '#' || (c) == '%' || \
                    (c) == '\\'  || (c) == '[' || (c) == ']' || (c) == '_' || \
                    (c) == ' ' || (c) == '$' || (c) == '~' || (c) == '^' ||  \
                    (c) == '&')

#define punctuation(c) ((c)== '`' || (c) == '\'' || (c) == ','  || \
                        (c) == '.' || (c) == '?' || (c) == '"' || \
                        (c)  == ';' || (c) == ':' || (c) == '-')

#define whitespace(c) ((c) == ' ' || (c) == '\t' || (c) == '\n')
#define delim(c) \
  (whitespace(c) || special(c) || punctuation(c))



Token unget_toke;

/* return current token to the input stream */
void
unget_token(void)
{
    last_token = 1;
    unget_toke.type = token.type;
    unget_toke.id = alloc_string(token.id - 1);
}


int
get_token(void)
{
    int c, ws;
    int nls = 0;
    static int seen_white = 0;
    static char buffer[1024];
    char *buf = buffer;

    if (last_token) {
        last_token = 0;
        token.type = unget_toke.type;
        strcpy(buffer, unget_toke.id);
        free(unget_toke.id);
        token.id = buffer + 1;
        if (token.type == EOF)
            return EOF;
        else
            return 0;
    }
    seen_white = nls = 0;
    do {
        c = get_char();
        ws = whitespace(c);
        if (ws)
            seen_white++;
        if (c == '\n') {
            if (nls) {
                token.type = Par;
                return 0;
            }
            else
                nls++;
        }
    } while (ws);

    /* first character of string indicates number of spaces before token */

    if (!keyword)
        *buf++ = seen_white;
    else
        *buf++ = 0;

    keyword = 0;
    if (input_type != FromSpadSocket && c == '%') {
        while ((c = get_char()) != '\n' && c != EOF);
/* trying to fix the comment problem: a comment line forces words on either side together*/
/* try returning the eol */
        unget_char(c);
        return get_token();
    }
    if (input_type == FromFile && c == '$') {
        token.type = Dollar;
        return 0;
    }
    switch (c) {
      case EOF:
        token.type = -1;
        return EOF;
      case '\\':
        keyword_fpos = fpos - 1;
        c = get_char();
        if (!isalpha(c)) {
            *buf++ = c;
            token.type = Word;
            *buf = '\0';
            seen_white = 0;
        }
        else {
            do {
                *buf++ = c;
            } while ((c = get_char()) != EOF && isalpha(c));

            unget_char(c);
            *buf = '\0';
            keyword = 1;
            token.id = buffer + 1;
            return (keyword_type());
        }
        break;
      case '{':
        token.type = Lbrace;
        break;
      case '}':
        token.type = Rbrace;
        break;
      case '[':
        token.type = Lsquarebrace;
        *buf++ = c;
        *buf = '\0';
        token.id = buffer + 1;
        break;
      case ']':
        token.type = Rsquarebrace;
        *buf++ = c;
        *buf = '\0';
        token.id = buffer + 1;
        break;
      case '#':
        token.type = Pound;

        /*
         * if I get a pound then what I do is parse until I get something
         * that is not an integer
         */
        c = get_char();
        while (isdigit(c) && (c != EOF)) {
            *buf++ = c;
            c = get_char();
        }
        unget_char(c);
        *buf = '\0';
        token.id = buffer + 1;
        break;
      case '`':
      case '\'':
      case ',':
      case '.':
      case '!':
      case '?':
      case '"':
      case ':':
      case ';':
        token.type = Punctuation;
        *buf++ = c;
        *buf = '\0';
        /** Now I should set the buffer[0] as my flag for whether I had
          white-space in front of me, and whether I had white space
          behind me **/
        if (buffer[0])
            buffer[0] = FRONTSPACE;
        c = get_char();
        if (whitespace(c))
            buffer[0] |= BACKSPACE;
        unget_char(c);
        token.id = buffer + 1;
        break;
      case '-':
        do {
            *buf++ = c;
        } while (((c = get_char()) != EOF) && (c == '-'));
        unget_char(c);
        *buf = '\0';
        token.type = Dash;
        token.id = buffer + 1;
        break;
      default:
        do {
            *buf++ = c;
        } while ((c = get_char()) != EOF && !delim(c));
        unget_char(c);
        *buf = '\0';
        token.type = Word;
        token.id = buffer + 1;
        break;
    }
/*    dumpToken("get_token",token);*/
    return 0;
}


/*
 * Here are the structures and stuff needed for the begin and end routines.
 * The stack stores the begin types that have been seen and the end
 * pops them off and checks to insure that they are reversed properly.
 */

typedef struct be_struct {
    int type;
    char *id;
    struct be_struct *next;
}   BeStruct;

BeStruct *top_be_stack;


void
push_be_stack(int type,char * id)
{
    BeStruct *be = (BeStruct *) halloc(sizeof(BeStruct), "BeginENd stack");

    if (gWindow != NULL) {
        be->type = type;
        be->next = top_be_stack;
        be->id = alloc_string(id);
        top_be_stack = be;
    }
    return;
}
void
check_and_pop_be_stack(int type,char * id)
{
    BeStruct *x;

    /*
     * this routine pops the be stack and compares types. If they are
     * the same then I am okay and return a 1. Else I return a two and try to
     * print a meaningful message
     */
    if (gWindow == NULL)
        return;
    if (top_be_stack == NULL) { /* tried to pop when I shouldn't have */
        fprintf(stderr, "Unexpected \\end{%s} \n", token.id);
        print_page_and_filename();
        print_next_ten_tokens();
        jump();
    }
    x = top_be_stack;
    if (x->type == type) {
        top_be_stack = top_be_stack->next;
        free(x->id);
        free(x);
        return;
    }
    /* else I didn't have a match. Lets try to write a sensible message */
    fprintf(stderr, "\\begin{%s} ended with \\end{%s} \n", x->id, id);
    print_page_and_filename();
    print_next_ten_tokens();
    jump();
}

int
clear_be_stack(void)
{
    BeStruct *x = top_be_stack, *y;

    top_be_stack = NULL;
    while (x != NULL) {
        y = x->next;
        free(x);
        x = y;
    }
    return 1;
}

int
be_type(char *which)
{
    Token store;

    get_expected_token(Lbrace);
    get_expected_token(Word);
    switch (token.id[0]) {
      case 't':
        if (!strcmp(token.id, "titems")) {
            token.type = Begintitems;
        }
        else {
            return -1;
        }
        break;
      case 'p':
        if (!strcmp(token.id, "page")) {
            token.type = Page;
        }
        else if (!strcmp(token.id, "paste")) {
            token.type = Paste;
        }
        else if (!strcmp(token.id, "patch")) {
            token.type = Patch;
        }
        else {
            return -1;
        }
        break;
      case 'v':         /* possibly a verbatim mode */
        if (!strcmp(token.id, "verbatim")) {
            token.type = Verbatim;
        }
        else {
            return -1;
        }
        break;
      case 's':         /* possibly a scroll mode */
        if (!strcmp("scroll", token.id)) {
            token.type = Beginscroll;
        }
        else if (!strcmp(token.id, "spadsrc")) {
            token.type = Spadsrc;
        }
        else {
            return -1;
        }
        break;
      case 'i':         /* possibly a item */
        if (!strcmp("items", token.id)) {
            token.type = Beginitems;
        }
        else {
            return -1;
        }
        break;
      default:
        return -1;
    }
    store.type = token.type;
    /* store.id = alloc_string(token.id); */
    get_expected_token(Rbrace);
    token.type = store.type;

    /*
     * strcpy(token.id, store.id); free(store.id);
     */
    return 0;

}
int
begin_type(void)
{
    /*Token store;*/
    int ret_val;

    /*
     * This routine parses a statement of the form \begin{word}. Once it has
     * read the word it tries to assign it a type. Once that is done it sends
     * the word id, and the type to push_be_stack and then returns the type.
     * For the moment I am not even going to use a has_table, although in the
     * future this may be needed
     */
    ret_val = be_type("begin");
    if (ret_val == -1) {
        if (gWindow == NULL || gInVerbatim)
            return 1;
        else {
            fprintf(stderr, "Unknown begin type \\begin{%s} \n", token.id);
            print_page_and_filename();
            print_next_ten_tokens();
            jump();
        }
    }
    else {
        if (gWindow != NULL && !gInVerbatim && token.type != Verbatim
            && token.type != Spadsrc) {
            /* Now here I should push the needed info and then get */
            push_be_stack(token.type, token.id);
        }
        return 1;
    }
    return 1;
}


int
end_type(void)
{
    int ret;

    /*
     * This routine gets the end type just as the begin_type routine does,
     * But then it checks to see if received the proper end_type. By a clever
     * trick, the proper end type is 3000 + type. When environments this will
     * have to change
     */
    ret = be_type("end");
    if (ret == -1) {
        /* unrecognized end token */
        if (gWindow == NULL || gInVerbatim) {
            return 1;
        }
        else {
            fprintf(stderr, "Unknown begin type \\begin{%s} \n", token.id);
            print_page_and_filename();
            print_next_ten_tokens();
            jump();
        }
    }
    else {
        if (gWindow != NULL && !gInVerbatim) {
            check_and_pop_be_stack(token.type, token.id);
            token.type += 3000;
            return 1;
        }
        else {
            if (gWindow != NULL && ((gInVerbatim && token.type == Verbatim) ||
                                (gInSpadsrc && token.type == Spadsrc))) {
                check_and_pop_be_stack(token.type, token.id);
                token.type += 3000;
                return 1;
            }
            else {
                token.type += 3000;
                return 1;
            }
        }
    }
    return 1;
}



static int
keyword_type(void)
{
    Token *token_ent;

    /* first check to see if it is a reserved token */
    token_ent = (Token *) hash_find(&tokenHashTable, token.id);
    if (token_ent != NULL) {
        token.type = token_ent->type;

        /*
         * if I am a keyword I also have to check to see if I am a begin or
         * an end
         */
        if (token.type == Begin)
            return begin_type();
        if (token.type == End)
            return end_type();
        /* next check to see if it is a macro */
    }
    else if (gWindow != NULL) {
        if (hash_find(gWindow->fMacroHashTable, token.id) != NULL)
            token.type = Macro;
        else if (gPageBeingParsed->box_hash != NULL &&
                 hash_find(gPageBeingParsed->box_hash, token.id) != NULL)
        {
            token.type = Boxcond;
        }
        else if (hash_find(gWindow->fCondHashTable, token.id) != NULL)
            token.type = Cond;
        else                    /* We have no idea what we've got */
            token.type = Unkeyword;
    }
    else {                      /* We are probably in htadd so just return. It
                                 * is only concerned with pages anyway */
        token.type = Unkeyword;
    }
    return 0;
}

/* read a token, and report a syntax error if it has the wrong type */
void
get_expected_token(int type)
{
    get_token();
    if (token.type != type) {
        token_name(type);
        fprintf(stderr, "syntax error: expected a %s\n", ebuffer);
        if (token.type == EOF) {
            print_page_and_filename();
            fprintf(stderr, "Unexpected EOF\n");
        }
        else {
            token_name(token.type);
            fprintf(stderr, "not a %s\n", ebuffer);
            print_page_and_filename();
            print_next_ten_tokens();
        }
        longjmp(jmpbuf, 1);
        fprintf(stderr, "Could not jump to Error Page\n");
        exit(-1);
    }
}


#ifndef HTADD
static void
spad_error_handler(void)
{
    /* fprintf(stderr, "got a spad error\n"); */
    longjmp(jmpbuf, 1);
    fprintf(stderr, "(HyperDoc) Fatal Error: Could not jump to Error Page.\n");
    exit(-1);
}

extern int still_reading, str_len;
void
reset_connection(void)
{
    if (spad_socket) {
        FD_CLR(spad_socket->socket, &socket_mask);
        purpose_table[spad_socket->purpose] = NULL;
        close(spad_socket->socket);
        spad_socket->socket = 0;
        spad_socket = NULL;
        if (input_string)
            input_string = "";
        read_again = 0;
        str_len = 0;
        still_reading = 0;
        connect_spad();
    }
}
#endif


/* returns true if spad is currently computing */
int
spad_busy(void)
{
    if (session_server == NULL)
        return 1;
    send_int(session_server, QuerySpad);
    return get_int(session_server);
}

/* connect to FRICAS, return 0 if successful, 1 if not */
int
connect_spad(void)
{
    if (!MenuServerOpened) {
        fprintf(stderr, "(HyperDoc) Warning: Not connected to FriCAS Server!\n");
        LoudBeepAtTheUser();
        return NotConnected;
    }
    if (spad_socket == NULL) {
        spad_socket = connect_to_local_server(SpadServer, MenuServer, Forever);
        if (spad_socket == NULL) {
            fprintf(stderr, "(HyperDoc) Warning: Could not connect to FriCAS Server!\n");
            LoudBeepAtTheUser();
            return NotConnected;
        }
    }
    /* if (spad_busy()) return SpadBusy; */
    return Connected;
}
