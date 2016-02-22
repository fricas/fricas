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

/******************************************************************************
 *
 * parse-types.h: HyperDoc parsing routines for node types.
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"

#include "parse.h"
#include "parse-types.h"
#include "hyper.h"
#include "lex.h"
#include "extent.h"
#include "hterror.h"

#include "all_hyper_proto.H1"

static void parse_condnode(void);
static void parse_hasreturnto(void);

boolean gInButton = FALSE;
boolean gInIf = FALSE;
boolean gInItems = FALSE;
boolean gInOptional = FALSE;

void
parse_ifcond(void)
{
    TextNode *ifnode = curr_node;
    TextNode *endif;
    TextNode *condnode;

    /*
     * parse a conditional. At first I am just going to parse if
     * <hypertext> fi
     */
    if (gInIf) {
        curr_node->type = Noop;
        fprintf(stderr, "\\if found within \\if \n");
        longjmp(jmpbuf, 1);
        fprintf(stderr, "Longjump failed, Exiting\n");
        exit(-1);
    }
    gInIf++;
    curr_node->type = Ifcond;
    curr_node->space = token.id[-1];
    curr_node->data.ifnode = alloc_ifnode();
    /* Now get the cond node I hope */

    condnode = curr_node->data.ifnode->cond = alloc_node();
    curr_node = condnode;
    parse_condnode();

    endif = alloc_node();
    endif->type = Endif;
    ifnode->data.ifnode->thennode = alloc_node();
    curr_node = ifnode->data.ifnode->thennode;
    parse_HyperDoc();
    if (token.type == Fi) {
        curr_node->type = Fi;
        curr_node->next = endif;
        ifnode->data.ifnode->elsenode = endif;
    }
    else if (token.type == Else) {
        /* first finish up the then part */
        curr_node->type = Fi;
        curr_node->next = endif;
        /* the go and parse the else part */
        ifnode->data.ifnode->elsenode = alloc_node();
        curr_node = ifnode->data.ifnode->elsenode;
        parse_HyperDoc();
        if (token.type != Fi) {
            token_name(token.type);
            curr_node->type = Noop;
            fprintf(stderr, "Expected a \\fi not a %s", ebuffer);
            longjmp(jmpbuf, 1);
            fprintf(stderr, "Longjump failed, Exiting\n");
            exit(-1);
        }
        curr_node->type = Fi;
        curr_node->next = endif;
    }
    else {
        curr_node->type = Noop;
        token_name(token.type);
        fprintf(stderr, "Expected a \\fi not a %s", ebuffer);
        longjmp(jmpbuf, 1);
        fprintf(stderr, "Longjump failed, Exiting\n");
        exit(-1);
    }
    ifnode->next = ifnode->data.ifnode->thennode;
    ifnode->width = -1;         /* A flag for compute if extents */
    curr_node = endif;
    gInIf--;
}

static void
parse_condnode(void)
{
    get_token();

    switch (token.type) {
      case Cond:
        curr_node->type = Cond;
        curr_node->data.text = alloc_string(token.id);
        break;
      case Haslisp:
      case Hasreturn:
      case Lastwindow:
      case Hasup:
        curr_node->type = token.type;
        break;
      case Boxcond:
        curr_node->type = Boxcond;
        curr_node->data.text = alloc_string(token.id);
        break;
      case Hasreturnto:
        parse_hasreturnto();
        break;
      default:
        {
            char eb[128];
            token_name(token.type);
            sprintf(eb, "Unexpected Token %s\n", eb);
            htperror(eb, HTCONDNODE);
        }
        break;
    }
}

static void
parse_hasreturnto(void)
{
    TextNode *hrt = curr_node, *arg_node = alloc_node();

    curr_node->type = Hasreturnto;
    curr_node = arg_node;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    curr_node->type = Endarg;
    hrt->data.node = arg_node;
    curr_node = hrt;
}

void
parse_newcond(void)
{
    char label[256];

    get_expected_token(Lbrace);
    get_expected_token(Unkeyword);
    strcpy(label, token.id);
    get_expected_token(Rbrace);
    insert_cond(label, "0");
    curr_node->type = Noop;
}

void
parse_setcond(void)
{
    char label[256], cond[256];

    get_expected_token(Lbrace);
    get_expected_token(Cond);
    strcpy(label, token.id);
    get_expected_token(Rbrace);
    get_expected_token(Lbrace);
    get_expected_token(Word);
    strcpy(cond, token.id);
    get_expected_token(Rbrace);
    change_cond(label, cond);
    curr_node->type = Noop;
}

void
parse_begin_items(void)
{
    TextNode *bi = curr_node;

    /*
     * This procedure parses a begin item. It sets the current
     * node and sees if there is an optional argument for the itemspace
     */

    bi->type = token.type;
    get_token();
    if (token.type == Lsquarebrace) {
        bi->data.node = alloc_node();
        curr_node = bi->data.node;
        gInOptional++;
        parse_HyperDoc();
        gInOptional--;
        curr_node->type = Enddescription;
        if (token.type != Rsquarebrace) {
            fprintf(stderr, "(HyperDoc) Optional arguments must end with ].\n");
            print_next_ten_tokens();
            print_page_and_filename();
            jump();
        }
        curr_node = bi;
    }
    else
        unget_token();
    gInItems++;
}

void
parse_item(void)
{
    if (!gInItems) {
        fprintf(stderr, "\\item found outside an items environment\n");
        print_page_and_filename();
        print_next_ten_tokens();
        jump();
    }
    curr_node->type = Item;
    get_token();
    if (token.type == Lsquarebrace) {
        /* I should parse the optional argument */
        curr_node->next = alloc_node();
        curr_node = curr_node->next;
        curr_node->type = Description;
        curr_node->next = alloc_node();
        curr_node = curr_node->next;
        gInOptional++;
        parse_HyperDoc();
        gInOptional--;
        curr_node->type = Enddescription;
        if (token.type != Rsquarebrace) {
            fprintf(stderr, "(HyperDoc) Optional arguments must end with ].\n");
            print_next_ten_tokens();
            print_page_and_filename();
            jump();
        }
    }
    else {
        unget_token();
    }
}

void
parse_mitem(void)
{
    if (!gInItems) {
        fprintf(stderr, "\\mitem found outside an items environment\n");
        print_page_and_filename();
        print_next_ten_tokens();
        jump();
    }
    curr_node->type = Mitem;
}

char *vbuf = NULL;
int vbuf_size = 0;

#define VbufSlop 10
#define resizeVbuf()\
  if (size == vbuf_size) { \
                             vbuf = resizeBuffer(size + VbufSlop, vbuf, &vbuf_size); \
                               vb = vbuf + size; \
                               }

#define new_verb_node() \
  resizeVbuf(); \
  *vb = '\0'; \
  curr_node->data.text = alloc_string(vbuf); \
  curr_node->next = alloc_node(); \
  curr_node = curr_node->next; \
  curr_node->type = Newline; \
  curr_node->next = alloc_node(); \
  curr_node = curr_node->next; \
  curr_node->type = type; \
  if (*end_string == '\n') es = end_string+1; \
  else es = end_string; \
  size = 0; \
  vb = vbuf;

void
parse_verbatim(int type)
{
    int size = 0, c;
    char *end_string, *vb = vbuf, *es;

    curr_node->type = type;
    if (token.id[-1])
        curr_node->space = 1;
    if (type == Spadsrctxt) {
        es = end_string = "\n\\end{spadsrc}";
    }
    else if (type == Math)
        es = end_string = "$";
    else
        es = end_string = "\\end{verbatim}";
    while ((c = get_char()) != EOF) {
        resizeVbuf();
        size++;
        if (c == '\n') {
            new_verb_node();
            continue;
        }
        *vb++ = c;
        if (*es++ != c)
            es = end_string;
        if (!*es)
            break;
    }
    if (c == EOF) {
        fprintf(stderr, "parse_verbatim: Unexpected EOF found\n");
        longjmp(jmpbuf, 1);
    }
    resizeVbuf();
    if (*end_string == '\n')
        es = end_string + 1;
    else
        es = end_string;
    vbuf[size - strlen(es)] = '\0';
    if (*vbuf) {
        curr_node->data.text = alloc_string(vbuf);
        curr_node->next = alloc_node();
        curr_node = curr_node->next;
    }
    if (type == Spadsrctxt)
        curr_node->type = Endspadsrc;
    else if (type == Math)
        curr_node->type = Endmath;
    else
        curr_node->type = Endverbatim;
}

void
parse_input_pix(void)
{
    TextNode *pixnode;
    char *filename;

    pixnode = curr_node;
    pixnode->type = token.type;
    pixnode->space = token.id[-1];
    pixnode->width = -1;
    get_expected_token(Lbrace);
    filename = get_input_string();
    pixnode->data.text = alloc_string(filename);
    curr_node = pixnode;
    if (pixnode->type == Inputimage) {
        char f[256];
        char *p;

        if ((gXDisplay && DisplayPlanes(gXDisplay, gXScreenNumber) == 1) || gSwitch_to_mono ==1) {
            pixnode->type = Inputbitmap;
            strcpy(f, pixnode->data.text);
            strcat(f, ".bm");
            p=pixnode->data.text;
            pixnode->data.text = alloc_string(f);
            free(p);
        }
        else {
            pixnode->type = Inputpixmap;
            strcpy(f, pixnode->data.text);
#ifdef OLD
            strcat(f, ".pm");
#endif
            strcat(f, ".xpm");
            p=pixnode->data.text;
            pixnode->data.text = alloc_string(f);
            free(p);
        }
    }
}

void
parse_centerline(void)
{
    curr_node->type = token.type;
    curr_node->space = token.id[-1];
    curr_node->width = -1;
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    if (token.type != Rbrace) {
        curr_node->type = Noop;
        fprintf(stderr, "(HyperdDoc) \\centerline was expecting a }\n");
        print_page_and_filename();
        print_next_ten_tokens();
        longjmp(jmpbuf, 1);
    }
    curr_node->type = Endcenter;
}

void
parse_command(void)
{
    TextNode *link_node, *save_node, *arg_node;

    gInButton++;
    if (gParserMode == SimpleMode) {
        curr_node->type = Noop;
        fprintf(stderr, "Parser Error token %s unexpected\n",
                token_table[token.type]);
        longjmp(jmpbuf, 1);
    }
    gStringValueOk = 1;

    /* set the values for the current node */
    curr_node->type = token.type;
    curr_node->space = token.id[-1];

    /* now parse for the label */
    link_node = curr_node;
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    curr_node->type = Endbutton;
    save_node = curr_node;
    arg_node = alloc_node();
    curr_node = arg_node;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    curr_node->type = Endarg;
    link_node->link = make_link_window(arg_node, link_node->type, 0);
    gStringValueOk = 0;
    curr_node = save_node;
    gInButton--;
}

void
parse_button(void)
{
    TextNode *link_node, *save_node;

    gInButton++;
    if (gParserMode == SimpleMode) {
        curr_node->type = Noop;
        fprintf(stderr, "Parser Error token %s unexpected\n",
                token_table[token.type]);
        longjmp(jmpbuf, 1);
    }
    /* fill the node */
    curr_node->type = token.type;
    curr_node->space = token.id[-1];

    /* the save the current node for creating the link and stuff */
    link_node = curr_node;

    /* then parse the label */
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    curr_node->type = Endbutton;

    /* now try to get the argument node */
    save_node = curr_node;
    get_expected_token(Lbrace);
    save_node->data.node = alloc_node();
    curr_node = save_node->data.node;
    parse_HyperDoc();
    curr_node->type = Endarg;

    /*
     * buffer[0] = '\0'; print_to_string(arg_node, buffer + 1);
     */
    link_node->link =
        make_link_window(save_node->data.node, link_node->type, 0);
    curr_node = save_node;
    gInButton--;
}

extern int example_number;

void
parse_spadcommand(TextNode *spad_node)
{
    /*TextNode *node = NULL;*/

    example_number++;
    gInButton++;
    spad_node->type = token.type;
    spad_node->space = token.id[-1];
    get_expected_token(Lbrace);
    cur_spadcom = curr_node;

    spad_node->next = alloc_node();
    curr_node = spad_node->next;
    parse_HyperDoc();
    curr_node->type = Endspadcommand;
    cur_spadcom = NULL;
    spad_node->link = make_link_window(spad_node->next, spad_node->type, 1);
    gInButton--;
}

void
parse_spadsrc(TextNode *spad_node)
{
    char buf[512], *c = buf;
    int ch, start_opts = 0;
    /*TextNode *node = NULL;*/

    example_number++;
    gInButton++;
    gInSpadsrc++;
    spad_node->type = Spadsrc;
    spad_node->space = token.id[-1];

    cur_spadcom = curr_node;
    spad_node->next = alloc_node();
    curr_node = spad_node->next;

    do {
        ch = get_char();
        if (ch == ']')
            start_opts = 0;
        if (start_opts)
            *c++ = ch;
        if (ch == '[')
            start_opts = 1;
    } while (ch != '\n');
    *c = '\0';
    parse_verbatim(Spadsrctxt);
    parse_from_string(buf);

    curr_node->type = Endspadsrc;
    cur_spadcom = NULL;
    spad_node->link = make_link_window(spad_node->next, Spadsrc, 1);
    gInButton--;
    gInSpadsrc--;
}

void
parse_env(TextNode *node)
{
    char *env;
    int  noEnv = 0;

    get_expected_token(Lbrace);
    get_expected_token(Word);
    env = getenv(token.id);

    if (env == NULL) {
        /** The environment variable was not found **/

        fprintf(stderr, "(HyperDoc) Warning: environment variable \'%s\' was not found.\n",
                token.id);

        env = halloc(1, "string");
        env[0] = '\0';
        noEnv = 1;
    }

    node->data.text = alloc_string(env);
    node->type = Word;

    if (noEnv) {
        free(env);
    }
    get_expected_token(Rbrace);
}

/*
 * This parse_value routine accepts an empty {} but makes it a zero instead
 * of a one. Thus \indent{} is equivelant to \indent{0}
 */

void
parse_value1(void)
{
    TextNode *value_node, *ocn = curr_node;
    char *s;

    curr_node->type = token.type;
    curr_node->space = token.id[-1];

    value_node = alloc_node();
    value_node->type = Word;
    curr_node->data.node = value_node;
    get_expected_token(Lbrace);
    s = get_input_string();
    if (!is_number(s)) {
        fprintf(stderr,
           "Parser Error: parse for value was expecting a numeric value\n");
        strcpy(value_node->data.text, "0");
    }
    else {
        value_node->data.text = alloc_string(s);
    }
    curr_node = ocn;
}

/*
 * This command accepts an empty argument command. Thus \space{} is
 * equivelant \space{1}
 */

void
parse_value2(void)
{
    TextNode *value_node, *ocn = curr_node;
    char *s;

    curr_node->type = token.type;
    curr_node->space = token.id[-1];

    value_node = alloc_node();
    value_node->type = Word;
    curr_node->data.node = value_node;
    get_expected_token(Lbrace);
    s = get_input_string();
    if (!is_number(s)) {
        fprintf(stderr,
           "Parser Error: parse for value was expecting a numeric value\n");
        strcpy(value_node->data.text, "1");
    }
    else {
        value_node->data.text = alloc_string(s);
    }
    curr_node = ocn;
}


/* parse a \table sommand */

void
parse_table(void)
{
    TextNode *tn = curr_node;

    if (gParserMode != AllMode) {
        curr_node->type = Noop;
        fprintf(stderr, "Parser Error token %s unexpected\n",
                token_table[token.type]);
        longjmp(jmpbuf, 1);
    }
    curr_node->type = Table;
    get_expected_token(Lbrace);
    curr_node->next = alloc_node();
    curr_node = curr_node->next;

    get_token();
    if (token.type == Lbrace) {
        while (token.type != Rbrace) {
            curr_node->type = Tableitem;
            curr_node->next = alloc_node();
            curr_node = curr_node->next;
            parse_HyperDoc();
            curr_node->type = Endtableitem;
            curr_node->next = alloc_node();
            curr_node = curr_node->next;
            get_token();
        }
        curr_node->type = Endtable;
    }
    else {                      /* a patch for SG for empty tables */
        if (token.type != Rbrace) {
            token_name(token.type);
            fprintf(stderr,
                    "Unexpected Token %s found while parsing a table\n",
                    ebuffer);
            print_page_and_filename();
            jump();
        }
        tn->type = Noop;
        tn->next = NULL;
        free(curr_node);
        curr_node = tn;
    }
}

void
parse_box(void)
{
    curr_node->type = token.type;
    curr_node->space = token.id[-1];
    curr_node->width = -1;
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    curr_node->type = Endbox;
}

void
parse_mbox(void)
{
    curr_node->type = token.type;
    curr_node->space = token.id[-1];
    curr_node->width = -1;
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    curr_node->type = Endbox;
}

void
parse_free(void)
{
    TextNode *free_node = curr_node;

    curr_node->type = token.type;
    curr_node->space = token.id[-1];
    curr_node->width = -1;
    curr_node->data.node = alloc_node();
    curr_node = curr_node->data.node;
    get_expected_token(Lbrace);
    parse_HyperDoc();
    curr_node->type = Endarg;
    curr_node = free_node;
}

void
parse_help(void)
{
    curr_node->type = Noop;
    get_token();
    if (token.type != Lbrace) {
        token_name(token.type);
        fprintf(stderr, "\\helppage was expecting a { and not a %s\n", ebuffer);
        print_page_and_filename();
        jump();
    }

/* before we clobber this pointer we better free the contents (cf. alloc_page) */
    free(gPageBeingParsed->helppage);
    gPageBeingParsed->helppage = alloc_string(get_input_string());

    if (token.type != Rbrace) {
        token_name(token.type);
        fprintf(stderr, "\\helppage was expecting a } and not a %s\n",
                ebuffer);
        print_page_and_filename();
        jump();
    }
}
