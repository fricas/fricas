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
#include "hyper.h"

#include "all_hyper_proto.H1"

static void get_parameter_strings(int number, char * macro_name);

/* #define DEBUG 1 */
extern FILE *cfile;


/*
 * This routine keeps scanning until it reaches it pops off 1 more
 * right brace then left brace
 */
void
scan_HyperDoc(void)
{
    HDWindow *twin = gWindow;
    int ret_val;
    int number_of_left_braces = 1;

    gWindow = NULL;
    while (number_of_left_braces) {
        ret_val = get_token();
        if (ret_val == EOF && number_of_left_braces) {
            fprintf(stderr, "Scan_Hypertex: Unexpected End of File\n");
            longjmp(jmpbuf, 1);
        }
        switch (token.type) {
          case Page:
            fprintf(stderr, "scan_HyperDoc: Unexpected Page Declaration\n");
            break;
          case NewCommand:
            fprintf(stderr, "scan_HyperDoc: Unexpected Macro Declaration\n");
            break;
          case Lbrace:
            number_of_left_braces++;
            break;
          case Endpatch:
          case Rbrace:
            number_of_left_braces--;
            break;
          default:
            break;
        }
    }
    gWindow = twin;
}

int
number(char *str)
{
    char *t = str;

    while (*t)
        if (!isdigit(*t++))
            return 0;
    return 1;
}

/* Parse a given macro given the pointer to the unlaoded macro ** */

static char *
load_macro(MacroStore *macro)
{
    int ret_val;
    long start_fpos;
    int size = 0;
    char *trace;
    char *macro_buff;

    save_scanner_state();
    cfile = find_fp(macro->fpos);


    init_scanner();

    /** First thing I should do is make sure that the name is correct ***/
    get_expected_token(NewCommand);
    get_expected_token(Lbrace);
    get_expected_token(Macro);
    if (strcmp(token.id, macro->name)) {
        /** WOW, Somehow I had the location of the wrong macro **/
        fprintf(stderr, "Expected macro name %s got insted %s in load_macro\n",
                macro->name, token.id);
        longjmp(jmpbuf, 1);
    }
    get_expected_token(Rbrace);

    /** Next I should check to see if I have any parameters **/
    get_token();
    if (token.type == Lsquarebrace) {
        /** The person is telling me the number of macros he is going to use **/
        get_expected_token(Word);
        if (!number(token.id)) {
            fprintf(stderr, "load_macro: Expected A Value Instead Got %s\n",
                    token.id);
            longjmp(jmpbuf, 1);
        }
        /** if it is a number, then I should store it in the parameter number
          member of the macro structure **/
        macro->number_parameters = atoi(token.id);
#ifdef DEBUG
        fprintf(stderr,
              "The number of parameters is %d\n", macro->number_parameters);
#endif
        get_expected_token(Rsquarebrace);
        get_token();
    }
    else
        macro->number_parameters = 0;

    /*** Now I should be able to check the token, and insure that I have read
      a leftbrace, then the string will follow                    ****/
    if (token.type != Lbrace) {
        /** The macro is not in a group, uh oh **/
        fprintf(stderr, "load_macro:Expected a Left Brace got type %d\n",
                token.type);
        longjmp(jmpbuf, 1);
    }
    start_fpos = fpos;
    scan_HyperDoc();
    ret_val = fseek(cfile, macro->fpos.pos + start_fpos, 0);
    size = fpos - start_fpos;
    macro_buff = (char *) halloc((size + 1) * sizeof(char), "Macro_buf");
    for (size = 0, trace = macro_buff; size < fpos - (start_fpos) - 1; size++)
        *trace++ = getc(cfile);
    *trace = '\0';
    macro->loaded = 1;
    restore_scanner_state();
    return macro_buff;
}


/** Here are the functions and declarations for the parameter stack **/
ParameterList parameters = NULL;

ParameterList
init_parameter_elem(int number)
{
    ParameterList new;
    int count;

    /** allocate the space neeeded **/
    new = (ParameterList) halloc(sizeof(struct parameter_list_type),
                                 "ParameterList");
    /** now allocate the memeory  for the pointers to the  parameters **/
    if (number) {
        new->list = (char **) halloc(number * sizeof(char *), "Parameter List");

        /** initialize my pointers **/
        for (count = 0; count < number; count++)
            (new->list)[count] = NULL;
    }
    new->number = number;
    return new;
}

int
push_parameters(ParameterList new)
{

    if (new == NULL) {
        fprintf(stderr, "Tried pushing a null list onto the parameter stack\n");
        longjmp(jmpbuf, 1);
    }

    new->next = parameters;
    parameters = new;
    return 1;
}
int
pop_parameters(void)
{
    /** Simply pops the top of the parameter list, being good and freeing
      all the memory **/
    ParameterList old;
    int count;

    if (!parameters) {
        return 0;
    }

    old = parameters;
    parameters = old->next;

    /** Free the parameter text and pointers **/
    if (old->number >0) {
        for (count = 0; count < old->number; count++)
                if ( (old->list)[count] )  free((char *) (old->list)[count]);
        free(old->list);
        }

    free(old);                  /** free the parameter **/

    return 1;
}

int
parse_macro(void)
{

    /*
     * This routine loads a macro if needed, and then parses it from the
     * string
     */
    MacroStore *macro;
    int s;

    curr_node->type = Macro;
    curr_node->space = token.id[-1];
    curr_node->next = alloc_node();
    curr_node = curr_node->next;
    macro = (MacroStore *) hash_find(gWindow->fMacroHashTable, token.id);
    if (macro != NULL) {
        if (!macro->loaded)
            macro->macro_string = load_macro(macro);
        get_parameter_strings(macro->number_parameters, macro->name);
        parse_from_string(macro->macro_string);
        if (gEndedPage) {
            s = curr_node->type;
            curr_node->type = Endmacro;
            curr_node->next = alloc_node();
            curr_node = curr_node->next;
            curr_node->type = s;
        }
        else
            curr_node->type = Endmacro;
        if (pop_parameters())
            return 1;
        else {
            fprintf(stderr,
                    "parse_macro: Tried to pop an empty paramter stack\n");
            longjmp(jmpbuf, 1);
        }
    }
    else {
        fprintf(stderr, "parse_macro: Unknown keyword %s\n", token.id);
        longjmp(jmpbuf, 1);
    }
}

#define numeric(c) ((c >= '0' && c <= '9')?1:0)

static void
get_parameter_strings(int number,char * macro_name)
{
    static char buffer[4096];
    char *buffer_pntr;
    int count;
    int lbrace_counter;
    char c;
    int size;
    ParameterList new = init_parameter_elem(number);
    int pnum;
    char pnum_chars[5];
    int pc;

    if (!number) {              /* nothing to be done */
        push_parameters(new);
        return;
    }
    for (count = 0; count < number; count++) {
        get_token();
        if (token.type != Lbrace) {
            /** The macro is not in a group, uh oh **/
            fprintf(stderr, "Wrong number of arguments to the macro %s\n",
                    macro_name);
            jump();
        }
        for (lbrace_counter = 1, buffer_pntr = buffer;
             lbrace_counter;) {
            switch (c = get_char()) {
              case EOF:
                fprintf(stderr, "GetParameterStrings: Unexpected EOF\n");
                longjmp(jmpbuf, 1);
              case '}':
                lbrace_counter--;
                if (lbrace_counter)
                    *buffer_pntr++ = c;
                break;
              case '{':
                lbrace_counter++;
                *buffer_pntr++ = c;
                break;
              case '#':
                /* uh oh, I have a paramter reference inside a paramter */
                /* get the number */
                if (parameters == NULL) {
                    *buffer_pntr++ = c;
                    break;
                }
                if (
                    ((buffer_pntr > buffer + 1) &&
                     *(buffer_pntr - 1) == '\\' &&
                     *(buffer_pntr - 2) != '\\') ||
                    ((buffer_pntr > buffer) &&
                     *(buffer_pntr - 1) == '\\')) {
                    /* I had a \# */
                    *buffer_pntr++ = c;
                }
                else {
                    c = get_char();
                    for (pc = 0; numeric(c); pc++) {
                        pnum_chars[pc] = c;
                        c = get_char();
                    }
                    unget_char(c);
                    if (pc == 0) {
                        fprintf(stderr, "No number after parameter marker\n");
                        jump();
                    }
                    pnum_chars[pc] = '\0';
                    pnum = atoi(pnum_chars);
                    pc = 0;
                    if (pnum > parameters->number || pnum == 0) {
                        /** had a bad parameter number **/
                        fprintf(stderr,
                            "Parse_parameter: Had a bad parameter number %d\n",
                            pnum);
                        jump();
                    }
                    /* Now copy the paramter */
                    while ((parameters->list)[pnum - 1][pc] != '\0')
                        *buffer_pntr++ = (parameters->list)[pnum - 1][pc++];
                }
                break;
              default:
                *buffer_pntr++ = c;
                break;
            }
        }
        *buffer_pntr = '\0';
        /*** Now add it to the current parameter list **/
        size = strlen(buffer) + 1;
        new->list[count] = (char *) halloc(size, "Parameter Strings");
        strcpy(new->list[count], buffer);
    }
    push_parameters(new);
    return ;
}
void
parse_parameters(void)
{
    int value;

    if (!number(token.id)) {
        fprintf(stderr,
                "Parse_parameter: Error Expected a number, got %s instead\n", token.id);
        longjmp(jmpbuf, 1);
    }

    if (!parameters) {
        fprintf(stderr,
                "Parse_parameter: no parameters\n");
        longjmp(jmpbuf, 1);
    }

    if ((value = atoi(token.id)) > parameters->number) {
        /** had a bad parameter number **/
        fprintf(stderr,
                "Parse_parameter: Had a bad parameter number %d\n", value);
        longjmp(jmpbuf, 1);
    }

    parse_from_string((parameters->list)[value - 1]);
    curr_node->type = Endparameter;
    return;
}
