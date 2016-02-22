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
 * cond.c:  Routines for handling "cond" nodes.
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"

#include "hyper.h"

#include "all_hyper_proto.H1"
#include "sockio-c.H1"


void
insert_cond(char *label, char *cond)
{
    CondNode *condnode = (CondNode *) hash_find(gWindow->fCondHashTable, label);

    /*
     * This routine creates a new cond node and inserts it into the
     * current cond table
     */
    if (condnode) {
        fprintf(stderr, "Error: \\%s is declared twice \n", label);
        print_page_and_filename();
        jump();
    }
    condnode = alloc_condnode();
    condnode->label = halloc(strlen(label) + 1, "Condnode->label");
    condnode->cond = halloc(strlen(cond) + 1, "Condnode->cond");
    strcpy(condnode->label, label);
    strcpy(condnode->cond, cond);
    hash_insert(gWindow->fCondHashTable, (char *) condnode, condnode->label);
}

void
change_cond(char *label, char *newcond)
{
    CondNode *condnode = (CondNode *) hash_find(gWindow->fCondHashTable, label);

    if (condnode == NULL) {
        fprintf(stderr, "Error: Tried to set an uncreated cond %s\n", label);
    }
    else {
        free(condnode->cond);
        condnode->cond = halloc(strlen(newcond) + 1, "Condnode->cond");
        strcpy(condnode->cond, newcond);
    }
}

static int
check_memostack(TextNode *node)
{
    char *buffer;
    int stackp = gWindow->fMemoStackIndex;
    int found = 0;
    HyperDocPage *page;

    buffer = print_to_string(node->data.node);

    /*
     * Once we have done that much, search down the stack for the
     * proper page
     */

    while (!found && stackp > 0) {
        page = gWindow->fMemoStack[--stackp];
        if (!strcmp(page->name, buffer))
            found = 1;
    }
    return found;
}

int
check_condition(TextNode *node)
{
    CondNode *cond;
    InputBox *box;
    int ret_val;

    /* checks the condition presented and returns a 1 or a 0 */
    switch (node->type) {
      case Cond:
        cond = (CondNode *) hash_find(gWindow->fCondHashTable, node->data.text);
        if (!strcmp("0", cond->cond))
            return 0;
        else
            return 1;
      case Boxcond:
        box = (InputBox *) hash_find(gWindow->page->box_hash, node->data.text);
        return (box->picked);
      case Haslisp:
        if (spad_socket != NULL) {
            ret_val = send_int(spad_socket, TestLine);
            return (ret_val + 1);
        }
        else
            return 0;
      case Hasup:
        return need_up_button;
      case Hasreturn:
        return gWindow->fMemoStackIndex;
      case Hasreturnto:
        return (check_memostack(node));
      case Lastwindow:
        return (gSessionHashTable.num_entries == 1 || gParentWindow == gWindow);
      default:
        return 0;
    }
}
