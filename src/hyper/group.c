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
 * group.c: Routines for managing the HyperDoc group stack.
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/
#define _GROUP_C
#include "debug.h"

#include "hyper.h"

#include "all_hyper_proto.H1"

GroupItem *gTopOfGroupStack = NULL;



int
pop_group_stack(void)
{
    /* This routine pops the top of the current group stack */
    GroupItem *junk;

    /*
     * If the the stack has only a single item, then pop it anyway so the
     * user can see the problem
     */
    if (! gTopOfGroupStack->next)
        return -1;

    /* Else, Pop the thing */

    junk = gTopOfGroupStack;
    gTopOfGroupStack = gTopOfGroupStack->next;
    junk->next = NULL;

    free(junk);

    /* Now change the font to the cur_font and the cur_color */

    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);
    return 1;

}

void
push_group_stack(void)
{
    /*
     * This routine makes room by pushing a new item on the stack
     */
    GroupItem *newgp;

    newgp = (GroupItem *) halloc(sizeof(GroupItem), "Push Group Stack");
    newgp->cur_font = gTopOfGroupStack->cur_font;
    newgp->cur_color = gTopOfGroupStack->cur_color;
    newgp->center = gTopOfGroupStack->center;
    newgp->next = gTopOfGroupStack;

    gTopOfGroupStack = newgp;
}

void
init_group_stack(void)
{
    gTopOfGroupStack = (GroupItem *) halloc(sizeof(GroupItem), "Push Group Stack");
    gTopOfGroupStack->center = 0;
    gTopOfGroupStack->next = NULL;
    gTopOfGroupStack->cur_color = 0;
    gTopOfGroupStack->cur_font = NULL;
}

void
em_top_group(void)
{
    if (! gTopOfGroupStack->next)
        push_group_stack();
    gTopOfGroupStack->cur_color = gEmColor;
    gTopOfGroupStack->cur_font = gEmFont;
    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);
}

void
rm_top_group(void)
{
    if (! gTopOfGroupStack->next)
        push_group_stack();
    gTopOfGroupStack->cur_color = gRmColor;
    gTopOfGroupStack->cur_font = gRmFont;
    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);

}

void
line_top_group(void)
{
    if (! gTopOfGroupStack->next)
        push_group_stack();
    gTopOfGroupStack->cur_color = gBorderColor;
    gTopOfGroupStack->cur_font = gRmFont;
    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);

}

void
bf_top_group(void)
{
    /*
     * Just in case the person is tryin a \em without a grouping
     */

    if (! gTopOfGroupStack->next)
        push_group_stack();
    gTopOfGroupStack->cur_color = gBfColor;
    gTopOfGroupStack->cur_font = gBfFont;
    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);
}

void
tt_top_group(void)
{
    if (! gTopOfGroupStack->next)
        push_group_stack();
    gTopOfGroupStack->cur_color = gTtColor;
    gTopOfGroupStack->cur_font = gTtFont;
    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);
}

void
push_active_group(void)
{
    push_group_stack();
    gTopOfGroupStack->cur_font = gActiveFont;
    gTopOfGroupStack->cur_color = gActiveColor;
    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);
}

void
push_spad_group(void)
{
    push_group_stack();
    gTopOfGroupStack->cur_font = fricas_font;
    gTopOfGroupStack->cur_color = fricas_color;
    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);
}

void
init_top_group(void)
{
    /* clear the group stack */
    while (pop_group_stack() >= 0)
        ;

    /* then set the colors to be normal */

    gTopOfGroupStack->cur_color = gRmColor;
    gTopOfGroupStack->cur_font = gRmFont;
    change_text(gTopOfGroupStack->cur_color, gTopOfGroupStack->cur_font);
}

void
center_top_group(void)
{
    push_group_stack();
    gTopOfGroupStack->center = 1;
}

GroupItem *
copy_group_stack(void)
{
    GroupItem *newgp = NULL;
    GroupItem *first = NULL;
    GroupItem *prev  = NULL;
    GroupItem *trace = gTopOfGroupStack;

    while (trace) {
        newgp = (GroupItem *) halloc(sizeof(GroupItem), "Copy Group Stack");
        newgp->cur_font = trace->cur_font;
        newgp->cur_color = trace->cur_color;
        newgp->center = trace->center;
        if (!first)
            first = newgp;
        else
            prev->next = newgp;
        prev = newgp;
        trace = trace->next;
    }
    if (newgp)
        newgp->next = NULL;
    return first;
}

void
free_group_stack(GroupItem *g)
{
    GroupItem *trace = g;

    while (trace) {
        GroupItem *junk = trace;
        trace = trace->next;
        free(junk);
    }
}
