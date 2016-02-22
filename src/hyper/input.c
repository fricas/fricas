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

#include "hyper.h"

#include "all_hyper_proto.H1"

static void clear_rbs(InputBox * list);

void
fill_box(Window w,ImageStruct * image)
{
    XClearWindow(gXDisplay, w);
    XPutImage(gXDisplay, w, gWindow->fControlGC,
              image->image.xi, 0, 0, 0, 0,
              image->width,
              image->height);
}

void
toggle_input_box(HyperLink *link)
{
    InputBox *box;

    box = link->reference.box;

    if (box->picked) {
        box->picked = 0;
        unpick_box(box);
    }
    else {
        box->picked = 1;
        pick_box(box);
    }

}
void
toggle_radio_box(HyperLink *link)
{
    InputBox *box;

    box = link->reference.box;

    if (box->picked) {

        /*
         * box->picked = 0; unpick_box(box);
         */
    }
    else {
        /* the first thing I do is clear his buddies */
        clear_rbs(box->rbs->boxes);
        box->picked = 1;
        pick_box(box);
    }
}

static void
clear_rbs(InputBox *list)
{
    InputBox *trace = list;

    while (trace && !trace->picked)
        trace = trace->next;

    if (trace != NULL) {
        trace->picked = 0;
        unpick_box(trace);
    }
}
void
change_input_focus(HyperLink *link)
{
    InputItem *new_item = link->reference.string;
    InputItem *old_item = gWindow->page->current_item;
    XWindowChanges wc;

    /** first thing I should do is see if the user has clicked in the same
      window that I am in                                         ****/
    if (old_item == new_item)
        return;

    /**  Now change the current pointer **/
    gWindow->page->current_item = new_item;

    /** Now I have to change the border width of the selected input window **/
    wc.border_width = 1;
    XConfigureWindow(gXDisplay, new_item->win,
                     CWBorderWidth,
                     &wc);

    wc.border_width = 0;
    XConfigureWindow(gXDisplay, new_item->win,
                     CWBorderWidth,
                     &wc);
    update_inputsymbol(old_item);
    update_inputsymbol(new_item);
}
void
next_input_focus(void)
{
    InputItem *old_item = gWindow->page->current_item, *new_item, *trace;

    if (gWindow->page->current_item == NULL ||
        (gWindow->page->current_item->next == NULL
         && gWindow->page->current_item == gWindow->page->input_list)) {
        BeepAtTheUser();
        return;
    }

    /*
     * Now I should  find the new item
     */
    new_item = NULL;
    trace = old_item->next;

    if (trace == NULL)
        new_item = gWindow->page->input_list;
    else
        new_item = trace;

    gWindow->page->current_item = new_item;
    draw_inputsymbol(old_item);
    draw_inputsymbol(new_item);
}
void
prev_input_focus(void)
{
    InputItem *old_item = gWindow->page->current_item, *new_item, *trace;

    if (gWindow->page->current_item == NULL) {
        BeepAtTheUser();
        return;
    }

    /*
     * Now I should  find the new item
     */
    new_item = NULL;
    trace = gWindow->page->input_list;

    if (trace == old_item) {

        /*
         * I started at the front of the list, so move forward until I hit
         * the end
         */
        while (trace->next != NULL)
            trace = trace->next;
        new_item = trace;
    }
    else {
        while (trace->next != old_item)
            trace = trace->next;
        new_item = trace;
    }

    gWindow->page->current_item = new_item;
    draw_inputsymbol(old_item);
    draw_inputsymbol(new_item);

}

InputItem *
return_item(char *name)
{
    InputItem *list;

    list = gWindow->page->input_list;
    while (list != NULL) {
        if (!strcmp(name, list->name))
            return list;
        list = list->next;
    }
    return NULL;
}
int
delete_item(char *name)
{
    InputItem *list;
    InputItem *prev = NULL;

    list = gWindow->page->input_list;
    while (list != NULL) {
        if (!strcmp(name, list->name)) {
            if (prev)
                prev->next = list->next;
            else
                gWindow->page->input_list = list->next;
            if (gWindow->page->current_item == list)
                gWindow->page->current_item = gWindow->page->input_list;
            free_input_item(list, 1);
            free(list);
            return 1;
        }
        prev = list;
        list = list->next;
    }
    fprintf(stderr, "Can't delete input item %s\n", name);
    return 0;
}
