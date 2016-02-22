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
 * extent1.h:  HyperDoc extent computation routines
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"


#include "extent.h"
#include "hyper.h"

#include "all_hyper_proto.H1"

static void compute_begin_items_extent(TextNode * node);
static void compute_bf_extent(TextNode * node);
static void compute_box_extent(TextNode * node);
static void compute_button_extent(TextNode * node);
static void compute_center_extent(TextNode * node);
static void compute_em_extent(TextNode * node);
static void compute_ifcond_extent(TextNode * node);
static void compute_image_extent(TextNode * node);
static void compute_ir_extent(TextNode * node);
static void compute_it_extent(TextNode * node);
static void compute_item_extent(TextNode * node);
static void compute_mbox_extent(TextNode * node);
static void compute_mitem_extent(TextNode * node);
static void compute_paste_extent(TextNode * node);
static void compute_pastebutton_extent(TextNode * node);
static void compute_rm_extent(TextNode * node);
static void compute_spadcommand_extent(TextNode * node);
static void compute_spadsrc_extent(TextNode * node);
static void compute_table_extent(TextNode * * node);
static void end_spadcommand_extent(TextNode * node);
static void end_spadsrc_extent(TextNode * node);
static void endbutton_extent(TextNode * node);
static void endif_extent(TextNode * node);
static void endpastebutton_extent(TextNode * node);

/*
 * Now we declare all the values which are shared among the extent routines
 * and the showing routines
 */

int noop_count;

TextNode *link_node = NULL;
TextNode *paste_node = NULL;
TextNode *spad_node = NULL;
TextNode *if_node = NULL;


short int gExtentRegion;


short int gInDesc;
short int gInLine;               /* true iff there have been words printed  */
short int gInItem;               /* true iff we are in a \item */
short int in_fricas_command;          /* true iff we are in a \spadcommand */
short int gInTable;

/* Variables for the formatting state */

int right_margin_space;
int right_margin;
int indent;
int item_indent;
int text_x;
int text_y;
int y_off;
int scroll_bot;
int need_scroll_up_button;
int need_scroll_down_button;
int item_space;
int present_line_height;
int past_line_height;
int line_height;                /* space between lines              */
int normal_text_height;         /* space between lines              */
int space_width;                /* the maximum width of a character */
int word_off_height;            /* the diff between text height and */

TextNode *gLineNode;

/*
 * Computes the extent of the input string or box
 */

static void
compute_input_extent(TextNode * node)
{
    InputItem *item;
    int t_width;
    int num_lines;

    /* search the symbol table for the proper entry */

    item = node->link->reference.string;
    num_lines = item->num_lines;

    /*
     * Once we have gotten this far, we should just be able to calculate the
     * width using the normal font
     */

    t_width = (item->size + 1) * gInputFont->max_bounds.width + 10;

    if (gInLine)
        text_x += inter_word_space;

    if (text_x + t_width > right_margin) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;

    /* now figure out the height of the current window */

    node->height = line_height * (num_lines);
    node->y = text_y - line_height + node->height - 1;
    if (node->height > present_line_height)
        present_line_height = plh(node->height);
    node->width = t_width;
    gInLine = 1;
    text_x += t_width;
}

static void
compute_punctuation_extent(TextNode * node)
{
    int twidth;
    int nextwidth;
    int incwidth;

    node->height = normal_text_height;
    node->width = strlen(node->data.text);
    incwidth = twidth = XTextWidth(gTopOfGroupStack->cur_font, node->data.text,
                                   node->width);

    /* always check to see if there was some space in front of us */

    if (gInLine && (node->space & FRONTSPACE))
        twidth += inter_word_space;

    /*
     * now calcualte the width of the next one if it needs to be considered
     */

    if (!(node->space & BACKSPACE))
        nextwidth = total_width(node->next, Endtokens);
    else
        nextwidth = 0;

    if ((!(node->space & BACKSPACE)) &&
        (text_x + twidth + nextwidth > right_margin) && gInLine) {
        start_newline(present_line_height, node);
        if (in_fricas_command) {
            text_x = indent + spadcom_indent;
        }
        else
            text_x = indent;
    }

    if (node->space & FRONTSPACE)
        text_x += inter_word_space;

    node->x = text_x;

    /*
     * Now try to see if we should leave space after myself. Always leave
     * space when there is space
     */

    if (node->space & BACKSPACE) {
        switch (node->data.text[0]) {
          case '.':
          case '?':
          case '!':
            text_x += term_punct_space;
            break;
        }
    }

    text_x += incwidth;
    node->y = text_y - word_off_height;
    gInLine = 1;
}

static void
compute_word_extent(TextNode * node)
{
    int twidth;
    int nextwidth;
    int incwidth;

    node->height = normal_text_height;
    node->width = strlen(node->data.text);
    incwidth = twidth = XTextWidth(gTopOfGroupStack->cur_font, node->data.text,
                                   node->width);

    /*
     * Now if we should drop some space in front of me, then add it to twidth
     */

    if (gInLine && node->space)
        twidth += inter_word_space;

    /*
     * Now what we should do is find all the things after us that have no
     * space in front and add there width on.
     */

    nextwidth = total_width(node->next, Endtokens);


    /*
     * Should we start a new line?
     */

    if (text_x + twidth + nextwidth > right_margin && gInLine) {
        start_newline(present_line_height, node);
        if (in_fricas_command) {
            text_x = indent + spadcom_indent;
        }
        else
            text_x = indent;
    }

    /*
     * Now see if we am on the beginning of a line, and if not add some space
     * if we need to
     */

    if (gInLine && node->space)
        text_x += inter_word_space;

    node->x = text_x;
    node->y = text_y - word_off_height;
    text_x += incwidth;
    gInLine = 1;
}

static void
compute_verbatim_extent(TextNode *node)
{
    node->height = normal_text_height;
    node->width = strlen(node->data.text);

    node->x = text_x;
    node->y = text_y - word_off_height;
    gInLine = 1;
    return;
}

static void
compute_spadsrctxt_extent(TextNode *node)
{
    node->height = normal_text_height;
    node->width = strlen(node->data.text);

    if (gInLine) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    node->y = text_y - word_off_height;
    gInLine = 1;
    return;
}

static void
compute_dash_extent(TextNode *node)
{
    int num_dashes;
    int twidth;
    int nextwidth;

    node->height = normal_text_height;

    num_dashes = strlen(node->data.text);

    if (num_dashes > 1)
        twidth = node->width = num_dashes * dash_width;
    else
        twidth = node->width = XTextWidth(gTopOfGroupStack->cur_font,
                                          node->data.text, 1);

    if (gInLine && node->space)
        twidth += inter_word_space;

    /*
     * Now what we should do is find all the things after us that have no
     * space in front and add there width on.
     */

    nextwidth = total_width(node->next, Endtokens);

    /*
     * Should we start a new line?
     */

    if (text_x + twidth + nextwidth > right_margin) {
        start_newline(present_line_height, node);
        if (in_fricas_command) {
            text_x = indent + spadcom_indent;
        }
        else
            text_x = indent;
    }

    /*
     * Now see if we am on the beginning of a line, and if not add some space
     * if we need to
     */

    if (gInLine && node->space)
        text_x += inter_word_space;

    node->x = text_x;
    if (num_dashes > 1)
        node->y = text_y - dash_y;
    else
        node->y = text_y - word_off_height;
    text_x += node->width;
    gInLine = 1;
    return;
}

void
compute_text_extent(TextNode *node)
{
    for (; node != NULL; node = node->next) {
        switch (node->type) {
          case Endpastebutton:
            endpastebutton_extent(node);
            break;
          case Paste:
            compute_paste_extent(node);
            break;
          case Endpaste:
            if (gInLine) {
                start_newline(present_line_height, node);
                text_x = indent;
            }
            break;
          case Pastebutton:
            compute_pastebutton_extent(node);
            break;
          case Ifcond:
            compute_ifcond_extent(node);
            break;
          case Fi:
            break;
          case Endif:
            if (if_node == NULL) {
                return;
            }
            else
                endif_extent(node);
            break;
          case Endcenter:
            start_newline(present_line_height, node->next);
            pop_group_stack();
            text_x = indent;
            break;
          case Pound:
          case Macro:
            /* check to see if we had space in front of me, if so add it */

            if (node->space && gInLine)
                text_x += inter_word_space;
            break;
          case Punctuation:
            compute_punctuation_extent(node);
            break;
          case Endmath:
            break;
          case Endverbatim:
            if (gInLine) {
                start_newline(present_line_height, node);
                text_x = indent;
            }
            break;
          case Spadsrctxt:
            compute_spadsrctxt_extent(node);
            break;
          case Math:
            compute_word_extent(node);
            break;
          case Verbatim:
            compute_verbatim_extent(node);
            break;
          case WindowId:
          case Word:
          case Lsquarebrace:
          case Rsquarebrace:
            compute_word_extent(node);
            break;
          case Dash:
            compute_dash_extent(node);
            break;
          case HSpace:
            node->height = line_height;
            node->x = text_x;
            node->y = text_y;
            if (gInLine) {
                text_x +=
                    (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            }
            break;
          case VSpace:
            node->height = line_height;
            node->x = text_x;
            node->y = text_y + present_line_height;;
            text_y +=
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1) +
                present_line_height;
            past_line_height = (node->data.node != NULL ?
                                atoi(node->data.node->data.text) : 1)
                + present_line_height;

            present_line_height = line_height;
            break;
          case Space:
            node->height = line_height;
            node->x = text_x;
            node->y = text_y;
            text_x += (gTopOfGroupStack->cur_font->max_bounds.width) *
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            break;
          case Tab:
            node->height = line_height;
            text_x = indent + (gTopOfGroupStack->cur_font->max_bounds.width) *
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            gInLine = 0;
            break;
          case Par:
            node->height = line_height;
            if (gInItem)
                text_x = indent;
            else
                text_x = indent + paragraph_space;
            if (gInLine) {
                start_newline(present_line_height, node);
            }
            break;
          case Newline:
            if (gInLine) {
                start_newline(present_line_height, node);
                text_x = indent;
            }
            break;
          case Horizontalline:
            if (gInLine) {
                start_newline(present_line_height, node);
                text_x = indent;
            }
            node->height = line_height;
            gInLine = 0;
            node->y = text_y - line_height / 2;
            node->x = text_x;
            start_newline(present_line_height, node);
            break;
          case Center:
            compute_center_extent(node);
            break;
          case Box:
            compute_box_extent(node);
            break;
          case Mbox:
            compute_mbox_extent(node);
            break;
          case Beginitems:
          case Begintitems:
            compute_begin_items_extent(node);
            break;
          case Enditems:
          case Endtitems:
            pop_item_stack();
            if (gInLine) {
                start_newline(present_line_height, node);
            }
            text_x = indent;
            break;
          case Titem:
            if (gInLine) {
                start_newline(present_line_height, node);
            }
            text_x = indent - item_space;
            break;
          case Item:
            compute_item_extent(node);
            break;
          case Mitem:
            compute_mitem_extent(node);
            break;
          case Upbutton:
          case Returnbutton:
          case Memolink:
          case Downlink:
          case Link:
          case Windowlink:
            compute_button_extent(node);
            break;
          case Unixlink:
          case Lisplink:
          case Lispwindowlink:
          case Spadcall:
          case Spadcallquit:
          case Qspadcall:
          case Qspadcallquit:
          case LispDownLink:
          case LispMemoLink:
          case Lispcommand:
          case Lispcommandquit:
          case Spadlink:
          case Spaddownlink:
          case Spadmemolink:
          case Unixcommand:
            compute_button_extent(node);
            break;
          case Endbutton:
            endbutton_extent(node);
            break;
          case Endlink:
            if (link_node == NULL)
                return;
            else
                endbutton_extent(node);
            break;
          case Spadsrc:
            compute_spadsrc_extent(node);
            break;
          case Spadcommand:
          case Spadgraph:
            compute_spadcommand_extent(node);
            break;
          case Endspadsrc:
            end_spadsrc_extent(node);
            break;
          case Endspadcommand:
            end_spadcommand_extent(node);
            break;
          case Indent:
            indent = left_margin +
                atoi(node->data.node->data.text) *
                (gTopOfGroupStack->cur_font->max_bounds.width);
            if (!gInLine)
                text_x = indent;
            break;
          case Indentrel:
            indent += atoi(node->data.node->data.text) *
                (gTopOfGroupStack->cur_font->max_bounds.width);
            if (!gInLine)
                text_x = indent;
            break;
          case Group:
            push_group_stack();
            node->y = text_y;
            if (gInLine && node->space)
                text_x += inter_word_space;
            break;
          case Endgroup:
            pop_group_stack();
            break;
          case Tableitem:
            push_group_stack();
            node->y = text_y;
            if (gInLine && node->space)
                text_x += inter_word_space;
            break;
          case Endtableitem:
            pop_group_stack();
            return;
          case Controlbitmap:
          case Inputbitmap:
            if (node->width == -1)
                insert_bitmap_file(node);
            compute_image_extent(node);
            break;
          case Inputpixmap:
            if (node->width == -1)
                insert_pixmap_file(node);
            compute_image_extent(node);
            break;
          case Table:
            compute_table_extent(&node);
            break;
          case BoldFace:
            compute_bf_extent(node);
            break;
          case Emphasize:
            compute_em_extent(node);
            break;
          case It:
            compute_it_extent(node);
            break;
          case Rm:
          case Sl:
          case Tt:
            compute_rm_extent(node);
            break;
          case Inputstring:
            compute_input_extent(node);
            break;
          case SimpleBox:
          case Radiobox:
            compute_ir_extent(node);
            break;
          case Endbox:
            text_x += box_width;
            break;
          case Endmacro:
          case Endparameter:
            break;
          case Description:
            bf_top_group();
            break;
          case Enddescription:
            pop_group_stack();
            if (gInDesc)
                return;
            break;
          case Endscrolling:

            /*
             * What we should do here is if we am in the middle of a line, we
             * should end it here an now.
             */

            if (gInLine)
                start_newline(present_line_height, node);
            break;
          case Noop:
            noop_count++;
            break;
          case Endinputbox:
          case Endheader:
          case Endtitle:
          case Endfooter:
          case Rbrace:
          case Free:
          case Bound:
          case Beep:
          case 0:
            break;
          default:
            fprintf(stderr, "Compute_text_extent: Unknown node type %d\n",
                    node->type);
            break;
        }
    }
}

static void
compute_begin_items_extent(TextNode * node)
{
    int store_x, store_y, lh;

    /*
     * This routine pushes the current item_stack, and then tries to set the
     * item_indent, and the indent level. It checks for an optional argument
     * to begin{items} and if found uses its width.
     */
    if (gInLine) {
        start_newline(present_line_height, node);
    }
    store_x = text_x, store_y = text_y, lh = present_line_height;
    text_x = indent;
    push_item_stack();
    gInItem++;
    item_indent = indent;
    if (node->data.node != NULL) {
        /* we have a desc */
        gInDesc = 1;
        compute_text_extent(node->data.node);
        gInDesc = 0;
        item_space = text_width(node->data.node, Enddescription);
        text_x = store_x;
        text_y = store_y;
        present_line_height = lh;
        indent = item_indent + item_space;
    }
    else
        indent = item_indent + 30;
    gInLine = 0;
}

static void
compute_item_extent(TextNode * node)
{
    if (gInLine)
        start_newline(present_line_height, node);
    text_x = item_indent;
}

static void
compute_mitem_extent(TextNode *node)
{
    if (gInLine) {
        start_newline(present_line_height, node);
    }
    text_x = item_indent;
}

static void
endif_extent(TextNode *node)
{
    /*
     * This node has the responsibilty for updating text_x and text_y so that
     * they are the maxaimum width of teh else and then statements
     */

    text_x = if_node->x;
    text_y = if_node->y;
    if_node = NULL;
}

static void
compute_ifcond_extent(TextNode *node)
{
    TextNode *condnode = node->data.ifnode->cond;
    TextNode *tln = gLineNode;
    int store_x = text_x, store_y = text_y, lh = present_line_height;
    int then_x, then_y;

    /*
     * This routine checks the value of the condition and swaps in the else
     * or the then depending
     */

    /*
     * we have to compute the maximum width and height of the rest of the
     * text and stuff
     */
    push_group_stack();
    if (gInLine && node->space)
        text_x += inter_word_space;
    compute_text_extent(node->data.ifnode->thennode);
    then_x = text_x;
    then_y = text_y;
    text_x = store_x;
    text_y = store_y;
    present_line_height = lh;
    gLineNode = tln;
    if (gInLine && node->space)
        text_x += inter_word_space;
    compute_text_extent(node->data.ifnode->elsenode);
    /* Now choose the best one that is biggest and put it into ifnode */
    if (then_y > text_y) {
        node->y = then_y;
        node->x = then_x;
    }
    else if (text_y > then_y) {
        node->y = text_y;
        node->x = text_x;
    }
    else if (text_x > then_x) {
        node->y = text_y;
        node->x = text_x;
    }
    else {
        node->y = then_y;
        node->x = then_x;
    }
    /* restore everything */
    text_x = store_x;
    text_y = store_y;
    present_line_height = lh;
    gLineNode = tln;
    node->width = 0;

    if_node = node;
    if (gInLine && node->space)
        text_x += inter_word_space;
    if (check_condition(condnode)) {
        node->next = node->data.ifnode->thennode;
    }
    else {
        node->next = node->data.ifnode->elsenode;
    }
    pop_group_stack();
}

static void
compute_center_extent(TextNode * node)
{
    if (gInLine)
        start_newline(present_line_height, node);

    center_top_group();

    if (gLineNode)
        text_x = indent;
    else {
        fprintf(stderr, "(HyperDoc) Internal error: unexpected state in compute_center_extent.\n");
        exit(-1);
    }
}

static void
compute_bf_extent(TextNode *node)
{
    if (gInLine && node->space)
        text_x += inter_word_space;
    node->x = text_x;
    node->y = text_y;
    bf_top_group();
}

static void
compute_em_extent(TextNode *node)
{
    if (gInLine && node->space)
        text_x += inter_word_space;
    node->x = text_x;
    node->y = text_y;
    if (gTopOfGroupStack->cur_font == gEmFont)
        rm_top_group();
    else
        em_top_group();
}

static void
compute_it_extent(TextNode *node)
{
    if (gInLine && node->space)
        text_x += inter_word_space;
    node->x = text_x;
    node->y = text_y;
}

static void
compute_rm_extent(TextNode *node)
{
    if (gInLine && node->space)
        text_x += inter_word_space;
    node->x = text_x;
    node->y = text_y;
    rm_top_group();
}

static void
compute_button_extent(TextNode *node)
{
    int twidth;
    /*int store_x = text_x;*/
    /*int store_y = text_y;*/
    /*int lh = present_line_height;*/

    push_active_group();

    /* First see if we should leave a little space in front of myself * */
    if (gInLine && node->space)
        text_x += inter_word_space;

    twidth = text_width(node->next, Endbutton);
    if (gInLine && node->space)
        text_x += inter_word_space;
    if (text_x + twidth > right_margin && gInLine) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    node->y = text_y;
    link_node = node;
}

static void
endbutton_extent(TextNode *node)
{
    int temp;
    int height;
    int twidth;
    int y;
    int maxx;

    maxx = max_x(link_node, Endbutton);
    link_node->width = twidth = text_width(link_node->next, Endbutton);
    height = link_node->y;
    temp = text_height(link_node->next, Endbutton);
    link_node->height = temp - link_node->y + line_height;

    if (gInLine)
        y = text_y;
    else
        y = text_y - past_line_height;
    if (y > height) {
        link_node->y = temp;    /* height + link_node->height -
                                 * normal_text_height; */
        link_node->width = maxx - indent;
        if (gInLine) {
            start_newline(present_line_height, node);
            text_x = indent;
        }
    }
    else {
        link_node->width = twidth;
        link_node->y = text_y + link_node->height - line_height;
    }
    pop_group_stack();
    link_node = NULL;
}

static void
compute_pastebutton_extent(TextNode *node)
{
    int twidth;

    push_active_group();

    /*
    First see if we should leave a little space in front of myself * */

    if (gInLine && node->space)
        text_x += inter_word_space;

    twidth = text_width(node->next, Endpastebutton);
    if (gInLine && node->space)
        text_x += inter_word_space;
    if (text_x + twidth > right_margin && gInLine) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    node->y = text_y;
    paste_node = node;
    return;
}

static void
endpastebutton_extent(TextNode *node)
{
    int temp;
    int height;
    int twidth;

    paste_node->width = twidth = text_width(paste_node->next, Endpastebutton);
    height = paste_node->y;
    temp = text_height(paste_node->next, Endpastebutton);
    paste_node->height = temp - paste_node->y + line_height;
    if (text_y > height) {
        paste_node->y = temp;
        paste_node->width = right_margin - indent;
        if (gInLine) {
            start_newline(present_line_height, node);
            text_x = indent;
        }
    }
    else {
        paste_node->width = twidth;
        paste_node->y = text_y + paste_node->height - line_height;
    }
    pop_group_stack();
    paste_node = NULL;
    gInLine = 1;
}

static void
compute_paste_extent(TextNode *node)
{
    if (gInLine) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    node->y = text_y;
    node->height = line_height;
}

/* Compute the text extent of a spadcommand node */

static void
compute_spadcommand_extent(TextNode *node)
{
    /*
     * From now on if there is an example which will take over a line, then
     * it will start and end with a newline
     */

    /*int height;*/
    int t_width;
    /*int store_x = text_x;*/
    /*int store_y = text_y;*/
    /*int lh = present_line_height;*/

    in_fricas_command = 1;

    push_spad_group();

    /* Check to see if we should space in front of myself         */
    if (gInLine && node->space)
        text_x += inter_word_space;
    t_width = text_width(node->next, Endspadcommand);
    if (gInLine && ((text_x + t_width) > right_margin)) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    node->y = text_y;
    spad_node = node;

}

static void
compute_spadsrc_extent(TextNode *node)
{
    /*
     * From now on if there is an example which will take over a line, then
     * it will start and end with a newline
     */

    /*int store_x = text_x;*/
    /*int store_y = text_y;*/
    /*int lh = present_line_height;*/

    in_fricas_command = 1;

    push_spad_group();

    if (gInLine) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    node->y = text_y;
    spad_node = node;

}

static void
end_spadcommand_extent(TextNode *node)
{
    int temp;
    int height;
    int twidth;
    int maxx;
    /*int y = (gInLine) ? (text_y) : (text_y - past_line_height);*/

    maxx = max_x(spad_node, Endspadcommand);
    twidth = spad_node->width = text_width(spad_node->next, Endspadcommand);
    height = spad_node->y;
    temp = text_height(spad_node->next, Endspadcommand);

    spad_node->height = temp - height + line_height;
    if (text_y > height && gInLine) {
        spad_node->y = temp;
        spad_node->width = maxx - indent;
        start_newline(present_line_height, node);
        text_x = indent;
    }
    else {
        spad_node->width = twidth;
        spad_node->y = text_y - line_height + spad_node->height;
    }
    pop_group_stack();
    in_fricas_command = 0;
    spad_node = NULL;
}

static void
end_spadsrc_extent(TextNode *node)
{
    int temp;
    int height;
    int twidth;
    int maxx;
    int y = (gInLine) ? (text_y) : (text_y - past_line_height);

    maxx = max_x(spad_node, Endspadsrc);

    twidth = spad_node->width = text_width(spad_node->next, Endspadsrc);
    height = spad_node->y;
    temp = text_height(spad_node->next, Endspadsrc);
    spad_node->height = temp - height + line_height;
    if (y > height && gInLine) {
        spad_node->y = temp;
        spad_node->width = maxx - indent;
        start_newline(present_line_height, node);
        text_x = indent;
    }
    else {
        spad_node->width = twidth;
        spad_node->y = text_y - line_height + spad_node->height;
    }
    pop_group_stack();
    in_fricas_command = 0;
    spad_node = NULL;
}

static void
compute_mbox_extent(TextNode *node)
{

    node->width = text_width(node->next, Endmbox);
    if (node->space)
        text_x += inter_word_space;
    if (text_x + node->width > right_margin) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    node->y = text_y;
}

static void
compute_box_extent(TextNode *node)
{
    int t_width;

    /*
     * First thing we do is see if we need to skip some space in front of the
     * word
     */

    if (gInLine && node->space)
        text_x += inter_word_space;

    /* Calculate the actual width of the box */

    t_width = text_width(node->next, Endbox) + 2 * box_width;

    if (text_x + t_width > right_margin) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    text_x = text_x + box_width;
    node->y = text_y - 2;
    node->width = t_width;
    node->height = line_height - 2;
    gInLine = 1;
}

static void
compute_ir_extent(TextNode *node)
{
    int t_width;

    /*
     * First thing we do is see if we need to skip some space in front of the
     * word
     */

    if (gInLine && node->space)
        text_x += inter_word_space;

    /* Calculate the actual width of the box */

    t_width = node->width;

    if (text_x + t_width > right_margin) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    if (node->height > line_height) {
        node->height = present_line_height = plh(node->height + inter_line_space);
        node->y = text_y + node->height - normal_text_height;
    }
    else {
        node->y = text_y - line_height + node->height;
    }
    gInLine = 1;
    text_x += node->width;
}

/* read a bitmap file into memory */

static void
compute_image_extent(TextNode *node)
{
    if (text_x + node->width > right_margin) {
        start_newline(present_line_height, node);
        text_x = indent;
    }
    node->x = text_x;
    if (node->height > line_height) {
        present_line_height = plh(node->height + inter_line_space);
        node->y = text_y + node->height - line_height;
    }
    else {
        node->y = text_y - line_height + node->height;
    }
    text_x += node->width;
    gInLine = 1;
}

/*
 * compute the coordinates of the entries in a table
 */

static void
compute_table_extent(TextNode **node)
{
    int num_cols, num_lines;
    int max_width = 0, node_width, col_width;
    int x, y, num_entries = 0,/* n=0, */ screen_width, table_top;
    TextNode *front = *node;
    TextNode *tn;

    gInTable = 1;
    front->x = text_x;
    front->y = text_y;
    for (tn = front->next; tn->type != Endtable; num_entries++, tn = tn->next) {
        /* Now we need to scan the table group by group */
        node_width = text_width(tn->next, Endtableitem);
        if (node_width > max_width)
            max_width = node_width;
        /* Get to the beginning og the next group */
        for (; tn->type != Endtableitem; tn = tn->next);
    }
    col_width = max_width + min_inter_column_space;
    screen_width = gWindow->width - right_margin_space - indent;
    num_cols = screen_width / col_width;
    if (num_cols == 0)
        num_cols = 1;
    num_lines = num_entries / num_cols;
    if (num_entries % num_cols != 0)
        ++num_lines;
    if (gInLine) {
        start_newline(present_line_height, *node);
    }
    table_top = text_y;
    num_cols = num_entries / num_lines;
    if (num_entries % num_lines != 0)
        ++num_cols;
    col_width = screen_width / num_cols;
    for (tn = front->next, x = 0; x < num_cols; x++)
        for (y = 0; y < num_lines && tn->type != Endtable; y++) {
            if (num_cols == 1 && y > 0)
                text_y += line_height;
            else
                text_y = table_top + y * line_height;
            text_x = indent + x * col_width;
            gInLine = 0;
            compute_text_extent(tn->next);
            for (; tn->type != Endtableitem; tn = tn->next);
            tn = tn->next;
        }
    front->height = num_lines * line_height;
    front->width = screen_width;
    text_x = indent;
    if (num_cols == 1)
        text_y += line_height;
    else
        text_y = table_top + front->height;
    *node = tn;
    gInLine = 0;
}

void
compute_title_extent(HyperDocPage *page)
{
    right_margin_space = non_scroll_right_margin_space;
    page->title->height = twheight + gWindow->border_width;
    page->title->x = gWindow->border_width + 2 * twwidth + (int) gWindow->border_width / 2;
    gLineNode = page->title->next;
    init_title_extents(page);
    text_y = top_margin + line_height;
    compute_text_extent(page->title->next);
    page->title->height = max(text_height(page->title->next, Endtitle),
                              twheight);
}

void
compute_header_extent(HyperDocPage *page)
{

    /*
     * Hopefully we will soon be able to actually compute the needed height
     * for the header here
     */

    int ty; /* UNUSED */

    gExtentRegion = Header;
    right_margin_space = non_scroll_right_margin_space;
    init_extents();
    ty = text_y = 3 * top_margin + line_height + max(page->title->height, twheight);
    gLineNode = page->header->next;
    compute_text_extent(page->header->next);
    page->header->height = text_height(page->header->next, Endheader);
    if (page->header->height) {
        page->header->height += 1 / 2 * line_height;
        page->top_scroll_margin = (gInLine) ? text_y : text_y - past_line_height;
        if (!(page->page_flags & NOLINES))
            page->top_scroll_margin += (int) line_height / 2;
        page->top_scroll_margin += gWindow->border_width + 2 * top_margin;
    }
    else {
        page->top_scroll_margin = page->title->height + gWindow->border_width +
            2 * scroll_top_margin;
    }
}

void
compute_footer_extent(HyperDocPage * page)
{
    if (page->footer) {
        gExtentRegion = Footer;
        right_margin_space = non_scroll_right_margin_space;
        init_extents();
        present_line_height = line_height;
        text_y = line_height;
        gLineNode = page->footer->next;
        compute_text_extent(page->footer->next);
        page->footer->height = text_height(page->footer->next, Endfooter);
        if (page->footer->height) {
            if ((!page->page_flags & NOLINES))
                page->footer->height += (int) line_height / 2;
            page->bot_scroll_margin = gWindow->height -
                page->footer->height - bottom_margin
                - gWindow->border_width + top_margin;
        }
        else
            page->bot_scroll_margin = gWindow->height;
    }
}

void
compute_scrolling_extent(HyperDocPage *page)
{
    /* Check to see if there is a scrolling region  */

    if (!page->scrolling) {
        return;
    }
    noop_count = 0;

    /* If there is then compute all the proper locations */
    gExtentRegion = Scrolling;
    right_margin_space = non_scroll_right_margin_space + gScrollbarWidth;
    init_extents();
    text_y = line_height;
    gLineNode = page->scrolling->next;
    compute_text_extent(page->scrolling->next);

    /*
     * the following is an attempt to fix the bug where one cannot scroll
     * down to a bitmap that is opened at the bottom of a page.
     */

    /*
     * TTT trial if(!gInLine)
     */
    if (0) {
        text_y = text_y - past_line_height;
    }
    else if (present_line_height > line_height)
        text_y = text_y + present_line_height - line_height;
    page->scrolling->height = text_y;

}
