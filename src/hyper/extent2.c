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
 * extent2.h:  HyperDoc extent computation routines
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"


#include "extent.h"
#include "hyper.h"

#include "all_hyper_proto.H1"
#include "pixmap.H1"

static void center_nodes(TextNode * begin_node, TextNode * end_node);
static int text_height1(TextNode * node, int Ender);
static int x_value(TextNode * node);

static int cur_height = 0;
static int max_x_value = 0;

/*
 * start_newline updates the current header node, and also allocates if needed
 * memory for the next Line Header. It also assigns the first TextNode on the
 * line to the structure, because this is the last time I will be able to do
 * this
 */

void
start_newline(int distance, TextNode * node)
{
    if (gLineNode != NULL) {
        if (gTopOfGroupStack->center)
            center_nodes(gLineNode, node);
        gLineNode = node;
    }
    text_y += distance;
    past_line_height = distance;
    present_line_height = line_height;
    gInLine = 0;
}

/*
 * center_nodes goes through and centers all the text between the two
 * given nodes.
 */

static void
center_nodes(TextNode * begin_node, TextNode * end_node)
{
    int begin_x, end_x, wmid_x, offset, mid_x;
    TextNode *node;

    end_x = text_x;
    begin_x = x_value(begin_node);
    mid_x = (int) (end_x + begin_x) / 2;
    wmid_x = (int) (right_margin + indent) / 2;

    if (mid_x > wmid_x)
        offset = 0;
    else
        offset = wmid_x - mid_x;

    for (node = begin_node; node != end_node; node = node->next)
        if (node->x > 0)
            node->x += offset;
}

static int
punctuation_width(TextNode * node)
{
    int twidth, width = strlen(node->data.text);

    twidth = XTextWidth(gTopOfGroupStack->cur_font, node->data.text, width);

    /* check to see if there was some space in front */

    if (gInLine && (node->space & FRONTSPACE))
        twidth += inter_word_space;

# if 0
    if (node->space & BACKSPACE) {
        switch (node->data.text[0]) {
            case '.':
            case '?':
            case '!':
                twidth += term_punct_space;
                break;
        }
    }
#endif

    return twidth;
}

static int
input_string_width(TextNode * node)
{
    InputItem *item;
    int t_width;

    /** search the symbol table for the proper entry **/

    item = node->link->reference.string;

    /** Once I have gotten this far, I should just be able to calculate
      the width using the normal font **/

    t_width = (item->size + 1) * gInputFont->max_bounds.width + 10;
    return t_width;

}

static int
word_width(TextNode * node)
{
    int twidth, len = strlen(node->data.text);

    twidth = XTextWidth(gTopOfGroupStack->cur_font, node->data.text, len);
    if (node->space & FRONTSPACE)
        twidth += inter_word_space;

    return twidth;
}

static int
verbatim_width(TextNode * node)
{
    int twidth, len = strlen(node->data.text);

    twidth = XTextWidth(gTopOfGroupStack->cur_font, node->data.text, len);
    if (node->space)
        twidth += inter_word_space;

    return twidth;
}

static int
width_of_dash(TextNode * node)
{
    int num_dashes, twidth;

    num_dashes = strlen(node->data.text);
    if (num_dashes > 1)
        twidth = node->width = num_dashes * dash_width;
    else
        twidth = node->width = XTextWidth(gTopOfGroupStack->cur_font,
                                          node->data.text, 1);
    if (node->space)
        twidth += inter_word_space;
    return twidth;
}

/*
 * return the gWindow->width in pixels of the given text node, when
 * displayed
 */

int
text_width(TextNode * node, int Ender)
{
    int twidth = 0, num_words;

    for (num_words = 0; node != NULL; num_words++, node = node->next) {
        if (Ender == Endtokens) {
            if (node->type == Endtokens)
                return twidth;
        }
        else if (node->type == Ender)
            return twidth;

        switch (node->type) {
          case Macro:
          case Pound:
            if (node->space && gInLine)
                twidth += inter_word_space;
            break;
          case Punctuation:
            twidth += punctuation_width(node);
            break;
          case Dash:
            if (gInLine && node->space)
                twidth += inter_word_space;
            twidth += width_of_dash(node);
            break;
          case Verbatim:
          case Spadsrctxt:
            twidth += verbatim_width(node);
            break;
          case Lsquarebrace:
          case Rsquarebrace:
          case Word:
            twidth += word_width(node);
            break;
          case Box:
            twidth += 2 * box_space;
            break;
          case Link:
          case Downlink:
          case Memolink:
          case Windowlink:
          case LispMemoLink:
          case Lispwindowlink:
          case Lisplink:
          case Unixlink:
          case Spadcall:
          case Spadcallquit:
          case Qspadcall:
          case Qspadcallquit:
          case LispDownLink:
          case Lispcommand:
          case Lispcommandquit:
          case Spadlink:
          case Spaddownlink:
          case Spadmemolink:
          case Unixcommand:
          case Upbutton:
          case Returnbutton:
          case Description:
            push_active_group();
            break;
          case Endbutton:
          case Endspadcommand:
          case Enddescription:
            pop_group_stack();
            break;
          case Endlink:
            pop_group_stack();
            break;
          case Inputstring:
            twidth += input_string_width(node);
            break;
          case SimpleBox:
          case Radiobox:
            twidth += node->width + ((node->space) ? inter_word_space : 0);
            break;
          case Spadcommand:
          case Spadgraph:
            push_spad_group();
            break;
          case VSpace:
            break;
          case HSpace:
            twidth +=
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            break;
          case Space:
            twidth += (gTopOfGroupStack->cur_font->max_bounds.width) *
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            break;
          case Tab:
            twidth = (gTopOfGroupStack->cur_font->max_bounds.width) *
                (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
            break;
          case Table:
            twidth = gWindow->width - left_margin - right_margin_space;
            break;
          case Tableitem:
          case Group:
            twidth += (node->space) ? inter_word_space : 0;
            push_group_stack();
            break;
          case BoldFace:
            if (node->space)
                twidth += inter_word_space;
            bf_top_group();
            break;
          case Emphasize:
            if (node->space)
                twidth += inter_word_space;
            if (gTopOfGroupStack->cur_font == gRmFont)
                em_top_group();
            else
                rm_top_group();
            break;
          case It:
            if (node->space)
                twidth += inter_word_space;
            em_top_group();
            break;
          case Rm:
          case Sl:
          case Tt:
            if (node->space)
                twidth += inter_word_space;
            rm_top_group();
            break;
          case Endgroup:
            pop_group_stack();
            break;
          case Controlbitmap:
          case Inputbitmap:
            if (node->width == -1)
                insert_bitmap_file(node);
            twidth += node->width;
            break;
          case Inputpixmap:
            if (node->width == -1)
                insert_pixmap_file(node);
            twidth += node->width;
            break;
          case Mbox:
          case Indent:
          case Endmacro:
          case Free:
          case Bound:
          case Beep:
          case Item:
          case Titem:
          case Beginitems:
          case Noop:
          case Endinputbox:
          case Fi:
          case Ifcond:
          case Endif:
          case Begintitems:
          case Enditems:
          case Endtitems:
          case Endtableitem:
          case Endtable:
          case Endparameter:
          case Endbox:
          case Endheader:
          case Endfooter:
          case Endscrolling:
          case Endverbatim:
          case Endspadsrc:
            break;
          case Newline:
            /* WOw, I guess I should ertunr a really big number */
            twidth += gWindow->width;
            break;
          default:

            /*
             * fprintf(stderr, "Unknown nodetype %d in text_width\n",
             * node->type);
             */
            break;
        }
    }
    return twidth;
}

/*
 * total_width traces through the nodes, until it finds a blank space. It is
 * used by compute_word_extent, and compute_punctuation extent to determine
 * How far we go before we actually see white space.
 */

int
total_width(TextNode * node, int Ender)
{
    int twidth = 0;

    for (; (node != NULL); node = node->next) {
        if (Ender == Endtokens) {
            if (node->type >= Endtokens)
                return twidth;
        }
        else if (node->type == Ender)
            return twidth;

        /*
         * The first thing we check for is to see if there was space in front
         * of the current node, if so we are done
         */

        if (node->space)
            return twidth;

        /*** Else depending on the node type ***/

        switch (node->type) {
          case Noop:
          case Endinputbox:
          case Pound:
          case Ifcond:
          case Fi:
          case Endif:
            break;
          case Rsquarebrace:
          case Punctuation:
          case Word:
          case Dash:
            twidth += XTextWidth(gTopOfGroupStack->cur_font, node->data.text,
                                 strlen(node->data.text));
            break;
          case Box:
          case Link:
          case Downlink:
          case Memolink:
          case Windowlink:
          case LispMemoLink:
          case Lispwindowlink:
          case Lisplink:
          case Unixlink:
          case Spadcall:
          case Spadcallquit:
          case Qspadcall:
          case Qspadcallquit:
          case LispDownLink:
          case Lispcommand:
          case Lispcommandquit:
          case Spadlink:
          case Spaddownlink:
          case Spadmemolink:
          case Unixcommand:
          case Inputstring:
          case SimpleBox:
          case Radiobox:
          case Upbutton:
          case Returnbutton:
          case Spadcommand:
          case Spadgraph:
          case VSpace:
          case HSpace:
          case Space:
          case Table:
          case Group:
          case Controlbitmap:
          case Inputbitmap:
          case Inputpixmap:
          case Free:
          case Beep:
          case Bound:
          case Lsquarebrace:
          case BoldFace:
          case Emphasize:
          case It:
          case Rm:
          case Sl:
          case Tt:
          case Newline:
          case Verbatim:
          case Spadsrctxt:
            return twidth;
          default:
            break;
        }
    }
    return twidth;
}

/*
 * init_extents initialize some text size variables
 */

void
init_extents(void)
{
    present_line_height = line_height;
    gInLine = 0;
    gInItem = 0;
    in_fricas_command = 0;
    item_indent = 0;
    gInDesc = 0;
    indent = left_margin;
    text_x = indent;
    gTopOfGroupStack->cur_font = gRmFont;
    gTopOfGroupStack->cur_color = gRmColor;
    right_margin = gWindow->width - right_margin_space;
    clear_item_stack();
}

/*
 * init_title_extents initialize some title text size variables
 */

void
init_title_extents(HyperDocPage * page)
{
    present_line_height = line_height;
    gInLine = 0;
    in_fricas_command = 0;
    item_indent = 0;
    gInDesc = 0;
    indent = left_margin + page->title->x;
    text_x = indent;
    gTopOfGroupStack->cur_font = gRmFont;
    gTopOfGroupStack->cur_color = gRmColor;
    right_margin = gWindow->width - right_margin_space - gWindow->border_width -
        2 * twwidth;
    clear_item_stack();
}

/*
 * init_text initialize some text size variables
 */

void
init_text(void)
{
    normal_text_height = gRmFont->ascent + gRmFont->descent;
    line_height = gRmFont->ascent + gRmFont->descent + inter_line_space;
    word_off_height = line_height - normal_text_height;
    space_width = gRmFont->max_bounds.width;
}

/*
 * text_height returns the height of a piece of formatted text in pixels
 */

int
text_height(TextNode * node, int Ender)
{
    cur_height = 0;
    return text_height1(node, Ender);
}

/*
 * text_height1 is the recursive part of text_height
 */

static int
text_height1(TextNode * node, int Ender)
{
    for (; node != NULL; node = node->next) {
        if (Ender == Endtokens) {
            if (node->type > -Endtokens)
                return cur_height;
        }
        else if (node->type == Ender)
            return cur_height;
        switch (node->type) {
          case Center:
          case Downlink:
          case Link:
          case Spadcommand:
          case Spadgraph:
          case Upbutton:
          case Returnbutton:
          case Windowlink:
          case Memolink:
          case Lispwindowlink:
          case Lisplink:
          case Unixlink:
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
          case SimpleBox:
          case Radiobox:
          case Group:
          case Box:
          case Controlbitmap:
          case Inputbitmap:
          case Inputpixmap:
          case Horizontalline:
          case Punctuation:
          case Lsquarebrace:
          case Rsquarebrace:
          case Word:
          case Verbatim:
          case Math:
          case Spadsrctxt:
          case Dash:
          case Inputstring:
            cur_height = max(node->y, cur_height);
            break;
          case Mbox:
          case Macro:
          case Pound:
          case Emphasize:
          case BoldFace:
          case It:
          case Rm:
          case Sl:
          case Tt:
          case Endparameter:
          case Description:
          case Enddescription:
          case Noop:
          case Fi:
          case Ifcond:
          case Endif:
          case Endinputbox:
          case Tab:
          case Newline:
          case Space:
          case VSpace:
          case HSpace:
          case Beginitems:
          case Begintitems:
          case Endtitems:
          case Titem:
          case Enditems:
          case Endtable:
          case Endtableitem:
          case Item:
          case Par:
          case Beep:
          case Free:
          case Bound:
          case Endgroup:
          case Endcenter:
          case Endbutton:
          case Endmacro:
          case Tableitem:
          case Endlink:
          case Endspadcommand:
          case Indent:
          case Indentrel:
          case Endbox:
          case Endmbox:
          case Table:
          case Endverbatim:
          case Endmath:
          case Spadsrc:
          case Endspadsrc:
            break;
          case Beginscroll:
          case Endscroll:
            break;
          case Endscrolling:
            return cur_height;
          default:

            /*
             * fprintf(stderr, "Text_height1: Unknown Node Type %d\n",
             * node->type);
             */
            break;
        }
    }
    return cur_height;
}

/*
 * max_x returns the height of a piece of formatted text in pixels
 */

int
max_x(TextNode * node, int Ender)
{
    max_x_value = 0;
    for (; node != NULL; node = node->next) {
        if (Ender == Endtokens) {
            if (node->type >= Endtokens)
                return max_x_value;
        }
        else if (node->type == Ender)
            return max_x_value;
        switch (node->type) {
          case Lsquarebrace:
          case Rsquarebrace:
          case Word:
            max_x_value = max(max_x_value, node->x + word_width(node));
            break;
          case Verbatim:
          case Spadsrctxt:
            max_x_value = max(max_x_value, node->x + verbatim_width(node));
            break;
          case Punctuation:
            max_x_value = max(max_x_value, node->x + punctuation_width(node));
            break;
          case Dash:
            max_x_value = max(max_x_value, node->x + width_of_dash(node));
            break;
          case HSpace:
            max_x_value = max(max_x_value, node->x +
                              (node->data.node != NULL ? atoi(node->data.node->data.text) : 1));
            break;
          case Space:
            max_x_value = max(max_x_value, node->x +
                           (gTopOfGroupStack->cur_font->max_bounds.width) *
                              (node->data.node != NULL ? atoi(node->data.node->data.text) : 1));
            break;
          case Group:
            push_group_stack();
            break;
          case BoldFace:
            bf_top_group();
            break;
          case Emphasize:
            if (gTopOfGroupStack->cur_font == gRmFont)
                em_top_group();
            else
                rm_top_group();
            break;
          case It:
            em_top_group();
            break;
          case Rm:
          case Sl:
          case Tt:
            rm_top_group();
            break;
          case Endgroup:
            pop_group_stack();
            break;
          case Controlbitmap:
          case Inputbitmap:
            if (node->width == -1)
                insert_bitmap_file(node);
            max_x_value = max(max_x_value, node->x + node->width);
            break;
          case Inputpixmap:
            if (node->width == -1)
                insert_pixmap_file(node);
            max_x_value = max(max_x_value, node->y + node->width);
            break;
          default:
            break;
        }
    }
    return cur_height;
}

static int
x_value(TextNode * node)
{
    for (; node != NULL; node = node->next) {
        switch (node->type) {
          case Controlbitmap:
          case Inputbitmap:
          case Inputpixmap:
          case Lsquarebrace:
          case Rsquarebrace:
          case Word:
          case Verbatim:
          case Spadsrctxt:
          case Dash:
          case Punctuation:
          case VSpace:
          case HSpace:
          case Horizontalline:
          case Box:
          case Downlink:
          case Link:
          case Lispwindowlink:
          case Lisplink:
          case Unixlink:
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
          case Spadcommand:
          case Spadgraph:
          case Unixcommand:
          case Space:
          case SimpleBox:
          case Radiobox:
            return node->x;
          default:
#ifdef DEBUG
            fprintf(stderr, "X_value did not know x value of type %d\n", node->type);
#endif
            return x_value(node->next);
        }
    }
    return 0;
}

/*
 * trailing_space computes the length of the trailing spaces of a node
 */

int
trailing_space(TextNode * node)
{
    int space = 0;

    for (; node->type < Endtokens; node = node->next);
    if (node->type == Space)
        space += inter_word_space *
            (node->data.node != NULL ? atoi(node->data.node->data.text) : 1);
    return space;
}

/*
 * insert_bitmap_file reads a bitmap file into memory
 */

void
insert_bitmap_file(TextNode * node)
{
    char *filename = node->data.text;
    int bm_width, bm_height;
    XImage *im;
    ImageStruct *image;

    if (*filename == ' ')
        filename++;
    if (node->image.pm == 0) {
        if (
        ((image = (ImageStruct *) hash_find(&gImageHashTable, filename)) == NULL)
            || (getenv("HTCACHE"))) {

            /*
             * read the bitmap if not already in memory or if the environment
             * variable HTCACHE is set (NAG addition).
             */

            im = HTReadBitmapFile(gXDisplay, gXScreenNumber, filename,
                                  &bm_width, &bm_height);

            /** now add the image to the gImageHashTable **/
            image = (ImageStruct *) halloc(sizeof(ImageStruct), "ImageStruct");
            image->image.xi = im;
            image->width = image->image.xi->width;
            image->height = image->image.xi->height;
            image->filename = (char *) halloc(sizeof(char) * strlen(filename) +1,"Image Filename");
            /* strcpy(image->filename, filename); */
            sprintf(image->filename, "%s", filename);
            hash_insert(&gImageHashTable, (char *)image, image->filename);
        }
        node->width = image->width;
        node->height = image->height;
        node->image.xi = image->image.xi;
    }
}

/*
 * insert_pixmap_file reads a pixmap file into memory
 */

void
insert_pixmap_file(TextNode * node)
{
    char *filename = node->data.text;
    int bm_width, bm_height, ret_val;
    XImage *xi;
    ImageStruct *image;

    if (*filename == ' ')
        filename++;
    if (node->image.xi == 0) {
        if ((image = (ImageStruct *) hash_find(&gImageHashTable, filename)) == NULL) {
            ret_val = read_pixmap_file(gXDisplay, gXScreenNumber, filename, &xi,
                                       &bm_width, &bm_height);
            switch (ret_val) {
              case(-1):
                gSwitch_to_mono = 1;
                return;
              case BitmapFileInvalid:
                fprintf(stderr, "File %s contains invalid bitmap data\n", filename);
                return;
              case BitmapOpenFailed:
                fprintf(stderr, "couldn't open bitmap file %s\n", filename);
                return;
              case BitmapNoMemory:
                fprintf(stderr, "not enough memory to store bitmap\n");
                return;
            }
            image = (ImageStruct *) halloc(sizeof(ImageStruct), "ImageStruct");
            image->width = bm_width;
            image->height = bm_height;
            image->filename = (char *) halloc(sizeof(char) * strlen(filename) +1,
                                              "insert_pixmap--filename");
            /* strcpy(image->filename, filename); */
            sprintf(image->filename, "%s", filename);
            image->image.xi = xi;
            hash_insert(&gImageHashTable, (char *)image, image->filename);
        }
        node->width = image->width;
        node->height = plh(image->height + inter_line_space);
        node->image.xi = image->image.xi;
    }
}

/*
 * plh calculates the closet value of line_height > height
 */

int
plh(int height)
{
    int rheight = height;

    if (gExtentRegion == Scrolling) {
        for (rheight = line_height; rheight < height; rheight += line_height)
            ;
    }
    return rheight;
}
