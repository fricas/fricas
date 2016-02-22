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
 * show_types.c:  Show the various types of things that can show up in text
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"


#include "extent.h"
#include "hyper.h"

#include "all_hyper_proto.H1"

static void show_image(TextNode * node, GC gc);
static void show_input(TextNode * node);
static void show_link(TextNode * node);
static void show_paste(TextNode * node);
static void show_pastebutton(TextNode * node);
static void show_simple_box(TextNode * node);
static void show_spadcommand(TextNode * node);

/*
 * Display the page whose extent has been computed, using the actual size of
 * the window, and y_off to determine clipped areas
 */

void
show_text(TextNode *node, int Ender)
{
    /*int twidth, len;*/
    /*int otext_x, otext_y, t;*/
    /*XFontStruct *old_font;*/
    /*int old_color;*/

    for (; node != NULL; node = node->next) {
        switch (node->type) {
          case 0:
          case Beginitems:
          case Begintitems:
          case Bound:
          case Center:
          case Free:
          case HSpace:
          case Indent:
          case Indentrel:
          case Item:
          case Macro:
          case Mbox:
          case Newline:
          case Noop:
          case Par:
          case Pound:
          case Rbrace:
          case Space:
          case Tab:
          case Table:
          case Titem:
          case VSpace:
            break;

          case Dash:
          case Fi:
          case Ifcond:
            if (visible(node->y, node->height)) {
                if (strlen(node->data.text) > 1) {
                    XDrawLine(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, node->x,
                              node->y + gRegionOffset + y_off
                              - gTopOfGroupStack->cur_font->descent -
                              word_off_height,
                              node->x + node->width,
                         node->y + gRegionOffset + y_off - word_off_height -
                              gTopOfGroupStack->cur_font->descent);
                }
                else {
                    XDrawString(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, node->x, node->y +
                       gRegionOffset - gTopOfGroupStack->cur_font->descent + y_off,
                                node->data.text, 1);
                }
            }
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            break;

          case Lsquarebrace:
          case Math:
          case Punctuation:
          case Rsquarebrace:
          case Spadsrctxt:
          case WindowId:
          case Word:
            if (visible(node->y, node->height))
                XDrawString(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, node->x, node->y +
                       gRegionOffset - gTopOfGroupStack->cur_font->descent + y_off,
                            node->data.text, node->width);
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            break;

          case Verbatim:
            push_group_stack();
            tt_top_group();
            if (visible(node->y, node->height))
                XDrawString(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, node->x, node->y +
                       gRegionOffset - gTopOfGroupStack->cur_font->descent + y_off,
                            node->data.text, node->width);
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            pop_group_stack();
            break;

          case Horizontalline:
            if (visible(node->y, node->height)) {
                line_top_group();
                XDrawLine(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC, 0,
                          node->y + gRegionOffset + y_off,
                          gWindow->width,
                          node->y + gRegionOffset + y_off);
                pop_group_stack();
            }
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            break;

          case Box:
            if (visible(node->y, node->height))
                XDrawRectangle(gXDisplay, gWindow->fDisplayedWindow, gWindow->fStandardGC,
                               node->x,
                             node->y + gRegionOffset + y_off - node->height,
                               node->width,
                               node->height);
            else {
                if (above(node->y))
                    need_scroll_up_button = 1;
                else if (below(node->y))
                    need_scroll_down_button = 1;
            }
            break;


          case Downlink:
          case Link:
          case LispDownLink:
          case LispMemoLink:
          case Lispcommand:
          case Lispcommandquit:
          case Lisplink:
          case Lispwindowlink:
          case Memolink:
          case Qspadcall:
          case Qspadcallquit:
          case Returnbutton:
          case Spadcall:
          case Spadcallquit:
          case Spaddownlink:
          case Spadlink:
          case Spadmemolink:
          case Unixcommand:
          case Unixlink:
          case Upbutton:
          case Windowlink:
            if (pix_visible(node->y, node->height))
                show_link(node);
            break;

          case Spadcommand:
          case Spadgraph:
          case Spadsrc:
            show_spadcommand(node);
            break;

          case Pastebutton:
            if (visible(node->y, node->height))
                show_pastebutton(node);
            break;

          case Paste:
            show_paste(node);
            break;

          case Group:
          case Tableitem:
            push_group_stack();
            break;

          case Controlbitmap:
            show_image(node, gWindow->fControlGC);
            break;

          case Inputbitmap:
            show_image(node, gWindow->fStandardGC);
            break;

          case Inputpixmap:
            show_image(node, gWindow->fStandardGC);
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

          case Sl:
          case Rm:
            rm_top_group();
            break;

          case Tt:
            tt_top_group();
            break;

          case Inputstring:
            show_input(node);
            break;

          case Radiobox:
          case SimpleBox:
            show_simple_box(node);
            break;

          case Beep:
            LoudBeepAtTheUser();
            break;

          case Description:
            bf_top_group();
            break;

          case Endspadsrc:
          case Endspadcommand:
              in_fricas_command = 1;
          case Endtableitem:
          case Enddescription:
          case Endpastebutton:
          case Endlink:
          case Endbutton:
          case Endgroup:
            pop_group_stack();
          case Endverbatim:
          case Endmath:
          case Endbox:
          case Endtable:
          case Endmbox:
          case Endparameter:
          case Endpaste:
          case Endinputbox:
          case Endcenter:
          case Endmacro:
          case Endif:
          case Endtitems:
          case Enditems:

            /*
             * Now since I can show specific regions of the text, then at
             * this point I should check to see if I am the end
             */
            if (node->type == Ender)
                return;
            break;
          case Endfooter:
          case Endscrolling:
          case Endheader:
          case Endtitle:

            /*
             * regardless of what ender I have, I always terminate showing
             * with one of these
             */
            return;
          default:
            fprintf(stderr, "Show_text: Unknown Node Type %d\n", node->type);
            break;
        }
    }
}

static void
show_link(TextNode *node)
{
    /* XFontStruct *old_font;*/
    XWindowChanges wc;
    /*int twidth, boxwidth, old_color;*/
    int active;

    switch (node->type) {
      case Upbutton:
        if (!need_up_button) {
            XClearArea(gXDisplay, gWindow->fDisplayedWindow, node->x,
                       node->y - node->height + gRegionOffset,
                       node->width, node->height, 0);
            active = 0;
        }
        else
            active = 1;
        break;
      case Returnbutton:
        if (!need_return_button) {
            XClearArea(gXDisplay, gWindow->fDisplayedWindow, node->x,
                       node->y - node->height + gRegionOffset,
                       node->width, node->height, 0);
            active = 0;
        }
        else
            active = 1;
        break;
      case Helpbutton:
        if (!need_help_button) {
            XClearArea(gXDisplay, gWindow->fDisplayedWindow, node->x,
                       node->y - node->height + gRegionOffset,
                       node->width, node->height, 0);
            active = 0;
        }
        else
            active = 1;
        break;
      default:
        active = 1;
        break;
    }

    if (active) {
        ButtonList *bl = alloc_button_list();

        push_active_group();
        wc.x = node->x;
        wc.y = node->y - node->height + y_off + gRegionOffset;
        wc.height = node->height;
        wc.width = node->width - trailing_space(node->next);
        bl->x0 = wc.x;
        bl->y0 = wc.y;
        bl->x1 = bl->x0 + wc.width;
        bl->y1 = bl->y0 + wc.height;
        bl->link = node->link;
        if (!not_in_scroll) {
            bl->y0 += gWindow->page->top_scroll_margin + scroll_top_margin;
            bl->y1 += gWindow->page->top_scroll_margin + scroll_top_margin;
            bl->next = gWindow->page->s_button_list;
            gWindow->page->s_button_list = bl;
        }
        else {
            bl->next = gWindow->page->button_list;
            gWindow->page->button_list = bl;
        }
    }
    else
        rm_top_group();
}

static void
show_paste(TextNode *node)
{
    PasteNode *paste;

    if (!(paste = (PasteNode *) hash_find(gWindow->fPasteHashTable,
        node->data.text)))
            return;

    /*
     * Once I have got this far, then I had better save the current group
     * stack and the item stack
     */
    if (paste->group)
        free_group_stack(paste->group);
    paste->group = (GroupItem *) copy_group_stack();
    if (paste->item_stack)
        free_item_stack(paste->item_stack);
    paste->item_stack = (ItemStack *) copy_item_stack();
}

static void
show_pastebutton(TextNode *node)
{
    /*XFontStruct *old_font;*/
    XWindowChanges wc;
    /*int twidth, boxwidth, old_color;*/
    /*int active;*/

    push_active_group();
    wc.x = node->x;
    wc.y = node->y - node->height + y_off + gRegionOffset;
    wc.height = node->height;
    wc.width = node->width - trailing_space(node->next);
#ifdef DEBUG
    fprintf(stderr, "Configure in  show_link %d %d %d %d\n",
            wc.x, wc.y, wc.width, wc.height);
#endif
    XConfigureWindow(gXDisplay, node->link->win,
        CWX | CWY | CWHeight | CWWidth, &wc);
    XMapWindow(gXDisplay, node->link->win);
}

/* display an input string window */

static void
show_input(TextNode *node)
{
    /*XFontStruct *old_font;*/
    XWindowChanges wc;
    /*int twidth, boxwidth, old_color;*/
    /*Window root, child;*/
    /*int root_x, root_y, win_x, win_y, buttons;*/
    InputItem *item;
    char *inpbuffer;

    item = node->link->reference.string;
    inpbuffer = item->curr_line->buffer;

    wc.border_width = 0;
    wc.x = node->x;
    wc.y = node->y + gRegionOffset + y_off - node->height + 2;
    wc.height = node->height - 2;
    wc.width = node->width;
    if (pix_visible(node->y, node->height)) {
        XConfigureWindow(gXDisplay, node->link->win,
                         CWX | CWY | CWHeight | CWWidth | CWBorderWidth,
                         &wc);
        XMapWindow(gXDisplay, node->link->win);
    }
    XFlush(gXDisplay);
    draw_inputsymbol(item);
}

static void
show_simple_box(TextNode *node)
{
    XWindowChanges wc;
    InputBox *box;

    /* first configure the box size properly */
    box = node->link->reference.box;
    wc.x = node->x;
    wc.y = node->y + gRegionOffset + y_off - node->height;
    wc.height = ((box->picked) ?
                 (box->selected->height) : (box->unselected->height));
    wc.width = node->width;
    if (visible(node->y + gTopOfGroupStack->cur_font->ascent, node->height)) {
        XConfigureWindow(gXDisplay, node->link->win, CWX | CWY | CWHeight | CWWidth,
                         &wc);
        XMapWindow(gXDisplay, node->link->win);
        if (box->picked)
            pick_box(box);
        else
            unpick_box(box);
    }
}

/* display a spad command node */

static void
show_spadcommand(TextNode *node)
{
    XWindowChanges wc;

    in_fricas_command = 1;

    push_spad_group();
    wc.x = node->x;
    if (node->type == Spadsrc)
        wc.y = node->y + gRegionOffset + y_off - 2 * node->height;
    else
        wc.y = node->y + gRegionOffset + y_off - node->height;
    wc.height = node->height;
    wc.width = node->width;
#ifdef DEBUG
    fprintf(stderr, "Spadcommand configured %d x %d -- (%d, %d)\n",
            wc.width, wc.height, wc.x, wc.y);
#endif
    XConfigureWindow(gXDisplay, node->link->win,
        CWX | CWY | CWHeight | CWWidth, &wc);
    XMapWindow(gXDisplay, node->link->win);
}


/* display a pixmap */

static void
show_image(TextNode *node, GC gc)
{
    int src_x, src_y, src_width, src_height, dest_x, dest_y, ret_val;

    if (!pix_visible(node->y, node->height))
        return;
    if (node->image.xi == NULL)
        return;

    dest_x = node->x;
    src_x = 0;
    src_y = 0;
    dest_y = node->y + gRegionOffset - node->height + y_off;
    need_scroll_up_button = 1;
    if (node->width > (right_margin - node->x))
        src_width = right_margin - node->x;
    else
        src_width = node->width;

    if (gDisplayRegion != Scrolling) {
        src_y = 0;
        src_height = node->image.xi->height;
    }
    else {
        /* I may have only a partial image */
        if (dest_y < 0) {       /* the top is cut off */
            src_y = -dest_y;
            dest_y = 0;
            src_height = node->image.xi->height - src_y;
        }
        else if (dest_y + node->image.xi->height > gWindow->scrollheight) {
            /* the bottom is cut off */
            src_y = 0;
            src_height = gWindow->scrollheight - dest_y;
        }
        else {                  /* the whole thing is visible */
            src_y = 0;
            src_height = node->image.xi->height;
        }
    }

    ret_val = XPutImage(gXDisplay, gWindow->fDisplayedWindow, gc,
            node->image.xi, src_x, src_y, dest_x, dest_y,
            src_width, src_height);

    switch (ret_val) {
      case BadDrawable:
        fprintf(stderr, "(HyperDoc: show_image) bad drawable\n");
        break;
      case BadGC:
        fprintf(stderr, "(HyperDoc: show_image) bad GC");
        break;
      case BadMatch:
        fprintf(stderr, "(HyperDoc: show_image) bad match");
        break;
      case BadValue:
#ifndef HP9platform
        fprintf(stderr, "(HyperDoc: show_image) bad value");
#endif /* HP complains about this*/
        break;
    }
}
