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

#ifndef _EXTENT_H_
#define _EXTENT_H_ 1

#include "fricas_c_macros.h"
#include "hyper.h"

/*
 * This file contains global macros extern declarations for the extent
 * computation routines found in extent1.c and extent2.c.
 */

/*
 * Definitions of standard text formatting dimensions, etc.
 *  dimensions given in pixels
 */

#define left_margin 20
#define non_scroll_right_margin_space 20
#define scroll_right_margin_space 40
#define bottom_margin 15
#define top_margin 5
#define scroll_top_margin top_margin
#define scrollingTopMargin 5
#define inter_line_space 5
#define inter_word_space 5
#define term_punct_space 5
#define paragraph_space 30
#define box_space 3
#define horiz_line_space 3
#define spadcom_indent 30
#define min_inter_column_space 10
#define box_width 3
#define dash_width 5
#define dash_y     4



#define not_in_scroll (!(gDisplayRegion == Scrolling))

#define visible(y, h) \
  (not_in_scroll  || ((y) + gRegionOffset + gWindow->page->scroll_off \
                  <= gWindow->scrollheight   && \
                  (y) + gRegionOffset + gWindow->page->scroll_off - (h) >=  0))

#define pix_visible(y, h) \
  (not_in_scroll  || ((y) + gRegionOffset + gWindow->page->scroll_off - h +  \
                   line_height  < gWindow->page->bot_scroll_margin  \
                      - gWindow->page->top_scroll_margin  && \
                  (y) + gRegionOffset + gWindow->page->scroll_off >=  0))

#define above(y) ((y) +  gWindow->page->scroll_off < gWindow->page->top_scroll_margin)
#define below(y) ((y) + gWindow->page->scroll_off >= gWindow->page->bot_scroll_margin)


/* Variables for the formatting state */

extern int right_margin_space;
extern int right_margin;
extern int indent;
extern int item_indent;
extern int text_x;
extern int text_y;
extern int y_off;
extern int scroll_bot;
extern int need_scroll_up_button;
extern int need_scroll_down_button;
extern int item_space;
extern int present_line_height;
extern int past_line_height;
extern int line_height;                /* space between lines              */
extern int normal_text_height;         /* space between lines              */
extern int space_width;                /* the maximum width of a character */
extern int word_off_height;            /* the diff between text height and */


/*
 * externs from extent1.c
 */

extern short int gExtentRegion;

extern short int in_fricas_command;   /* true iff we are in a \spadcommand */
extern short int gInDesc;
extern short int gInItem;        /* true iff we are in a \item */
extern short int gInLine;        /* true iff there have been words printed  */
extern short int gInTable;

extern TextNode *gLineNode;

#endif
