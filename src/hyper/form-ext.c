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

#include "extent.h"
#include "hyper.h"

#include "all_hyper_proto.H1"

static int window_height(HyperDocPage * page);
static void form_header_extent(HyperDocPage * page);
static void form_footer_extent(HyperDocPage * page);
static void form_scrolling_extent(HyperDocPage * page);

/*
 * A few routines used to help with form extents
 */

void
compute_form_page(HyperDocPage *page)
{

    /*
     * To solve the problem of improperly nested \em, I will have to keep and
     * always initialize the top of the stack
     */
    while (pop_group_stack() >= 0);

    /*
     * The compute the text extents
     */
    form_header_extent(page);
    form_footer_extent(page);
    form_scrolling_extent(page);
    gWindow->height = window_height(gWindow->page);

}

/*
 * A simple function that returns the width needed to store show the number
 * of columns given
 */
int
window_width(int cols)
{
    return (left_margin + cols * space_width + non_scroll_right_margin_space);
}


static int
window_height(HyperDocPage *page)
{
    int temp;

    temp = page->header->height + top_margin + bottom_margin;

    if (page->scrolling)
        temp += page->scrolling->height + page->footer->height;

    return (temp);
}


static void
form_header_extent(HyperDocPage *page)
{

    /*
     * Hopefully I will soon be able to actually compute the needed height
     * for the header here
     */
    gExtentRegion = Header;
    right_margin_space = non_scroll_right_margin_space;
    init_extents();
    text_y = top_margin + line_height;
    compute_text_extent(page->header->next);
    page->header->height = (gInLine) ? text_y : text_y - past_line_height;
    if (!(page->page_flags & NOLINES))
        page->header->height += (int) line_height / 2;
    page->header->height += gWindow->border_width;
}

static void
form_footer_extent(HyperDocPage *page)
{
    if (page->footer) {
        gExtentRegion = Footer;
        right_margin_space = non_scroll_right_margin_space;
        init_extents();

        compute_text_extent(page->footer->next);

        /*
         * I inserted the 2nd arg to text_height below because it
         * was missing. Perhaps there is a better value for it.
         */

        page->footer->height = text_height(page->footer->next,
            page->footer->next->type);
        if ((!page->page_flags & NOLINES))
            page->footer->height += (int) line_height / 2;
    }
}

static void
form_scrolling_extent(HyperDocPage *page)
{

    /*
     * Check to see if there is a scrolling region
     */

    if (page->scrolling) {
        /*
         * If there is then compute all the proper locations
         */

        gExtentRegion = Scrolling;
        right_margin_space = non_scroll_right_margin_space + gScrollbarWidth;
        init_extents();
        text_y = line_height;
        compute_text_extent(page->scrolling->next);
        if (!gInLine)
            text_y = text_y - past_line_height;
        else if (present_line_height > line_height)
            text_y = text_y + present_line_height - line_height;
        page->scrolling->height = text_y;
    }
}
