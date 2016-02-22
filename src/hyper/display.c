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
 * display.c:  HyperDoc functions to format and display a page.
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

/*
 *  Notes:
 *      Display is performed in two steps.  First the page is formatted
 *      assuming that we have an infinitely long window.  In this stage
 *      we compute and store the coordinates of every text node.  Next
 *      the page is actually drawn on the screen.  In this process we
 *      use the value of page->y_off as an offset into the scrolling
 *      region to compute what is actually to be displayed on the page.
 */
#define _DISPLAY_C
#include "fricas_c_macros.h"
#include "debug.h"


#include "extent.h"
#include "hyper.h"

#include "all_hyper_proto.H1"


extern ItemStack *gTopOfItemStack;
short int gDisplayRegion = 0;
int gRegionOffset = 0;


/* Display a HyperDoc page in the top-level window */

void
show_page(HyperDocPage *page)
{
    XWindowChanges wc;
    int doShowScrollBars = 1;

    init_top_group();

    /* Clear the areas so we can rewrite the page */

    XClearWindow(gXDisplay, gWindow->fMainWindow);
    XClearWindow(gXDisplay, gWindow->fScrollWindow);

    /* Free the active button list */

    free_button_list(page->s_button_list);
    page->s_button_list = NULL;
    free_button_list(page->button_list);
    page->button_list = NULL;

    /* The compute the text extents  */
    compute_title_extent(page);
    compute_header_extent(page);
    compute_footer_extent(page);
    compute_scrolling_extent(page);

    /*
     * Now that we have all the extents computed, reconfigure and map the
     * scroll window
     */

    if (page->scrolling) {
        int width, height;
        calculateScrollBarMeasures();
        wc.x = 0;
        wc.y = page->top_scroll_margin + scroll_top_margin;

        wc.height = gWindow->scrollheight;
        if (gWindow->page->scrolling->height <= gWindow->scrollheight) {
            gWindow->page->scroll_off = 0;
            wc.width = gWindow->width;
        }
        else
            wc.width = gWindow->width - gScrollbarWidth;

        getScrollBarMinimumSize(&width, &height);
        if (height > wc.height) {
            wc.height = gWindow->scrollheight = 1;
            doShowScrollBars = 0;
        }
        else
            gWindow->scrollwidth = wc.width;

        if (doShowScrollBars) {
            XConfigureWindow(gXDisplay, gWindow->fScrollWindow, CWX | CWY | CWHeight | CWWidth, &wc);
            XMapWindow(gXDisplay, gWindow->fScrollWindow);
        }
        else {
            XUnmapWindow(gXDisplay, gWindow->fScrollWindow);
            hideScrollBars(gWindow);
        }
    }
    /* clear the group stack */

    while (pop_group_stack() >= 0)
        ;

    /* Now start displaying all the text */

    gWindow->fDisplayedWindow = gWindow->fMainWindow;
    gRegionOffset = 0;
    y_off = 0;
    gDisplayRegion = Header;
    show_text(page->header->next, Endheader);

    if (doShowScrollBars && page->scrolling) {
        /* Show the footer  */
        if (page->footer->next) {
            gDisplayRegion = Footer;
            gRegionOffset = gWindow->page->bot_scroll_margin +
                (!((gWindow->page->page_flags & NOLINES)) ? ((int) line_height / 2) : (0));
            show_text(page->footer->next, Endfooter);
            /* Show the scrolling region */
            if (page->scrolling->next)
                gDisplayRegion = Scrolling;
            gRegionOffset = 0;
            gWindow->fDisplayedWindow = gWindow->fScrollWindow;
            y_off = gWindow->page->scroll_off;
            show_text(page->scrolling->next, Endscrolling);
            showScrollBars(gWindow);
        }
        drawScrollLines();
    }
    if (gTopOfItemStack != NULL) {
        fprintf(stderr, "warning: unbalanced \\beginitems .. \\enditems\n");
        gTopOfItemStack = NULL;
    }
    showTitleBar();
    XFlush(gXDisplay);
}

void
expose_page(HyperDocPage *page)
{
    int width, height, doShowScrollBars = 1;
    init_top_group();

    /*
     * Now start displaying all the text
     */

    y_off = 0;
    gWindow->fDisplayedWindow = gWindow->fMainWindow;
    gRegionOffset = 0;
    gDisplayRegion = Header;
    show_text(page->header->next, Endheader);
    getScrollBarMinimumSize(&width, &height);

    /*
     * Now see If I have anything left to display
     */
    if (page->scrolling) {
        if (page->footer->next) {
            gDisplayRegion = Footer;
            gRegionOffset = gWindow->page->bot_scroll_margin +
                (!((gWindow->page->page_flags & NOLINES)) ? ((int) line_height / 2) : (0));
            show_text(page->footer->next, Endfooter);
        }

        if (height > gWindow->scrollheight) {
            gWindow->scrollheight = 1;
            doShowScrollBars = 0;
            XUnmapWindow(gXDisplay, gWindow->fScrollWindow);
            hideScrollBars(gWindow);
        }

        if (page->scrolling->next) {
            gRegionOffset = page->top_scroll_margin;
            gDisplayRegion = Scrolling;
            gRegionOffset = 0;
            gWindow->fDisplayedWindow = gWindow->fScrollWindow;
            y_off = gWindow->page->scroll_off;
            show_text(page->scrolling->next, Endscrolling);
            if (doShowScrollBars)
                showScrollBars(gWindow);
        }
        if (doShowScrollBars)
            drawScrollLines();
    }
    showTitleBar();
    XFlush(gXDisplay);
}

void
scroll_page(HyperDocPage *page)
{
    init_top_group();
    /* free the active button list */
    free_button_list(page->s_button_list);
    page->s_button_list = NULL;
    /** Clear the scrolling area */
    XUnmapSubwindows(gXDisplay, gWindow->fScrollWindow);
    gDisplayRegion = Scrolling;
    gRegionOffset = 0;
    gWindow->fDisplayedWindow = gWindow->fScrollWindow;
    y_off = gWindow->page->scroll_off;
    show_text(page->scrolling->next, Endscrolling);
    moveScroller(gWindow);
    XFlush(gXDisplay);
}

void
paste_page(TextNode *node)
{
    int width, height;
    int old_off = gWindow->page->scroll_off;

    /* free the active button list */
    free_button_list(gWindow->page->s_button_list);
    gWindow->page->s_button_list = NULL;
    free_button_list(gWindow->page->button_list);
    gWindow->page->button_list = NULL;
    XUnmapSubwindows(gXDisplay, gWindow->fScrollWindow);

    init_top_group();

    /* recompute the extent of the scrolling region */

    compute_scrolling_extent(gWindow->page);

    calculateScrollBarMeasures();
    getScrollBarMinimumSize(&width, &height);

    /* get ready to show the scrolling area */
    gRegionOffset = 0;
    y_off = gWindow->page->scroll_off;
    gDisplayRegion = Scrolling;
    gWindow->fDisplayedWindow = gWindow->fScrollWindow;
    if (gWindow->page->scroll_off == old_off) {
        XClearArea(gXDisplay, gWindow->fScrollWindow, 0,
                   node->y - line_height + gRegionOffset + y_off,
                   gWindow->width,
                   gWindow->scrollheight - node->y + line_height - y_off,
                   False);
        XFlush(gXDisplay);
    }
    else
        XClearWindow(gXDisplay, gWindow->fScrollWindow);

    show_text(gWindow->page->scrolling->next, Endscrolling);
    XFlush(gXDisplay);
    hideScrollBars(gWindow);

    if (height > gWindow->scrollheight) {
        gWindow->scrollheight = 1;
        XUnmapWindow(gXDisplay, gWindow->fScrollWindow);
    }
    else {
        showScrollBars(gWindow);
        drawScrollLines();
        /* moveScroller(); */
    }
    XFlush(gXDisplay);
}
