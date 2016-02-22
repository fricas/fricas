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
 * scrollbar.c:  HyperDoc Scrollbar routines
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"

#include "extent.h"
#include "hyper.h"
#include "parse.h"

#include "all_hyper_proto.H1"

/*************************************************************************
  Scrollbar Comments                                    10/08/89

  The scrollbar is displayed on the side of the HyperDoc display, if needed.
  It is composed of four windows

  fScrollUpWindow --  the arrowed box at the top of the scrollbar. Scrolls the
  window up a line at a time.
  fScrollDownWindow --  Located at the bottom of the window, it is used to scroll
  down a single line at a time.
  scrollbar -- this is the window which does the variable scrolling. It
  houses the actual scroller.
  scroller -- This is the scroller inside the scroll bar.

  The procedure below, makes all these windows, and also makes three bitmaps,
  sup -- The up arrow for the fScrollUpWindow.
  sdown -- the down arrow for the fScrollDownWindow.
  scroller -- the scroller stipple.
  It then fills the window with the proper Pixmap background.

  The scrollbar and scroller works as follows. The size of the scroller is
  calculated as

  size of scroller            size of visible text
  -----------------  ===  ------------------------------  .
  size of scrollbar       size of whole scrolling region

  The top of the scroller shows the relative position in the page of
  the top of the scrolling region. This way the user knows how far
  down the page he or she has moved.
  When the user clicks in the scrollbar, the center of the
  scroller, if possible, is placed at the point of the click.

  See the routines
  showScrollBars --  to see how the scroll bars are actually realized.
  moveScroller --  to see how the scroller is moved when the user scrolls


  **************************************************************************/

static int  ch(int height);
static void changeWindowBackgroundPixmap(Window window, Pixmap pixmap);

static Pixmap sup = 0, sdown = 0, sup_pressed = 0, sdown_pressed = 0, scroller = 0, scrollbar_pix = 0;


/* #undef BITMAPS2D to get old style 2d effect */

#ifdef BITMAPS2D

#define CONTROLS_3D 0

#include "sdown.bitmap"
#include "sup.bitmap"

#define BACKCOLOR gBackgroundColor
#define FORECOLOR gActiveColor

#else

#define CONTROLS_3D 1

#include "sdown3d.bitmap"
#include "sdown3dpr.bitmap"

#include "sup3d.bitmap"
#include "sup3dpr.bitmap"

#define sup_width  sup3d_width
#define sup_height sup3d_height
#define sup_bits sup3d_bits

#define sdown_width  sdown3d_width
#define sdown_height sdown3d_height
#define sdown_bits   sdown3d_bits

#define BACKCOLOR gControlBackgroundColor
#define FORECOLOR gControlForegroundColor

#endif


#if 0
#define scroller_width 3
#define scroller_height 3
static char scroller_bits[] = {0x05, 0x07, 0x03};

#endif

#define scroller_width 2
#define scroller_height 2
static char scroller_bits[] = {
                               0x01, 0x02};



static int supheight = sup_height;
static int supwidth = sup_width;

#define scrollbar_pix_width 3
#define scrollbar_pix_height 3
static char scrollbar_pix_bits[] = {0x00, 0x03, 0x00};



int gScrollbarWidth = sup_width + 2;



void
makeScrollBarWindows(void)
{
    XSetWindowAttributes at;

    at.cursor = gActiveCursor;
    at.event_mask = ButtonPress;
    /** create the bitmaps **/
    if (supwidth != sdown_width || supheight != sdown_height) {
        fprintf(stderr,
                "Scrollbar error, up and down pointers must have the same dimensions\n");
        exit(-1);
    }

    if (sup == 0)
        sup = XCreatePixmapFromBitmapData(gXDisplay,
                       RootWindow(gXDisplay, gXScreenNumber),
                       ucharp_to_charp(sup_bits), supwidth, supheight,
                       FORECOLOR, BACKCOLOR,
                       DefaultDepth(gXDisplay, gXScreenNumber));

    if (sdown == 0)
        sdown = XCreatePixmapFromBitmapData(gXDisplay,
                       RootWindow(gXDisplay, gXScreenNumber),
                       ucharp_to_charp(sdown_bits), sdown_width, sdown_height,
                       FORECOLOR, BACKCOLOR,
                       DefaultDepth(gXDisplay, gXScreenNumber));

    if (CONTROLS_3D) {
        sup_pressed = XCreatePixmapFromBitmapData(gXDisplay,
                        RootWindow(gXDisplay, gXScreenNumber),
                        ucharp_to_charp(sup3dpr_bits),
                        sup3dpr_width, sup3dpr_height,
                        FORECOLOR, BACKCOLOR,
                        DefaultDepth(gXDisplay, gXScreenNumber));
        sdown_pressed = XCreatePixmapFromBitmapData(gXDisplay,
                          RootWindow(gXDisplay, gXScreenNumber),
                          ucharp_to_charp(sdown3dpr_bits),
                          sdown3dpr_width, sdown3dpr_height,
                          FORECOLOR, BACKCOLOR,
                          DefaultDepth(gXDisplay, gXScreenNumber));
    }

    gWindow->fScrollUpWindow = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                                1, 1, supwidth, supheight,
                                                gWindow->border_width,
                                                gBorderColor,
                                                BACKCOLOR);

    gWindow->fScrollDownWindow = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                            1, 1, sdown_width, sdown_height,
                                                  gWindow->border_width,
                                                  gBorderColor,
                                                  BACKCOLOR);

    gWindow->scrollbar = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                              1, 1, 1, 1,
                                              gWindow->border_width,
                                              gBorderColor,
                                              BACKCOLOR);
    gWindow->scroller = XCreateSimpleWindow(gXDisplay, gWindow->scrollbar,
                                             1, 1, 1, 1, 0,
                                             gBorderColor,
                                             BACKCOLOR);

#ifdef DEBUG
    fprintf(stderr, "Changing Window Attributes in scrollbar.c #2\n");
#endif

    at.background_pixmap = sup;
    XChangeWindowAttributes(gXDisplay, gWindow->fScrollUpWindow,
                            CWBackPixmap | CWEventMask | CWCursor, &at);

    at.background_pixmap = sdown;
    XChangeWindowAttributes(gXDisplay, gWindow->fScrollDownWindow,
                            CWBackPixmap | CWEventMask | CWCursor, &at);

    XChangeWindowAttributes(gXDisplay, gWindow->scrollbar,
                            CWEventMask | CWCursor, &at);


    if (scroller == 0)
        scroller = XCreatePixmapFromBitmapData(gXDisplay,
                                    RootWindow(gXDisplay, gXScreenNumber),
                                               scroller_bits, scroller_width,
                                               scroller_height,
                                               FORECOLOR,
                                               BACKCOLOR,
                                 DefaultDepth(gXDisplay, gXScreenNumber));
    if (scrollbar_pix == 0)
        scrollbar_pix = XCreatePixmapFromBitmapData(gXDisplay,
                                    RootWindow(gXDisplay, gXScreenNumber),
                                                    scrollbar_pix_bits,
                                                    scrollbar_pix_width,
                                                    scrollbar_pix_height,
                                                    FORECOLOR,
                                                    BACKCOLOR,
                                 DefaultDepth(gXDisplay, gXScreenNumber));

    at.background_pixmap = scroller;
    XChangeWindowAttributes(gXDisplay, gWindow->scroller,
                            CWBackPixmap | CWCursor, &at);
    at.background_pixmap = scrollbar_pix;
    XChangeWindowAttributes(gXDisplay, gWindow->scrollbar,
                            CWBackPixmap, &at);
}

static void
drawScroller3DEffects(HDWindow * hdWindow, int x1, int y1, int x2, int y2)
{
    XClearWindow(gXDisplay, hdWindow->scroller);

    /* draw right "black" line */

    XDrawLine(gXDisplay, hdWindow->scroller, hdWindow->fControlGC,
              x2 - 3, y1 + 2, x2 - 3, y2 - 3);

    /* draw bottom "black" line */

    XDrawLine(gXDisplay, hdWindow->scroller, hdWindow->fControlGC,
              x1 + 2, y2 - 3, x2 - 3, y2 - 3);

    /* flip foreground and background colors */

    XSetBackground(gXDisplay, hdWindow->fControlGC, gControlForegroundColor);
    XSetForeground(gXDisplay, hdWindow->fControlGC, gControlBackgroundColor);

    /* draw top "white" line */

    XDrawLine(gXDisplay, hdWindow->scroller, hdWindow->fControlGC,
              x1 + 2, y1 + 2, x2 - 3, y1 + 2);

    /* draw left "white" line */

    XDrawLine(gXDisplay, hdWindow->scroller, hdWindow->fControlGC,
              x1 + 2, y1 + 2, x1 + 2, y2 - 3);

    /* reset colors */

    XSetBackground(gXDisplay, hdWindow->fControlGC, gControlBackgroundColor);
    XSetForeground(gXDisplay, hdWindow->fControlGC, gControlForegroundColor);
}

void
showScrollBars(HDWindow * hdWindow)
{
    XWindowChanges wc;
    /*int src_x = 0, src_y = 0;*/
    /*unsigned int width = supwidth, height = supheight;*/
    /*int dest_x = 0, dest_y = 0;*/

    /* see if we even need scroll bars */

    if (hdWindow->page->scrolling->height <= hdWindow->scrollheight)
        return;

    wc.x = hdWindow->scrollx;
    wc.y = hdWindow->scrollupy;
    wc.height = supheight;
    wc.width = supwidth;
    XConfigureWindow(gXDisplay, hdWindow->fScrollUpWindow, CWX | CWY | CWHeight
                     | CWWidth, &wc);
    wc.y = hdWindow->scrolldowny;
    XConfigureWindow(gXDisplay, hdWindow->fScrollDownWindow,
                     CWX | CWY | CWHeight | CWWidth,
                     &wc);
    wc.height = hdWindow->fScrollBarHeight;
    wc.y = hdWindow->scrollbary;
    XConfigureWindow(gXDisplay, hdWindow->scrollbar,
                     CWX | CWY | CWHeight | CWWidth,
                     &wc);
    wc.x = 0;
    wc.y = hdWindow->fScrollerTopPos;
    wc.width = supwidth;
    wc.height = hdWindow->fScrollerHeight;
    XConfigureWindow(gXDisplay, hdWindow->scroller,
                     CWX | CWY | CWHeight | CWWidth,
                     &wc);

    /*
     * Now we map the windows, since the bitmaps are the backgrounds for the
     * windows, we need to worry about redrawing them.
     */

    XMapWindow(gXDisplay, hdWindow->fScrollUpWindow);
    XMapWindow(gXDisplay, hdWindow->fScrollDownWindow);
    XMapWindow(gXDisplay, hdWindow->scrollbar);
    XMapWindow(gXDisplay, hdWindow->scroller);

    drawScroller3DEffects(hdWindow, 0, 0, wc.width, wc.height);
}


/************************************************************************

  Moves the scroller to its proper place within the scrollbar. It
  calculates how far down the page we are, and then moves the scroller
  accordingly

  **************************************************************************/

void
moveScroller(HDWindow * hdWindow)
{
    XWindowChanges wc;

    /** moves the scroller to it's proper place **/

    int t = (int) (hdWindow->fScrollBarHeight * (-hdWindow->page->scroll_off));
    hdWindow->fScrollerTopPos = (int) (t / hdWindow->page->scrolling->height);
    wc.x = 0;
    wc.y = hdWindow->fScrollerTopPos;
    wc.width = supwidth;
    wc.height = hdWindow->fScrollerHeight;
    XConfigureWindow(gXDisplay, hdWindow->scroller,
                     CWX | CWY | CWHeight | CWWidth,
                     &wc);
    drawScroller3DEffects(hdWindow, 0, 0, wc.width, wc.height);
}

#define tophalf(y) ((y % 2 == 0)?(y/2):(y/2) + 1)
#define bothalf(y) (y/2)

void
drawScrollLines(void)
{
    /* Checks the page_flags to see if we need a top, or a bottom line.   */
    /* These are the horizontal lines framing a scrolling region when the */
    /* scrolling region is not the entire window.                         */

    if (!(gWindow->page->page_flags & NOLINES)) {
        line_top_group();
        if (gWindow->page->header->height) {
            XDrawLine(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC,
                      0,
                      gWindow->page->top_scroll_margin - tophalf(gWindow->border_width)
                      - 2 * scroll_top_margin,
                      gWindow->scrollwidth,
                      gWindow->page->top_scroll_margin - tophalf(gWindow->border_width)
                      - 2 * scroll_top_margin);
        }
        if (gWindow->page->footer->height) {
            XDrawLine(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC,
                      0,
                      gWindow->page->bot_scroll_margin + bothalf(gWindow->border_width) - 1,
                      gWindow->scrollwidth,
                      gWindow->page->bot_scroll_margin + bothalf(gWindow->border_width) - 1);
        }
        pop_group_stack();
    }
}


/*
 * Calculates all the measures for the scrollbars
 */

void
calculateScrollBarMeasures(void)
{
    int t;

    /*
     * The scrollhieght is the height of the scrolling region visible in the
     * HT window. Notice how it is a multiple of line height. This was needed
     * to make everything scroll nicely.
     */

    gWindow->scrollheight = gWindow->page->bot_scroll_margin -
        gWindow->page->top_scroll_margin - scroll_top_margin;
    gWindow->scrollheight = gWindow->scrollheight - gWindow->scrollheight % line_height;

    /*
     * Now do a quick check to see if I really need a scroll bar, and if not,
     * just return right away
     */

    if (gWindow->scrollheight >= gWindow->page->scrolling->height) {
        gWindow->page->scroll_off = 0;
        return;
    }

    /*
     * The height of the scrollbar region, extends form the top page margin
     * all the way to the bottom, excluding the room needed for the up and
     * down windows
     */

    gWindow->fScrollBarHeight = gWindow->page->bot_scroll_margin -
        gWindow->page->top_scroll_margin - 2 * supheight -
        2 * gWindow->border_width;

    gWindow->scrollupy = gWindow->page->top_scroll_margin - gWindow->border_width;
    gWindow->scrollupy -= 2 * scroll_top_margin;
    gWindow->scrolldowny = gWindow->page->bot_scroll_margin
        - supheight - gWindow->border_width;
    gWindow->scrollbary = gWindow->scrollupy + supheight + gWindow->border_width;
    gWindow->scrollx = gWindow->width - supwidth - gWindow->border_width;

    /*
     * the scroller height is calculated from the following formula
     *
     * fScrollerHeight                    scrollheight --------------       ==
     * --------- ------------- fScrollBarHeight
     * page->scrolling_height
     *
     */

    gWindow->fScrollerHeight = 1 + 2 * scroll_top_margin +        /** possible integer error correction **/
        (int) (gWindow->fScrollBarHeight * gWindow->scrollheight / gWindow->page->scrolling->height);

    /*
     * Check the scroll offset, to see if it is too Large
     */

    if (-(gWindow->page->scroll_off) >
        (gWindow->page->scrolling->height - gWindow->scrollheight))
        gWindow->page->scroll_off =
            -(gWindow->page->scrolling->height - gWindow->scrollheight);

    /*
     * Then move the top of the scroller to it's proper position
     */

    gWindow->fScrollBarHeight += 2 * scroll_top_margin;
    t = (int) (gWindow->fScrollBarHeight * (-gWindow->page->scroll_off));
    gWindow->fScrollerTopPos = (int) (t / (gWindow->page->scrolling->height));
}

void
linkScrollBars(void)
{
    HyperLink *uplink = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");
    HyperLink *downlink = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");
    HyperLink *barlink = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");

    uplink->win = gWindow->fScrollUpWindow;
    downlink->win = gWindow->fScrollDownWindow;
    barlink->win = gWindow->scrollbar;
    uplink->type = Scrollupbutton;
    downlink->type = Scrolldownbutton;
    barlink->type = Scrollbar;
    barlink->x = barlink->y = 0;
    uplink->x = uplink->y = 0;
    downlink->x = downlink->y = 0;
    uplink->reference.node = NULL;
    downlink->reference.node = NULL;
    hash_insert(gLinkHashTable, (char *)uplink,(char *) &uplink->win);
    hash_insert(gLinkHashTable, (char *)barlink,(char *) &barlink->win);
    hash_insert(gLinkHashTable, (char *)downlink,(char *) &downlink->win);
}

void
scrollUp(void)
{

    if (gWindow->page->scroll_off == 0);       /* BeepAtTheUser(); *//* The
                                                 * beeping annoyed me. RSS */
    else {
        changeWindowBackgroundPixmap(gWindow->fScrollUpWindow, sup_pressed);

        gWindow->page->scroll_off += line_height;      /* Scroll a line */
        if (gWindow->page->scroll_off > 0)
            gWindow->page->scroll_off = 0;
        XCopyArea(gXDisplay, gWindow->fScrollWindow, gWindow->fScrollWindow, gWindow->fStandardGC,
                  0, 0,
            gWindow->scrollwidth, gWindow->scrollheight - line_height + 1,
                  0, line_height);
        XClearArea(gXDisplay, gWindow->fScrollWindow, 0, 0,
                   gWindow->scrollwidth,
                   line_height, False);
        scroll_page(gWindow->page);

        changeWindowBackgroundPixmap(gWindow->fScrollUpWindow, sup);
    }

}

void
scrollUpPage(void)
{
    if (gWindow->page->scroll_off == 0);       /* BeepAtTheUser(); */
    else {
        /* Scroll a page */

        gWindow->page->scroll_off += ch(gWindow->scrollheight) - line_height;
        if (gWindow->page->scroll_off > 0)
            gWindow->page->scroll_off = 0;

        XClearWindow(gXDisplay, gWindow->fScrollWindow);
        scroll_page(gWindow->page);
    }
}

void
scrollToFirstPage(void)
{
    if (gWindow->page->scroll_off == 0);       /* BeepAtTheUser(); */
    else {
        gWindow->page->scroll_off = 0;
        XClearWindow(gXDisplay, gWindow->fScrollWindow);
        scroll_page(gWindow->page);
    }
}

void
scrollDown(void)
{

    if (-(gWindow->page->scroll_off) >=
        (gWindow->page->scrolling->height - gWindow->scrollheight)) {
        ;                       /* BeepAtTheUser(); */
    }
    else {
        changeWindowBackgroundPixmap(gWindow->fScrollDownWindow, sdown_pressed);

        gWindow->page->scroll_off -= line_height;      /* Scroll a line */

        XCopyArea(gXDisplay, gWindow->fScrollWindow, gWindow->fScrollWindow, gWindow->fStandardGC,
                  0, line_height,
            gWindow->scrollwidth, gWindow->scrollheight - line_height + 1,
                  0, 0);
        XClearArea(gXDisplay, gWindow->fScrollWindow, 0,
                   gWindow->scrollheight - line_height,
                   gWindow->scrollwidth,
                   line_height, False);
        scroll_page(gWindow->page);

        changeWindowBackgroundPixmap(gWindow->fScrollDownWindow, sdown);
    }
}


void
scrollDownPage(void)
{
    if (gWindow->page->scrolling == NULL || (-(gWindow->page->scroll_off) >=
            (gWindow->page->scrolling->height - gWindow->scrollheight))) {
        ;                       /* BeepAtTheUser(); */
    }
    else {
        gWindow->page->scroll_off -= ch(gWindow->scrollheight) - line_height;

        if (-(gWindow->page->scroll_off) >
            (gWindow->page->scrolling->height - gWindow->scrollheight))
            gWindow->page->scroll_off = -
                (gWindow->page->scrolling->height - gWindow->scrollheight);

        XClearWindow(gXDisplay, gWindow->fScrollWindow);

        scroll_page(gWindow->page);
    }
}

void
scrollScroller(XButtonEvent * event)
{

    /*
     * This routine checks to see where in the window the button press
     * occured. It then tries to move the scroller so that the top of the
     * scroller is at the spot of the event
     */

    int y = event->y;
    int top = y;

    if (top < 0) {
        top = 0;
        if (gWindow->fScrollerTopPos == 0)
            return;
        gWindow->page->scroll_off = 0;
    }
    else if ((top + gWindow->fScrollerHeight) > gWindow->fScrollBarHeight) {
        top = gWindow->fScrollBarHeight - gWindow->fScrollerHeight;
        if (top == gWindow->fScrollerTopPos)
            return;
        gWindow->page->scroll_off =
            -(gWindow->page->scrolling->height - gWindow->scrollheight);
        gWindow->page->scroll_off -= gWindow->page->scroll_off % line_height;
    }
    else {                      /** top is in an ok spot **/
        int t;

        t = -(gWindow->page->scrolling->height) * top;
        t = t / (gWindow->fScrollBarHeight);
        if (gWindow->page->scroll_off == (t -= t % line_height))
            return;
        gWindow->page->scroll_off = t;
        gWindow->fScrollerTopPos = top;
    }
    XClearWindow(gXDisplay, gWindow->fScrollWindow);
    scroll_page(gWindow->page);
}


void
hideScrollBars(HDWindow * hdWindow)
{
    XUnmapWindow(gXDisplay, hdWindow->fScrollDownWindow);
    XUnmapWindow(gXDisplay, hdWindow->fScrollUpWindow);
    XUnmapWindow(gXDisplay, hdWindow->scrollbar);
    XUnmapWindow(gXDisplay, hdWindow->scroller);
}

void
getScrollBarMinimumSize(int *width, int *height)
{
    (*width)  = sup_width + 4;
    (*height) = sup_height + sdown_height + 5;
}

static int
ch(int height)
{
    /*int rheight;*/
    int rem = height % line_height;

    if (rem == 0)
        return height;
    return height - rem + line_height;
}

static void
changeWindowBackgroundPixmap(Window window, Pixmap pixmap)
{
    if (pixmap) {
        XSetWindowAttributes at;

        at.background_pixmap = pixmap;
        XChangeWindowAttributes(gXDisplay, window, CWBackPixmap, &at);
        XClearWindow(gXDisplay, window);
    }
}
