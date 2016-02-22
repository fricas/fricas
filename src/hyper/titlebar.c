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
 * titlebar.c:  Produces HyperDoc titlebar
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"

#include <stdlib.h>

#include "hyper.h"
#include "parse.h"

#include "all_hyper_proto.H1"

static void readTitleBarImages(void);

extern int y_off;               /* y offset for scrolling regions */

/* Images for the title bar windows */

static XImage *tw1image = NULL,
              *tw2image = NULL,
              *tw3image = NULL,
              *tw4image = NULL,
              *noopimage = NULL;

/* #undef BITMAPS2D to get old style 2d effect */

#ifdef BITMAPS2D

static char *tw1file  = "exit.bitmap";
static char *tw2file  = "help2.bitmap";
static char *tw3file  = "return3.bitmap";
static char *tw4file  = "up3.bitmap";
static char *noopfile = "noop.bitmap";

#define BACKCOLOR gBackgroundColor
#define BUTTGC    fStandardGC

#else

static char *tw1file  = "exit3d.bitmap";
static char *tw2file  = "help3d.bitmap";
static char *tw3file  = "home3d.bitmap";
static char *tw4file  = "up3d.bitmap";
static char *noopfile = "noop3d.bitmap";

#define BACKCOLOR gControlBackgroundColor
#define BUTTGC    fControlGC

#endif


int twwidth, twheight;   /* the width and height for all windows in the */
                         /* title bar */

void
makeTitleBarWindows(void)
{
    XSetWindowAttributes at;
    unsigned long valuemask = 0L;

    /* read the images if we don't have them already */

    if (tw1image == NULL)
        readTitleBarImages();

    /* set the window attributes */

    at.cursor = gActiveCursor;
    valuemask |= CWCursor;
    at.event_mask = ButtonPress;
    valuemask |= CWEventMask;

    /* create the windows for the buttons */

    gWindow->fTitleBarButton1 = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                   1, 1, twwidth, twheight,
                                   0, gBorderColor, BACKCOLOR);
    XChangeWindowAttributes(gXDisplay, gWindow->fTitleBarButton1, valuemask, &at);

    gWindow->fTitleBarButton2 = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                   1, 1, twwidth, twheight,
                                   0, gBorderColor, BACKCOLOR);
    XChangeWindowAttributes(gXDisplay, gWindow->fTitleBarButton2, valuemask, &at);

    gWindow->fTitleBarButton3 = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                   1, 1, twwidth, twheight,
                                   0, gBorderColor, BACKCOLOR);
    XChangeWindowAttributes(gXDisplay, gWindow->fTitleBarButton3, valuemask, &at);

    gWindow->fTitleBarButton4 = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                   1, 1, twwidth, twheight,
                                   0, gBorderColor, BACKCOLOR);
    XChangeWindowAttributes(gXDisplay, gWindow->fTitleBarButton4, valuemask, &at);
}

void
showTitleBar(void)
{
    XWindowChanges wc;
    int height, hbw = (int) gWindow->border_width / 2;
    XImage *image;

    /*
     * the first thing we do is pop up all the windows and
     * place them properly
     */

    if (gWindow->page->title->height != twheight)
        height = gWindow->page->title->height;
    else
        height = twheight;

    push_active_group();

    /* configure and map button number 1 */

    wc.x = 0;
    wc.y = 0;
    wc.height = twheight;
    wc.width = twwidth;
    XConfigureWindow(gXDisplay, gWindow->fTitleBarButton1, CWX | CWY | CWHeight | CWWidth, &wc);
    XMapWindow(gXDisplay, gWindow->fTitleBarButton1);

    image = tw1image;
    XPutImage(gXDisplay, gWindow->fTitleBarButton1, gWindow->BUTTGC,
              image, 0, 0, 0, 0,
              image->width,
              image->height);

    /* configure and map button number 2 */

    wc.x += twwidth + gWindow->border_width;
    XConfigureWindow(gXDisplay, gWindow->fTitleBarButton2, CWX | CWY | CWHeight | CWWidth, &wc);
    XMapWindow(gXDisplay, gWindow->fTitleBarButton2);

    image = need_help_button ? tw2image : noopimage;
    XPutImage(gXDisplay, gWindow->fTitleBarButton2, gWindow->BUTTGC,
              image, 0, 0, 0, 0,
              image->width,
              image->height);

    /* configure and map button number 4 */

    wc.x = gWindow->width - twwidth;
    XConfigureWindow(gXDisplay, gWindow->fTitleBarButton4, CWX | CWY | CWHeight | CWWidth, &wc);
    XMapWindow(gXDisplay, gWindow->fTitleBarButton4);

    image = need_up_button ? tw4image : noopimage;
    XPutImage(gXDisplay, gWindow->fTitleBarButton4, gWindow->BUTTGC,
              image, 0, 0, 0, 0,
              image->width,
              image->height);

    /* configure and map button number 3 */

    wc.x = wc.x - twwidth - gWindow->border_width;
    XConfigureWindow(gXDisplay, gWindow->fTitleBarButton3, CWX | CWY | CWHeight | CWWidth, &wc);
    XMapWindow(gXDisplay, gWindow->fTitleBarButton3);

    image = need_return_button ? tw3image : noopimage;
    XPutImage(gXDisplay, gWindow->fTitleBarButton3, gWindow->BUTTGC,
              image, 0, 0, 0, 0,
              image->width,
              image->height);

    gWindow->fDisplayedWindow = gWindow->fMainWindow;
    gDisplayRegion = Title;
    gRegionOffset = 0;
    y_off = 0;

    pop_group_stack();

    show_text(gWindow->page->title->next, Endheader);

    /* Now draw the box around the title */

    line_top_group();

#if BITMAPS2D
    XDrawRectangle(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC, gWindow->page->title->x,
                   -hbw,
                   wc.x - gWindow->page->title->x - hbw,
                   height + 2 * hbw);
#endif

    XDrawLine(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC, 0, height + hbw,
              gWindow->width, height + hbw);

#if BITMAPS2D
    /* Now draw the lines down the middle */

    XDrawLine(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC,
              twwidth + hbw, 0,
              twwidth + hbw, height);
    XDrawLine(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC,
              gWindow->width - twwidth - hbw, 0,
              gWindow->width - twwidth - hbw, height);
#endif

    pop_group_stack();

#if BITMAPS2D
    /* now fill the areas under the bitmaps if we have to */

    if (gWindow->page->title->height > twheight) {
        push_active_group();
        height = height - twheight;

        XFillRectangle(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC, 0,
                       twheight, twwidth, height);
        XFillRectangle(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC,
                       twwidth + gWindow->border_width,
                       twheight, twwidth, height);
        XFillRectangle(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC,
                       gWindow->width - 2 * twwidth - gWindow->border_width,
                       twheight, twwidth, height);
        XFillRectangle(gXDisplay, gWindow->fMainWindow, gWindow->fStandardGC,
                       gWindow->width - twwidth,
                       twheight, twwidth, height);
        pop_group_stack();
    }
#endif
}

void
linkTitleBarWindows(void)
{
    HyperLink *tw1link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink"),
              *tw2link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink"),
              *tw3link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink"),
              *tw4link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");

    tw1link->win = gWindow->fTitleBarButton1;
    tw1link->type = Quitbutton;
    tw1link->reference.node = NULL;
    tw1link->x = tw1link->y = 0;

    tw2link->win = gWindow->fTitleBarButton2;
    tw2link->type = Helpbutton;
    tw2link->reference.node = NULL;
    tw2link->x = tw2link->y = 0;

    tw3link->win = gWindow->fTitleBarButton3;
    tw3link->type = Returnbutton;
    tw3link->reference.node = NULL;
    tw3link->x = tw3link->y = 0;

    tw4link->win = gWindow->fTitleBarButton4;
    tw4link->type = Upbutton;
    tw4link->reference.node = NULL;
    tw4link->x = tw4link->y = 0;

    hash_insert(gLinkHashTable, (char *)tw1link,(char *) &tw1link->win);
    hash_insert(gLinkHashTable, (char *)tw2link,(char *) &tw2link->win);
    hash_insert(gLinkHashTable, (char *)tw3link,(char *) &tw3link->win);
    hash_insert(gLinkHashTable, (char *)tw4link,(char *) &tw4link->win);
}

static void
readTitleBarImages(void)
{
    int w, h;
    char filename[128];
    char *fricas_env_var = NULL;

    fricas_env_var = getenv("AXIOM");

    if (fricas_env_var)
        sprintf(filename, "%s/share/hypertex/bitmaps/%s",
                fricas_env_var, tw1file);
    else
        sprintf(filename, "%s", tw1file);
    tw1image = HTReadBitmapFile(gXDisplay, gXScreenNumber, filename,
                                &twwidth, &twheight);

    if (fricas_env_var)
        sprintf(filename, "%s/share/hypertex/bitmaps/%s",
                fricas_env_var, tw2file);
    else
        sprintf(filename, "%s", tw2file);
    tw2image = HTReadBitmapFile(gXDisplay, gXScreenNumber, filename,
                                &w, &h);
    twwidth = ((twwidth >= w) ? (twwidth) : (w));

    if (fricas_env_var)
        sprintf(filename, "%s/share/hypertex/bitmaps/%s",
                fricas_env_var, tw3file);
    else
        sprintf(filename, "%s", tw3file);
    tw3image = HTReadBitmapFile(gXDisplay, gXScreenNumber, filename,
                                &w, &h);
    twwidth = ((twwidth >= w) ? (twwidth) : (w));

    if (fricas_env_var)
        sprintf(filename, "%s/share/hypertex/bitmaps/%s",
                fricas_env_var, tw4file);
    else
        sprintf(filename, "%s", tw4file);
    tw4image = HTReadBitmapFile(gXDisplay, gXScreenNumber, filename,
                                &w, &h);
    twwidth = ((twwidth >= w) ? (twwidth) : (w));


    if (fricas_env_var)
        sprintf(filename, "%s/share/hypertex/bitmaps/%s",
                fricas_env_var, noopfile);
    else
        sprintf(filename, "%s", noopfile);
    noopimage = HTReadBitmapFile(gXDisplay, gXScreenNumber, filename,
                                 &twwidth, &twheight);
}

void
getTitleBarMinimumSize(int *width, int *height)
{
    (*width)  = 4 * twwidth + 40;
    (*height) = twheight + 2;
}
