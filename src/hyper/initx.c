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

/****************************************************************
 *
 * initx.c:  HyperDoc X Window window initialization code
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

/* #define DEBUG  1 */

#include "fricas_c_macros.h"
#include "debug.h"

#include <unistd.h>
#include <sys/signal.h>
#include <setjmp.h>
#include <X11/cursorfont.h>
#include <X11/Xresource.h>
#include <X11/Xatom.h>

#ifdef SUN4OS5platform
extern int gethostname(char *, int );
#endif

#include "ht_icon"
#include "extent.h"
#include "hyper.h"

#include "all_hyper_proto.H1"
#include "util.H1"

#include "spadcolors.h"
#include "spadcolors.H1"


#include "mouse11.bitmap"
#include "mouse11.mask"

static void get_GCs(HDWindow * window);
static int get_border_properties(void);
static int get_color(char * name, char * class, int def, Colormap * map);
static void ingItColors_and_fonts(void);
static void mergeDatabases(void);
static void open_window(Window w);
static void set_name_and_icon(void);
static void set_size_hints(Window w);

static GContext server_font;
unsigned long *spadColors;
int scrn;  /* used in spad_colors */

extern int received_window_request;     /* true iff Spad wants a pop-up */
extern int in_next_event;       /* true when in XNextEvent      */

extern int gTtFontIs850;

#define MIN_WINDOW_SIZE 300


int gActiveColor,
    fricas_color,
    gBackgroundColor,
    gBfColor,
    gControlBackgroundColor,
    gControlForegroundColor,
    gEmColor,
    gInputBackgroundColor,
    gInputForegroundColor,
    gItColor,
    gRmColor,
    gSlColor,
    gTtColor;

XFontStruct * fricas_font,
            *gActiveFont,
            *gBfFont,
            *gEmFont,
            *gInputFont,
            *gItFont,
            *gRmFont,
            *gSlFont,
            *gTitleFont,
            *gTtFont;

XrmDatabase rDB;
int gBorderColor;     /* The Border Color */

/* Initialize the X Window System  */

void
initializeWindowSystem(void)
{
    char *display_name = NULL;
    XColor fg, bg;
#if 0
    XColor rgbdef;
#endif
    Colormap cmap;
    Pixmap  mousebits, mousemask;
/*    fprintf(stderr,"initx:initializeWindowSystem:entered\n");*/
    /* Try to open the display */
/*    fprintf(stderr,"initx:initializeWindowSystem:XOpenDisplay\n");*/
    if ((gXDisplay = XOpenDisplay(display_name)) == NULL) {
        fprintf(stderr, "(HyperDoc) Cannot connect to the X11 server!\n");
        exit(-1);
    }

    /* Get the screen */
/*    fprintf(stderr,"initx:initializeWindowSystem:DefaultScreen\n");*/
    gXScreenNumber = scrn = DefaultScreen(gXDisplay);
/*    fprintf(stderr,"initx:initializeWindowSystem:XGContextFromGC\n");*/
    server_font =XGContextFromGC(DefaultGC(gXDisplay, gXScreenNumber));

    /* Get the cursors we need. */

/*    fprintf(stderr,"initx:initializeWindowSystem:DefaultColormap\n");*/
    cmap = DefaultColormap(gXDisplay, gXScreenNumber);
/*    fprintf(stderr,"initx:initializeWindowSystem:WhitePixel\n");*/
    fg.pixel = WhitePixel(gXDisplay,gXScreenNumber);
/*    fprintf(stderr,"initx:initializeWindowSystem:XQueryColor\n");*/
    XQueryColor(gXDisplay, cmap, &fg );
/*    fprintf(stderr,"initx:initializeWindowSystem:BlackPixel\n");*/
    bg.pixel = BlackPixel(gXDisplay,gXScreenNumber);
/*    fprintf(stderr,"initx:initializeWindowSystem:XQueryColor2\n");*/
    XQueryColor(gXDisplay, cmap, &bg );
#if 0
    XAllocNamedColor(gXDisplay, cmap, "Black", &fg, &rgbdef);
    XAllocNamedColor(gXDisplay, cmap, "White", &bg, &rgbdef);
#endif

#ifdef USE_BORING_OLD_CURSORS
    gActiveCursor = XCreateFontCursor(gXDisplay, XC_circle);
    gNormalCursor = XCreateFontCursor(gXDisplay, XC_dot);
#else
/*  fprintf(stderr,"initx:initializeWindowSystem:XCreateBitmapFromData 1\n");*/
    mousebits = XCreateBitmapFromData(gXDisplay,
        RootWindow(gXDisplay, gXScreenNumber),
        ucharp_to_charp(mouseBitmap_bits),
        mouseBitmap_width,mouseBitmap_height);
/* fprintf(stderr,"initx:initializeWindowSystem:XCreateBitmapFromData 2\n");*/
    mousemask = XCreateBitmapFromData(gXDisplay,
        RootWindow(gXDisplay, gXScreenNumber),
        ucharp_to_charp(mouseMask_bits), mouseMask_width,mouseMask_height);
/* fprintf(stderr,"initx:initializeWindowSystem:XCreateBitmapFromData 2\n");*/
    gActiveCursor = XCreatePixmapCursor(gXDisplay,
        mousebits, mousemask, &fg, &bg,
        mouseBitmap_x_hot,mouseBitmap_y_hot);

/*    fprintf(stderr,"initx:initializeWindowSystem:XCreateFontCursor\n");*/
    gNormalCursor = XCreateFontCursor(gXDisplay, XC_left_ptr);
#endif

/*    fprintf(stderr,"initx:initializeWindowSystem:XCreateFontCursor 2\n");*/
    gBusyCursor = XCreateFontCursor(gXDisplay, XC_watch);

    /* Now initialize all the colors and fonts */

/*    fprintf(stderr,"initx:initializeWindowSystem:ingItColors_and_fonts\n");*/
    ingItColors_and_fonts();
/*    fprintf(stderr,"initx:initializeWindowSystem:init_text\n");*/
    init_text();
/*    fprintf(stderr,"initx:initializeWindowSystem:exited\n");*/

}

/*
 * This routine is responsible for initializing a HyperDoc Window. At this
 * point, all the fonts have been loaded, and X has been initialized. All I
 * need worry about is starting up the window, and creating some of its
 * children.
 */


/*
 * init_top_window tries to start up a window with the page name. If the
 * page name is NULL,
 * it doesn't try to find it in the Hash Table, but rather just allocates a
 * page of no name
 */

int
init_top_window(char *name)
{
    HyperDocPage *page;
    XSetWindowAttributes wa;    /* The X attributes structure */
    HDWindow *old_win = gWindow;

    gWindow = alloc_hd_window();

    if (name == NULL) {
        /** Then allocate an empty page, and assign it to gWindow->page */
        page = alloc_page((char *) NULL);
    }
    else {
        /* Try to find the page in the page hash table */
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, name);
        if (page == NULL) {
            fprintf(stderr, "(HyperDoc)  Couldn\'t find page %s in page hash table \n",
                    name);
            if (gParentWindow == NULL)
                /* Gaak, This is a start up error */
                exit(-1);
            else {
                gWindow = old_win;
                return -1;
            }
        }
    }

    /* First allocate memory for the new window structure   */
    gWindow->page = page;

    if (old_win == NULL)
        open_window(0);
    else
        open_window(old_win->fMainWindow);

    get_GCs(gWindow);
    XMapWindow(gXDisplay, gWindow->fMainWindow);
    hash_insert(&gSessionHashTable, (char *)gWindow,(char *) &gWindow->fMainWindow);

    change_text(gRmColor, gRmFont);
    wa.background_pixel = gBackgroundColor;
    XChangeWindowAttributes(gXDisplay, gWindow->fMainWindow, CWBackPixel, &wa);
    XChangeWindowAttributes(gXDisplay, gWindow->fScrollWindow, CWBackPixel,&wa);
    return 1;
}

/* Create and initialize a form HyperDoc window */

static void
open_form_window(void)
{
    int x, y, width, height;
    unsigned int fwidth = 0, fheight = 0;
    unsigned int xadder = 0, yadder = 0;
    /*char *window_name = "HyperDoc";*/
    /*char *icon_name = "HT";*/
    XrmValue value;
    char *str_type[50];
    XSizeHints size_hints;
    int userSpecified = 0;

    char userdefaults[50], progdefaults[50];

    strcpy(progdefaults, "=950x450+0+0");
    if (XrmGetResource(rDB, "FriCAS.hyperdoc.FormGeometry",
        "FriCAS.hyperdoc.FormGeometry", str_type, &value) == True)
    {
        strncpy(userdefaults, value.addr, (int) value.size);
        userSpecified = 1;
    }
    else
        strcpy(userdefaults, progdefaults);

    XGeometry(gXDisplay, gXScreenNumber, userdefaults, progdefaults,
              0, fwidth, fheight, xadder, yadder,
              &x, &y, &width, &height);

    gWindow->border_width = get_border_properties();

    gWindow->width = 1;
    gWindow->height = 1;

    gWindow->fMainWindow = XCreateSimpleWindow(gXDisplay, RootWindow(gXDisplay, gXScreenNumber),
                                    x, y, width, height, gWindow->border_width,
                                    gBorderColor,
                                    WhitePixel(gXDisplay, gXScreenNumber));
    gWindow->fScrollWindow = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                         1, 1, 1, 1, 0,
                                         BlackPixel(gXDisplay, gXScreenNumber),
                                         WhitePixel(gXDisplay, gXScreenNumber));
    makeScrollBarWindows();
    makeTitleBarWindows();

    set_name_and_icon();

    XSelectInput(gXDisplay, gWindow->fScrollWindow, PointerMotionMask);
    XSelectInput(gXDisplay, gWindow->fMainWindow, StructureNotifyMask | PointerMotionMask);
    XDefineCursor(gXDisplay, gWindow->fMainWindow, gNormalCursor);

    /* now give the window manager some hints */

    size_hints.flags = 0;

    size_hints.min_width  = width;
    size_hints.min_height = height;
    size_hints.flags |= PMinSize;

    size_hints.width  = width;
    size_hints.height = height;
    size_hints.flags |= (userSpecified ? USSize : PSize);

    size_hints.x = x;
    size_hints.y = y;
    size_hints.flags |= (userSpecified ? USPosition : PPosition);

    XSetNormalHints(gXDisplay, gWindow->fMainWindow, &size_hints);
    XFlush(gXDisplay);
}


int
init_form_window(char *name, int cols)
{
    XSetWindowAttributes wa;    /* The X attributes structure */

    /* First allocate memory for the new window structure   */

    gWindow = alloc_hd_window();
    open_form_window();
    gWindow->width = window_width(cols);

    if (name == NULL) {
        /** Then allocate an empty page, and assign it to gWindow->page */
        gWindow->page = alloc_page((char *) NULL);
    }
    else {
        /* Try to find the page in the page hash table */
        gWindow->page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, name);
        if (gWindow->page == NULL) {
            fprintf(stderr, "Couldn't find page %s\n", name);
            return (-1);
        }
    }

    get_GCs(gWindow);
    hash_insert(&gSessionHashTable, (char *)gWindow,(char *) &gWindow->fMainWindow);

    wa.background_pixel = gBackgroundColor;
    XChangeWindowAttributes(gXDisplay, gWindow->fMainWindow, CWBackPixel, &wa);
    XChangeWindowAttributes(gXDisplay, gWindow->fScrollWindow, CWBackPixel,&wa);
    return 1;
}


static void
set_name_and_icon(void)
{
    char *icon_name = "HyperDoc";
    char *s;
    Pixmap icon_pixmap;
    XWMHints wmhints;
    XClassHint ch;

    ch.res_name = "HyperDoc";
    ch.res_class = gArgv[0];
    for (s = gArgv[0] + strlen(gArgv[0]) - 1; s != gArgv[0]; s--) {
        if (*s == '/') {
            ch.res_class = s + 1;
            break;
        }
    }
    XSetClassHint(gXDisplay, gWindow->fMainWindow, &ch);

    XStoreName(gXDisplay, gWindow->fMainWindow, "HyperDoc");

    /* define and assign the pixmap for the icon */
    icon_pixmap = XCreateBitmapFromData(gXDisplay, gWindow->fMainWindow,
                                        ucharp_to_charp(ht_icon_bits),
                                        ht_icon_width, ht_icon_height);
    wmhints.icon_pixmap = icon_pixmap;
    wmhints.flags = IconPixmapHint;

    XSetWMHints(gXDisplay, gWindow->fMainWindow, &wmhints);

    /* name the icon */
    XSetIconName(gXDisplay, gWindow->fMainWindow, icon_name);
}

static int
get_border_properties(void)
{
    char *bwidth;
    /*char *bc = NULL;*/
    int bw;
    /*XColor color_def, color_db;*/
    Colormap cmap;
    /*int ret_val;*/


    bwidth = "2";  /* XGetDefault(gXDisplay, "FriCAS.hyperdoc", "BorderWidth") */

    if (bwidth == NULL)
        bw = 1;
    else {
        bw = atoi(bwidth);
        if (bw < 1) {
            fprintf(stderr,
                    "%s: The line width value must be greater than zero\n",
                    "FriCAS.hyperdoc");
            bw = 1;
        }
    }

    /* Now try to find the user preferred border color */

    if (DisplayPlanes(gXDisplay, gXScreenNumber) == 1)
        gBorderColor = BlackPixel(gXDisplay, gXScreenNumber);
    else {
        cmap = DefaultColormap(gXDisplay, gXScreenNumber);
        gBorderColor = get_color("BorderColor", "Foreground",
            BlackPixel(gXDisplay, gXScreenNumber), &cmap);
    }
    return bw;
}


/* Create and initialize the HyperDoc window */

static void
open_window(Window w)
{
    int x = 0, y = 0;
    /*int border_width = 2;*/
    unsigned int width = 1;
    unsigned int height = 1;
    unsigned int fwidth = 0, fheight = 0;
    unsigned int xadder = 0, yadder = 0;
    char *str_type[50];
    XrmValue value;

    char userdefaults[50], progdefaults[50];

    strcpy(progdefaults, "=700x450+0+0");
    if (XrmGetResource(rDB, "FriCAS.hyperdoc.Geometry",
        "FriCAS.hyperdoc.Geometry", str_type, &value) == True)
    {
        strncpy(userdefaults, value.addr, (int) value.size);
    }
    else
        strcpy(userdefaults, progdefaults);

    XGeometry(gXDisplay, gXScreenNumber, userdefaults, progdefaults,
              0, fwidth, fheight, xadder, yadder,
              &x, &y, ( int *)&width,( int *) &height);

    gWindow->border_width = get_border_properties();

    gWindow->fMainWindow = XCreateSimpleWindow(gXDisplay, RootWindow(gXDisplay, gXScreenNumber),
                                    x, y, width, height, gWindow->border_width,
                                    gBorderColor,
                                    WhitePixel(gXDisplay, gXScreenNumber));

    gWindow->fScrollWindow = XCreateSimpleWindow(gXDisplay, gWindow->fMainWindow,
                                         1, 1, 1, 1, 0,
                                         gBorderColor,
                                         WhitePixel(gXDisplay, gXScreenNumber));


    makeScrollBarWindows();
    makeTitleBarWindows();

    /* Now set all the little properties for the top level window */

    set_name_and_icon();
    set_size_hints(w);
    XSelectInput(gXDisplay, gWindow->fScrollWindow, PointerMotionMask);
    XSelectInput(gXDisplay, gWindow->fMainWindow, StructureNotifyMask | PointerMotionMask);
    XDefineCursor(gXDisplay, gWindow->fMainWindow, gNormalCursor);
}

/***
  This routine gets and sets the size for a new window. If the w parameter
  is null, it means that this is the initial window. Thus the user
  preferences are checked. If this is not the first window, then the
  window w is used as a guidline, and the new window is placed on top of
  it.
  ***/

static void
set_size_hints(Window w)
{
    int x, y;
    unsigned int width, height;
    char userdefaults[50];
    char progdefaults[50];
    char *str_type[50];
    unsigned int fwidth = 0, fheight = 0;
    unsigned int xadder = 0, yadder = 0;
    int geo = 0;                /* return flag from XGetGeometry */
    unsigned int depth, bw=0;
    Window root;
    XSizeHints size_hints;
    XPoint xp;
    XrmValue value;

    size_hints.flags = 0;

    strcpy(progdefaults, "=600x450+0+0");

    if (w) {
        /*
         * The window should be queried for it's size and position. Then the
         * new window should be given almost the same locations
         */

        if (XGetGeometry(gXDisplay, w, &root, &x, &y, &width, &height, &bw, &depth))
        {
            xp = getWindowPositionXY(gXDisplay, w);
            x = xp.x + 40;
            y = xp.y + 40;
            if (x < 0)
                x = 0;
            if (y < 0)
                y = 0;
            size_hints.flags |= (USSize | USPosition);
        }
        else {
            fprintf(stderr, "(HyperDoc) Error Querying window configuration: %ld.\n", w);
            x = y = 0;
            width = 600;
            height = 450;
            size_hints.flags |= (PSize | PPosition);
        }
    }
    else {
        /* this is the first window, so lets try to find a nice spot for it */

        if (XrmGetResource(rDB, "FriCAS.hyperdoc.Geometry",
                           "FriCAS.hyperdoc.Geometry", str_type,
                           &value) == True) {
            strncpy(userdefaults, value.addr, (int) value.size);
            geo = XParseGeometry(userdefaults, &x, &y, &width, &height);
        } else {
            strcpy(userdefaults, progdefaults);
        }

        size_hints.flags |= (geo & (WidthValue | HeightValue)) ? USSize : PSize;
        size_hints.flags |= (geo & (XValue | YValue)) ? USPosition : PPosition;

        geo = XGeometry(gXDisplay, gXScreenNumber, userdefaults, progdefaults,
                        bw, fwidth, fheight, xadder, yadder,
                        &x, &y, (int *)&width, (int *)&height);
    }

    size_hints.x = x;
    size_hints.y = y;
    size_hints.width = width;
    size_hints.height = height;

    getTitleBarMinimumSize(&(size_hints.min_width), &(size_hints.min_height));
#if 0
    size_hints.min_width  = MIN_WINDOW_SIZE;
    size_hints.min_height = MIN_WINDOW_SIZE;
#endif
    size_hints.flags |= PMinSize;

    XSetNormalHints(gXDisplay, gWindow->fMainWindow, &size_hints);
    /* just in case a hint isn't enough ... */
    XFlush(gXDisplay);
/*  XMoveResizeWindow(gXDisplay, gWindow->fMainWindow, x, y, width, height); */
}

#define stipple_width 4
#define stipple_height 4
static char stipple_bits[] = {
                              0xff, 0xff, 0xff, 0xff};
Pixmap stipple;

/* Create the graphics contexts to be used for all drawing operations */

static void
get_GCs(HDWindow *window)
{
    /*unsigned long valuemask = 0;*/
    XGCValues values;

    values.background = gBackgroundColor;
    window->fStandardGC = XCreateGC(gXDisplay, window->fMainWindow, GCBackground, &values);

    XSetLineAttributes(gXDisplay, window->fStandardGC, window->border_width,
                       LineSolid, CapButt, JoinMiter);


    /* create the stipple for the gc */

    stipple = XCreateBitmapFromData(gXDisplay,
        RootWindow(gXDisplay, gXScreenNumber),
        stipple_bits, stipple_width, stipple_height);

    values.background = gInputBackgroundColor;
    values.foreground = gInputForegroundColor;

    values.font = gInputFont->fid;

    if (values.font == server_font )
        window->fInputGC = XCreateGC(gXDisplay, window->fMainWindow,
            GCBackground | GCForeground, &values);
    else {
        window->fInputGC  = XCreateGC(gXDisplay, window->fMainWindow,
            GCBackground | GCForeground | GCFont, &values);
    }

    window->fCursorGC = XCreateGC(gXDisplay, window->fMainWindow, 0, NULL);

    if (values.font != server_font)
        XSetFont(gXDisplay,   window->fCursorGC, gInputFont->fid);

    XSetBackground(gXDisplay, window->fCursorGC, gInputForegroundColor);
    XSetForeground(gXDisplay, window->fCursorGC, gInputBackgroundColor);

    window->fControlGC = XCreateGC(gXDisplay, window->fMainWindow, 0, NULL);
    XSetBackground(gXDisplay, window->fControlGC, gControlBackgroundColor);
    XSetForeground(gXDisplay, window->fControlGC, gControlForegroundColor);
}

/* Load a font and store the information in the font_info parameter */

static void
load_font(XFontStruct **font_info, char *fontname)
{
   if ((*font_info = XLoadQueryFont(gXDisplay, fontname)) == NULL) {
        fprintf(stderr, "(HyperDoc) Cannot load font %s ; using default.\n",
            fontname);

        if ((*font_info = XQueryFont(gXDisplay,
               XGContextFromGC(DefaultGC(gXDisplay, gXScreenNumber)))) == NULL)
        {
            fprintf(stderr, "(HyperDoc) Cannot get default font ; exiting.\n");
            exit(-1);
        }
   }
}


/*
 * This routine initializes all the colors and fonts that the user wishes to
 * use. It checks for all the following properties in $HOME/.Xdefaults.
 *
 *  FriCAS.hyperdoc.ActiveColor:
 *  FriCAS.hyperdoc.Background:
 *  FriCAS.hyperdoc.EmphasizeColor:
 *  FriCAS.hyperdoc.EmphasizeFont:
 *  FriCAS.hyperdoc.Foreground:
 *  FriCAS.hyperdoc.InputBackground:
 *  FriCAS.hyperdoc.InputForeground:
 *  FriCAS.hyperdoc.SpadColor:
 *  FriCAS.hyperdoc.SpadFont:
 */

static void
ingItColors_and_fonts(void)
{
    char property[256];
    char *prop = &property[0];
    char *str_type[50];
    XrmValue value;
    Colormap cmap;
    int ts;

    /** get the color map for the display **/
/*    fprintf(stderr,"initx:ingItColors_and_fonts:entered\n");*/

/*    fprintf(stderr,"initx:ingItColors_and_fonts:DefaultColorMap\n");*/
    cmap = DefaultColormap(gXDisplay, gXScreenNumber);

/*    fprintf(stderr,"initx:ingItColors_and_fonts:init_group_stack\n");*/
    init_group_stack();


    /** then start getting the fonts **/

/*    fprintf(stderr,"initx:ingItColors_and_fonts:mergeDatabases\n");*/
    mergeDatabases();

/*    fprintf(stderr,"initx:ingItColors_and_fonts:XrmGetResource\n");*/
    if (XrmGetResource(rDB, "FriCAS.hyperdoc.RmFont",
                       "FriCAS.hyperdoc.Font", str_type, &value) == True) {
        (void) strncpy(prop, value.addr, (int) value.size);
    } else {
        (void) strcpy(prop, RmFontDefault);
    }

/*    fprintf(stderr,"initx:ingItColors_and_fonts:load_font 1\n");*/
    load_font(&gRmFont, prop);
/*    fprintf(stderr,"initx:ingItColors_and_fonts:load_font 2\n");*/
    load_font(&gInputFont, prop);


/*    fprintf(stderr,"initx:ingItColors_and_fonts:XrmGetResource 2\n");*/
    if (XrmGetResource(rDB, "FriCAS.hyperdoc.TtFont",
                       "FriCAS.hyperdoc.Font", str_type, &value) == True) {
        (void) strncpy(prop, value.addr, (int) value.size);
    } else {
        (void) strcpy(prop, TtFontDefault);
    }
/*    fprintf(stderr,"initx:ingItColors_and_fonts:load_font 3\n");*/
    load_font(&gTtFont, prop);
/*    fprintf(stderr,"initx:ingItColors_and_fonts:is_it_850\n");*/
    gTtFontIs850=is_it_850(gTtFont);

/*    fprintf(stderr,"initx:ingItColors_and_fonts:XrmGetResource 5\n");*/
    if (XrmGetResource(rDB, "FriCAS.hyperdoc.ActiveFont",
                       "FriCAS.hyperdoc.Font", str_type, &value) == True) {
        (void) strncpy(prop, value.addr, (int) value.size);
    } else {
        (void) strcpy(prop, ActiveFontDefault);
    }
/*    fprintf(stderr,"initx:ingItColors_and_fonts:load_font 4\n");*/
    load_font(&gActiveFont, prop);

    /* maintain backwards compatibility */

/*    fprintf(stderr,"initx:ingItColors_and_fonts:XrmGetResource 6\n");*/
    if (XrmGetResource(rDB, "FriCAS.hyperdoc.FriCASFont",
                       "FriCAS.hyperdoc.Font", str_type, &value) == True) {
        (void) strncpy(prop, value.addr, (int) value.size);
    } else {
        if (XrmGetResource(rDB, "FriCAS.hyperdoc.SpadFont",
                        "FriCAS.hyperdoc.Font", str_type, &value) == True) {
            (void) strncpy(prop, value.addr, (int) value.size);
        } else {
            (void) strcpy(prop, fricas_font_default);
        }
    }

/*    fprintf(stderr,"initx:ingItColors_and_fonts:load_font 5\n");*/
    load_font(&fricas_font, prop);

/*    fprintf(stderr,"initx:ingItColors_and_fonts:XrmGetResource 7\n");*/
    if (XrmGetResource(rDB, "FriCAS.hyperdoc.EmphasizeFont",
                       "FriCAS.hyperdoc.Font", str_type, &value) == True) {
        (void) strncpy(prop, value.addr, (int) value.size);
    } else {
        (void) strcpy(prop, EmphasizeFontDefault);
    }
/*    fprintf(stderr,"initx:ingItColors_and_fonts:load_font 6\n");*/
    load_font(&gEmFont, prop);

/*    fprintf(stderr,"initx:ingItColors_and_fonts:XrmGetResource 8\n");*/
    if (XrmGetResource(rDB, "FriCAS.hyperdoc.BoldFont",
                       "FriCAS.hyperdoc.Font", str_type, &value) == True) {
        (void) strncpy(prop, value.addr, (int) value.size);
    } else {
        (void) strcpy(prop, BoldFontDefault);
    }
/*    fprintf(stderr,"initx:ingItColors_and_fonts:load_font 7\n");*/
    load_font(&gBfFont, prop);


    /*
     * If we are on a monochrome screen, then we ignore user preferences, and
     * set the foreground and background as I wish
     */

/*    fprintf(stderr,"initx:ingItColors_and_fonts:DisplayPlanes\n");*/
    if (DisplayPlanes(gXDisplay, gXScreenNumber) == 1) {
        gActiveColor       = fricas_color
                            = gControlBackgroundColor
                            = gInputBackgroundColor
                            = gBfColor
                            = gEmColor
                            = gRmColor
                            = gSlColor
                            = gTtColor
                            = BlackPixel(gXDisplay, gXScreenNumber);

        gBackgroundColor   = gInputForegroundColor
                            = gControlForegroundColor
                            = WhitePixel(gXDisplay, gXScreenNumber);
    }
    else {

        /*
         * If I have gotten here, then we must be on a color screen, so see
         * what the user likes, and set it up
         */

/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 1\n");*/
        gRmColor =
            get_color("RmColor", "Foreground",
                      BlackPixel(gXDisplay, gXScreenNumber), &cmap);
/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 2\n");*/
        gBackgroundColor =
            get_color("Background", "Background",
                      WhitePixel(gXDisplay, gXScreenNumber), &cmap);
/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 3\n");*/
        gActiveColor =
            get_color("ActiveColor", "Foreground",
                       BlackPixel(gXDisplay, gXScreenNumber), &cmap);

        /*
         * for next two, I want name arg = class arg, ie do not want
         * Background and Foreground.
         */

/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 4\n");*/
        gControlBackgroundColor = get_color("ControlBackground",
            "ControlBackground", WhitePixel(gXDisplay, gXScreenNumber), &cmap);
/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 5\n");*/
        gControlForegroundColor = get_color("ControlForeground",
            "ControlForeground", BlackPixel(gXDisplay, gXScreenNumber), &cmap);

        /* maintain backwards compatibility */

/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 6\n");*/
        fricas_color = get_color("FriCASColor", "Foreground", 0, &cmap);
/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 7\n");*/
        if (fricas_color == 0)
            fricas_color = get_color("SpadColor", "Foreground",
                BlackPixel(gXDisplay, gXScreenNumber), &cmap);

/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 8\n");*/
        gInputBackgroundColor =
            get_color("InputBackground", "Foreground", gRmColor, &cmap);
/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 9\n");*/
        gInputForegroundColor =
           get_color("InputForeground", "Background", gBackgroundColor, &cmap);

/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 10\n");*/
        gEmColor =
            get_color("EmphasizeColor", "Foreground", gRmColor, &cmap);
/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 11\n");*/
        gTtColor =
            get_color("TtColor", "Foreground", gRmColor, &cmap);
/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 12\n");*/
        gSlColor =
            get_color("EmphasizeColor", "Foreground", gRmColor, &cmap);
/*        fprintf(stderr,"initx:ingItColors_and_fonts:get_color 13\n");*/
        gBfColor =
            get_color("BoldColor", "Foreground", gRmColor, &cmap);
    }

/*    fprintf(stderr,"initx:ingItColors_and_fonts:makeColors\n");*/
    makeColors(gXDisplay, gXScreenNumber, &cmap, &spadColors, &ts);
    /*
     * Now set the current color and font, so I never have to do it again
     */

    gTopOfGroupStack->cur_color = gRmColor;
    gTopOfGroupStack->cur_font = gRmFont;
/*    fprintf(stderr,"initx:ingItColors_and_fonts:exited\n");*/
}

void
change_text(int color, XFontStruct *font)
{
    if (font) {
        XGCValues gcv;
        gcv.foreground = color;
        gcv.background = gBackgroundColor;

        XChangeGC(gXDisplay, gWindow->fStandardGC, GCForeground | GCBackground , &gcv);

        if (font->fid != server_font)
            XSetFont(gXDisplay, gWindow->fStandardGC, font->fid);
    }
}

/*
 * This routine checks the .Xdefaults file of the user for the
 * specified color. If found it allocates a place in the color map for it. If
 * not found, or if an error occurrs, it writes an error message, and
 * uses the given default value
 */

static int
get_color(char *name, char *class, int def, Colormap *map)
{
    char fullname[256];
    char fullclass[256];
    char property[256];
    char *prop = &property[0];
    char *str_type[50];
    XrmValue value;
    int ret_val;
    XColor color_def, color_db;

#ifdef DEBUG
    printf("get_color: %s %s %d -> ", name, class, def);
#endif

    strcpy(fullname, "FriCAS.hyperdoc.");
    strcat(fullname, name);
    strcpy(fullclass, "FriCAS.hyperdoc.");
    strcat(fullclass, class);

    if (XrmGetResource(rDB, fullname, fullclass, str_type, &value) == True) {
        (void) strncpy(prop, value.addr, (int) value.size);
        ret_val = XAllocNamedColor(gXDisplay, *map, prop, &color_def, &color_db);
        if (ret_val) {
#ifdef DEBUG
            printf("%d\n", color_def.pixel);
#endif
            return (color_def.pixel);
        }
        else {
            fprintf(stderr,
                "(HyperDoc) Defaulting on color for %s. Unknown color is %s.\n",
                    name, prop);
#ifdef DEBUG
            printf("%d\n", def);
#endif
            return (def);
        }
    }
    else {
#ifdef DEBUG
        printf("%d\n", def);
#endif
        return (def);
    }
}


static void
mergeDatabases(void)
{
    XrmDatabase homeDB, serverDB, applicationDB;
    char filenamebuf[1024];
    char *filename = &filenamebuf[0];
    char *classname = "FriCAS";
    char name[255];

/*    fprintf(stderr,"initx:mergeDatabases:entered\n");*/
/*    fprintf(stderr,"initx:mergeDatabases:XrmInitialize\n");*/
    (void) XrmInitialize();
    (void) strcpy(name, "/usr/lib/X11/app-defaults/");
    (void) strcat(name, classname);
/*  fprintf(stderr,"initx:mergeDatabases:XrmGetFileDatabase name=%s\n",name);*/
    applicationDB = XrmGetFileDatabase(name);
/*    fprintf(stderr,"initx:mergeDatabases:XrmMergeDatabases\n");*/
    (void) XrmMergeDatabases(applicationDB, &rDB);

/*    fprintf(stderr,"initx:mergeDatabases:XrmGetStringDatabase\n");*/
    if (XResourceManagerString(gXDisplay) != NULL) {
        serverDB = XrmGetStringDatabase(XResourceManagerString(gXDisplay));
    }
    else {
        (void) strcpy(filename, getenv("HOME"));
        (void) strcat(filename, "/.Xdefaults");
/*        fprintf(stderr,"initx:mergeDatabases:XrmGetFileDatase\n");*/
        serverDB = XrmGetFileDatabase(filename);
    }
/*    fprintf(stderr,"initx:mergeDatabases:XrmMergeDatabases 2\n");*/
    XrmMergeDatabases(serverDB, &rDB);
    if (getenv("XENVIRONMENT") == NULL) {
        int len;

        (void) strcpy(filename, getenv("HOME"));
        (void) strcat(filename, "/.Xdefaults-");
        len = strlen(filename);
        (void) gethostname(filename + len, 1024 - len);
    }
    else {
        (void) strcpy(filename, getenv("XENVIRONMENT"));
    }
/*    fprintf(stderr,"initx:mergeDatabases:filename=%s\n",filename);*/
    homeDB = XrmGetFileDatabase(filename);
/*    fprintf(stderr,"initx:mergeDatabases:XrmMergeDatabases 3\n");*/
    XrmMergeDatabases(homeDB, &rDB);
}



int
is_it_850(XFontStruct *fontarg)
{
 char *s;
 int i,val;
 static struct {
      char *name;
      Atom format;
      Atom atom;
      } proptbl = { "CHARSET_ENCODING", XA_ATOM };
 proptbl.atom = XInternAtom(gXDisplay,proptbl.name,0);
 for (i=0;i<fontarg->n_properties;i++)
  {
    if (fontarg->properties[i].name != proptbl.atom) continue;


/* return 1 if it is 850 */

    s = XGetAtomName(gXDisplay,(Atom)fontarg->properties[i].card32);
    val = !( strcmp("850",s) * strcmp("ibm-850",s));
    XFree(s);
    return( val );
  }
 return(0);
}
