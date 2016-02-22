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

#ifndef _HYPER_H_
#define _HYPER_H_ 1

#include "fricas_c_macros.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include "com.h"
#include "token.h"
#include "hash.h"

#define boolean unsigned short int

#ifndef TRUE
#define TRUE   ((boolean) 0x1)
#endif
#ifndef FALSE
#define FALSE  ((boolean) 0x0)
#endif

/* Struct forward declarations */

struct text_node;
struct input_box;
struct input_window;
struct paste_node;
struct radio_boxes;
struct group_item;

#define Scrollupbutton     1
#define Scrolldownbutton   2
#define Noopbutton         6

#define Scrolling 1
#define Header    2
#define Footer    3
#define Title     4

#ifndef HTADD
extern int MenuServerOpened;

/* These are all the colors one can use in HyperDoc. */

extern int  gActiveColor,
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

/* These are all the different fonts one can use in HyperDoc. */

extern XFontStruct  *gActiveFont,
                    * fricas_font,
                    *gBfFont,
                    *gEmFont,
                    *gInputFont,
                    *gItFont,
                    *gRmFont,
                    *gSlFont,
                    *gTitleFont,
                    *gTtFont;


#endif


/** I am implementing a state node stack, this is the structure I store **/

typedef struct state_node {
 int last_ch, last_token, input_type;
 long fpos, keyword_fpos;
 long page_start_fpos;
 Token token;
 char *input_string;
 FILE *cfile;
 int keyword;
 struct state_node *next;
} StateNode;

#ifndef HTADD
/** pointer to the top of the state node graph **/
extern StateNode *top_state_node;
#endif


/* structure for a hyper text link */
typedef struct hyper_link {
  int type;                     /* Memolink, Spadlink, Downlink etc. */
  Window win;                   /* X11 window containing active area */
  union {
    struct text_node *node;     /* ID of link to which link refers */
    struct input_box *box;
    struct input_window *string;
    struct paste_node *paste;   /* the paste node area */
  } reference;
  int x,y;                      /* relative position inside page */
} HyperLink;


typedef struct if_node {
    struct text_node *cond;                /* the condition nodes*/
    struct text_node *thennode;
    struct text_node *elsenode;
} IfNode;

typedef struct item_stack {
    int indent;
    int item_indent;
    int in_item;
    struct item_stack *next;
} ItemStack;

typedef struct paste_node {
   char *name;
   int where;                /* where should I be parsing from? */
   short int hasbutton;
   short int haspaste;
   struct group_item *group;
   ItemStack *item_stack;
   struct text_node *arg_node;
   struct text_node *end_node;
   struct text_node *begin_node;
   struct input_window *paste_item;
} PasteNode;

/* Structure for formatted hypertext */

typedef struct text_node {
  short  type;                  /* type of node (text, link, etc.) */
  int x,y, width, height;       /* relative location on screen */
  int space;                    /* was there space in front of me ? */
  union {
    char *text;                 /* piece of text to display */
    struct text_node *node;     /* argument text */
    struct if_node *ifnode;
  } data;
  HyperLink *link;              /* link for active text */
  union {
    Pixmap pm;                  /* pixmap for bit images */
    XImage *xi;                 /* pixmap image */
  } image;
  struct text_node *next;       /* next node in formatted text */
} TextNode;

/** Structure used to store pixmaps and bitmaps **/

typedef struct image_struct {
   int width,height;   /** It's width and height **/
   union {
     Pixmap pm;
     XImage *xi;
   } image;
   char *filename;       /** The filename used to reference it **/
} ImageStruct;

/* Structure for locating HyperDoc pages in a source file */

typedef struct {
  char *name;           /* file name */
  long  pos;            /* position in file */
  int   ln;             /* the line number */
} FilePosition;

/*** The structure needed for storing a macro **/

typedef struct macro_store {
  short int loaded;
  FilePosition fpos;
  char *name;
  char *macro_string;
  short number_parameters;
} MacroStore;


/** Structure needed for storing a patch **/
typedef struct patch_store {
  short int loaded;
  FilePosition fpos;
  char *name;
  char *string;
} PatchStore;

/*  Here are the structures needed for doing input to HyperDoc windows. */

typedef struct line_struct {
   char *buffer;
   int changed;      /* Has the line changed */
   int line_number;
   int buff_pntr;
   int len;
   struct line_struct *prev, *next;
} LineStruct;

typedef struct input_window {
  char *name;                   /* symbol name                **/
  int size;                      /* the length of the window   **/
  int cursor_x;                  /* x-coordinate for the cursor **/
  int entered;                   /* tells me whether I have typed here
                                                    before       */
  int num_lines;                 /* number of lines needed to store
                                      buffer                     */
  LineStruct *lines;
  LineStruct *curr_line;         /* the current line on which the cursor */
  Window win;
  struct input_window  *next;
}  InputItem;


/* structure for storing input boxes **/
typedef struct input_box {
    char *name;
    ImageStruct *selected, *unselected;
    short int picked;
    struct input_box  *next;
    struct radio_boxes *rbs;
    Window win;
} InputBox;

typedef struct radio_boxes {
     char *name;
     InputBox *boxes;
     ImageStruct *selected, *unselected;
     int width, height;
     struct radio_boxes *next;
} RadioBoxes;

/* Structure for spadcommand dependencies hash table entries */
typedef struct spadcom_depend {
  char *label;                  /* dependency label */
  TextNode *spadcom;            /* spadcommand defining the label */
  short executed;               /* true iff spadcommand has benn executed */
} SpadcomDepend;

typedef struct button_list {
  int           x0,y0,x1,y1;
  HyperLink             *link;
  Window                win;
  struct button_list    *next;
} ButtonList;

/* Stucture for unformatted hyper text page */

typedef struct hyperdoc_page {
  short type;                   /* Normal, Quitbutton, Upbutton etc.       */
  char *name;                   /* ID of page                              */
  char *filename;               /* The name of the file in which the page
                                   occurs, Null if not                     */
  int scroll_off;             /* The offset in the scrolling region        */
  int bot_scroll_margin;        /* bottom of the scrolling region          */
  int top_scroll_margin;        /* top of the scrolling region             */
  TextNode *title;              /* the title of the page                   */
  TextNode *header;             /* formatted version of page               */
  TextNode *scrolling;          /* Top of scrolling region                 */
  TextNode *footer;             /* top of non-scrolling region at bottom   */
  Sock *sock;                   /* socket connection for spad buffer       */
  HashTable *fLinkHashTable;         /* active link hash table                  */
  ButtonList *s_button_list;    /* active buttons on page                  */
  ButtonList *button_list;      /* active buttons on page                  */
  HashTable *depend_hash;       /* Hash tables of spadcommand dependencies */
  InputItem *input_list;        /* List of input structures                */
  InputItem *current_item;      /* a pntr to the currently active item     */
  HashTable *box_hash;          /* place where all the boxes are stored    */
  RadioBoxes *radio_boxes;      /* a linked list of radio boxes            */
  short page_flags;             /* A list of flags for the page            */
  char *helppage;               /* the name of the helppage                */
} HyperDocPage;

/* Structure for an unloaded page */

typedef struct unloaded_page {
  short type;                   /* indicator of unloaded page */
  char *name;                   /* name of page */
  FilePosition fpos;            /* where to find the page */
} UnloadedPage;

/* Structure for a HyperDoc Window */

typedef struct {
  Window fMainWindow;           /* The main text field window.           */
  Window fScrollWindow;         /* The scrolling area of the window      */
  Window fDisplayedWindow;      /* The current window of the above two,  */
                                /*   being filled by display             */

  Window fScrollUpWindow;       /* Window for scrolling up a line        */
  Window fScrollDownWindow;     /* Window for scrolling down a line      */

  Window scrollbar;             /* the window for scrolling              */
  Window scroller;              /* the scroller window                   */

  Window fTitleBarButton1;      /* 1st titlebar bitmap button            */
  Window fTitleBarButton2;      /* 2nd titlebar bitmap button            */
  Window fTitleBarButton3;      /* 3rd titlebar bitmap button            */
  Window fTitleBarButton4;      /* 4th titlebar bitmap button            */

  int fScrollerTopPos;          /* where the top of the scroller is      */
  int fScrollerHeight;          /* the height of the scroller            */
  int fScrollBarHeight;         /* the height for the scrollbar          */

  int scrollwidth;              /* the width of the scrolling area       */
  int scrollheight;             /* the height of the scrolling area      */
  int scrollupy;                /* Current y position of the scroll up   */
                                /*        button                         */
  int scrolldowny;              /* Current y position of the scroll      */
                                /*        downbutton                     */
  int scrollbary;               /* Current y position of teh scrollbar   */
  int scrollx;                  /* X coordinates for all of the above    */
  int border_width;             /* Width of the border                   */
  HyperDocPage *page;           /* currently displayed page              */
  int width, height;            /* in pixels                             */
  int columns;                  /* Width in characters, only setable     */
                                /*      for form pages                   */
  HyperDocPage **fMemoStack;    /* stack of memo links */
  HyperDocPage **fDownLinkStack;/* stack of down links */

  int *fDownLinkStackTop;       /* stack of down links */
  int fMemoStackIndex;            /* memo stack pointer */
  int fDownLinkStackIndex;        /* downlink stack pointer */

  HashTable *fWindowHashTable;  /* hash table of active subwindows */
  HashTable *fPageHashTable;    /* hash table of HyperDoc pages */
  HashTable *fPasteHashTable;   /* hash table for paste in areas */
  HashTable *fMacroHashTable;   /* hash table of HyperDoc macros */
  HashTable *fCondHashTable;    /* hash table for values         */
  HashTable *fPatchHashTable;   /* hash table for patch locations */

  int fricas_frame;             /* FriCAS frame number initializing window */
  GC fStandardGC;               /* Graphics context for window */
  GC fInputGC;                  /* Graphics context for the input windows */
  GC fCursorGC;                 /* Graphics context for the cursors       */
  GC fControlGC;                /* Graphics context for the buttons       */
  Cursor fDisplayedCursor;      /* The currently displayed cursor          */
} HDWindow;

/* Structure for identifying appropriate link hash tables */

typedef struct {
  int code;                     /* code of active area */
  HyperDocPage *page;           /* page for which hash table applies */
} LinkHashID;

/*** Flags for the page ***/

#define NOLINES 0000001          /* Ibid, for the bottom of the page      ***/


/* external variables and functions.  See the source file for a description
 of their purposes */

extern HashTable gSessionHashTable;   /* hash table of HD windows */

extern HDWindow *gParentWindow;       /* the parent window. The one that
                                        * appears when you first start HD */

extern HyperLink *quitLink; /** a special link to the protected quit page **/


#ifndef HTADD
/* From hyper.c */
extern int      gXScreenNumber;
extern Display *gXDisplay;
extern int gSwitch_to_mono;
extern unsigned long * spadColors;
extern int gIsEndOfOutput;
extern HDWindow *gWindow;
extern Sock *session_server;
extern Sock *spad_socket;
extern HashTable gFileHashTable;
extern HashTable gImageHashTable;           /* A global hash table for images */
extern Cursor gNormalCursor;          /* The normal mouse cursor                */
extern Cursor gActiveCursor;          /* The cursor in active regions           */
extern Cursor gBusyCursor;            /* The clock cursor for when I am busy    */
/* true iff HyperDoc is acting as an FriCAS server */
extern int is_fricas_server;
extern int    gArgc;                  /* original argc from main */
extern char **gArgv;                  /* original argv from main */
/* from lex.c */
extern long fpos, keyword_fpos;
extern Token token;
extern int last_token, input_type, last_ch;
extern char *input_string;
extern FILE *cfile;
/* from input.c */
extern XImage *picked;
extern int picked_height;
extern int picked_width;
extern XImage *unpicked;
extern int unpicked_height;
extern int unpicked_width;
/* from display.c */
extern int line_height;
extern int need_scroll_up_button;
extern int scrolling;
extern int need_scroll_down_button;
extern int space_width;
#endif

/* Here are some of the functions and constants declared and needed in
      htadd.c                                                    ******/

#define NoChar   -9999
#define temp_dir "/tmp/"
#define db_file_name "ht.db"
#define def_spad "/usr/local/fricas"


/* Types of HyperDoc pages */

#define UlUnknownPage    9993 /*I hate this hack, but I have to know whether*/
#define UnknownPage      9994 /*this page has been loaded or not.           */
#define ErrorPage        9995
#define Unixfd           9996
#define SpadGen          9997
#define Normal           9998
#define UnloadedPageType 9999

/* Commands from FriCAS */

#define EndOfPage        99
#define SendLine         98
#define StartPage        97          /* A normal HyperDoc page */
#define LinkToPage       96
#define PopUpPage        95          /* A pop-up page          */
#define PopUpNamedPage   94
#define KillPage         93
#define ReplacePage      92
#define ReplaceNamedPage 91
#define SpadError        90

/* Constants declaring size of page stacks */

#define MaxMemoDepth    25              /* max nesting level for memolinks */
#define MaxDownlinkDepth 50             /* max downlink nesting depth */

/* Constants defining the size of various hash tables */

#define PageHashSize     1000
#define FileHashSize     30
#define SessionHashSize  10
#define MacroHashSize    100
#define ImageHashSize    100
#define CondHashSize     100
#define BoxHashSize      20
#define PasteHashSize    100
#define PatchHashSize    100

/* A couple of macros for memo and down links */

#define need_up_button \
  (gWindow->fMemoStackIndex ? gWindow->fDownLinkStackIndex >= \
   gWindow->fDownLinkStackTop[gWindow->fMemoStackIndex-1] \
   : gWindow->fDownLinkStackIndex)

#define need_return_button (gWindow->fMemoStackIndex)

#define need_help_button (gWindow->page->helppage != NULL)

#define max(x,y) ((x) > (y) ? (x) : (y))


#define pick_box(box) fill_box(box->win, box->selected)
#define unpick_box(box) fill_box(box->win, box->unselected)

#define TopLevelHelpPage  "ugHyperPage"
#define NoMoreHelpPage    "NoMoreHelpPage"
#define KeyDefsHelpPage   "ugHyperKeysPage"
#define InputAreaHelpPage "ugHyperInputPage"

/* definitions for connecting to the FriCAS server */

#define Connected       0
#define NotConnected    1
#define SpadBusy        2

/* some GUI-dependent stuff */

#define BeepAtTheUser()         /* (XBell(gXDisplay,  5)) */
#define LoudBeepAtTheUser()     /* (XBell(gXDisplay, 50)) */


/***      default fonts      ***/

#if defined(RTplatform) || defined(PS2platform) || defined(RIOSplatform) || defined(AIX370platform)
#define RmFontDefault         "Rom14"
#define TtFontDefault         "Erg14"
#define ActiveFontDefault     "Bld14"
#define fricas_font_default      "Erg14"
#define EmphasizeFontDefault  "Itl14"
#define BoldFontDefault       "Bld14"
#endif

#if defined(SUNplatform) || defined (SUN4OS5platform) || defined(SGIplatform) || defined (HP9platform)  || defined(HP10platform) || defined (ALPHAplatform) || defined(LINUXplatform) || defined(MACOSXplatform) || defined(BSDplatform) || defined(CYGWINplatform)
#define RmFontDefault         "-adobe-courier-medium-r-normal--18-*-*-*-m-*-iso8859-1"
#define TtFontDefault         "-adobe-courier-medium-r-normal--18-*-*-*-m-*-iso8859-1"
#define ActiveFontDefault     "-adobe-courier-bold-r-normal--18-*-*-*-m-*-iso8859-1"
#define fricas_font_default      "-adobe-courier-bold-o-normal--18-*-*-*-m-*-iso8859-1"
#define EmphasizeFontDefault  "-adobe-courier-medium-o-normal--18-*-*-*-m-*-iso8859-1"
#define BoldFontDefault       "-adobe-courier-bold-r-normal--18-*-*-*-m-*-iso8859-1"
#endif






typedef struct group_item {
    int cur_color;
    XFontStruct *cur_font;
    int center;
    struct group_item *next;
} GroupItem;


extern GroupItem   *gTopOfGroupStack;


typedef struct cond_node {
   char *label;
   char *cond;
} CondNode;

typedef struct parameter_list_type {
    char          **list;       /** The parameters in string form **/
    short           number;     /** How many parameters are there **/
    struct parameter_list_type *next;
}              *ParameterList;

extern char *gDatabasePath;

extern int gBorderColor;

extern GroupItem   *gTopOfGroupStack;

extern short int    gDisplayRegion;
extern int          gRegionOffset;

extern Window gActiveWindow;
extern int    gNeedIconName;

extern int  gScrollbarWidth;

/* the width and height for all windows in the title bar */
extern int  twwidth, twheight;

extern short int gInPaste;

#define ucharp_to_charp(x) ((char *)x)

#endif
