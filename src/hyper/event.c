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

#include "hyper.h"

#include <X11/X.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <sys/signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <sys/time.h>

#ifdef SGIplatform
#include <bstring.h>
#endif

#include "keyin.h"
#include "parse.h"

#include "all_hyper_proto.H1"
#include "sockio-c.H1"

static void create_window(void);
static void handle_button(int button, XButtonEvent * event);
static void handle_event(XEvent * event);
static void handle_motion_event(XMotionEvent * event);
static void init_cursor_states(void);
static void make_busy_cursors(void);
static void set_error_handlers(void);
static int set_window(Window window);
static void clear_exposures(Window w);

jmp_buf env;
Window gActiveWindow;
int motion = 0;
int gNeedIconName = 0;
unsigned long bigmask= 0xffffffff;
static HyperLink *gSavedInputAreaLink = NULL;



/*
 * This is the main X loop. It keeps grabbing events. Since the only way the
 * window can die is through an event, it never actually end. One of the
 * subroutines it calls is responsible for killing everything
 */

void
mainEventLoop(void)
{
    XEvent event;
    int  Xcon;
    fd_set rd, dum1, dum2;
    motion = 0;
    gActiveWindow = -1;
    set_error_handlers();
    Xcon = ConnectionNumber(gXDisplay);

    while (1) {
/*        fprintf(stderr,"event:mainEventLoop: loop top\n");*/
        while (gSessionHashTable.num_entries == 0)
            pause();

        /* XFlush(gXDisplay);      */

        if (!motion)
            init_cursor_states();
        motion = 0;

        if (!spad_socket == 0) {
            FD_ZERO(&rd);
            FD_ZERO(&dum1);
            FD_ZERO(&dum2);
            FD_CLR(0, &dum1);
            FD_CLR(0, &dum2);
            FD_CLR(0, &rd);
            FD_SET(spad_socket->socket, &rd);
            FD_SET(Xcon, &rd);
            if (!session_server == 0) {
                FD_SET(session_server->socket, &rd);
            }
            if (XEventsQueued(gXDisplay, QueuedAlready)) {
                XNextEvent(gXDisplay, &event);
                handle_event(&event);
            }
            else {
              select(FD_SETSIZE,(void *)&rd,(void *)&dum1,(void *)&dum2,NULL);
              if (FD_ISSET(Xcon, &rd) ||
                  XEventsQueued(gXDisplay, QueuedAfterFlush)) {
                    XNextEvent(gXDisplay, &event);
                    handle_event(&event);
                }
                else if (FD_ISSET
                    (spad_socket->socket, &rd))
                    /*
                     * FriCAS Socket do what handle_event does The 100 is
                     * $PageStuff in hypertex.boot
                     */
                {
                    if (100 == get_int(spad_socket)) {
                        set_window(gParentWindow->fMainWindow);
                        make_busy_cursors();
                        get_new_window();
                    }
                }
                /*
                 * Session Socket Telling us about the death of a spadbuf
                 * (plus maybe more later) service_session_socket in
                 * spadint.c
                 */
                else
                 if (session_server && FD_ISSET(session_server->socket, &rd)) {
                    service_session_socket();
                 }
            }
        }
        else {
            XNextEvent(gXDisplay, &event);
            handle_event(&event);
        }
    }
}

static void
handle_event(XEvent * event)
{
    XWindowAttributes wa;
/*    fprintf(stderr,"event:handle_event entered\n");*/
    set_window(event->xany.window);
    if (event->type == MotionNotify) {
/*        fprintf(stderr,"event:handle_event type=MotionNotify\n");*/
        handle_motion_event((XMotionEvent *)event);
        motion = 1;
        return;
    }
    make_busy_cursors();
    switch (event->type) {
      case DestroyNotify:
/*        fprintf(stderr,"event:handle_event type=DestroyNotify\n");*/
        break;
      case Expose:
/*        fprintf(stderr,"event:handle_event type=Expose\n");*/
        XGetWindowAttributes(gXDisplay, gWindow->fMainWindow, &wa);
        if ((gWindow->width == 0 && gWindow->height == 0) ||
            (wa.width != gWindow->width || wa.height != gWindow->height)) {
            gWindow->width = wa.width;
            gWindow->height = wa.height;
            display_page(gWindow->page);
            gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;
        }
        else                    /** just redraw the thing **/
            expose_page(gWindow->page);
        XFlush(gXDisplay);
        clear_exposures(gWindow->fMainWindow);
        clear_exposures(gWindow->fScrollWindow);
        break;
      case ButtonPress:
/*        fprintf(stderr,"event:handle_event type=ButtonPress\n");*/
        handle_button(event->xbutton.button, (XButtonEvent *)event);
        XFlush(gXDisplay);
        if (gWindow) {
            while (XCheckTypedWindowEvent(gXDisplay, gWindow->fMainWindow,
                                          Expose, event));
            while (XCheckTypedWindowEvent(gXDisplay, gWindow->fScrollWindow,
                                          Expose, event));
        }
        break;
      case KeyPress:
/*        fprintf(stderr,"event:handle_event type=KeyPress\n");*/
        handle_key(event);
        if (gWindow) {
            while (XCheckTypedWindowEvent(gXDisplay, gWindow->fMainWindow,
                                          Expose, event));
            while (XCheckTypedWindowEvent(gXDisplay, gWindow->fScrollWindow,
                                          Expose, event));
        }
        break;
      case MapNotify:
/*        fprintf(stderr,"event:handle_event type=MapNotify\n");*/
        create_window();
        break;

      case SelectionNotify:
/*        fprintf(stderr,"event:handle_event type=SelectionNotify\n");*/
        /* this is in response to a previous request in an input area */
        if ( gSavedInputAreaLink ) {
            XSelectionEvent *pSelEvent;
            Atom dataProperty;
            pSelEvent = (XSelectionEvent *) event;
            dataProperty = XInternAtom(gXDisplay, "PASTE_SELECTION", False);
            /* change the input focus */

        /*  change_input_focus(gSavedInputAreaLink); */

            /* try to get the selection as a window property */

            if ( pSelEvent->requestor == gWindow->fMainWindow &&
                 pSelEvent->selection == XA_PRIMARY &&
            /*   pSelEvent->time      == CurrentTime && */
                 pSelEvent->target    == XA_STRING &&
                 pSelEvent->property == dataProperty )
            {
                Atom actual_type;
                int  actual_format;
                unsigned long nitems, leftover;
                unsigned char *pSelection = NULL;

                if (Success == XGetWindowProperty(gXDisplay,
                    gWindow->fMainWindow,
                    pSelEvent->property, 0L, 100000000L, True,
                    AnyPropertyType, &actual_type, &actual_format,
                    &nitems, &leftover, (unsigned char **) &pSelection) )
                {
                    char *pBuffer;
                    InputItem *item = gSavedInputAreaLink->reference.string;

                    for (pBuffer = pSelection; *pBuffer; ++pBuffer)
                        add_buffer_to_sym(pBuffer, item);

                    XFree(pSelection);
                }
            }

            /* clear the link info */

            gSavedInputAreaLink = NULL;
        }
        break;

      default:
/*        fprintf(stderr,"event:handle_event type=default\n");*/
        break;
    }

}

static void
create_window(void)
{
    XWindowAttributes wa;

    XGetWindowAttributes(gXDisplay, gWindow->fMainWindow, &wa);

    gWindow->width = wa.width;
    gWindow->height = wa.height;
    display_page(gWindow->page);
    gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;

    /* then select for the events I normally would like to catch */
    XSelectInput(gXDisplay, gWindow->fMainWindow, ButtonPress | KeyPressMask |
                 PointerMotionMask |
                 ExposureMask /* | EnterWindowMask | LeaveWindowMask */ );
    XSelectInput(gXDisplay, gWindow->fScrollWindow, ExposureMask);

}

/*
 * This routine is called when the quitbutton is hit. For the moment I am
 * just going to leave it all behind
 */

void
quitHyperDoc(void)
{
    HyperDocPage *page;

    if (gSessionHashTable.num_entries == 1 || gParentWindow == gWindow) {
        if (!strcmp(gWindow->page->name, "ProtectedQuitPage")){
        exitHyperDoc();
                }
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "ProtectedQuitPage");
        if (page == NULL) {
            fprintf(stderr, "Unknown page name %s\n", "ProtectedQuitPage");
            exitHyperDoc();
            return;
        }
        if (gWindow->fDownLinkStackIndex == MaxDownlinkDepth)
            fprintf(stderr, "exceeded maximum link nesting level\n");
        else
            gWindow->fDownLinkStack[gWindow->fDownLinkStackIndex++] = gWindow->page;
        gWindow->page = page;
        display_page(gWindow->page);
        gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;
    }
    else
        exitHyperDoc();
}


/*
 * find_page takes as an argument the HyperDoc for a page name and returns
 * the associated page
 */

static HyperDocPage *
find_page(TextNode * node)
{
    char *page_name;
    HyperDocPage *page;

    /* try and find the page name */
    page_name = print_to_string(node);
    page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, page_name);

    if (page == NULL) {
        /* try to find the unknown page */
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "UnknownPage");
        if (page == NULL) {
            /* Yikes, Even that could not be found */
            fprintf(stderr, "Unknown page name %s\n", page_name);
        }
        else {
            if (page->type == UnloadedPageType)
                page->type = UlUnknownPage;
            else
                page->type = UnknownPage;
        }
    }
    return page;
}

/*
 * These are macros for taking care of the downlink stack, and the memolink
 * stack.
 */

#define NotSpecial(t) ((t == Quitbutton || t == Returnbutton || \
                        t == Upbutton || t == UnknownPage || \
                        t == UlUnknownPage || t == ErrorPage) ?(0):(1))

/* pushes a page onto the down link stack */

static void
downlink(void)
{
    if (gWindow->fDownLinkStackIndex == MaxDownlinkDepth)
        fprintf(stderr, "exceeded maximum link nesting level\n");
    else
        gWindow->fDownLinkStack[gWindow->fDownLinkStackIndex++] = gWindow->page;
}

static void
memolink(void)
{
    if (gWindow->fMemoStackIndex == MaxMemoDepth)
        fprintf(stderr, "exceeded maximum link nesting level\n");
    else {
        gWindow->fMemoStack[gWindow->fMemoStackIndex] = gWindow->page;
        gWindow->fDownLinkStackTop[gWindow->fMemoStackIndex++] = gWindow->fDownLinkStackIndex;
    }
}

static void
kill_fricas_page(HyperDocPage * page)
{
    char command[2048];

    sprintf(command, "(|htpDestroyPage| '%s)", page->name);
    send_lisp_command(command);
}

static void
kill_page(HyperDocPage * page)
{
    page->scroll_off = 0;
    if (page->type == SpadGen) {
        hash_delete(gWindow->fPageHashTable, page->name);
        kill_fricas_page(page);
        free_page(page);
    }
}

/* pops the memo stack */

static HyperDocPage *
returnlink(void)
{
    int i;

    if (gWindow->fMemoStackIndex == 0) {
        BeepAtTheUser();
        return NULL;
    }
    else {
        kill_page(gWindow->page);
        for (i = gWindow->fDownLinkStackIndex - 1;
             i >= gWindow->fDownLinkStackTop[gWindow->fMemoStackIndex - 1];
             i--)
        {
            kill_page(gWindow->fDownLinkStack[i]);
        }
        gWindow->fDownLinkStackIndex =
            gWindow->fDownLinkStackTop[--gWindow->fMemoStackIndex];
        return (gWindow->fMemoStack[gWindow->fMemoStackIndex]);
    }
}

/* pops a page if it can from the downlink stack */

static HyperDocPage *
uplink(void)
{
    if (gWindow->fDownLinkStackIndex == 0)
        return returnlink();
    else {
        kill_page(gWindow->page);
        return (gWindow->fDownLinkStack[--gWindow->fDownLinkStackIndex]);
    }
}

static void
windowlink_handler(TextNode * node)
{
    char *page_name;

    /* first try and find the page */
    page_name = print_to_string(node);

    if (init_top_window(page_name) == -1) {
        return;
    }
/*    gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;*/
}

void
make_window_link(char *name)
{
    if (init_top_window(name) != -1)
{}/*        gWindow->fWindowHashTable = gWindow->page->fLinkHashTable; */
}


static void
lispwindowlink_handler(HyperLink * link)
{

    /*
     * Since we are popping up a new window, then we had better change all
     * the cursors right away. We won't get another chance at it.
     */

    if (init_top_window(NULL) != -1) {
        HyperDocPage *page = NULL;
        int frame = gWindow->fricas_frame;

        page = issue_server_command(link);
        gWindow->fricas_frame = frame;
        gWindow->page = page;
/*        gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;*/
    }
}

static HyperDocPage *
paste_button(PasteNode * paste)
{
    HyperDocPage *page = NULL;
    int pastewhere=paste->where;


    if ( paste->end_node ==NULL || paste->begin_node==NULL || paste->arg_node==NULL ){
        BeepAtTheUser();
        return NULL;
        }

    page=parse_patch(paste);
/* paste has changed after this call so use pastewhere*/

    if (pastewhere && page ) {
        if (0 == strcmp(page->name, "ErrorPage"))
            page = NULL;
    }
    else
        BeepAtTheUser();

    return page;
}

void
helpForHyperDoc(void)
{
    HyperDocPage *page = NULL;

    /* do not do anything if we are already at the "no more help" page */

    if (0 == strcmp(gWindow->page->name, NoMoreHelpPage))
        return;

    /* if no help page recorded, use the standard "no more help" page */

    if (!gWindow->page->helppage)
        gWindow->page->helppage = alloc_string(NoMoreHelpPage);

    /* if we are on the main help page, use "no more help" page */

    if (0 == strcmp(gWindow->page->name, TopLevelHelpPage))
        gWindow->page->helppage = alloc_string(NoMoreHelpPage);

    page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, gWindow->page->helppage);

    if (page)
        make_window_link(gWindow->page->helppage);
    else
        BeepAtTheUser();
}

static HyperLink *
findButtonInList(HDWindow * window, int x, int y)
{
    ButtonList *bl;

    if (!window || window->page->type == UnloadedPageType)
        return NULL;
    for (bl = window->page->s_button_list; bl != NULL; bl = bl->next)
        if (x >= bl->x0 && x <= bl->x1 && y >= bl->y0 && y <= bl->y1)
            return bl->link;
    for (bl = window->page->button_list; bl != NULL; bl = bl->next)
        if (x >= bl->x0 && x <= bl->x1 && y >= bl->y0 && y <= bl->y1)
            return bl->link;
    return NULL;
}

static HyperLink *
get_hyper_link(XButtonEvent * event)
{
    HyperLink *l1, *l2;

    l1 = (HyperLink *) hash_find(gWindow->fWindowHashTable, (char *)&(event->window));
    if (l1)
        return l1;
    l2 = findButtonInList(gWindow, event->x, event->y);
    return l2;
}

/*
 * Handle a button pressed event. window is the subwindow in which the event
 * occured, and button is the button which was pressed
 */

static void
handle_button(int button, XButtonEvent * event)
{
    HyperLink *link;
    HyperDocPage *page = NULL;
    char *page_name;

    /* Hack to support mouse wheel */
    if (event->window == gWindow->fMainWindow ||
        event->window == gWindow->fScrollWindow) {
        if (button == 4) {
            scrollUp();
            return;
        } else if (button == 5) {
            scrollDown();
            return;
        }
    }

    /* find page name from sub-window handle */

    link = get_hyper_link(event);

    if (link == NULL) {         /* user clicked on an inactive area */
/*      BeepAtTheUser();    */  /* I always thought this was annoying. RSS */
        return;
    }

    switch (link->type) {
      case Pastebutton:
        page = paste_button(link->reference.paste);
        break;
      case Link:
        page_name = print_to_string(link->reference.node);
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, page_name);
        break;
      case Helpbutton:
        helpForHyperDoc();
        page = NULL;
        break;
      case Scrollbar:
        scrollScroller(event);
        break;
      case Scrollupbutton:
        scrollUp();
        break;
      case Scrolldownbutton:
        scrollDown();
        break;

      case Inputstring:
        /* We must be changing input focus or getting a selection */

        change_input_focus(link);
        if ( button == Button2 ) {
            XConvertSelection(gXDisplay, XA_PRIMARY, XA_STRING,
                XInternAtom(gXDisplay, "PASTE_SELECTION", False),
                gWindow->fMainWindow, CurrentTime);
            gSavedInputAreaLink = link;
        }
        break;

      case SimpleBox:
        page = NULL;
        toggle_input_box(link);
        break;
      case Radiobox:
        page = NULL;
        toggle_radio_box(link);
        break;
      case Quitbutton:
        quitHyperDoc();
        break;
      case Returnbutton:        /* pop memo information */
        page = returnlink();
        break;
      case Upbutton:            /* pop downlink information */
        page = uplink();
        break;
      case Downlink:
        page = find_page(link->reference.node);
        if (page  && NotSpecial(page->type))
            downlink();
        break;
      case Memolink:
        page = find_page(link->reference.node);
        if (page && NotSpecial(page->type))
            memolink();
        break;
      case Windowlink:
        page = find_page(link->reference.node);
        if (page && NotSpecial(page->type)) {
            windowlink_handler(link->reference.node);
            gNeedIconName = 1;
            page = NULL;
        }
        break;
      case Lispwindowlink:
        lispwindowlink_handler(link);
        gNeedIconName = 1;
        page = NULL;
        break;
      case LispMemoLink:
      case Spadmemolink:
        page = issue_server_command(link);
        if (page && NotSpecial(page->type))
            memolink();
        break;
      case LispDownLink:
      case Spaddownlink:
        page = issue_server_command(link);
        if (page && NotSpecial(page->type))
            downlink();
        break;
      case Spadlink:
      case Lisplink:
        page = issue_server_command(link);
        break;
      case Lispcommand:
      case Qspadcall:
      case Spadcall:
        page = issue_server_command(link);
        break;
      case Lispcommandquit:
      case Spadcallquit:
      case Qspadcallquit:
        page = issue_server_command(link);
        exitHyperDoc();
        break;
      case Spadcommand:
      case Spadgraph:
      case Spadsrc:
        issue_spadcommand(gWindow->page, link->reference.node,
                          button == Button1, link->type);
        break;
      case Unixlink:
        page = issue_unixlink(link->reference.node);
        if (page && NotSpecial(page->type)) {
            downlink();
        }
        break;
      case Unixcommand:
        issue_unixcommand(link->reference.node);
        break;
      default:
        break;
    }

    if (page) {
        switch (page->type) {   /* check for special button types */
          case Quitbutton:
            exitHyperDoc();
            return;
          case Returnbutton:
            gWindow->page = returnlink();
            break;
          case Upbutton:
            gWindow->page = uplink();
            break;
          case ErrorPage:
          case UnknownPage:
          case UlUnknownPage:
            if (page->type == UlUnknownPage)
                page->type = UnloadedPageType;
            downlink();
            gWindow->page = page;
            break;
          default:              /* a normal link */
            gWindow->page = page;
            break;
        }
        if (link->type != Pastebutton)
            display_page(gWindow->page);
        gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;      /* reset the window hash */
    }
}


void
exitHyperDoc(void)
{
    XEvent event;


    if (gSessionHashTable.num_entries == 1 || gParentWindow == gWindow) {
        free_hd_window(gWindow);
        exit(0);
    }
    hash_delete(&gSessionHashTable, (char *)&gWindow->fMainWindow);

    /*
     * Now we quickly want to flush all the events associated with this
     * window from existence
     */

    XFlush(gXDisplay);
    while (XCheckWindowEvent(gXDisplay, gWindow->fMainWindow, bigmask, &event)) {
    }
    while (XCheckWindowEvent(gXDisplay, gWindow->fScrollWindow,bigmask, &event)) {
    }
    while (XCheckWindowEvent(gXDisplay, gWindow->fDisplayedWindow, bigmask, &event)) {
    }
    while (XCheckWindowEvent(gXDisplay, gWindow->fScrollUpWindow, bigmask, &event)) {
    }
    while (XCheckWindowEvent(gXDisplay, gWindow->fScrollDownWindow, bigmask, &event)) {
    }
    while (XCheckWindowEvent(gXDisplay, gWindow->scrollbar, bigmask, &event)) {
    }
    while (XCheckWindowEvent(gXDisplay, gWindow->scroller, bigmask, &event)) {
    }

    XDestroyWindow(gXDisplay, gWindow->fMainWindow);
    free_hd_window(gWindow);
    gWindow = NULL;
    gActiveWindow = -1;
    XFlush(gXDisplay);
}

static int
set_window(Window window)
{
    Window root, parent, *children, grandparent,myarg;
    HDWindow *htw;
    unsigned int nchildren;
    int st;

    myarg=window;
    nchildren = 0;
    htw = (HDWindow *) hash_find(&gSessionHashTable, (char *)&myarg);
    if (htw != NULL) {
        gWindow = htw;
        return 1;
    }
    st = XQueryTree(gXDisplay, myarg, &root, &parent, &children, &nchildren);
    if (st==0) goto ERROR;
    if (nchildren > 0)
        XFree(children);
    htw = (HDWindow *) hash_find(&gSessionHashTable, (char *)&parent);
    if (htw != NULL) {
        gWindow = htw;
        return 1;

    }
    else {
        /* check for a grandparent */
        st = XQueryTree(gXDisplay, parent, &root, &grandparent, &children, &nchildren);
        if (st==0) goto ERROR;
        if (nchildren > 0)
            XFree(children);
        htw = (HDWindow *) hash_find(&gSessionHashTable, (char *)&grandparent);
        if (htw != NULL) {
            gWindow = htw;
            return 1;
        }
    }

    /*
     * fprintf(stderr, "window(%d) and it's parent(%d) aren't in
     * gSessionHashTable\n", window, parent);

     we never found that window. this happens if (not iff) we exit from
     an unfocused non-main window under certain wm's and click-to-type. the program returns here with
     the window handle that was just destroyed. So let's set the global gWindow
     to the main window.
     */

ERROR:
    gWindow=gParentWindow;
    return 0;
}

/*
 * This procedure whips thru the stack and clears all expose events for the
 * given routine
 */
static void
clear_exposures(Window w)
{
    XEvent report;

    XFlush(gXDisplay);
    while (XCheckTypedWindowEvent(gXDisplay, w, Expose, &report));
}
void
get_new_window(void)
{

    int val;
    char buf[128];
    int frame;
    Window wid;
    HDWindow *htw;
    HyperDocPage *hpage;


    /*
     * If I am going to try and start a new window, then I should make sure I
     * have a coonection to listen on
     *
     * BUT This code is entered when a socket selects
     *
     * if (spad_socket == NULL) { spad_socket =
     * connect_to_local_server(SpadServer, MenuServer, 10); if (spad_socket
     * == NULL) { fprintf(stderr, "Get_new_window: Couldn't Connect to
     * SpadServer\n"); return -1; } }
     *
     */


    frame = get_int(spad_socket);
    val = get_int(spad_socket);
    switch (val) {
      case StartPage:
        init_top_window(NULL);
        val = get_int(spad_socket);
        init_scanner();
        input_type = FromSpadSocket;
        input_string = "";
        gWindow->page = parse_page_from_socket();
        gWindow->fricas_frame = frame;
        XFlush(gXDisplay);
        break;
      case LinkToPage:
        get_string_buf(spad_socket, buf, 128);
        if (init_top_window(buf) == -1) {
            fprintf(stderr, "get_new_window: Did not find page %s\n", buf);
            /* return -1; */
        }
        gWindow->fricas_frame = frame;
        break;
      case PopUpPage:
        val = get_int(spad_socket);
        init_form_window(NULL, val);
        send_int(spad_socket, gWindow->fMainWindow);
        init_scanner();
        input_type = FromSpadSocket;
        input_string = "";
        gWindow->page = parse_page_from_socket();
        compute_form_page(gWindow->page);

        XMapWindow(gXDisplay, gWindow->fMainWindow);

        gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;
        gWindow->fricas_frame = frame;
        XFlush(gXDisplay);
        break;
      case PopUpNamedPage:
        val = get_int(spad_socket);
        get_string_buf(spad_socket, buf, 128);

        if (init_form_window(buf, val) == -1) {
            send_int(spad_socket, -1);
            break;
        }
        load_page(gWindow->page);
        compute_form_page(gWindow->page);

        XMapWindow(gXDisplay, gWindow->fMainWindow);

        gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;
        gWindow->fricas_frame = frame;
        XFlush(gXDisplay);
        send_int(spad_socket, gWindow->fMainWindow);
        /* fprintf(stderr, "Window Id was %d\n", gWindow->fMainWindow); */
        break;
      case ReplaceNamedPage:
        wid = (Window) get_int(spad_socket);
        get_string_buf(spad_socket, buf, 128);

        htw = (HDWindow *) hash_find(&gSessionHashTable,(char *)&wid);
        if (htw == NULL) break;
        hpage = (HyperDocPage *) hash_find(gWindow->fPageHashTable, buf);
        if (hpage == NULL) break;
        gWindow = htw;
        gWindow->page = hpage;
        display_page(gWindow->page);
        gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;
        clear_exposures(gWindow->fMainWindow);
        clear_exposures(gWindow->fScrollWindow);
        XFlush(gXDisplay);
        break;
      case ReplacePage:
        wid = (Window) get_int(spad_socket);
        set_window(wid);
        init_scanner();
        input_type = FromSpadSocket;
        input_string = "";
        gWindow->page = parse_page_from_socket();
        display_page(gWindow->page);
        gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;
        clear_exposures(gWindow->fMainWindow);
        clear_exposures(gWindow->fScrollWindow);
        XFlush(gXDisplay);
        break;
      case KillPage:
        /* Here the user wishes to kill the page */
        wid = (Window) get_int(spad_socket);
        htw = (HDWindow *) hash_find(&gSessionHashTable,(char *)&wid);
        if (htw !=NULL) {
            gWindow = htw;
            exitHyperDoc();
            break;
          }
        break;
    }
  }
static void
set_cursor(HDWindow *window,Cursor state)
{
    if (state == gBusyCursor)
        XDefineCursor(gXDisplay, window->fMainWindow, gBusyCursor);
    else if (state == gActiveCursor)
        XDefineCursor(gXDisplay, window->fMainWindow, gActiveCursor);
    else
        XDefineCursor(gXDisplay, window->fMainWindow, gNormalCursor);
    XFlush(gXDisplay);
}

static void
change_cursor(Cursor state, HDWindow *window)
{
    if (window->fDisplayedCursor == state)
        return;
    window->fDisplayedCursor = state;
    set_cursor(window, state);
}

static void
handle_motion_event(XMotionEvent *event)
{
    if (!gWindow)
        return;
    if (findButtonInList(gWindow, event->x, event->y) != NULL)
        change_cursor(gActiveCursor, gWindow);
    else
        change_cursor(gNormalCursor, gWindow);
}

static void
init_cursor_state(HDWindow *window)
{
    if (window) {
        int x, y, rx, ry, but;
        Window r, c;

        XQueryPointer(gXDisplay, window->fMainWindow,
                             &r, &c, &rx, &ry, &x, &y,(unsigned int *) &but);
        if (findButtonInList(window, x, y) != NULL)
            change_cursor(gActiveCursor, window);
        else
            change_cursor(gNormalCursor, window);
    }
}

static void
init_cursor_states(void)
{
    hash_map(&gSessionHashTable,(MappableFunction) init_cursor_state);
}


static void
make_busy_cursor(HDWindow *window)
{
    change_cursor(gBusyCursor, window);
}

static void
make_busy_cursors(void)
{
    hash_map(&gSessionHashTable, (MappableFunction)make_busy_cursor);
}

static int
HyperDocErrorHandler(Display *display, XErrorEvent *xe)
{
    if (xe->request_code != 15) {
        char buf[1024];

        XGetErrorText(display, xe->error_code, buf, sizeof(buf));

        fprintf(stderr, "error code = %d\n", xe->error_code);
        fprintf(stderr, "major op code = %d\n", xe->request_code);
        fprintf(stderr, "minor op code = %d\n", xe->minor_code);
        fprintf(stderr, "XID = %ld\n", xe->resourceid);
        fprintf(stderr, "%s\n", buf);

        if (xe->request_code != 15)
            exit(-1);
      }
    return(0);
}



static void
set_error_handlers(void)
{
    XSetErrorHandler(HyperDocErrorHandler);
}
