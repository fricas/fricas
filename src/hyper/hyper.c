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

/*
 * This is the main module of the HyperDoc program. It contains the main
 * routine which initializes all the X stuff, and the tables. Then it passes
 * control over to the main event loop.
 */

/* #define DEBUG         1 */

/* Include all the needed include files  */

#include "debug.h"


#include "hyper.h"

#include <sys/signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <setjmp.h>
#include <X11/cursorfont.h>

#include "keyin.h"
#include "bsdsignal.h"

#include "all_hyper_proto.H1"
#include "sockio-c.H1"
#include "bsdsignal.H1"

static void init_hash(void);
static void make_server_connections(void);
static void check_arguments(void);

/*
 * Here is a flag used to tell me whether I made a good connection to the
 * menu server. Needed so I don't send spad commands when I should not
 */

int MenuServerOpened = 1;

/* include icon bitmap data */

#define BITMAPDEPTH 1

/* X11 display and screen variables */

Display *gXDisplay;
int      gXScreenNumber;

/*
 * Information about the top level HyperDoc window
 */

HDWindow *gWindow = NULL;      /* the current window */
HDWindow *gParentWindow =NULL; /* the parent window. The one that appears
                                 * when you first start HyperDoc       */

HashTable gSessionHashTable;   /* hash table of HD windows */
HashTable init_page_hash;       /* initial hash table of HD pages */
HashTable init_macro_hash;      /* initial hash table of HD macros */
HashTable init_patch_hash;      /* initial hash table of HD patches */

/* The various Cursors we use */

Cursor gNormalCursor;          /* The normal mouse cursor                */
Cursor gActiveCursor;          /* The cursor in active regions           */
Cursor gBusyCursor;            /* The clock cursor for when I am busy    */


HashTable gFileHashTable;            /* hash table of HyperDoc files */
HashTable gImageHashTable;           /* hash table for images */


/* Some things needed for Handling interupts properly                      */

int gIsEndOfOutput;              /* set to true when spad has finished output */
int received_window_request = 0;/* true iff Spad wants a pop-up    */
int in_next_event = 0;          /* true when in XNextEvent                 */
int make_input_file = 0;        /* true when making input files from ht */
int make_patch_files = 0;       /* true when making patch files from ht */
int gmake_record_file= 0;       /* true when making record files from ht */
int gverify_record_file = 0;    /* true when verifying record files from ht */
int gverify_dates = 0;          /* true when we want hypertex to verify ht.db dates */

Sock *session_server;           /* socket connecting to session manager */

/* true iff HyperDoc is acting as a FriCAS server */
int is_fricas_server = 0;

int kill_spad = 0;              /* kill spad when finished with paste file */

int gSwitch_to_mono=0;         /* will be set to 1 if at any time we don't have
                                enough colours for the images. We will know this
                                when read_pixmap_file returns -1. We will use this
                                when deciding what to do in case of \inputimage */

int gTtFontIs850=0;            /* a flag that tells us if the Tt font is a IBM pagecode 850
                                font and hence supports the graphics chars
                                set when the TtFont is opened*/

/*
 * Global copies of the command line arguments, so they never have to be
 * passed as parameters. This is also so any child process starting up also
 * has the same values.
 */

int gArgc;
char **gArgv;

char **input_file_list;
int input_file_count;

/*
 * SIGUSR2 is raised by the spadbuf program when it is done with the current
 * command
 */

void
sigusr2_handler(int sig)
{
  gIsEndOfOutput = 1;
  return ;
}

void
sigcld_handler(int sig)
{

    /* why were we waiting after the child had already died ??
      because we don't want zombies */

  int x;
  wait(&x);

}

extern jmp_buf env;


/* Clean up spad sockets on exit */
void
clean_socket(void )
{
    char name[256];

    make_server_name(name, MenuServerName);
    unlink(name);
}

/*
 * initialize hash tables, signal handlers and windows, then call the main
 * event handling loop
 */

int
main(int argc, char **argv)
{
    int ret_status;

    /* Initialize some global values */
/*    fprintf(stderr,"hyper:main:entered\n");*/
    gArgc = argc;
    gArgv = argv;
    gIsEndOfOutput = 1;

/*    fprintf(stderr,"hyper:main:calling  check_arguments\n");*/
    check_arguments();
/*    fprintf(stderr,"hyper:main:returned check_arguments\n");*/

    /*
     * initialize the hash tables for the files and the windows and images
     */
/*    fprintf(stderr,"hyper:main:calling  init_hash\n");*/
    init_hash();
/*    fprintf(stderr,"hyper:main:returned init_hash\n");*/

    /*
     * initialize the parser keyword hash table
     */
/*    fprintf(stderr,"hyper:main:calling  parser_init\n");*/
    parser_init();
/*    fprintf(stderr,"hyper:main:returned parser_init\n");*/

/*    fprintf(stderr,"hyper:main:calling  read_ht_db\n");*/
    read_ht_db(&init_page_hash, &init_macro_hash, &init_patch_hash);
/*    fprintf(stderr,"hyper:main:returned read_ht_db\n");*/

    /*
     * Now initialize x. This includes opening the display, setting the
     * screen and display global values, and also gets all the fonts and
     * colors we will need.
     */

    if (!make_input_file && !gmake_record_file && !gverify_record_file) {
/*        fprintf(stderr,"hyper:main:calling  initializeWindowSystem\n");*/
        initializeWindowSystem();
/*        fprintf(stderr,"hyper:main:returned initializeWindowSystem\n");*/

        /*
         * Initialize some of the global values used by the input string
         * routines
         */
/*        fprintf(stderr,"hyper:main:calling  init_keyin\n");*/
        init_keyin();
/*        fprintf(stderr,"hyper:main:returned init_keyin\n");*/

        /*
         * regardless of what else happened, we should always pop up an
         * initial window.
         */

/*        fprintf(stderr,"hyper:main:calling  init_top_window\n");*/
        ret_status = init_top_window("RootPage");
/*        fprintf(stderr,"hyper:main:returned init_top_window\n");*/
        gParentWindow = gWindow;
        if (ret_status == -1) {
            fprintf(stderr,
               "(HyperDoc) Could not find RootPage for top-level window.\n");
            exit(-1);
        }

        /*
         * Tell it how to handle the user defined signals I may get
         */
        bsdSignal(SIGUSR2, sigusr2_handler,RestartSystemCalls);
        bsdSignal(SIGUSR1, SIG_IGN,RestartSystemCalls);
#if defined(BSDplatform) || defined(MACOSXplatform)
        bsdSignal(SIGCHLD, sigcld_handler,RestartSystemCalls);
#else
        bsdSignal(SIGCLD, sigcld_handler,RestartSystemCalls);
#endif
        bsdSignal(SIGINT, SIG_IGN,RestartSystemCalls);

        /*
         * Now go to the main event loop. I will never return, so just end
         * the main routine after that
         */

        /*
         * make an input file if requested
         */
    }
    else {

        /*
         * Try to establish all the socket connections I need. If I am an
         * is_fricas_server and the routine fails, it will exit for me
         */
/*        fprintf(stderr,"hyper:main:in else case\n");*/
/*        fprintf(stderr,"hyper:main:calling  make_server_connections\n");*/
        make_server_connections();
/*        fprintf(stderr,"hyper:main:returned make_server_connections\n");*/


        if (make_input_file) ht2_input();
        if (gmake_record_file) make_record();
        if (gverify_record_file) verify_record();
        exit(0);
    }

    /*
     * Try to establish all the socket connections I need. If I am an
     * is_fricas_server and the routine fails, it will exit for me
     */
/*    fprintf(stderr,"hyper:main:calling  make_server_connections\n");*/
    make_server_connections();
/*    fprintf(stderr,"hyper:main:returned make_server_connections\n");*/


/*    fprintf(stderr,"hyper:main:calling  mainEventLoop\n");*/
    mainEventLoop();
/*    fprintf(stderr,"hyper:main:returned mainEventLoop\n");*/

    return 0;
}

/*
 * Initializes the hash table for Files, and Windows
 */

static void
init_hash(void)
{
    hash_init(&gFileHashTable,
              FileHashSize,
              (EqualFunction)string_equal,
              (HashcodeFunction) string_hash);
    hash_init(&gSessionHashTable,
              SessionHashSize,
              (EqualFunction) window_equal,
              (HashcodeFunction) window_code);
    hash_init(&gImageHashTable,
              ImageHashSize,
              (EqualFunction) string_equal,
              (HashcodeFunction) string_hash);
}

/* initialize the HyperDoc page hierarchy data structures */

void
init_page_structs(HDWindow *w)
{
    int i;

    w->fMemoStackIndex = 0;
    for (i = 0; i < MaxMemoDepth; i++) {
        w->fMemoStack[i] = NULL;
        w->fDownLinkStackTop[i] = 0;
    }
    w->fDownLinkStackIndex = 0;
    for (i = 0; i < MaxDownlinkDepth; i++)
        w->fDownLinkStack[i] = NULL;
}

static void
check_arguments(void)
{
  int i;

  /*
   * Now check the command line arguments, to see if I am supposed to be a
   * server or not
   */
  for (i = 1; i < gArgc; i++) {
    if (gArgv[i][0] == '-')
      switch (gArgv[i][1]) {
      case 'p':
        gverify_dates=1;
        break;
      case 's':
        if (!MenuServerOpened) {
          fprintf(stderr, "(HyperDoc) Server already in use.\n");
          exit(-1);
        }
        is_fricas_server = 1;
        break;
      case 'i':
        if (gArgv[i][2] == 'p')
          make_patch_files = 1;
        make_input_file = 1;
        input_file_list = gArgv + i + 1;
        input_file_count = gArgc - i - 1;
        break;
      case 'k':
        kill_spad = 1;
        break;
      case 'r':
        if (gArgv[i][2] == 'm')
          gmake_record_file=1;
        else if (gArgv[i][2] == 'v')
          gverify_record_file=1;
        else
          fprintf(stderr, "(HyperDoc) v or m must follow -r\n");
        input_file_list = gArgv + i + 1;
        input_file_count = gArgc - i - 1;
        break;
      default:
        fprintf(stderr, "(HyperDoc) Unexpected Command Line Argument %s\n", gArgv[i]);
        fprintf(stderr, "           Usage: hypertex [-s]\n");
        break;
      }
  }
}

static void
make_server_connections(void)
{
    int i, wait_time;

    /*
     * Try to open the menuserver socket, if I can not, then set a flag
     */

    if (open_server(MenuServerName) == -2) {
        fprintf(stderr, "(HyperDoc) Warning: Not connected to FriCAS Server!\n");
        MenuServerOpened = 0;
    } else {
        atexit(&clean_socket);
        MenuServerOpened = 1;
    }


    /*
     * If I have opened the MenuServer socket, then I should also try to open
     * the SpadServer socket, so I can send stuff right to SPAD.
     */

    if (MenuServerOpened) {

        /*
         * If I am a ht server, then I should not continue on unless I
         * establish some sort of connection
         */

        /*
         * Modified on 11/20 so that it prints an error message every ten for
         * ten tries at opeing the socket. If it fails all ten times, it
         * gives up and exits.
         */

        if (!is_fricas_server)
            wait_time = 2;
        else
            wait_time = 1000;

        for (i = 0, spad_socket = NULL; i < 2 && spad_socket == NULL; i++) {
            spad_socket = connect_to_local_server(SpadServer,
                                                  MenuServer, wait_time);
            if (is_fricas_server && spad_socket == NULL)
                fprintf(stderr, "(HyperDoc) Error opening FriCAS server. Retrying ...\n");
            else
                i = 11;
        }
        if (! spad_socket) {
            fprintf(stderr, "(HyperDoc) Couldn't connect to FriCAS server!\n");
            if (!is_fricas_server)
                MenuServerOpened = 0;
            else {
                fprintf(stderr, "(HyperDoc) Couldn't connect to FriCAS server!\n");
                exit(-1);
            }
        }
        else {

            /*
             * Do the same thing for the SessionServer
             */

            for (i = 0, session_server = NULL; i < 2 && session_server == NULL
                 ; i++) {
                session_server =
                    connect_to_local_server(SessionServer, MenuServer,
                                            wait_time);
                if (is_fricas_server && session_server == NULL) {
                    fprintf(stderr,
                            "(HyperDoc) Error opening SessionServer, Retrying ...\n");
                }
                else
                    i = 11;
            }
            if (session_server == NULL) {
                fprintf(stderr, "(HyperDoc) Connection attempt to session manager timed out.\n");
                if (is_fricas_server) {
                    fprintf(stderr,
                            "(HyperDoc) Server unable to connect to session server\n");
                    exit(-1);
                }
                else {
                    MenuServerOpened = 0;
                }
            }
        }
    }
}
