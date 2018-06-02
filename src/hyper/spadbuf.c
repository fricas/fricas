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

#include <termios.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>

#include "bsdsignal.h"
#include "edible.h"
#include "com.h"

#include "bsdsignal.H1"
#include "sockio-c.H1"
#include "edin.H1"
#include "wct.H1"
#include "prt.H1"
#include "cursor.H1"
#include "fnct_key.H1"

unsigned char _INTR, _QUIT, _ERASE, _KILL, _EOF, _EOL, _RES1, _RES2;
int contNum;                    /* do reading and all the other fun stuff
                                 * depend on this for all there ioctl's */
int num_read;

/*
 * Here are the term structures I need for setting and resetting the terminal
 * characteristics.
 */
struct termios oldbuf;     /* the initial settings */
struct termios canonbuf;   /* set it to be canonical */
struct termios childbuf;

short INS_MODE;            /* Flag for insert mode */
short ECHOIT;              /* Flag for echoing */
short PTY;
int MODE;                  /* Am I in cbreak, raw, or canonical */

char in_buff[1024];        /* buffer for storing characters read
                              until they are processed */
char buff[MAXLINE];        /* Buffers for collecting input and */
int  buff_flag[MAXLINE];   /* flags for whether buff chars
                              are printing or non-printing */
int (*old_handler) ();
Sock *session_sock, *menu_sock;
char *buff_name = NULL;    /* name for the aixterm */

/*
 * This routine used to be used to send sigint onto spad, but now they go
 * through just fine on their own reinstated for AIX V3.2
 */

static void
spadbuf_inter_handler(int sig)
{
    send_signal(session_sock, SIGUSR2);
}

static void
spadbuf_function_chars(void)
{
    /** once I have that get the special characters         ****/
    _INTR = oldbuf.c_cc[VINTR];
    _QUIT = oldbuf.c_cc[VQUIT];
    _ERASE = oldbuf.c_cc[VERASE];
    _KILL = oldbuf.c_cc[VKILL];
    _EOF = oldbuf.c_cc[VEOF];
    _EOL = oldbuf.c_cc[VEOL];
    return;
}

/* act as terminal session for sock connected to stdin
   and stdout of another process */
static void
interp_io(void)
{
    char buf[1024];
    fd_set rd;
    int len, command;

    while (1) {
        FD_ZERO(&rd);
        FD_SET(menu_sock->socket, &rd);
        FD_SET(session_sock->socket, &rd);
        FD_SET(1, &rd);
        len = sselect(FD_SETSIZE, &rd, 0, 0, NULL);
        if (len == -1) {
            perror("stdio select");
            return;
        }
        if (FD_ISSET(session_sock->socket, &rd)) {
            len = sread(session_sock, buf, 1024, "stdio");
            if (len == -1)
                return;
            else {
                write(1, buf, len);
            }
        }
        if (FD_ISSET(menu_sock->socket, &rd)) {
            command = get_int(menu_sock);
            switch (command) {
              case -1:
                exit(0);
              case ReceiveInputLine:
                get_string_buf(menu_sock, in_buff, 1024);
                num_read = strlen(in_buff);
                clear_buff();
                do_reading();
                break;
              case TestLine:
                break;
              default:
                break;
            }
        }
        if (FD_ISSET(1, &rd)) {
            num_read = read(0, in_buff, 1024);
            do_reading();
        }
    }
}

static void
init_parent(void)
{

    /** get the original termio settings, so I never have to check again **/
    if (tcgetattr(0,&oldbuf) == -1) {
        perror("Clef Trying to get terms initial settings");
        exit(-1);
    }

    /** get the settings for my different modes ***/
    if (tcgetattr(0,&canonbuf) == -1) {
        perror("Clef Getting terminal settings");
        exit(-1);
    }

    /*** set the buffer to read before an eoln is typed ***/
    canonbuf.c_lflag &= ~(ICANON | ECHO | ISIG);
    canonbuf.c_lflag |= ISIG;

    /***  Accordingly tell it we want every character ***/
    canonbuf.c_cc[VMIN] = 1;          /* we want every character  */
    canonbuf.c_cc[VTIME] = 1;         /* these may require tweaking */

    if (tcsetattr(0, TCSAFLUSH, &canonbuf) == -1) {
        perror("Spadbuf setting parent to canon");
        exit(0);
    }

    /*
     * This routine is in edin.c and sets the users preferences for function
     * keys. In order to use it I have to set childbuf to be the same as
     * oldbuf
     */


    spadbuf_function_chars();
    INS_MODE = 0;
    ECHOIT = 1;
    Cursor_shape(2);
}

int
main(int argc,char **  argv)
{
    /*int name_found;*/
    /*FILE *junk;*/
    FILE *fopen();

    /*
     * Modified on 6/13/90 for the command line completion abiltities of
     * Since I am only calling this program from within spadint, I decided
     * that the usage should be
     *
     * spadbuf page_name [completion_ files]
     *
     */
    if (argc < 2) {
        fprintf(stderr, "Usage : spadbuf page_name [completion_files] \n");
        exit(-1);
    }
    buff_name = *++argv;

    while (*++argv) {
        load_wct_file(*argv);
    }
    skim_wct();

    session_sock = connect_to_local_server(SessionServer, InterpWindow, Forever);
    menu_sock = connect_to_local_server(MenuServerName, InterpWindow, Forever);

    bsdSignal(SIGINT, spadbuf_inter_handler,RestartSystemCalls);

    /*
     * set contNum so it is pointing down the socket to the childs
     */
    contNum = session_sock->socket;
    send_string(menu_sock, buff_name);
    init_parent();
    define_function_keys();
    init_reader();
    PTY = 0;
    interp_io();
    return(1);
}
