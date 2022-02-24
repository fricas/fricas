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

#define Cursor_shape(x)

extern int contNum;
extern struct termios childbuf;   /** the childs normal operating termio   ***/

/***   the terminals mapping of the function keys                        ***/
extern unsigned char  _INTR, _QUIT, _ERASE, _KILL, _EOF, _EOL, _RES1, _RES2;
extern short  INS_MODE ;     /** Flag for insert mode                **/
extern short ECHOIT;         /** Flag for echoing **/
extern short PTY;   /* A flag which lets me know whether or not I am
                       talking to a socket or a pty. If I am not
                       talking to a PTY then I have to do things like echo
                      back newlines, and send interuppts with an eoln
                      */

/***************************************************************************
    Here are the key mapping my routines need
****************************************************************************/


#define       _ESC   0X1B    /**  A character sent before every arrow key ***/
#define       _LBRACK  0X5B    /**  [                                 **/
#define       _EOLN    '\n'    /** eoln                               **/
#define       _CR      0X0D    /**  cr                                **/
#define       _BLANK   0X20    /**  blank                             **/
#define       _BKSPC   0X08    /**  backspace                         **/
#define       _DEL     0X7F    /**  delete                            **/
#define       _BELL    0X07    /***  ring the bell                    **/
#define       _INT     0X7F    /***  interrupt                        **/
#define       _SQUASH  0X03    /**   kill my process                  **/
#define       _CNTRL_A 0X01    /**   cntrl-a, to go to line start     **/
#define       _CNTRL_E 0X05    /**   cntrl-e, to go to line end       **/
#define       _CNTRL_W 0X17    /**   cntrl-w, to back up a word       **/
#define       _CARROT  0X5E    /** circumflex                         **/
#define       _TAB     0X09    /** tab forward                        **/



#define MAXLINE            1024   /** maximum chars. on a line            ***/
#define MAXBUFF             64   /** maximum lines saved in the buffer
                                       queue                            ***/

/***  Here are the constants for my three different modes. ******/
#define CLEFRAW                      0
#define CLEFCANONICAL                1
#define CLEFCBREAK                   2

extern int mode;   /** One of the above # defines *****/

/**   Here is the structure for storing bound  pf-keys             ***/
typedef struct Fkey
{
  char *str;
  short type;
} fkey;

extern fkey function_key[13] ;    /** strings which replace function
                                          keys when a key is hit          ***/


extern char editorfilename[];

/****  Here are a bunch of constant, variable and function defs for edin.c */
#define UP                   0   /** Tells the replace buffer command   ***/
#define DOWN                 1   /**   to look up or down                **/

#define inc(x)  ((x+1)%MAXBUFF)  /** returns the increment of the presented
                                       pointer                          ***/
#define dec(x)  ( ((x-1) < 0) ?(MAXBUFF - 1):(x-1))/** ibid for decrementing */

#define flip(x)   (x?(x=0):(x=1))      /*** flip the bit                  ***/

typedef struct wct {
    char *fname;
    off_t fsize;
    time_t ftime;
    char *fimage;
    int wordc;
    char **wordv;

    struct wct *next;
} Wct;

typedef struct wix {
    Wct *pwct;
    int word;
} Wix;


extern int ring_size;
extern int prev_check;
extern int MAXRING;

extern char in_buff[1024];   /**     buffer for characters read until they are
                                      processed                           **/
extern int num_read;
extern int num_proc;         /**   num chars processed after a read       **/

/***
   Some global defs needed for emulating a pty. This was taken from guru.h
***/



/* Return an integer that is represented by a character string */
#define ciret(x) ((cintu.c4[0]=(x)[0]), (cintu.c4[1]=(x)[1]), \
                  (cintu.c4[2]=(x)[2]), (cintu.c4[3]=(x)[3]), cintu.i4)

/* move an integer (x) to a character string (y) */

#define icmove(x, y) ((cintu.i4=(x)), ((y)[0]=cintu.c4[0]), \
                      ((y)[1]=cintu.c4[1]), ((y)[2]=cintu.c4[2]), \
                         ((y)[3]=cintu.c4[3]))

/* Return an integer that may not be on an integer boundary */
#define iiret(x) ciret(((char *)&(x)))

 /* Min of two expressions */
#define min(x, y) ((x)<(y)?(x):(y))

 /* Max of two expressions */
#define max(x, y) ((x)>(y)?(x):(y))
