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
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include "edible.h"

#include "e_buf.h"
#include "tio_funs.h"

static void del_print(int, int);

void
myputchar(char c)
{
    if (ECHOIT) {
        putchar(c);
    }
    return;
}

void
clear_buff(void) {
    int cp, char_cnt = 0;

    /*** called when spadbuf gives me a line in case there is something already
      on the line ****/
    if (buff_pntr > 0) {
        /*** backup to the beginning of the line ***/
        for (cp = 0; cp < curr_pntr; cp += dist_right(cp)) {
            myputchar(_BKSPC);
        }
        /** blank over the line      ***/
        for (cp = 0; cp < buff_pntr; cp += dist_right(cp)) {
            myputchar(_BLANK);
            char_cnt++;
        }
        /** back up again ***/
        while (char_cnt > 0) {
            myputchar(_BKSPC);
            char_cnt--;
        }
        re_init_buff();
        curr_pntr = buff_pntr = 0;
    }
}


void
move_end(void) {

    /* Moves cursor to the end of the line */
    if (curr_pntr == buff_pntr) {
        putchar(_BELL);
    } else {
        for (; curr_pntr < buff_pntr;) {
            curr_pntr += prt_char(curr_pntr);
        }
    }
    fflush(stdout);
}

void
move_home(void) {
    /* Moves the cursor to the front of the line */
    if (curr_pntr > 0) {
        for (; curr_pntr > 0;) {
            myputchar(_BKSPC);
            curr_pntr--;
        }
    } else {
        putchar(_BELL);
    }
    fflush(stdout);
}

void
move_fore_word(void) {
    /* move the cursor to the next blank space  */
    if (curr_pntr != buff_pntr) {
        curr_pntr += prt_char(curr_pntr);
        while (curr_pntr < buff_pntr && get_buff(curr_pntr) != ' ') {
            curr_pntr += prt_char(curr_pntr);
        }
    } else {
        putchar(_BELL);
    }
    fflush(stdout);
}

void
move_back_word(void) {
    /* moves the cursor to the last blank space */
    if (curr_pntr > 0) {
        myputchar(_BKSPC);
        curr_pntr -= dist_left(curr_pntr);
        while (curr_pntr > 0 && get_buff(curr_pntr - 1) != ' ') {
            myputchar(_BKSPC);
            curr_pntr -= dist_left(curr_pntr);
        }
    } else {
        putchar(_BELL);
    }
    fflush(stdout);
}

void
delete_current_char(void) {
    /*  deletes the char currently above the current_pntr, if it can be */
    if (curr_pntr != buff_pntr) {
        int flag = get_flag(curr_pntr);
        if (flag == 1 || flag == 0) {
            int ii = dist_right(curr_pntr);
            myputchar(_BLANK);
            myputchar(_BKSPC);
            shift_buff_backward(curr_pntr + ii, buff_pntr, -ii);
            buff_pntr -= ii;
            del_print(curr_pntr, 1);
        } else {
            /** lets delete two of the little buggers **/
            myputchar(_BLANK);
            myputchar(_BLANK);
            myputchar(_BKSPC);
            myputchar(_BKSPC);
            shift_buff_backward(curr_pntr + 2, buff_pntr, -2);
            buff_pntr -= 2;
            del_print(curr_pntr, 2);
        }
    } else {
        putchar(_BELL);
        fflush(stdout);
    }
    num_proc = num_proc + 3;
}

void
delete_to_end_of_line(void) {
    /* deletes from the curr_pntr to the end of line */
    int char_cnt = 0, cp;

    if (curr_pntr == buff_pntr) {
        return;                 /* There is nothing to do */
    }

    /* blank to the end of line */
    for (cp = curr_pntr; cp < buff_pntr;) {
        cp += dist_right(cp);
        char_cnt++;
        myputchar(_BLANK);
    }
    /* back up again */
    while (char_cnt > 0) {
        myputchar(_BKSPC);
        char_cnt--;
    }

    buff_pntr = curr_pntr;
    fflush(stdout);
}

void
delete_line(void) {
    /* deletes the entire line */

    if (buff_pntr == 0) {
        return;                 /* There is nothing to do */
    }

    /** first back up to the beginning of the line */
    while(curr_pntr) {
	curr_pntr -= dist_left(curr_pntr);
        myputchar(_BKSPC);
    }

    delete_to_end_of_line();

    /* Also clear the buffer */
    re_init_buff();
    buff_pntr = curr_pntr = 0;

    fflush(stdout);
}

void
printbuff(int start, int  cnt) {
    int cp;

    for (cp = start; cp < start + cnt;) {
        if (get_buff(cp) == '\0') {
            break;
        }
        cp += prt_char(cp);
    }
    fflush(stdout);
}

static void
del_print(int start, int num) {
    int char_cnt = 0, cp;

    /* print moved rest of the string */
    for (cp = start; cp < buff_pntr;) {
        cp += prt_char(cp);
        char_cnt++;
    }
    /* now blank out the number of chars we are supposed to */
    for (cp = 0; cp < num; cp++) {
        myputchar(_BLANK);
    }
    /* Now back up  */
    for (cp = 0; cp < num + char_cnt; cp++) {
        myputchar(_BKSPC);
    }
    fflush(stdout);
}


void
ins_print(int start, int cnt) {
    int char_cnt = 0, cp = start;

    /* write inserted part */
    while(cp < start + cnt) {
        cp += prt_char(cp);
    }
    /* write the rest of the line */
    while(cp < buff_pntr + cnt) {
        cp += prt_char(cp);
        char_cnt++;
    }
    /* now back up to where we should be */
    while (char_cnt > 0) {
        myputchar(_BKSPC);
        char_cnt--;
    }
    fflush(stdout);
}

void
reprint(int start) {
    /* simply reprints a single character */
    if (get_buff(start) == '\0') {
        myputchar(_BLANK);
    } else {
        prt_char(start);
    }
    myputchar(_BKSPC);
    fflush(stdout);
    return;
}

void
back_up_and_blank(int bp) {
    int char_cnt = back_up(bp), i;

    for(i = 0; i < char_cnt; i++) {
        myputchar(_BLANK);
    }
    for(i = 0; i < char_cnt; i++) {
        myputchar(_BKSPC);
    }
    fflush(stdout);
}

int
back_up(int bp) {
    int cp = 0, char_cnt = 0;

    while(cp < bp) {
        cp += dist_right(cp);
        char_cnt++;
        myputchar(_BKSPC);
    }
    fflush(stdout);
    return char_cnt;
}


void
print_whole_buff(void) {
    printbuff(0, buff_pntr);
}

void
move_ahead(void) {
    /* moves the pointer ahead a single character */
    if (curr_pntr == buff_pntr) {
        putchar(_BELL);
    } else {
        if (get_flag(curr_pntr) == 2) {
            curr_pntr += prt_char(curr_pntr);
        }
        curr_pntr += prt_char(curr_pntr);
    }
    fflush(stdout);
}

void
move_back(void) {
    /* moves the cursor back one character */
    if (curr_pntr == 0) {
        putchar(_BELL);
    } else {
        if (!get_flag(curr_pntr - 1)) {
            myputchar(_BKSPC);
            curr_pntr--;
        }
        myputchar(_BKSPC);
        curr_pntr -= dist_left(curr_pntr);
    }
    fflush(stdout);
}

void
back_over_current_char(void) {
    /* simply backs over the character behind the cursor */
    if (curr_pntr == 0) {
        putchar(_BELL);
    } else {
        if (!get_flag(curr_pntr - 1)) {
            myputchar(_BKSPC);
            myputchar(_BKSPC);
            myputchar(_BLANK);
            myputchar(_BLANK);
            myputchar(_BKSPC);
            myputchar(_BKSPC);
            shift_buff_backward(curr_pntr, buff_pntr, -2);
            buff_pntr -= 2;
            curr_pntr -= 2;
            del_print(curr_pntr, 2);
        } else {
            int ii = dist_left(curr_pntr);
            myputchar(_BKSPC);
            myputchar(_BLANK);
            myputchar(_BKSPC);
            shift_buff_backward(curr_pntr, buff_pntr, -ii);
            curr_pntr -= ii;
            buff_pntr -= ii;
            del_print(curr_pntr, 1);
        }
    }
    fflush(stdout);
}
