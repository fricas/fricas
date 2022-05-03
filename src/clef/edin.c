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

/* #define debug 1 */

#include <stdlib.h>
#include "fricas_c_macros.h"
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>

#include "e_buf.h"

#include "edible.h"

extern QueStruct *ring;
extern QueStruct *current;

#define HFT 0
#define SUN 1
#define DEC 2
#define control_to_alpha(x)   (x + ('A' - 0x01))
#define alpha_to_control(x)   (x - ('A' - 0x01))

/* Shorthands for some control sequences */
#define back_word(x) (((*(x) == '5') && (*(x+1) == '9') && \
                                        (*(x+2) == 'q'))?(1):(0))

#define fore_word(x) (((*(x) == '6') && (*(x+1) == '8') && \
                                        (*(x+2) == 'q'))?(1):(0))

#define cntrl_end(x) (((*(x) == '4') && (*(x+1) == '8') && \
                                        (*(x+2) == 'q'))?(1):(0))

#define insert_toggle(x) (((*(x) == '3') && (*(x+1) == '9') && \
                                        (*(x+2) == 'q'))?(1):(0))

#define end_key(x) (((*(x) == '4') && (*(x+1) == '6') && \
                                        (*(x+2) == 'q'))?(1):(0))

#define control_char(x) \
     (((x >= 0x01) && (x <= 0x1a))?(1):(0))

#define normal_char(x) ((((x) >= ' ') && ((x) < 0x7f)) || \
                        (((unsigned char)(x)) >= 128))

QueStruct *ring = NULL;
QueStruct *current = NULL;
int ring_size = 0;
int MAXRING = 64;
int prev_check = 10;
int num_pntr;
int num_proc;
int had_tab;
int had_tab_last;

#include "tio_funs.h"

static void send_line_to_child(void);
static void insert_buff_nonprinting(int);
static void prev_buff(void);
static void next_buff(void);
static void insert_buff_printing(int);
static void insert_queue(void);

void
init_reader(void) {
    re_init_buff();
    buff_pntr = curr_pntr = 0;

    had_tab = 0;
    had_tab_last = 0;
}


void
do_reading(void) {
    int ttt_read;
    int done_completely;

    done_completely = 0;
    num_proc = 0;
    while (num_proc < num_read) {
        if(in_buff[num_proc]== _ERASE) {
            back_over_current_char();
            num_proc++;
        } else {
            switch (in_buff[num_proc]) {
              case _EOLN:
              case _CR:
                /* Complete line, so send it to the child */
                send_line_to_child();
                if (!PTY) {
                    myputchar('\n');
                }
                break;

                /*
                 * Use 0x7f as delete
                 */
              case _DEL:
                /* Had a delete key */
                delete_current_char();
                break;

              case _CNTRL_W:
                move_back_word();
                num_proc++;
                break;
              case _TAB:
                had_tab = 1;
                /* command completion stuff */
                num_proc++;
                if (had_tab_last) {
                    rescan_wct();
                } else {
                    find_wct();
                }
                break;
              case _BELL:
                insert_buff_nonprinting(1);
                putchar(_BELL);
                fflush(stdout);
                break;
              case _CNTRL_A:
                move_home();
                num_proc++;
                break;
              case _CNTRL_E:
                move_end();
                num_proc++;
                break;

              case _ESC:
                /*
                 * get 2 characters more
                 */
                while (!(num_read - num_proc > 2)) {
                    ttt_read = read(0, in_buff + num_read,
                                    2 - (num_read - num_proc) + 1);
                    if (ttt_read > 0) {
                        num_read = num_read + ttt_read;
                    }
                }
                if (in_buff[num_proc + 1] == _LBRACK) {

                    /* ESC [  */

                    switch (in_buff[num_proc + 2]) {
                      /*  look for arrows */
                      case 'A':
                        /* up arrow */
                        prev_buff();
                        curr_pntr = buff_pntr;
                        num_proc = num_proc + 3;
                        break;
                      case 'B':
                        /* down arrow */
                        next_buff();
                        curr_pntr = buff_pntr;
                        num_proc = num_proc + 3;
                        break;
                      case 'C':
                        /* right arrow */
                        move_ahead();
                        num_proc = num_proc + 3;
                        break;
                      case 'D':
                        /* left arrow */
                        move_back();
                        num_proc = num_proc + 3;
                        break;

                      /*
                       * Use ^[[P as delete
                       */
                      case 'P':
                        /*** Had a delete key      ****/
                        delete_current_char();
                        break;
                      case 'H':
                      case 0:
                        move_home();
                        num_proc += 3;
                        break;
                      case 'F':
                        move_end();
                        num_proc += 3;
                        break;
                      case 'M':
                      case 'Z':
                        insert_buff_nonprinting(3);
                        done_completely = 1;
                        num_proc += 3;
                        break;
                      case 'x':
                        num_proc = num_read;
                        break;
                      case '1':
                      case '2':
                      case '0':
                        /*
                         * Possible function key hit, check for ESC ] x ~
                         */
                        while (!(num_read - num_proc > 3)) {
                            ttt_read = read(0, in_buff + num_read,
                                            3 - (num_read - num_proc) + 1);
                            if (ttt_read > 0) {
                                num_read = num_read + ttt_read;
                            }
                        }
                        if (in_buff[num_proc + 3] == '~') {
                            /*
                             * treat ESC ] x ~
                             */
                            switch (in_buff[num_proc + 2]) {
                              case '2':
                                flip(INS_MODE);
                                if (INS_MODE) {
                                    Cursor_shape(5);
                                } else {
                                    Cursor_shape(2);
                                }
                                reprint(curr_pntr);
                                num_proc += 4;
                                break;
                              default:
                                insert_buff_nonprinting(1);
                                break;
                            }
                            break;
                        }
                        /* check for ESC ] x y ~ */
                        while (!(num_read - num_proc > 4)) {
                            ttt_read = read(0, in_buff + num_read,
                                            4 - (num_read - num_proc) + 1);
                            if (ttt_read > 0) {
                                num_read = num_read + ttt_read;
                            }
                        }
                        if (in_buff[num_proc + 4] == '~') {
                            /*
                             * treat ESC ] x y ~
                             */
                            insert_buff_nonprinting(1);
                            break;
                        }

                        /* check for ESC ] x y z [q|z] */

                        while (!(num_read - num_proc > 5)) {
                            ttt_read = read(0, in_buff + num_read,
                                            5 - (num_read - num_proc) + 1);
                            if (ttt_read > 0) {
                                num_read = num_read + ttt_read;
                            }
                        }
                        if (insert_toggle(&in_buff[num_proc + 3])) {
                            flip(INS_MODE);
                            if (INS_MODE) {
                                Cursor_shape(5);
                            } else {
                                Cursor_shape(2);
                            }
                            reprint(curr_pntr);
                            num_proc = num_proc + 6;
                            break;
                        } else if (cntrl_end(&in_buff[num_proc + 3])) {
                            num_proc = num_proc + 6;
                            delete_to_end_of_line();
                            break;
                        } else if (back_word(&in_buff[num_proc + 3])) {
                            move_back_word();
                            num_proc += 6;
                            break;
                        } else if (fore_word(&in_buff[num_proc + 3])) {
                            move_fore_word();
                            num_proc += 6;
                            break;
                        } else if (end_key(&in_buff[num_proc + 3])) {
                            move_end();
                            num_proc += 6;
                            break;
                        }
                        switch (in_buff[num_proc + 5]) {
                          case 'q':
                            /*
                             * IBM function keys
                             */
                            {
                            char num[3];
                            int key;

                            num[0] = in_buff[num_proc + 3];
                            num[1] = in_buff[num_proc + 4];
                            num[2] = '\0';
                            key = atoi(num);
                            if (key > 0 && key < 13) {
                                if (function_key[key].str != NULL) {
                                    handle_function_key(key);
                                    done_completely = 1;
                                } else {
                                    insert_buff_nonprinting(6);
                                    done_completely = 1;
                                }
                            } else {
                                insert_buff_nonprinting(6);
                                done_completely = 1;
                            }
                            break;
                            }
                          case 'z':
                            /*
                             * Sun function keys
                             */
                            {
                            char num[3];
                            int key;

                            num[0] = in_buff[num_proc + 3];
                            num[1] = in_buff[num_proc + 4];
                            num[2] = '\0';
                            key = atoi(num) - 23;
                            if (key > 0 && key < 13) {
                                if (function_key[key].str != NULL) {
                                    handle_function_key(key);
                                    done_completely = 1;
                                } else {
                                    insert_buff_nonprinting(6);
                                    done_completely = 1;
                                }
                            } else if (atoi(num) == 14) {
                                move_home();
                                num_proc += 6;
                                done_completely = 1;
                            } else if (atoi(num) == 20) {
                                move_end();
                                num_proc += 6;
                                done_completely = 1;
                            } else if (atoi(num) == 47) {
                                flip(INS_MODE);
                                if (INS_MODE) {
                                    Cursor_shape(5);
                                } else {
                                    Cursor_shape(2);
                                }
                                reprint(curr_pntr);
                                num_proc = num_proc + 6;
                                done_completely = 1;
                            } else {
                                insert_buff_nonprinting(6);
                                done_completely = 1;
                            }
                            break;
                            }

                          default:
                            insert_buff_nonprinting(1);
                            break;
                        }
                      default:
                        if (!done_completely) {
                           insert_buff_nonprinting(1);
                        }
                        break;
                    }
                } else {              /* ESC w/o [ */
                    insert_buff_nonprinting(1);
                }
                break;

            case _BKSPC:
                back_over_current_char();
                num_proc++;
                break;
            default:
                if (in_buff[num_proc] == _KILL) {
                    delete_line();
                    num_proc++;
                } else if ((in_buff[num_proc] == _INTR) ||
                          (in_buff[num_proc] == _QUIT)) {
                    write(contNum, &in_buff[num_proc], num_read - num_proc);
                    if (!PTY) {
                        write(contNum, "\n", 1);
                    }
                    num_proc++;
                } else if (in_buff[num_proc] == _EOF) {
                    insert_buff_nonprinting(1);
                    if (!PTY) {
                        write(contNum, "\n", 1);
                    }
                    num_proc++;
                } else if (in_buff[num_proc] == _EOL) {
                    send_line_to_child();
                    if (!PTY) {
                        write(contNum, "\n", 1);
                    }
                } else if (in_buff[num_proc] == _ERASE) {
                    back_over_current_char();
                    num_proc++;
                } else {
                    if (control_char(in_buff[num_proc])) {
                        insert_buff_nonprinting(1);
                    } else {
                        int ii = 1;
                        while(ii + num_proc < num_read &&
                              normal_char(in_buff[num_proc + ii])) {
                            ii++;
                        }
                        insert_buff_printing(ii);
                    }
                }
                /* close the default case */
                break;
            }                       /* switch */
        } /*else*/
        if (had_tab) {
            had_tab_last = 1;
            had_tab = 0;
        } else {
            had_tab_last = 0;
        }

    }                           /* while */
}

static int
convert_buffer(char *target, int num);

static void
send_line_to_child(void) {
    static char converted_buffer[MAXLINE];
    int converted_num;

    /*  Takes care of sending a line to the child, and resetting the
        buffer for new input                                  */

    back_up(curr_pntr);

    /* start by putting the line into the command line ring ***/
    insert_queue();

    /* finish the line and send it to the child **/
    store_final_buff_char(in_buff[num_proc]);

    /*
     * Instead of actually writing the Line, I have to  substitute in the
     * actual characters received
     */
    converted_num = convert_buffer(converted_buffer, buff_pntr);
    write(contNum, converted_buffer, converted_num);

    /** reinitialize the buffer  ***/
    re_init_buff();
    /**  reinitialize my buffer pointers **/
    buff_pntr = curr_pntr = 0;

    /** reset the ring pointer **/
    current = NULL;
    num_proc++;
    return;
}

static int
convert_buffer(char *target, int num) {
    int i, j;
    char c;

    /*
     * Until I get something wierd, just keep copying
     */
    for (i = 0, j = 0; i < num; i++, j++) {
        switch (c = get_buff(i)) {
          case _CARROT:
            if (get_flag(i) == 1) {
                target[j] = c;
            } else {
                int c1 = get_buff(i + 1);
                if (c1 == _LBRACK) {
                    target[j] = _ESC;
                    i++;
                } else if (c1 >= 'A' && c1 <= 'Z') {
                    target[j] = alpha_to_control(c1);
                    i++;
                }
            }
            break;
          case '?':
          default:
            target[j] = c;
        }
    }
    return j;
}

static void
overwrite_buff_part(int amount) {
    /* Count inserted characters */
    int nn = 0, count = 0;
    while (count < amount) {
        count += dist_right(curr_pntr + count);
        nn++;
    }
    {
        /* Delete overwritten characters */
        int pp = curr_pntr + amount;
        count = 0;
        while(nn > 0 && pp + count < buff_pntr) {
            count += dist_right(pp + count);
            nn--;
        }
        shift_buff_backward(pp + count, buff_pntr, -count);
        buff_pntr -= count;
    }
}

static void
insert_buff_printing(int amount) {

    /* This procedure takes the character at in_buff[num_proc] and adds
       it to the buffer. It first checks to see if we should be inserting
       or overwriting, and then does the appropriate thing     */

    if ((buff_pntr + amount) > 1023) {
        putchar(_BELL);
        fflush(stdout);
        num_proc += amount;
        return;
    }
    shift_buff_forward(curr_pntr, buff_pntr, amount);
    store_buff_string(curr_pntr, amount, &(in_buff[num_proc]), 1);
    if (INS_MODE || curr_pntr == buff_pntr) {
        ins_print(curr_pntr, amount);
        buff_pntr = buff_pntr + amount;
    } else {
        buff_pntr = buff_pntr + amount;
        overwrite_buff_part(amount);
        /* Print inserted characters */
        printbuff(curr_pntr, amount);
    }
    num_proc = num_proc + amount;
    curr_pntr = curr_pntr + amount;
    fflush(stdout);
}

static void
insert_buff_nonprinting(int amount) {

    /* This procedure takes the character at in_buff[num_proc] and adds
       it to the buffer. It first checks to see if we should be inserting
       or overwriting, and then does the appropriate thing */

    /* it takes care of the special case, when I have an esc character */

    if ((buff_pntr + amount + 1) > 1023) {
        myputchar(_BELL);
        fflush(stdout);
        num_proc += amount;
        return;
    }
    shift_buff_forward(curr_pntr, buff_pntr, amount + 1);
    /** now insert the special character **/
    switch (in_buff[num_proc]) {
      case _ESC:
        /** in this case I insert a '^[' into the string ***/
        store_buff_char(curr_pntr, _CARROT, 2);
        store_buff_char(curr_pntr + 1, _LBRACK, 0);
        break;
      default:
        if (control_char(in_buff[num_proc])) {
            store_buff_char(curr_pntr, _CARROT, 2);
            store_buff_char(curr_pntr + 1,
                            control_to_alpha(in_buff[num_proc]), 0);
        } else {
            /* Hmm, try minimize breakage */
            store_buff_char(curr_pntr, '?', 2);
            store_buff_char(curr_pntr + 1, in_buff[num_proc], 0);
        }
        break;
    }
    /** Now add the normal characters **/
    store_buff_string(curr_pntr + 2, amount - 1, &(in_buff[num_proc + 1]), 1);
    if (INS_MODE || curr_pntr == buff_pntr) {
        ins_print(curr_pntr, amount + 1);
        buff_pntr = buff_pntr + amount + 1;
    } else {
        overwrite_buff_part(amount + 1);
        /* Print inserted characters */
        printbuff(curr_pntr, amount + 1);
    }
    num_proc = num_proc + amount;
    curr_pntr = curr_pntr + amount + 1;
    if (curr_pntr > buff_pntr) {
        buff_pntr = curr_pntr;
    }
}

static void switch_to_buff(QueStruct * current);

static void
prev_buff(void) {
    /*
     * If the current command ring is NULL, then I should NOT clear the
     * current line. Thus my business is already done
     */
    if (ring == NULL) {
        return;
    }
    if (current == NULL) {
        current = ring;
    } else {
        current = current->prev;
    }
    switch_to_buff(current);
}

static void
next_buff(void) {
    /*
     * If the current command ring is NULL, then I should NOT clear the
     * current line. Thus my business is already done
     */
    if (ring == NULL) {
        return;
    }
    if (current == NULL) {
        current = ring->next;
    } else {
        current = current->next;
    }
    switch_to_buff(current);
}

static void
switch_to_buff(QueStruct * current) {
    clear_buff();

    restore_ring_to_buff(current);

    /* first  back up and blank the line */
    fflush(stdout);
    curr_pntr = buff_pntr = buff_length();
    printbuff(0, buff_pntr);
    fflush(stdout);
}


static void
insert_queue(void) {
    QueStruct *trace;
    QueStruct *new;
    int c;

    if (!buff_pntr) {
        return;
    }

    if (!ECHOIT) {
        return;
    }
    null_termiante_buff();
    if (ring != NULL && !buff_cmp(ring->buff)) {
        return;
    }
    for (c = 0, trace = ring; trace != NULL && c < (prev_check - 1);
           c++, trace = trace->prev) {
        if (!buff_cmp(trace->buff)) {
            /*
             * throw this puppy at the end of the ring
             */
            trace->next->prev = trace->prev;
            trace->prev->next = trace->next;
            trace->prev = ring;
            trace->next = ring->next;
            ring->next = trace;
            trace->next->prev = trace;
            ring = trace;
            return;
        }
    }

    /*
     * simply places the buff command into the front of the queue
     */
    if (ring_size < MAXRING) {
        new = (QueStruct *) malloc(sizeof(struct que_struct));
        if (new == NULL) {
            fprintf(stderr, "Malloc Error: Ran out of memory\n");
            exit(-1);
        }
        if (ring_size == 0) {
            ring = new;
            ring->prev = ring->next = new;
        } else {
            new->next = ring->next;
            new->prev = ring;
            ring->next = new;
            new->next->prev = new;
            ring = new;
        }
        ring_size++;
    } else {
        ring = ring->next;
    }
    store_buff_to_ring(ring, buff_pntr);
}


void
send_function_to_child(void) {
    /* Takes care of sending a line to the child, and resetting the
       buffer for new input                                */

    back_up(curr_pntr);
    /** start by putting the line into the command line ring ***/
    insert_queue();

    /** finish the line and send it to the child **/
    store_final_buff_char(_EOLN);

    write_buff(contNum);

    /** reinitialize the buffer  ***/
    re_init_buff();

    /**  reinitialize my buffer pointers **/
    buff_pntr = curr_pntr = 0;

    /** reset the ring pointer **/
    current = NULL;

    num_proc++;
    return;
}
