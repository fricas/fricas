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

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>

#include "edible.h"

#include "e_buf.h"

#include "tio_funs.h"
#include "strutil.h"


/** Some constants for functio key defs ****/
#define DELAYED 0
#define IMMEDIATE 1
#define SPECIAL   2


/**   Here is the structure for storing bound  pf-keys             ***/
fkey function_key[13];          /** Strings which replace function
                                    keys when a key is hit          ***/

static char *defaulteditor = "clefedit";
char editorfilename[100];


static int get_key(int, char *);
static int get_str(int, char *);

/*
 * The following function environment variable clef editor. The command
 * should be the one that the user wishes to have execed
 */

static void
set_editor_key(void)
{
    int pid;

    fricas_sprintf_to_buf1(editorfilename, "/tmp/clef%d", pid = getpid());

    if (function_key[12].str == NULL) {
        (function_key[12]).type = SPECIAL;
        (function_key[12]).str = defaulteditor;
    }
}




/***  This routine id used to find the users function key mappings. It
    simply searches the users HOME directory for a file called ".clef".
    If found it gets the key bindings from within
    *****/

void
define_function_keys(void) {
    char *HOME, path[1024], string[1024];
    int key;
    int fd;
    char type;

    /* initialize the key pointers */
    for (key = 0; key < 13; key++) {
        (function_key[key]).str = NULL;
    }
    /** see if the user has a .clef file       ***/
    HOME = getenv("HOME");
    sprintf(path, "%s/.clef", HOME);
    if ((fd = open(path, O_RDONLY)) == -1) {
        return;
    } else {
        /*** If so, then get the key bindings **/
        while ((key = get_key(fd, &type))) {
            get_str(fd, string);
            switch (type) {
              case 'D':
                if (key == 12) {
                    fprintf(stderr,
                       "Clef Error: PF12 can only be of type E in .clef\n");
                    fprintf(stderr, "Line will be ignored\n");
                    type = -1;
                } else {
                    (function_key[key]).type = DELAYED;
                }
                break;
              case 'F':
                if (key == 12) {
                    fprintf(stderr,
                       "Clef Error: PF12 can only be of type E in .clef\n");
                    fprintf(stderr, "Line will be ignored\n");
                    type = -1;
                } else {
                    (function_key[key]).type = IMMEDIATE;
                }
                break;
              case 'E':
                if (key != 12) {
                    fprintf(stderr,
                       "Clef Error: PF12 can only be of type E in .clef\n");
                    fprintf(stderr, "Line will be ignored\n");
                    type = -1;
                } else {
                    (function_key[key]).type = SPECIAL;
                }
                break;
            }
            if (type != -1) {
                (function_key[key]).str =
                    (char *) malloc(strlen(string) + 1);
                sprintf((function_key[key]).str, "%s", string);
            }
        }
    }

    /*
     * Now set the editor function key
     */
    set_editor_key();
}


#define defof(c) ((c == 'F' || c == 'D' || c == 'E')?(1):(0))

int
get_key(int fd,char * ty) {

    /*
     * Determines the key number being mapped, and whether it is immediate or
     * delay. It reurns the key value, and modifies the parameter type
     */
    char keynum[1024];
    int nr;

    nr = read(fd, keynum, 3);
    if (nr != -1 && nr != 0) {
        if (!defof(keynum[0])) {
            return 0;
        } else {
            *ty = keynum[0];
            keynum[3] = '\0';
            return (atoi(&keynum[1]));
        }
    } else {
        return 0;
    }
}

int
get_str(int fd,char * string) {
    /** Gets the key mapping being bound **/
    char c;
    int count = 0;
    char *trace = string;

    read(fd, &c, 1);
    while (c == ' ') {
        read(fd, &c, 1);
    }
    while (c != '\n') {
        count++;
        *trace++ = c;
        if (read(fd, &c, 1) == 0) {
            break;
        }
    }
    *trace = '\0';
    return count;
}

void
handle_function_key(int key) {
    /** this procedure simply adds the string specified by the function key
      to the buffer                                               ****/
    int count;
    int amount = strlen(function_key[key].str);

    /*** This procedure takes the character at in_buff[num_proc] and adds
      it to the buffer. It first checks to see if we should be inserting
      or overwriting, and then does the appropriate thing      *******/

    switch ((function_key[key]).type) {
      case IMMEDIATE:
        if (INS_MODE) {
            shift_buff_forward(curr_pntr, buff_pntr, amount);
            buff_pntr = buff_pntr + amount;
            store_buff_string(curr_pntr, amount, function_key[key].str, '1');
            ins_print(curr_pntr, amount + 1);
        } else {
            store_buff_string(curr_pntr, amount, function_key[key].str, '1');
            for (count = 0; count < amount; count++) {
                myputchar((function_key[key].str)[count]);
            }
        }
        num_proc = num_proc + 6;
        curr_pntr = curr_pntr + amount;
        send_function_to_child();
        break;
      case DELAYED:
        if (INS_MODE) {
            shift_buff_forward(curr_pntr, buff_pntr, amount);
            store_buff_string(curr_pntr, amount, function_key[key].str, '1');
            ins_print(curr_pntr, amount + 1);
            buff_pntr = buff_pntr + amount;
        } else {
            store_buff_string(curr_pntr, amount, function_key[key].str, '1');
            for (count = 0; count < amount; count++) {
                myputchar((function_key[key].str)[count]);
            }
        }
        num_proc = num_proc + 6;
        curr_pntr = curr_pntr + amount;
        fflush(stdout);
        break;
    }
}
