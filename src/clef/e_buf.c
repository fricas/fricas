/* Handling for clef edit buffer */

#include <unistd.h>
#include <string.h>

#include "e_buf.h"

#include "tio_funs.h"

int buff_pntr;      /*   present length of buff            */
int curr_pntr;      /*   the current position in buff      */

static char buff[1024];
static char buff_flag[1024];

int get_buff(int i) {
    return buff[i];
}

int get_flag(int i) {
    return buff_flag[i];
}

int
prt_char(int i) {
    int res = dist_right(i), j;
    for(j = 0; j < res; j++) {
        myputchar(buff[i + j]);
    }
    return res;
}

int
dist_left(int i) {
    unsigned char c = buff[i - 1];
    int ii = 1;
    while((ii < i) && (ii <= 4) && (c >= 128) && (c < 192)) {
        ii++;
        c = buff[i - ii];
    }
    if (dist_right(i - ii) == ii) {
        return ii;
    } else {
        return 1;
    }
}

int
dist_right(int i) {
    unsigned char c = buff[i];
    if (c < 0x80) {
        return 1;
    } else if ((c >= 192) && (c < 224)) {
        return 2;
    } else if ((c >= 224) && (c < 240)) {
        return 3;
    } else if ((c >= 240) && (c < 248)) {
        return 4;
    } else {
        /* Invalid UTF-8, ignore */
        return 1;
    }
}

void
re_init_buff(void) {
    buff[0] = 0;
    buff_flag[0] = -1;
}

int
store_buff_char(int pos, char c, int f) {
    buff[pos] = c;
    buff_flag[pos] = f;
    return pos;
}

void
store_buff_string(int pos, int cnt, char * cp, int f) {
    int i;
    for(i = 0; i < cnt; i++) {
        buff[pos + i] = cp[i];
        buff_flag[pos + i] = f;
    }
}

void
store_final_buff_char(char c) {
    buff[buff_pntr] = c;
    buff_flag[buff_pntr] = 1;
    buff_pntr++;
    buff[buff_pntr] = 0;
    buff_flag[buff_pntr] = -1;
}

void
shift_buff_forward(int cp, int bp, int diff) {
    while(cp < bp) {
        bp--;
        buff[bp + diff] = buff[bp];
        buff_flag[bp + diff] = buff_flag[bp];
    }
}

void
shift_buff_backward(int cp, int bp, int diff) {
    while(cp < bp) {
        buff[cp + diff] = buff[cp];
        buff_flag[cp + diff] = buff_flag[cp];
        cp++;
    }
}

void
shift_buff(int cp, int bp, int diff) {
    if (diff < 0) {
        shift_buff_backward(cp, bp, diff);
    } else if (diff > 0) {
        shift_buff_forward(cp, bp, diff);
    }
}

void
null_termiante_buff(void) {
    buff[buff_pntr] = 0;
}

int
buff_length(void) {
    return strlen(buff);
}

int
buff_cmp(char *s) {
    return strcmp(buff, s);
}

void
write_buff(int fd) {
    write(fd, buff, buff_pntr);
}

void
store_buff_to_ring(QueStruct * ring, int buff_pntr) {
    int i;
    char * rb = ring->buff;
    char * rf = ring->flags;
    for(i = 0; i < buff_pntr; i++) {
        rb[i] = buff[i];
        rf[i] = buff_flag[i];
    }
    rb[i] = 0;
    rf[i] = -1;
}

void
restore_ring_to_buff(QueStruct * ring) {
    int i;
    char * rb = ring->buff;
    char * rf = ring->flags;
    for(i = 0; rb[i]; i++) {
        buff[i] = rb[i];
        buff_flag[i] = rf[i];
    }
    buff[i] = 0;
    buff_flag[i] = -1;
}
