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

/******************************************************************************
 *
 * dialog.c:
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"

#include "keyin.h"
#include "hyper.h"

#include <X11/keysym.h>

#define min(x,y)     ( (x<y)?(x):(y))

#include "all_hyper_proto.H1"

static void clear_cursor(InputItem * sym);
static void draw_cursor(InputItem * sym);

static void
redraw_win()
{
    XUnmapSubwindows(gXDisplay, gWindow->fMainWindow);
    XUnmapSubwindows(gXDisplay, gWindow->fScrollWindow);
    XFlush(gXDisplay);
    show_page(gWindow->page);
}

static char *
mystrncpy(char *buff1, char *buff2, int n)
{
    /*
     * copies the characters from buff1 to buff2 starting at position buff2 +
     * n and buff1 + n
     */

    int i;

    for (i = n - 1; i >= 0; i--)
        *(buff1 + i) = *(buff2 + i);
    return buff2;
}

static void
inc_line_numbers(LineStruct *line)
{
    for (; line != NULL; line = line->next)
        line->line_number++;
}

static void
dec_line_numbers(LineStruct *line)
{
    for (; line != NULL; line = line->next)
        line->line_number--;
    return;
}

static void
decrease_line_numbers(LineStruct *line, int am)
{
    for (; line != NULL; line = line->next)
        line->line_number -= am;
}

static void
overwrite_buffer(char *buffer, InputItem *item)
{
    LineStruct *newline;
    LineStruct *addline = item->curr_line;
    /*int bufflen = strlen(buffer);*/
    int nl = 0;
    int cursor_y;
    int size = item->size;

    /* add a single character */

    cursor_y = (addline->line_number - 1) * line_height;
    if (addline->buff_pntr == size) {
        clear_cursor(item);
        if (addline->len <= size) {
            nl = 1;
            addline->buffer[size] = '_';
            addline->buffer[size + 1] = 0;
            addline->len = size + 1;
            newline = (LineStruct *) alloc_inputline(size + 2);
            newline->line_number = addline->line_number + 1;
            inc_line_numbers(addline->next);
            newline->next = addline->next;
            newline->prev = addline;
            if (addline->next)
                addline->next->prev = newline;
            addline->next = newline;
            item->num_lines++;
            cursor_y += line_height;
            item->curr_line = addline = newline;
        }
        else {
            item->curr_line = addline = addline->next;
        }
        addline->len = 1;
        addline->buff_pntr = 1;
        addline->buffer[0] = buffer[0];
    }
    else {
        addline->buffer[addline->buff_pntr] = buffer[0];
        clear_cursor(item);
        if (++addline->buff_pntr > addline->len)
            addline->len++;
    }

    /* now set up the current line */
    if (item->curr_line->buff_pntr >= item->size &&
        item->curr_line->next != NULL && !item->curr_line->next->len) {
        /* I should actually be on the next line */
        item->curr_line->buffer[item->size] = '_';
        item->curr_line->len = item->size + 1;
        XDrawString(gXDisplay, item->win, gWindow->fInputGC, start_x,
                    cursor_y + start_y,
                    addline->buffer,
                    addline->len);
        item->curr_line = item->curr_line->next;
        item->curr_line->buff_pntr = 0;
        item->curr_line->changed = 1;
    }

    if (!nl) {
        XDrawString(gXDisplay, item->win, gWindow->fInputGC, start_x,
                    cursor_y + start_y,
                    addline->buffer,
                    addline->len);
        draw_cursor(item);
    }
    else
        redraw_win();
}

/*
 * This routine takes the current line and moves it num forward. The
 * only way I have to move any other lines forward is if this line has length
 * > size
 */

static int
move_sym_forward(LineStruct *line, int num, int size, InputItem *sym)
{
    LineStruct *newline;
    int diff;
    int nl = 0;

    if (line->len > size) {
        nl = move_sym_forward(line->next, num, size, sym);
        strncpy(line->next->buffer,
                &line->buffer[sym->size - num], line->len);
        strncpy(&line->buffer[num],
                line->buffer, num);
        line->changed = 1;
        return nl;
    }
    else {
        if (line->len + num > size) {
            diff = line->len + num - size;
            newline = alloc_inputline(size);
            newline->len = diff;
            newline->line_number = line->line_number++;
            inc_line_numbers(line->next);
            sym->num_lines++;
            newline->next = line->next;
            newline->prev = line;
            if (line->next)
                line->next->prev = newline;
            line->next = newline;
            strncpy(newline->buffer, &line->buffer[size - diff], diff);
            strncpy(&line->buffer[num], line->buffer, num);
            line->buffer[size] = '_';
            line->buffer[size + 1] = 0;
            line->len = size + 1;
            return 1;
        }
        else {
            strncpy(&line->buffer[num], line->buffer, line->len);
            line->len += num;
            line->changed = 1;
            return 0;
        }
    }
}

static void
clear_cursorline(InputItem *sym)
{
    XCharStruct extents;
    int dir, asc, des;
    int cursor_y;

    XTextExtents(gInputFont, sym->curr_line->buffer,
                 sym->curr_line->buff_pntr,
                 &dir, &asc, &des, &extents);
    cursor_y = (sym->curr_line->line_number - 1) * line_height;
    sym->cursor_x = start_x + extents.width;
    XClearArea(gXDisplay, sym->win, sym->cursor_x, cursor_y,
               gWindow->width, line_height, False);
    XDrawString(gXDisplay, sym->win, gWindow->fInputGC, start_x, cursor_y + start_y,
                sym->curr_line->buffer,
                sym->curr_line->len);
}

static void
insert_buffer(char *buffer, InputItem *sym)
{
    /*int num = strlen(buffer);*/
    LineStruct *line = sym->curr_line;
    LineStruct *newline;
    int nl = 0;
    int size = sym->size;

    if (line->len < size) {
        /* they will all fit where I am so just copy them forward */
        line->len++;
        mystrncpy(&(line->buffer[line->buff_pntr + 1]),
                  &(line->buffer[line->buff_pntr]),
                  line->len - line->buff_pntr + 1);
        line->buffer[line->buff_pntr] = buffer[0];
        clear_cursorline(sym);
        line->buff_pntr++;
        draw_cursor(sym);
        return;
    }

    if (line->len > sym->size) {
        nl = move_sym_forward(line->next, 1, size, sym);
        if (line->buff_pntr > size) {
            line->changed = 1;
            line = line->next;
            line->buffer[0] = buffer[0];
            line->len++;
            line->buff_pntr = 1;
            line->changed = 1;
        }
        else {
            line->next->buffer[0] = line->buffer[size - 1];
            line->changed = 1;
            strncpy(&line->buffer[line->buff_pntr + 1],
                &line->buffer[line->buff_pntr], size - line->buff_pntr - 1);
            line->buffer[line->buff_pntr++] = buffer[0];
            line->changed = 1;
            if (line->buff_pntr >= size) {
                sym->curr_line = line->next;
                sym->curr_line->buff_pntr = 0;
            }
        }
    }
    else {
        nl = 1;
        newline = alloc_inputline(size);
        newline->line_number = line->line_number + 1;
        inc_line_numbers(line->next);
        sym->num_lines++;
        newline->next = line->next;
        newline->prev = line;
        if (line->next)
            line->next->prev = newline;
        line->next = newline;

        /*
         * was line->buff_pntr++;
         */
        if (line->buff_pntr >= size) {
            /* we are the leaders of the line */
            newline->buff_pntr = 1;
            newline->buffer[0] = buffer[0];
            newline->len = 1;
            sym->curr_line = newline;
        }
        else {
            /* we are not the leaders */
            newline->buffer[0] = line->buffer[size - 1];
            newline->len = 1;
            strncpy(&line->buffer[line->buff_pntr + 1],
                    &line->buffer[line->buff_pntr], size - line->buff_pntr);
            if (line->buff_pntr < size - 1) {
                line->buffer[line->buff_pntr++] = buffer[0];
            }
            else {
                line->buffer[line->buff_pntr] = buffer[0];
                newline->buff_pntr = 0;
                sym->curr_line = newline;
            }

        }
        line->buffer[size] = '_';
        line->buffer[size + 1] = 0;
        line->len = size + 1;
    }
    if (nl)
        redraw_win();
    else
        update_inputsymbol(sym);

}

void
add_buffer_to_sym(char *buffer,InputItem *sym)
{
    if (gInInsertMode)
        insert_buffer(buffer, sym);
    else
        overwrite_buffer(buffer, sym);
}

void
draw_inputsymbol(InputItem *sym)
{
    int y_spot = start_y;
    LineStruct *cline;
    XCharStruct extents;
    int dir, asc, des;


#if 0
    int cursor_y;
    cursor_y = (sym->curr_line->line_number - 1) * line_height;
#endif

    XClearWindow(gXDisplay, sym->win);

    XTextExtents(gInputFont, sym->curr_line->buffer,
                 sym->curr_line->buff_pntr,
                 &dir, &asc, &des, &extents);
    sym->cursor_x = start_x + extents.width;

    /*
     * While the list of input strings is not NULL, I should just keep
     * drawing them
     */
    for (cline = sym->lines; cline != NULL;
         cline = cline->next, y_spot += line_height) {
        /* Now I should draw the initial string ** */
        cline->changed = 0;
        XDrawString(gXDisplay, sym->win, gWindow->fInputGC, start_x, y_spot,
                    cline->buffer,
                    cline->len);

    }
    if (gWindow->page->current_item == sym)
        draw_cursor(sym);
}

void
update_inputsymbol(InputItem *sym)
{
    int y_spot = start_y;
    LineStruct *cline;
    XCharStruct extents;
    int dir, asc, des;
    /*int cleared = 0;*/
    int clear_y;
    int clear_width;
    int clear_height;

#if 0
    int cursor_y;
    cursor_y = (sym->curr_line->line_number - 1) * line_height;
#endif

    clear_width = (sym->size + 1) * gInputFont->max_bounds.width + 10;
    clear_height = line_height;
    clear_y = 0;


    XTextExtents(gInputFont, sym->curr_line->buffer,
                 sym->curr_line->buff_pntr,
                 &dir, &asc, &des, &extents);
    sym->cursor_x = start_x + extents.width;

    /*
     * While the list of input strings is not NULL, I should just keep
     * drawing them
     */
    for (cline = sym->lines; cline != NULL;
         cline = cline->next, y_spot += line_height, clear_y += line_height)
        /* Now I should draw the initial string ** */
        if (cline->changed) {
            cline->changed = 0;
            XClearArea(gXDisplay, sym->win, 0, clear_y,
                       clear_width, clear_height, False);
            XDrawString(gXDisplay, sym->win, gWindow->fInputGC, start_x, y_spot,
                        cline->buffer,
                        cline->len);
        }
    draw_cursor(sym);
}


static void
draw_cursor(InputItem *sym)
{
    int cursor_y;
    XCharStruct extents;
    int dir, asc, des;


    cursor_y = (sym->curr_line->line_number - 1) * line_height;
    XTextExtents(gInputFont, sym->curr_line->buffer,
                 sym->curr_line->buff_pntr,
                 &dir, &asc, &des, &extents);
    sym->cursor_x = start_x + extents.width;
    /* now draw the cursor */
    if (gInInsertMode) {
        XFillRectangle(gXDisplay, sym->win, gWindow->fInputGC,
                       sym->cursor_x,
                       out_cursor_y + cursor_y,
                       out_cursor_width,
                       out_cursor_height);

        /* Now draw the character currently under the cursor */

        XDrawString(gXDisplay, sym->win, gWindow->fCursorGC,
                    sym->cursor_x, cursor_y + start_y,
                    &sym->curr_line->buffer[sym->curr_line->buff_pntr],
                    1);
    }
    else
        XFillRectangle(gXDisplay, sym->win, gWindow->fInputGC,
                       sym->cursor_x,
                       in_cursor_y + cursor_y,
                       in_cursor_width,
                       in_cursor_height);
}

static void
move_cursor_home(InputItem *sym)
{
    LineStruct *trace = sym->curr_line;

    /* now move the cursor  to the beginning of the current line */
    clear_cursor(sym);
    for (; trace && trace->prev && trace->prev->len > sym->size;)
        trace = trace->prev;
    sym->curr_line = trace;
    trace->buff_pntr = 0;
    draw_cursor(sym);
}

static void
move_cursor_end(InputItem *sym)
{
    LineStruct *trace = sym->curr_line;

    /* now move the cursor  to the beginning of the current line */
    clear_cursor(sym);
    for (; trace && trace->next && trace->len > sym->size;)
        trace = trace->next;
    sym->curr_line = trace;
    trace->buff_pntr = trace->len;
    draw_cursor(sym);
}

static void
move_cursor_forward(InputItem *sym)
{
    if (sym->curr_line->buff_pntr == sym->curr_line->len &&
        !sym->curr_line->next) {
        BeepAtTheUser();
        return;
    }


    if (sym->curr_line->buff_pntr == sym->curr_line->len ||
        sym->curr_line->buff_pntr == sym->size - 1)
    {

        /* I have to move down to a new line */

        if (sym->curr_line->next == NULL) {
            /* now where to move */
            BeepAtTheUser();
            return;
        }

        /* move down line */

        clear_cursor(sym);
        sym->curr_line = sym->curr_line->next;
        sym->curr_line->buff_pntr = 0;
    }
    else {
        clear_cursor(sym);
        sym->curr_line->buff_pntr++;
    }

    draw_cursor(sym);
}

static void
move_cursor_down(InputItem *sym)
{
    int bp = sym->curr_line->buff_pntr;
    /*int size = sym->size;*/
    LineStruct *trace;

    /* get to the end of the current line */

    for (trace = sym->curr_line; trace->len > sym->size; trace = trace->next)
        ;

    if (!trace->next)
        BeepAtTheUser();
    else {
        clear_cursor(sym);
        sym->curr_line = trace->next;
        if (bp > sym->curr_line->len)
            sym->curr_line->buff_pntr = sym->curr_line->len;
        else
            sym->curr_line->buff_pntr = bp;
        draw_cursor(sym);
    }
}

static void
move_cursor_up(InputItem *sym)
{
    int bp = sym->curr_line->buff_pntr;
    /*int size = sym->size;*/
    LineStruct *trace;

    /* get to the end of the current line */
    for (trace = sym->curr_line;
         trace->prev && trace->prev->len > sym->size;
         trace = trace->prev)
            ;

    if (!trace->prev)
        BeepAtTheUser();
    else {
        clear_cursor(sym);
        sym->curr_line = trace->prev;
        if (bp > sym->curr_line->len)
            sym->curr_line->buff_pntr = sym->curr_line->len;
        else
            sym->curr_line->buff_pntr = bp;
        draw_cursor(sym);
    }
}

static void
clear_cursor(InputItem *sym)
{
    XCharStruct extents;
    int dir, asc, des;
    int cursor_y;

    XTextExtents(gInputFont, sym->curr_line->buffer,
                 sym->curr_line->buff_pntr,
                 &dir, &asc, &des, &extents);
    cursor_y = (sym->curr_line->line_number - 1) * line_height;
    sym->cursor_x = start_x + extents.width;
    XClearArea(gXDisplay, sym->win, sym->cursor_x, cursor_y,
               in_cursor_width, line_height, False);

    XDrawString(gXDisplay, sym->win, gWindow->fInputGC,
                start_x, cursor_y + start_y,
                sym->curr_line->buffer,
                sym->curr_line->len);
}

static void
move_cursor_backward(InputItem *sym)
{
    if (sym->curr_line->buff_pntr == 0) {
        if (sym->curr_line->prev == NULL) {
            /* now where to move */
            BeepAtTheUser();
            return;
        }
        else {
            clear_cursor(sym);
            /* move up to the previous line */
            sym->curr_line = sym->curr_line->prev;
            if (sym->curr_line->len > sym->size)
                sym->curr_line->buff_pntr = sym->size - 1;
            else
                sym->curr_line->buff_pntr = sym->curr_line->len;
        }
    }
    else {                      /* just slide back a char. on the current
                                 * line */
        clear_cursor(sym);
        sym->curr_line->buff_pntr--;
    }
    draw_cursor(sym);
}

static char
move_rest_back(LineStruct *line, int size)
{
    char c = '\000';

    if (line != NULL && line->len != 0)
        c = line->buffer[0];
    else
        return c;

    while (line->next != NULL && line->len > size) {
        strncpy(line->buffer, &(line->buffer[1]), size - 1);
        line->buffer[size - 1] = line->next->buffer[0];
        line->changed = 1;
        line = line->next;
    }

    /*
     * once I get here I should be one the last line, so I can just copy all
     * the characters back one and then return from whence I came                                                  ***
     */
    if (line->len > 0) {
        line->changed = 1;
        if (line->len > 1)
            strncpy(line->buffer, &(line->buffer[1]), line->len - 1);
        line->buffer[--line->len] = 0;
        if (line->len == 0) {
            /* I have to fix the previous line */
            line->prev->len = size;
            line->prev->buffer[size] = 0;
        }
    }
    return c;
}

static void
delete_rest_of_line(InputItem *sym)
{
    LineStruct *curr_line = sym->curr_line;
    LineStruct *line=NULL;
    LineStruct *trash;
    LineStruct *trace;
    int num_changed = 0, i;

    if (curr_line->len > sym->size) {
        for (line = curr_line->next, num_changed = 0;
             line != NULL && line->len > 0 && line->len > sym->size;
             line = line->next, num_changed++) {
            line->len = 0;
            line->buffer[0] = 0;
            line->changed = 1;
        }
        num_changed++;
    }

    if (num_changed == 0 && curr_line->buff_pntr == curr_line->len) {
        if (curr_line->len == 0 && curr_line->next) {
            curr_line->next->prev = curr_line->prev;
            if (curr_line->prev)
                curr_line->prev->next = curr_line->next;
            else
                sym->lines = curr_line->next;
            dec_line_numbers(curr_line->next);
            sym->num_lines--;
            sym->curr_line = curr_line->next;
            sym->curr_line->buff_pntr = 0;
            free(curr_line->buffer);
            free(curr_line);
            redraw_win();
        }
        else
            BeepAtTheUser();
        return;
    }

    curr_line->len = curr_line->buff_pntr;

    /* curr_line->buffer[curr_line->len] = NULL; */

    for (i = curr_line->len; i <= sym->size + 2; i++)
        curr_line->buffer[i] = 0;

    curr_line->changed = 1;

    if (num_changed) {
        /* I should get rid of all these lines */
        trace = curr_line->next;
        curr_line->next = line->next;
        if (line->next)
            line->next->prev = curr_line;
        for (; trace && trace != line->next;) {
            trash = trace;
            trace = trace->next;
            free(trash->buffer);
            free(trash);
        }
        decrease_line_numbers(curr_line->next, num_changed);
        sym->num_lines -= num_changed;
        redraw_win();
    }
    else
        update_inputsymbol(sym);
}

static void
back_over_eoln(InputItem *sym)
{
    /*
     * This routine is very similar to a tough enter except it starts
     * combining lines with sym->curr_line->pre
     */

    char buff[1024];
    LineStruct *trace;
    LineStruct *last = NULL;
    char *tr = buff;
    int bp;
    int size = sym->size;

    /* copy all the stuff into the buffer */
    for (trace = sym->curr_line;
         trace->len > sym->size; trace = trace->next)
        for (bp = 0; bp < size; bp++)
            *tr++ = trace->buffer[bp];

    /* copy the last line */
    for (bp = 0; bp < trace->len; bp++)
        *tr++ = trace->buffer[bp];
    trace->len = 0;
    *tr = 0;

    /* Now that I have the buffer, let's put it back where it belongs. */
    last = trace;
    for (trace = sym->curr_line; trace != last; trace = trace->next);
    trace = sym->curr_line = sym->curr_line->prev;
    trace->buff_pntr = trace->len;
    trace->changed = 1;
    for (bp = trace->len, tr = buff; bp < size && *tr; bp++)
        trace->buffer[bp] = *tr++;

    if (!*tr) {
        trace->len = bp;
    }
    else {
        trace->len = size + 1;
        trace->buffer[size] = '_';
        trace->buffer[size + 1] = 0;
        for (trace = trace->next; *tr;) {
            for (bp = 0; bp < size && *tr; bp++)
                trace->buffer[bp] = *tr++;
            if (*tr) {
                trace->len = size + 1;
                trace->changed = 1;
                trace->buffer[size + 1] = 0;
                trace->buffer[size] = '_';
                trace = trace->next;
            }
            else {
                trace->len = bp;
                trace->buffer[bp] = 0;
            }
        }
    }
    /* Now once I am here, let me see if I can bag a line */
    if (last->len == 0) {
        /* rid myself of this line */
        last->prev->next = last->next;
        if (last->next)
            last->next->prev = last->prev;
        dec_line_numbers(last->next);
        sym->num_lines--;
        free(last->buffer);
        free(last);
        redraw_win();
    }
    else
        update_inputsymbol(sym);

}

static int
move_back_one_char(InputItem *sym)
{
    char c = '\000', d = '\000';
    int dl = 0;

    /* This routine moves all the characters back one */
    LineStruct *line = sym->curr_line;

    if (line->len > sym->size)
        c = move_rest_back(line->next, sym->size);

    line->changed = 1;

    if (line->buff_pntr == 0) { /* I am at the front of the line */
        if (line->prev == 0) {
            BeepAtTheUser();
            return 0;
        }
        else if (line->prev->len <= sym->size) {
            back_over_eoln(sym);
            return 1;
        }
        else if (line->len > 0) {
            d = line->buffer[0];
            if (line->len <= sym->size) {
                strncpy(line->buffer, &(line->buffer[1]), line->len - 1);
                if (c == 0) {
                    line->len--;
                    line->buffer[line->len] = 0;
                }
                else
                    line->buffer[line->len - 1] = c;
            }
            else {
                strncpy(line->buffer, &(line->buffer[1]), sym->size - 2);
                if (c == 0) {
                    line->buffer[sym->size - 1] = 0;
                    line->len--;
                }
                else {
                    line->buffer[sym->size - 1] = c;
                }
            }
        }
        else {
            /* the line is just going to be thrown away */
            if (line->next)
                line->next->prev = line->prev;
            line->prev->next = line->next;
            dec_line_numbers(line->next);
            sym->num_lines--;
            free(line->buffer);
            free(line);
            dl = 1;
        }
        c = d;
        sym->curr_line = line = line->prev;
        line->changed = 1;
        line->buff_pntr = sym->size;
    }


    if (line->len <= sym->size) {
        strncpy(&line->buffer[line->buff_pntr - 1],
                &(line->buffer[line->buff_pntr]),
                line->len - line->buff_pntr);
        if (c == 0)
            line->buffer[--line->len] = 0;
        else
            line->buffer[line->len - 1] = c;
    }
    else {
        strncpy(&(line->buffer[line->buff_pntr - 1]),
                &(line->buffer[line->buff_pntr]),
                sym->size - line->buff_pntr);
        if (c == 0) {
            line->buffer[sym->size - 1] = 0;
            line->len = sym->size - 1;
        }
        else {
            if (line->next->len == 0) {
                line->buffer[sym->size] = 0;
                line->len = sym->size;
            }
            line->buffer[sym->size - 1] = c;
        }
    }
    line->buff_pntr--;
    if (dl)
        redraw_win();
    else
        update_inputsymbol(sym);
    return 1;
}

static void
back_over_char(InputItem *sym)
{
    if (move_back_one_char(sym))
        update_inputsymbol(sym);
}

static void
delete_eoln(InputItem *sym)
{
    /* much the same as back_over eoln except my perspective has changed */
    char buff[1024];
    LineStruct *trace;
    LineStruct *last = 0;
    char *tr = buff;
    int bp;
    int size = sym->size;

    /* copy all the stuff into the buffer */
    for (trace = sym->curr_line->next;
         trace->len > sym->size; trace = trace->next)
        for (bp = 0; bp < size; bp++)
            *tr++ = trace->buffer[bp];

    /* copy the last line */
    for (bp = 0; bp < trace->len; bp++)
        *tr++ = trace->buffer[bp];
    trace->len = 0;
    *tr = 0;

    /* Now that I have the buffer, let's put it back where it belongs. */
    last = trace;
    trace = sym->curr_line;
    trace->changed = 1;
    for (bp = trace->len, tr = buff; bp < size && *tr; bp++)
        trace->buffer[bp] = *tr++;

    if (!*tr)
        trace->len = bp;
    else {
        trace->len = size + 1;
        trace->buffer[size] = '_';
        trace->buffer[size + 1] = 0;
        for (trace = trace->next; *tr;) {
            for (bp = 0; bp < size && *tr; bp++)
                trace->buffer[bp] = *tr++;
            if (*tr) {
                trace->len = size + 1;
                trace->changed = 1;
                trace->buffer[size + 1] = 0;
                trace->buffer[size] = '_';
                trace = trace->next;
            }
            else {
                trace->len = bp;
                trace->buffer[bp] = 0;
            }
        }
    }
    /* Now once I am here, let me see if I can bag a line */
    if (last->len == 0) {
        /* rid myself of this line */
        last->prev->next = last->next;
        if (last->next)
            last->next->prev = last->prev;
        dec_line_numbers(last->next);
        sym->num_lines--;
        free(last->buffer);
        free(last);
        redraw_win();
    }
    else
        update_inputsymbol(sym);

}

static int
delete_one_char(InputItem *sym)
{
    char c = '\000';

    /* This routine moves all the characters back one */
    LineStruct *line = sym->curr_line;

    if (line->len > sym->size)
        c = move_rest_back(line->next, sym->size);

    if (c == 0 && line->len == line->buff_pntr) {
        if (line->next == 0) {
            BeepAtTheUser();
            return 0;
        }
        else {
            delete_eoln(sym);
            return 1;
        }
    }

    /*
     * let me just try to do the copy and put the stupid character c if it
     * exists at the end
     */
    if (line->len <= sym->size) {
        strncpy(&line->buffer[line->buff_pntr],
                &(line->buffer[line->buff_pntr + 1]),
                line->len - line->buff_pntr);
        if (c == 0)
            line->buffer[--line->len] = 0;
        else
            line->buffer[line->len - 1] = c;
    }
    else {
        strncpy(&(line->buffer[line->buff_pntr]),
                &(line->buffer[line->buff_pntr + 1]),
                sym->size - line->buff_pntr);
        if (c == 0) {
            line->buffer[sym->size - 1] = 0;
            line->len = sym->size - 1;
        }
        else {
            if (line->next->len == 0) {
                line->buffer[sym->size] = 0;
                line->len = sym->size;
            }
            line->buffer[sym->size - 1] = c;
        }
    }
    line->changed = 1;
    return 1;
}

static void
delete_char(InputItem *sym)
{
    if (delete_one_char(sym))
        update_inputsymbol(sym);
}

static void
tough_enter(InputItem *sym)
{
    char buff[1024];

    /*
     * This routine takes all the characters from the current cursor
     * on, and copies them into a temp buffer, from which they are recopied
     * back starting at the next line.
     */

    LineStruct *trace;
    LineStruct *last = 0;
    LineStruct *newline;
    char *tr = buff;
    int bp = sym->curr_line->buff_pntr;
    int size = sym->size;

    /* Copy the stuff from the current line */
    for (; bp < size; bp++)
        *tr++ = sym->curr_line->buffer[bp];

    /* now get the stuff from the rest of the lines */
    for (trace = sym->curr_line->next;
         trace->len > sym->size; trace = trace->next)
        for (bp = 0; bp < size; bp++)
            *tr++ = trace->buffer[bp];

    /* copy the last line */
    for (bp = 0; bp < trace->len; bp++)
        *tr++ = trace->buffer[bp];
    *tr = 0;

    /* Now that I have the buffer, let's put it back where it belongs. */
    last = trace;
    trace = sym->curr_line;
    trace->len = trace->buff_pntr;
    trace->buffer[trace->len] = 0;
    trace->changed = 1;

    tr = buff;
    for (trace = trace->next; trace != last; trace = trace->next) {
        for (bp = 0; bp < size; bp++)
            trace->buffer[bp] = *tr++;
        trace->len = size + 1;
        trace->buffer[size + 1] = 0;
        trace->buffer[size] = '_';
        trace->changed = 1;
    }

    /* Once I am here, I should be able to copy this last line */
    for (bp = 0; bp < size && *tr; bp++)
        trace->buffer[bp] = *tr++;
    trace->changed = 1;

    /* If I still have more to copy, then do so onto a new line */
    if (*tr) {
        trace->len = size + 1;
        trace->buffer[size + 1] = 0;
        trace->buffer[size] = '_';
        newline = alloc_inputline(size);
        sym->num_lines++;
        newline->line_number = last->line_number + 1;
        inc_line_numbers(newline->next);
        for (bp = 0; *tr; bp++)
            newline->buffer[bp] = *tr++;
        newline->len = bp;
        newline->next = last->next;
        newline->prev = last;
        last->next = newline;
        if (newline->next)
            newline->next->prev = newline;
    }
    else {
        trace->len = bp;
        trace->buffer[bp] = 0;
    }
    /* Last but not least change the curr_line */
    sym->curr_line = sym->curr_line->next;
    sym->curr_line->buff_pntr = 0;
}

static void
enter_new_line(InputItem *sym)
{
    LineStruct *newline;
    LineStruct *trace;
    LineStruct *prev;
    LineStruct *line = sym->curr_line;
    int bp = line->buff_pntr;
    int l = line->len;
    int size = sym->size;

    /*
     * At this point the user has hit a return. Let me just be naive, and
     * take everything from the current spot on, and put it on a new line
     */

    if (bp == 0) {
        if (line->prev->len > size) {
            /* just add a return to the end of the last line */
            prev = line->prev;
            prev->buffer[size] = 0;
            prev->len = size;
            prev->changed = 1;
        }
        else {
            newline = alloc_inputline(size);
            newline->next = sym->curr_line;
            newline->prev = sym->curr_line->prev;
            line->prev = newline;
            sym->num_lines++;
            if (newline->prev)
                newline->prev->next = newline;
            newline->len = newline->buff_pntr = 0;
            newline->line_number = line->line_number;
            if (sym->curr_line == sym->lines)
                sym->lines = newline;
            for (trace = newline->next; trace != 0; trace = trace->next)
                trace->line_number++;
        }
    }
    else if (bp == size &&
             line->len > size) {
        /* line->next; */
        newline = alloc_inputline(size);
        if (line->next)
            line->next->prev = newline;
        newline->prev = sym->curr_line;
        line->next = newline;
        newline->len = 0;
        newline->buff_pntr = 0;
        sym->num_lines++;
        sym->curr_line = newline;
        newline->line_number = newline->prev->line_number + 1;
        for (trace = newline->next; trace != 0; trace = trace->next)
            trace->line_number++;
    }
    else {
        if (line->len > size)
            tough_enter(sym);
        else {
            newline = alloc_inputline(size);
            strncpy(newline->buffer, &sym->curr_line->buffer[bp], l - bp);
            sym->curr_line->len = bp;
            sym->curr_line->buffer[bp] = '\0';
            newline->next = sym->curr_line->next;
            if (sym->curr_line->next)
                sym->curr_line->next->prev = newline;
            newline->prev = sym->curr_line;
            sym->curr_line->next = newline;
            newline->len = l - bp;
            newline->buff_pntr = 0;
            sym->num_lines++;
            sym->curr_line = newline;
            newline->line_number = newline->prev->line_number + 1;
            for (trace = newline->next; trace != 0; trace = trace->next)
                trace->line_number++;
        }
    }
    redraw_win();
}

void
dialog(XEvent *event, KeySym keysym, char *buffer)
{
    InputItem *item;

    item = gWindow->page->current_item;
    if (item == 0) {
        if (!((keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R)))
            /** if something other than a modifier key was hit **/
            BeepAtTheUser();
        return;
    }


    /*
     * First check if the user had hit an enter key
     */

    if ((keysym == XK_Return) || (keysym == XK_KP_Enter))
        enter_new_line(item);
    /*
     * Else did the user actual type a character I can understand
     */

    else if (((keysym >= XK_KP_Space) && (keysym <= XK_KP_9))
             || ((keysym >= XK_space) && (keysym <= XK_asciitilde)))
    {
        /* only handle normal keys */

        if (event->xkey.state & UnsupportedModMask)
            BeepAtTheUser();
        else
            add_buffer_to_sym(buffer, item);
    }

    else if ((keysym >= XK_Shift_L) && (keysym <= XK_Hyper_R))
        ;

    /*
     * do nothing, a modifier was hit
     */

    else if ((keysym >= XK_F2) && (keysym <= XK_F35)) {

        /*
         * A function key was hit
         */

        if (strlen(buffer) == 0)
            BeepAtTheUser();
        else
            /* If I got characters then add it to the buffer */

            add_buffer_to_sym(buffer, item);
    }
    else
        switch (keysym) {
          case XK_Escape:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else {
                move_cursor_home(item);
                delete_rest_of_line(item);
            }
            break;
          case XK_F1:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else {
                gWindow->page->helppage = alloc_string(InputAreaHelpPage);
                helpForHyperDoc();
            }
            break;
          case XK_Up:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else
                move_cursor_up(item);
            break;
          case XK_Down:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else
                move_cursor_down(item);
            break;
          case XK_Delete:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else
                delete_char(item);
            break;
          case XK_BackSpace:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else
                back_over_char(item);
            break;
          case XK_Left:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else
                move_cursor_backward(item);
            break;
          case XK_Right:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else
                move_cursor_forward(item);
            break;
          case XK_Insert:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else {
                gInInsertMode = ((gInInsertMode) ? (0) : (1));
                item->curr_line->changed = 1;
                update_inputsymbol(item);
            }
            break;
          case XK_Home:
            if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else
                move_cursor_home(item);
            break;
          case XK_End:
            if (event->xkey.state & ControlMask)
                /* delete from here to the end of the line */

                delete_rest_of_line(item);
            else if (event->xkey.state & ModifiersMask)
                BeepAtTheUser();
            else
                move_cursor_end(item);
            break;
          default:
            BeepAtTheUser();
            break;
        }
}
