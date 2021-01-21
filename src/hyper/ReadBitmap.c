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

#include "all_hyper_proto.H1"
#include "pixmap.H1"

#define MAXLINE      256

static int read_hot(FILE * fd, char Line[], int * x_hot, int * y_hot);
static int read_w_and_h(FILE * fd, unsigned int * width,
                        unsigned int * height);

/*
 * This file was produced by J.M. Wiley with some help from the bitmap editor
 * routine. It reads in a bitmap file, and calls XCreatePixmapFromBitmapData
 * to transform it into a Pixmap. He did this because the routine
 * XReadBitmapFile does not seem to work to well (whatever that means).
 */

XImage *
HTReadBitmapFile(Display *display,int screen,char * filename,
                 int *width, int *height)
{
    XImage *image;
    FILE *fd;
    char Line[256], Buff[256];
    int num_chars;
    char *ptr;
    int rch;
    int version;
    int padding, chars_line, file_chars_line, file_chars;
    int bytes;
    int x_hot, y_hot;


    image = XCreateImage(display, DefaultVisual(display, screen), 1,
                         XYBitmap, 0, NULL, 0, 0, 8, 0);


    (image)->byte_order = LSBFirst;     /* byte_order    */
    (image)->bitmap_unit = 8;   /* bitmap-unit   */
    (image)->bitmap_bit_order = LSBFirst;       /* bitmap-bit-order */

    if (!(fd = zzopen(filename, "r"))) {
        fprintf(stderr, "ReadBitmapFile: File >%s< not found\n", filename);
        exit(-1);
    }

    /*
     * Once it is open, lets get the width and height
     */

    if ((read_w_and_h(fd, (unsigned int *)width,(unsigned int *) height)) < 0) {
        fprintf(stderr, "ReadBitmapFile: Bad file format in %s\n", filename);
        exit(-1);
    }


    /*
     * Now get the next line, and see if it is hot spots or bits
     */
    if (fgets(Line, MAXLINE, fd) == NULL) {
        fprintf(stderr, "ReadBitmapFile: Bad file format in %s\n", filename);
        exit(-1);
    }

    /*
     * Now check the first character to see if it is a # or an s
     */

    if (Line[0] == '#') {
        if ((read_hot(fd, Line, &x_hot, &y_hot)) < 0) {
            fprintf(stderr, "ReadBitmapFile: Bad file format in %s\n", filename);
            exit(-1);
        }
    }

    (image)->width = *width;
    (image)->height = *height;

    /*
     * figure out what version
     */

    if (sscanf(Line, "static short %s = {", Buff) == 1)
        version = 10;
    else if (sscanf(Line, "static unsigned char %s = {", Buff) == 1)
        version = 11;
    else if (sscanf(Line, "static char %s = {", Buff) == 1)
        version = 11;
    else {
        fprintf(stderr, "ReadBitmapFile: Bad file format in %s\n", filename);
        exit(-1);
    }

    padding = 0;
    if ((*width % 16) && ((*width % 16) < 9) && (version == 10))
        padding = 1;

    (image)->bytes_per_line = chars_line = (*width + 7) / 8;
    file_chars_line = chars_line + padding;

    num_chars = chars_line * (*height);
    file_chars = file_chars_line * (*height);
    (image)->data = (char *) halloc((image)->bytes_per_line * (image)->height,
                                    "Read Pixmap--Image data");

    /*
     * Since we are just holding the first line of the declaration, we can
     * just start reading from fd
     */

    if (version == 10)
        for (bytes = 0, ptr = (image)->data; bytes < file_chars; (bytes += 2)) {
            if (fscanf(fd, " 0x%x%*[,}]%*[ \n]", &rch) != 1) {
                fprintf(stderr, "ReadBitmapFile: Bad file format in %s\n", filename);
                exit(-1);
            }
            *(ptr++) = rch & 0xff;
            if (!padding || ((bytes + 2) % file_chars_line))
                *(ptr++) = rch >> 8;
        }
    else
        for (bytes = 0, ptr = (image)->data; bytes < file_chars; bytes++, ptr++) {
            if (fscanf(fd, " 0x%x%*[,}]%*[ \n]", &rch) != 1) {
                fprintf(stderr, "ReadBitmapFile: Bad file format in %s\n", filename);
                exit(-1);
            }
            *ptr = rch;
        }

    fclose(fd);

    return image;
}

static int
read_hot(FILE *fd,char Line[],int *x_hot,int *y_hot)
{
    char Buff[256];

    /*
     * Works much the same as get width and height, just new variables
     */

    if (sscanf(Line, "#define %s %d", Buff, x_hot) != 2)
        return -1;

    if (fgets(Line, MAXLINE, fd) == NULL)
        return -1;

    if (sscanf(Line, "#define %s %d", Buff, y_hot) != 2)
        return -1;

    if (fgets(Line, MAXLINE, fd) == NULL)
        return -1;
    return 1;
}

static int
read_w_and_h(FILE *fd,unsigned int *width,unsigned int *height)
{
    char Line[256], Buff[256];

    if (fgets(Line, MAXLINE, fd) == NULL)
        return -1;

    /*
     * Once we have the line, scan it for the width
     */

    if (sscanf(Line, "#define %s %d", Buff, width) != 2)
        return -1;

    /*
     * Hopefully we have the width, now get the height the same way
     */

    if (fgets(Line, MAXLINE, fd) == NULL)
        return -1;


    /*
     * Once we have the line, scan it for the height
     */

    if (sscanf(Line, "#define %s %d", Buff, height) != 2)
        return -1;

    return 1;
}


/* read a bitmap file into memory */

ImageStruct *
insert_image_struct(char *filename)
{
    int bm_width, bm_height;
    XImage *im;
    ImageStruct *image;

    if (*filename == ' ')
        filename++;
    if ((image = (ImageStruct *) hash_find(&gImageHashTable, filename)) == NULL) {
        im = HTReadBitmapFile(gXDisplay, gXScreenNumber, filename,
                              &bm_width, &bm_height);

        /*
         * now add the image to the gImageHashTable
         */

        image = (ImageStruct *) halloc(sizeof(ImageStruct), "ImageStruct");
        image->image.xi = im;
        image->width = image->image.xi->width;
        image->height = image->image.xi->height;
        image->filename = (char *) halloc(sizeof(char) * strlen(filename) +1,
                                          "insert_image--filename");

        /* strcpy(image->filename, filename); */

        sprintf(image->filename, "%s", filename);
        hash_insert(&gImageHashTable,(char *) image, image->filename);
    }
    return image;
}
