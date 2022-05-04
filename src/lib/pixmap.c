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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <stdlib.h>
#include <stdio.h>
#include <netinet/in.h>

#define yes 1
#define no  0



#include "spadcolors.h"

#include "pixmap.H1"
#include "halloc.H1"
#include "spadcolors.H1"
#include "strutil.h"



/* returns true if the file exists */

int
file_exists(char *file)
{
    FILE *f;

    if ((f = fopen(file, "r")) != NULL) {
        fclose(f);
        return 1;
    }
    return 0;
}

FILE *
zzopen(char *file,char * mode)
{
    char com[512], zfile[512];

    if (file_exists(file))
        return fopen(file, mode);
    fricas_sprintf_to_buf1(zfile, "%s.Z", file);
    if (file_exists(zfile)) {
        fricas_sprintf_to_buf1(com, "gunzip -c %s.Z 2>/dev/null", file);
        return popen(com, mode);
    }
    return NULL;
}


#include <X11/xpm.h>

int
read_pixmap_file(Display *display, int screen, char *filename,
                 XImage **xi, int *width, int *height)
{
  XpmAttributes attr;
  XImage *xireturn;
  char * real_filename;
  int status;

  attr.valuemask = 0;

  attr.bitmap_format=ZPixmap;             /* instead of XYPixmap */
  attr.valuemask |= XpmBitmapFormat;
  attr.valuemask |= XpmSize;              /* we want feedback on width,height */
  attr.valuemask |= XpmCharsPerPixel;     /* and cpp */
  attr.valuemask |= XpmReturnPixels;      /* and pixels, npixels */
  attr.valuemask |= XpmReturnAllocPixels; /* and alloc_pixels, nalloc_pixels */
  attr.exactColors = False;
  attr.valuemask |= XpmExactColors;       /* we don't want exact colors*/
  attr.closeness = 30000;
  attr.valuemask |= XpmCloseness;         /* we specify closeness*/
  attr.alloc_close_colors = False;
  attr.valuemask |= XpmAllocCloseColors;  /* we don't allocate close colors*/


  if (file_exists(filename)) {
      real_filename = filename;
  } else {
      real_filename = halloc (strlen(filename)+4, "read_pixmap_file");
      sprintf(real_filename, "%s.gz", filename);
      if (!file_exists (real_filename)) {
          sprintf(real_filename, "%s.Z", filename);
      }
  }
  status=XpmReadFileToImage(display,real_filename,xi,&xireturn, &attr );
  if (status != XpmSuccess) {
    fprintf(stderr, "read_pixmap_file: unable to open %s\n", filename);
    exit (1);
  }

  *width= (*xi)->width;
  *height=(*xi)->height;
#ifdef DEBUG
  fprintf(stderr,"image file:%s\n",filename);
  fprintf(stderr,"\twidth:%d\theight:%d\tcpp:%d\n",attr.width,attr.height,attr.cpp);
  fprintf(stderr,"\tused/alloc'ed color pixels:%d/%d\n",attr.npixels,attr.nalloc_pixels);
#endif
  return 0;
}


void
write_pixmap_file(Display *dsp, int scr, char  *fn,
                  Window wid, int x, int y, int width,int height)
{
  XImage *xi;
  int status;

  /* reads image structure in ZPixmap format */
  xi = XGetImage(dsp, wid, x, y, width, height, AllPlanes, ZPixmap);
  if (xi==0) return ;
  status=XpmWriteFileFromImage(dsp,fn,xi,0,0);

}
