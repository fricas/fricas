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

#define _WRITE2D_C
#include "fricas_c_macros.h"

#include <stdio.h>
#include <stdlib.h>

#include "header2.h"
#include "write.h"

#include "all_2d.H1"
#include "pixmap.H1"
#include "Gfun.H1"
#include "strutil.h"


#define numBits (8*sizeof(int))

int
writeViewport(int thingsToWrite)
{

  FILE              *viewDataFile;
  char              viewDirName[280],
                    viewBitmapFilename[280], viewDataFilename[280],
                    command[320];
  int               i,j,k,code,ii;
  pointListStruct   *aList;
  pointStruct       *aPoint;
  XWindowAttributes vwInfo;

  XGetWindowAttributes(dsply,viewport->titleWindow,&vwInfo);
  fricas_sprintf_to_buf2(viewDirName, "%s%s", filename, ".VIEW");
  fricas_sprintf_to_buf3(command, "%s%s%s", "rm -r ", viewDirName,
                         " >  /dev/null 2>&1");
  code = system(command);
  fricas_sprintf_to_buf3(command, "%s%s%s", "mkdir ", viewDirName,
                         " > /dev/null 2>&1");
  if (system(command)) {
    fprintf(stderr,"   Error: Cannot create %s\n",viewDirName);
    return(-1);
  } else {
            /*** Create the data file ***/
    fricas_sprintf_to_buf2(viewDataFilename, "%s%s", viewDirName, "/data");
    if ((viewDataFile = fopen(viewDataFilename,"w")) == NULL) {
      fprintf(stderr,"   Error: Cannot create %s\n",viewDataFilename);
      perror("fopen");
      return(-1);
    } else {
              /*** write out the view2DStruct stuff ***/
      fprintf(viewDataFile,"%d\n",view2DType);
      fprintf(viewDataFile,"%s\n",viewport->title);
      fprintf(viewDataFile,"%d %d %d %d\n",vwInfo.x,vwInfo.y,
              vwInfo.width,vwInfo.height);
      for (i=0; i<maxGraphs; i++) {
        fprintf(viewDataFile,"%d\n",graphArray[i].key);
        fprintf(viewDataFile,"%g %g\n",
                graphStateArray[i].scaleX,graphStateArray[i].scaleY);
        fprintf(viewDataFile,"%g %g\n",
                graphStateArray[i].deltaX,graphStateArray[i].deltaY);
        fprintf(viewDataFile,"%g %g\n",
                graphStateArray[i].centerX,graphStateArray[i].centerY);
        fprintf(viewDataFile,"%d %d %d %d %d %d %d\n",
                graphStateArray[i].pointsOn,graphStateArray[i].connectOn,
                graphStateArray[i].splineOn,
                graphStateArray[i].axesOn, graphStateArray[i].axesColor,
                graphStateArray[i].unitsOn, graphStateArray[i].unitsColor);
        fprintf(viewDataFile,"%d %d\n",
                graphStateArray[i].showing,graphStateArray[i].selected);
      }
      fclose(viewDataFile);
      for (i=0; i<maxGraphs; i++) {
        if (graphArray[i].key) {
          fricas_sprintf_to_buf3(viewDataFilename, "%s%s%d",
                                 viewDirName, "/graph", i);
          if ((viewDataFile = fopen(viewDataFilename,"w")) == NULL) {
            fprintf(stderr,"   Error: Cannot create %s\n",viewDataFilename);
            perror("fopen");
            return(-1);
          } else {
            fprintf(viewDataFile,"%g %g %g %g\n",
                    graphArray[i].xmin,graphArray[i].ymin,
                    graphArray[i].xmax,graphArray[i].ymax);
            fprintf(viewDataFile,"%g %g\n",
                    graphArray[i].xNorm,graphArray[i].yNorm);
            fprintf(viewDataFile,"%g %g\n",
                    graphArray[i].originX,graphArray[i].originY);
            fprintf(viewDataFile,"%g %g\n",
                    graphArray[i].spadUnitX,graphArray[i].spadUnitY);
            fprintf(viewDataFile,"%g %g\n",
                    graphArray[i].unitX,graphArray[i].unitY);
            fprintf(viewDataFile,"%d\n",graphArray[i].numberOfLists);
            for (j=0,aList=graphArray[i].listOfListsOfPoints;
                 j<graphArray[i].numberOfLists;
                 j++, aList++) {
              fprintf(viewDataFile,"%d\n",aList->numberOfPoints);
              fprintf(viewDataFile,"%d %d %d\n",
                      aList->pointColor,aList->lineColor,aList->pointSize);
              for (k=0,aPoint=aList->listOfPoints;
                   k<aList->numberOfPoints;
                   k++,aPoint++)
                fprintf(viewDataFile,"%g %g %g %g\n",
                        aPoint->x,aPoint->y,aPoint->hue,aPoint->shade);
            } /* for j, aList */
            fclose(viewDataFile);
          } /* else graph i */
        } /* if */
      } /* for */
    } /* else */

           /* write out special files */
    for (ii=1; ii<numBits; ii++) {   /* write.h is one-based */
      if (thingsToWrite & (1<<ii)) {
        switch (ii) {
        case Pixmap:
            /*** Create the pixmap (bitmaps need leaf name) ***/
          fricas_sprintf_to_buf2(viewBitmapFilename, "%s%s",
                                 viewDirName, "/image.xpm");
          XGetWindowAttributes(dsply,viewport->viewWindow,&vwInfo);
          write_pixmap_file(dsply,scrn,viewBitmapFilename,
                                   viewport->titleWindow,0,0,vwInfo.width,
                                   vwInfo.height+titleHeight);
          break;
        case Bitmap:
            /*** Create the bitmap (bitmaps need leaf name) ***/
          fricas_sprintf_to_buf2(viewBitmapFilename, "%s%s",
                                 viewDirName, "/image.bm");
          XGetWindowAttributes(dsply,viewport->viewWindow,&vwInfo);
          code = XWriteBitmapFile(dsply,viewBitmapFilename,
                                  viewport->titleWindow,vwInfo.width,
                                  vwInfo.height+vwInfo.border_width+20,-1,-1);
          break;
        case Image:
            /*** Create the pixmap (bitmaps need leaf name) ***/
          fricas_sprintf_to_buf2(viewBitmapFilename, "%s%s",
                                 viewDirName, "/image.xpm");
          XResizeWindow(dsply,viewport->titleWindow,300,300+titleHeight);
          XResizeWindow(dsply,viewport->viewWindow,300,300);
          XGetWindowAttributes(dsply,viewport->viewWindow,&vwInfo);
          drawViewport(Xoption);
          writeTitle();
          write_pixmap_file(dsply,scrn,viewBitmapFilename,
                                   viewport->titleWindow,0,0,vwInfo.width,
                                   vwInfo.height+titleHeight);
            /*** Create the bitmap (bitmaps need leaf name) ***/
          mono = 1;
          drawViewport(Xoption);
          writeTitle();
          fricas_sprintf_to_buf3(viewBitmapFilename, "%s%s%s",
                                 viewDirName, "/", "image.bm");
          code = XWriteBitmapFile(dsply,viewBitmapFilename,
                                  viewport->titleWindow,vwInfo.width,
                                  vwInfo.height+vwInfo.border_width+20,-1,-1);
          mono = 0;
          break;

        case Postscript:
            /*** Create postscript output for viewport (in fricas2D.ps) ***/
         fricas_sprintf_to_buf2(PSfilename, "%s%s", viewDirName,
                                "/fricas2D.ps");
         if (PSInit(viewport->viewWindow,viewport->titleWindow) == psError)
           return (-1);
         drawViewport(PSoption);  /* write new script file in /tmp */
         if (PSCreateFile(viewBorderWidth,viewport->viewWindow,
                          viewport->titleWindow, viewport->title) == psError)
           return(-1);          /* concat script & proc into fricas2D.ps */
          break;

        } /* switch on ii */
      }  /* if thingsToWrite >> ii */
    }  /* for ii */

    return(0);
  }    /* else create directory okay */

}
