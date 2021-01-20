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

#define _VIEWPORT3D_C
#include "fricas_c_macros.h"

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

#include "spadBitmap.bitmap"
#include "spadMask.mask"

#include "header.h"
    /*** definition for the axes and labels - this is the minimum that will be
         drawn on the window - thus allowing the user some idea of the
         orientation of the coordinate axes when rotating, etc. The
         drawing of the mesh is aborted when an appropriate X event occurs.
         The mesh should be scaled to the range of [-100..100] in all
         directions.  axisRange defines the range...change the stuff below
         if that has changed. ***/
#include "static.h"
#include "draw.h"
#include "volume.h"
#include "mode.h"

#include "util.H1"
#include "Gfun.H1"
#include "XSpadFill.H1"
#include "all_3d.H1"

#define axesOffset  5

Atom wm_delete_window;


/***************************
 ***  void writeTitle()  ***
 ***************************/

void
writeTitle (void)
{

  int                   strlength;
  XWindowAttributes     twInfo;

  XGetWindowAttributes(dsply, viewport->titleWindow, &twInfo);
  if (mono || viewport->monoOn)
    GSetForeground(anotherGC, (float)foregroundColor, Xoption);
  else
    GSetForeground(anotherGC, (float)titleColor, Xoption);
  XClearWindow(dsply, viewport->titleWindow);

  strlength = strlen(viewport->title);
  GDrawImageString(anotherGC, viewport->titleWindow,
                   centerX(anotherGC, viewport->title, strlength,
                           twInfo.width), 15,
                   viewport->title, strlength, Xoption);

}


/****************************
 * void drawPreViewport()   *
 *                          *
 * draws the axes and boxes *
 * before the actual stuff. *
 * all incoming signals     *
 * should be block and no   *
 * check for pending X      *
 * events are made.         *
 *                          *
 ****************************/

void
drawPreViewport (int dFlag)  /* display flag: PS, X, etc. */
{

  int i, j, vPx0, vPy0, vPx1, vPy1;
      /* for drawing the box */
  float vPz, absTransX, absTransY;
  XPoint blackbox[3], line[2];
  RGB    axes_rgb, clipbox_rgb, boundbox_rgb;

  axes_rgb.r = 0.8;     axes_rgb.g = 0.6;     axes_rgb.b = 0.2;
  clipbox_rgb.r = 0.4;  clipbox_rgb.g = 0.5;  clipbox_rgb.b = 0.9;
  boundbox_rgb.r = 0.4; boundbox_rgb.g = 0.7; boundbox_rgb.b = 0.9;

  XGetWindowAttributes(dsply, viewport->viewWindow, &vwInfo);
  graphWindowAttrib = vwInfo;

        /* Calculate various factors for use in projection */
        /* Scale so that plot the scaling between the axes remains constant
           and fits within the smaller of the two dimensions. */

  xCenter = vwInfo.width / 2;
  yCenter = vwInfo.height / 2;

  if (vwInfo.height <= vwInfo.width) {
    viewScale = viewport->scale * vwInfo.height / viewHeight;
  }
  else {
    viewScale = viewport->scale * vwInfo.width / viewWidth;
  }

             /* Draw the projected image */
    /** draw the axes without heeding to X interrupts, first **/

  if (dFlag == Xoption)   /* do this for X option only */
    XClearWindow(dsply, viewport->viewWindow);

  sinTheta  = sin(-viewport->axestheta);
  cosTheta  = cos(-viewport->axestheta);
  sinPhi    = sin(viewport->axesphi);
  cosPhi    = cos(viewport->axesphi);

  /* Create transformation matrices */
  ROTATE(R); /* angles theta and phi are global */
  SCALE(viewport->scaleX,viewport->scaleY,viewport->scaleZ,S);
  TRANSLATE(-viewport->deltaX,-viewport->deltaY,0.0,T);

  /**** Pre Draw Routine ****/

  if ((dFlag == PSoption) && (foregroundColor == white)) {
    GSetForeground(globGC,(float)backgroundColor,dFlag);
    blackbox[0].x = vwInfo.width;  blackbox[0].y = vwInfo.height;
    blackbox[1].x = 0;             blackbox[1].y = 0;
    blackbox[2].x = 0;             blackbox[2].y = vwInfo.height;
    if (viewport->monoOn || mono) {
      PSFillPolygon(globGC, blackbox, 3);
    } else {
      PSColorPolygon(0.0,0.0,0.0,blackbox,4);
    }
    blackbox[0].x = vwInfo.width;  blackbox[0].y = 0;
    blackbox[1].x = 0;             blackbox[1].y = 0;
    blackbox[2].x = vwInfo.width;  blackbox[2].y = vwInfo.height;
    if (viewport->monoOn || mono) {
      PSFillPolygon(globGC, blackbox, 3);
    } else {
      PSColorPolygon(0.0,0.0,0.0,blackbox,4);
    }
  }

  /*  axes  */

  for (i=0; i < 3; i++) {
    projectStuff(axes[i][0],axes[i][1],axes[i][2],&vPx0,&vPy0,&vPz);
    axesXY[i][0] = vPx0; axesXY[i][1] = vPy0; axesZ[i][0] = vPz;
    projectStuff(axes[i][3],axes[i][4],axes[i][5],&vPx1,&vPy1,&vPz);
    axesXY[i][2] = vPx1; axesXY[i][3] = vPy1; axesZ[i][1] = vPz;
    if (viewport->axesOn) {
      if (viewport->monoOn || mono) {
        GSetForeground(globalGC1,(float)foregroundColor,dFlag);
        GSetForeground(globGC,(float)foregroundColor,dFlag);
        GDrawLine(globalGC1,viewport->viewWindow,vPx0,vPy0,vPx1,vPy1,dFlag);
      } else {
        if (dFlag == PSoption) {
          GSetForeground(globGC,(float)foregroundColor,dFlag);
          line[0].x = vPx0;  line[0].y = vPy0;
          line[1].x = vPx1;  line[1].y = vPy1;
          PSDrawColor(axes_rgb.r,axes_rgb.g,axes_rgb.b,line,2);
        } else {
          GSetForeground(globalGC1,(float)monoColor(axesColor),dFlag);
          GSetForeground(globGC,(float)monoColor(labelColor),dFlag);
          GDrawLine(globalGC1,viewport->viewWindow,vPx0,vPy0,vPx1,vPy1,dFlag);
        }
      }
      if (i == 0) {
        if (axesXY[0][2] < axesXY[0][0]) vPx1 -= axesOffset;
        else vPx1 += axesOffset;
        if (axesXY[0][3] < axesXY[0][1]) vPy1 -= axesOffset;
        else vPy1 += axesOffset;
        if (!viewport->yzOn)
          GDrawString(globGC,viewport->viewWindow,vPx1,vPy1,"X",1,dFlag);
      } else {
        if (i == 1) {
          if (axesXY[1][2] < axesXY[1][0]) vPx1 -= axesOffset;
          else vPx1 += axesOffset;
          if (axesXY[1][3] < axesXY[1][1]) vPy1 -= axesOffset;
          else vPy1 += axesOffset;
          if (!viewport->xzOn)
            GDrawString(globGC,viewport->viewWindow,vPx1,vPy1,"Y",1,dFlag);
        } else {
          if (axesXY[2][2] < axesXY[2][0]) vPx1 -= axesOffset;
          else vPx1 += axesOffset;
          if (axesXY[2][3] < axesXY[2][1]) vPy1 -= axesOffset;
          else vPy1 += axesOffset;
          if (!viewport->xyOn)
            GDrawString(globGC,viewport->viewWindow,vPx1,vPy1,"Z",1,dFlag);
        }
      }
      GSetForeground(globalGC1,(float)monoColor(buttonColor),dFlag);
      GSetForeground(globGC,(float)monoColor(buttonColor),dFlag);
    }    /* if viewport->axesOn */
  }

  viewport->transX = (viewData.xmax + viewData.xmin)/2.0;
  viewport->transY = (viewData.ymax + viewData.ymin)/2.0;
  viewport->transZ = (viewData.zmax + viewData.zmin)/2.0;

  absTransX = absolute(viewport->transX);
  absTransY = absolute(viewport->transY);
  if ((absTransX > 0.5) || (absTransY > 0.5)) {
    if (absTransX > absTransY)
      reScale = 50.0 * absTransX / viewData.scaleToView;
    else
      reScale = 50.0 * absTransY / viewData.scaleToView;
    if (reScale < 100.0) reScale = 100.0;
  } else {
    reScale = 100.0;
  }

  sinTheta  = sin(-viewport->thetaObj);
  cosTheta  = cos(-viewport->thetaObj);
  sinPhi    = sin(viewport->phiObj);
  cosPhi    = cos(viewport->phiObj);
  ROTATE1(R1);

  if (viewport->originFlag) viewport->originFlag = no;
  sinTheta  = sin(-viewport->axestheta);
  cosTheta  = cos(-viewport->axestheta);
  sinPhi    = sin(viewport->axesphi);
  cosPhi    = cos(viewport->axesphi);
  ROTATE(R);

     /*  region box */
  if (viewData.clipbox) {
    clipCorners[0].x = viewData.clipXmin;
    clipCorners[0].y = viewData.clipYmin;
    clipCorners[0].z = viewData.clipZmin;
    clipCorners[1].x = viewData.clipXmax;
    clipCorners[1].y = viewData.clipYmin;
    clipCorners[1].z = viewData.clipZmin;
    clipCorners[2].x = viewData.clipXmax;
    clipCorners[2].y = viewData.clipYmin;
    clipCorners[2].z = viewData.clipZmax;
    clipCorners[3].x = viewData.clipXmin;
    clipCorners[3].y = viewData.clipYmin;
    clipCorners[3].z = viewData.clipZmax;
    clipCorners[4].x = viewData.clipXmin;
    clipCorners[4].y = viewData.clipYmax;
    clipCorners[4].z = viewData.clipZmin;
    clipCorners[5].x = viewData.clipXmax;
    clipCorners[5].y = viewData.clipYmax;
    clipCorners[5].z = viewData.clipZmin;
    clipCorners[6].x = viewData.clipXmax;
    clipCorners[6].y = viewData.clipYmax;
    clipCorners[6].z = viewData.clipZmax;
    clipCorners[7].x = viewData.clipXmin;
    clipCorners[7].y = viewData.clipYmax;
    clipCorners[7].z = viewData.clipZmax;

    GSetLineAttributes(trashGC,0,LineSolid,CapButt,JoinMiter,dFlag);

    /* project the 8 corners of the box */
    for (i=0;i<8;i++) projectAPoint(&(clipCorners[i]));

    for (i=0;i<6;i++) {
       clipBox[i].inside = ((clipBox[i].pointsPtr[2]->px -
                             clipBox[i].pointsPtr[1]->px) *
                            (clipBox[i].pointsPtr[1]->py -
                             clipBox[i].pointsPtr[0]->py) -
                            (clipBox[i].pointsPtr[2]->py -
                             clipBox[i].pointsPtr[1]->py) *
                            (clipBox[i].pointsPtr[1]->px -
                             clipBox[i].pointsPtr[0]->px)) < 0;
      if (clipBox[i].inside) {
         for (j=0; j<3; j++) {
             quadMesh[j].x = clipBox[i].pointsPtr[j]->px;
             quadMesh[j].y = clipBox[i].pointsPtr[j]->py;
         }
         if (viewport->monoOn || mono) {
           GSetForeground(trashGC,(float)foregroundColor,dFlag);
           GDrawLines(trashGC, viewport->viewWindow, quadMesh, 3,
                      CoordModeOrigin, dFlag);
         } else {
           if (dFlag == PSoption) {
             GSetForeground(trashGC,(float)clipBoxInline, dFlag);
             line[0].x = quadMesh[0].x;  line[0].y = quadMesh[0].y;
             line[1].x = quadMesh[1].x;  line[1].y = quadMesh[1].y;
             PSDrawColor(clipbox_rgb.r,clipbox_rgb.g,clipbox_rgb.b,line,2);
             line[0].x = quadMesh[1].x;  line[0].y = quadMesh[1].y;
             line[1].x = quadMesh[2].x;  line[1].y = quadMesh[2].y;
             PSDrawColor(clipbox_rgb.r,clipbox_rgb.g,clipbox_rgb.b,line,2);
           } else {
             GSetForeground(trashGC,(float)clipBoxInline, dFlag);
             GDrawLines(trashGC, viewport->viewWindow, quadMesh, 3,
                        CoordModeOrigin, dFlag);
           }
         }
      }
    }
  }  /* if viewData.clipbox */

     /* VOLUME panel stuff */
  if ((doingPanel == VOLUMEpanel) || viewData.box) {

    GSetLineAttributes(trashGC,0,LineSolid,CapButt,JoinMiter,dFlag);

    for (i=0;i<8;i++) {
        /* project the 8 corners of the box */
        projectAPoint(&(corners[i]));
        if (i) {
           if (corners[i].pz > pzMax) pzMax = corners[i].pz;
           else if (corners[i].pz < pzMin) pzMin = corners[i].pz;
        } else
           pzMax = pzMin = corners[i].pz;
    }

    for (i=0;i<6;i++) {
        /* Process the 6 sides of the boxes.
           Here, we calculate, for each side (defined by two segments)
           whether it is facing towards or away from the viewer. if
           facing, away, we draw them first. later we draw the ones
           facing the viewer. (this is a sort of backface removal
           scheme. */
        /* We define the normal vector for the box as vA X vB where
           vA=p2-p0 and vB=p1-p0. All we really care about, though,
           is what sign the normal is (whether it is towards or away
           from the viewer - so we just take the triple product of
           it against the eye vector, which is, conveniently enough,
           simply [0 0 1]. Hence, only the Z component of the
           cross product is calculated. (Actually, we are using the
           projected normal - that's how we are able to use the
           trick of just taking the Z component. */
      box[i].inside = ((box[i].pointsPtr[2]->px -
                        box[i].pointsPtr[0]->px) *              /* Ax *  */
                       (box[i].pointsPtr[1]->py -
                        box[i].pointsPtr[0]->py) -              /* By -  */
                       (box[i].pointsPtr[2]->py -
                        box[i].pointsPtr[0]->py) *              /* Ay *  */
                       (box[i].pointsPtr[1]->px -
                        box[i].pointsPtr[0]->px))               /* Bx    */
                       < 0;
      if (box[i].inside) {
         for (j=0; j<3; j++) {
             quadMesh[j].x = box[i].pointsPtr[j]->px;
             quadMesh[j].y = box[i].pointsPtr[j]->py;
         }
         if (viewport->monoOn || mono) {
           GSetForeground(trashGC,(float)foregroundColor,dFlag);
           GDrawLines(trashGC, viewport->viewWindow, quadMesh, 3,
                      CoordModeOrigin, dFlag);
         } else {
           if (dFlag == PSoption) {
             GSetForeground(trashGC,(float)boxInline, dFlag );
             line[0].x = quadMesh[0].x;  line[0].y = quadMesh[0].y;
             line[1].x = quadMesh[1].x;  line[1].y = quadMesh[1].y;
             PSDrawColor(boundbox_rgb.r,boundbox_rgb.g,boundbox_rgb.b,line,2);
             line[0].x = quadMesh[1].x;  line[0].y = quadMesh[1].y;
             line[1].x = quadMesh[2].x;  line[1].y = quadMesh[2].y;
             PSDrawColor(boundbox_rgb.r,boundbox_rgb.g,boundbox_rgb.b,line,2);
           } else {
             GSetForeground(trashGC,(float)boxInline, dFlag );
             GDrawLines(trashGC, viewport->viewWindow, quadMesh, 3,
                        CoordModeOrigin, dFlag);
           }
         }
      }
    }
  }  /* if viewData.box */

         /* Write out view data */
  if (dFlag == Xoption) {   /* do this only for X option */
     writeControlMessage();
     XFlush(dsply);
  }
}


/********************************/
/***  void drawTheViewport()  ***/
/********************************/

void
drawTheViewport (int dFlag)  /* display flag: PS, X,... */
{

  int    i,j;
  XPoint line[2];
  RGB    clipbox_rgb, boundbox_rgb;

  clipbox_rgb.r = 0.4;  clipbox_rgb.g = 0.5;  clipbox_rgb.b = 0.9;
  boundbox_rgb.r = 0.4; boundbox_rgb.g = 0.7; boundbox_rgb.b = 0.9;

     /**** Draw Routine ****/

  if (viewport->allowDraw && (doingPanel != VOLUMEpanel)) {
    /* Do not draw the mesh stuff if we're in the process of changing
       the viewing volume; we just want to see the volume */

    /* drawMore allows the drawing to continue if no relevant XEvent occurs */
    drawMore = yes;
    drawMore = keepDrawingViewport();
    draw3DComponents(dFlag);

  } /*if viewport->allowDraw */

     /**** Post Draw Routine ****/
  if (viewData.clipbox) {   /* draw the front 3 lines of region box */
    GSetLineAttributes(trashGC,0,LineSolid,CapButt,JoinMiter,dFlag);
    for (i=0; i<6; i++) {
      if (!(clipBox[i].inside)) {
        for (j=0; j<4; j++) {
          quadMesh[j].x = clipBox[i].pointsPtr[j]->px;
          quadMesh[j].y = clipBox[i].pointsPtr[j]->py;
        }
        if (viewport->monoOn || mono) {
          GSetForeground(trashGC,(float)foregroundColor,dFlag);
          GDrawLines(trashGC, viewport->viewWindow, quadMesh, 3,
                     CoordModeOrigin, dFlag);
        } else {
          if (dFlag == PSoption) {
            GSetForeground(trashGC,(float)boxInline, dFlag );
            line[0].x = quadMesh[0].x;  line[0].y = quadMesh[0].y;
            line[1].x = quadMesh[1].x;  line[1].y = quadMesh[1].y;
            PSDrawColor(clipbox_rgb.r,clipbox_rgb.g,clipbox_rgb.b,line,2);
            line[0].x = quadMesh[1].x;  line[0].y = quadMesh[1].y;
            line[1].x = quadMesh[2].x;  line[1].y = quadMesh[2].y;
            PSDrawColor(clipbox_rgb.r,clipbox_rgb.g,clipbox_rgb.b,line,2);
          } else {
            GSetForeground(trashGC,(float)boxInline, dFlag );
            GDrawLines(trashGC, viewport->viewWindow, quadMesh, 3,
                       CoordModeOrigin, dFlag);
          }
        }
      }
    }
  }

  if ((doingPanel==VOLUMEpanel) || viewData.box) {
    GSetLineAttributes(trashGC,0,LineSolid,CapButt,JoinMiter,dFlag);
    for (i=0; i<6; i++) {
      if (!(box[i].inside)) {
        for (j=0; j<4; j++) {
          quadMesh[j].x = box[i].pointsPtr[j]->px;
          quadMesh[j].y = box[i].pointsPtr[j]->py;
        }
        if (viewport->monoOn || mono) {
          GSetForeground(trashGC,(float)foregroundColor,dFlag);
          GDrawLines(trashGC, viewport->viewWindow, quadMesh, 3,
                     CoordModeOrigin, dFlag);
        } else {
          if (dFlag == PSoption) {
            GSetForeground(trashGC,(float)boxInline, dFlag );
            line[0].x = quadMesh[0].x;  line[0].y = quadMesh[0].y;
            line[1].x = quadMesh[1].x;  line[1].y = quadMesh[1].y;
            PSDrawColor(boundbox_rgb.r,boundbox_rgb.g,boundbox_rgb.b,line,2);
            line[0].x = quadMesh[1].x;  line[0].y = quadMesh[1].y;
            line[1].x = quadMesh[2].x;  line[1].y = quadMesh[2].y;
            PSDrawColor(boundbox_rgb.r,boundbox_rgb.g,boundbox_rgb.b,line,2);
          } else {
            GSetForeground(trashGC,(float)boxInline, dFlag );
            GDrawLines(trashGC, viewport->viewWindow, quadMesh, 3,
                       CoordModeOrigin, dFlag);
          }
        }
      }
    }
  }

  if (dFlag == Xoption)  /* do this for X option only */
     XFlush(dsply);

  if (smoothError) {
    strcpy(control->message,"Cannot alloc more smooth shades.");
    writeControlMessage();
    smoothError = no;
  }

} /* drawTheViewport */

/************************************
 ***  viewPoints *makeViewport()  ***
 ************************************/

viewPoints *
makeViewport (void)
{

  Pixmap               spadbits,spadmask;
  XSetWindowAttributes viewAttrib;
  XSizeHints           titleSizeHints;
  Window               viewTitleWindow, viewGraphWindow;
  XColor               foreColor, backColor;

            /**** create a viewport ****/

  if (!(viewport = (viewPoints *)saymem("viewport3D.c",
                                        1,sizeof(viewPoints)))) {
    fprintf(stderr,"Ran out of memory trying to create a viewport.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }
  /* Definition of the 4x4 identity matrix. */
  I[0][0] = 1.0; I[0][1] = 0.0; I[0][2] = 0.0; I[0][3] = 0.0;
  I[1][0] = 0.0; I[1][1] = 1.0; I[1][2] = 0.0; I[1][3] = 0.0;
  I[2][0] = 0.0; I[2][1] = 0.0; I[2][2] = 1.0; I[2][3] = 0.0;
  I[3][0] = 0.0; I[3][1] = 0.0; I[3][2] = 0.0; I[3][3] = 1.0;

  viewport->viewportKey  = viewportKeyNum++;
  viewport->nextViewport = 0;
  viewport->prevViewport = 0;
  viewport->deltaX       = viewport->deltaX0  = viewData.deltaX;
  viewport->deltaY       = viewport->deltaY0  = viewData.deltaY;
  viewport->deltaZ       = viewport->deltaZ0  = viewData.deltaZ;
  viewport->scale        = viewport->scale0   = viewData.scale;
  viewport->scaleX       = viewData.scaleX;
  viewport->scaleY       = viewData.scaleY;
  viewport->scaleZ       = viewData.scaleZ;
  viewport->transX       = (viewData.xmax + viewData.xmin)/2.0;
  viewport->transY       = (viewData.ymax + viewData.ymin)/2.0;
  viewport->transZ       = (viewData.zmax + viewData.zmin)/2.0;

  viewport->theta = viewport->axestheta = viewport->theta0 = viewData.theta;
  viewport->phi   = viewport->axesphi   = viewport->phi0   = viewData.phi;
  viewport->thetaObj = 0.0;
  viewport->phiObj   = 0.0;

  strcpy(viewport->title,viewData.title);

  viewport->axesOn       = yes;
  viewport->regionOn     = no;
  viewport->monoOn       = no;
  viewport->zoomXOn      = yes;
  viewport->zoomYOn      = yes;
  viewport->zoomZOn      = yes;

  viewport->originrOn    = yes;
  viewport->objectrOn    = no;
  viewport->originFlag   = no;

  viewport->xyOn         = no;
  viewport->xzOn         = no;
  viewport->yzOn         = no;

  viewport->closing      = no;
  viewport->allowDraw    = yes;   /*if no, just draw axes the first time */
  viewport->needNorm     = yes;

  viewport->lightVector[0] = -0.5;
  viewport->lightVector[1] = 0.5;
  viewport->lightVector[2] = 0.5;
  viewport->translucency   = viewData.translucency;

  viewport->hueOffset      = viewData.hueOff;
  viewport->numberOfHues   = viewData.numOfHues;
  viewport->hueTop         = viewData.hueOff + viewData.numOfHues;
  if (viewport->hueTop > totalHues-1) viewport->hueTop = totalHues-1;
  viewport->diagonals      = viewData.diagonals;

  /* make theta in [0..2pi) and phi in (-pi..pi] */
  while (viewport->theta >= two_pi) {
    viewport->theta -= two_pi;
  }
  while (viewport->theta < 0.0) {
    viewport->theta += two_pi;
  }
  while (viewport->phi > pi) {
    viewport->phi -= two_pi;
  }
  while (viewport->phi <= -pi) {
    viewport->phi += two_pi;
  }

  while (viewport->axestheta >= two_pi) {
    viewport->axestheta -= two_pi;
  }
  while (viewport->axestheta < 0.0) {
    viewport->axestheta += two_pi;
  }
  while (viewport->axesphi > pi) {
    viewport->axesphi -= two_pi;
  }
  while (viewport->axesphi <= -pi) {
    viewport->axesphi += two_pi;
  }

  /* Initialize the rotation matrix about the origin. */
  sinTheta  = sin(-viewport->theta);
  cosTheta  = cos(-viewport->theta);
  sinPhi    = sin(viewport->phi);
  cosPhi    = cos(viewport->phi);
  ROTATE(R);  /* angles theta and phi are global */

  /* Initialize the rotation matrix about the object's center of volume. */
  sinTheta  = sin(-viewport->thetaObj);
  cosTheta  = cos(-viewport->thetaObj);
  sinPhi    = sin(viewport->phiObj);
  cosPhi    = cos(viewport->phiObj);
  ROTATE1(R1);  /* angles theta and phi are global  */


  /* Initialize the non-uniform scaling matrix. */
  SCALE(viewport->scaleX,viewport->scaleY,viewport->scaleZ,S);
  /* Initialize the translation matrix. */
  TRANSLATE(-viewport->deltaX,-viewport->deltaY,0.0,T);
           /**** make the windows for the viewport ****/
  spadbits = XCreateBitmapFromData(dsply,rtWindow,
                                   spadBitmap_bits,
                                   spadBitmap_width,spadBitmap_height);
  spadmask = XCreateBitmapFromData(dsply,rtWindow,
                                   spadMask_bits,
                                   spadMask_width,spadMask_height);
  viewAttrib.background_pixel = backgroundColor;
  viewAttrib.border_pixel = foregroundColor;

  viewAttrib.override_redirect = overrideManager;

  viewAttrib.colormap = colorMap;
  foreColor.pixel = foregroundColor;
  backColor.pixel = backgroundColor;
/*
  foreColor.pixel = viewCursorForeground;
  backColor.pixel = viewCursorBackground;
*/
  XQueryColor(dsply,colorMap,&foreColor);
  XQueryColor(dsply,colorMap,&backColor);
  viewAttrib.cursor = XCreatePixmapCursor(dsply,spadbits,spadmask,
                                          &foreColor,&backColor,spadBitmap_x_hot,spadBitmap_y_hot);
  viewAttrib.event_mask = titleMASK;
  if (viewData.vW) {
    titleSizeHints.flags  = PPosition | PSize;
    titleSizeHints.x      = viewData.vX;
    titleSizeHints.y      = viewData.vY;
    titleSizeHints.width  = viewData.vW;
    titleSizeHints.height = viewData.vH;
  } else {    /* ain't gonna allow this for now... */
    titleSizeHints.flags  = PSize;
    titleSizeHints.width  = viewWidth;
    titleSizeHints.height = viewHeight;
  }

  viewTitleWindow = XCreateWindow(dsply                   /* display */,
                                  rtWindow,               /* parent */
                                  viewData.vX,            /* x */
                                  viewData.vY,            /* y */
                                  viewData.vW,            /* width */
                                  viewData.vH,            /* height */
                                  /* viewBorderWidth+3*/ 0,      /* border width */
                                  CopyFromParent,         /* depth */
                                  InputOutput,            /* class */
                                  CopyFromParent,         /* visual */
                                  viewportTitleCreateMASK,/* valuemask */
                                  &viewAttrib             /* attributes */);

  wm_delete_window = XInternAtom(dsply, "WM_DELETE_WINDOW", False);
  (void) XSetWMProtocols(dsply, viewTitleWindow, &wm_delete_window, 1);

  XSetNormalHints(dsply,viewTitleWindow,&titleSizeHints);
  if (strlen(viewport->title) < 30)
    XSetStandardProperties(dsply,viewTitleWindow,"FriCAS 3D",viewport->title,
                           None,NULL,0,&titleSizeHints);
  else
    XSetStandardProperties(dsply,viewTitleWindow,"FriCAS 3D","3D FriCAS Graph",
                           None,NULL,0,&titleSizeHints);
  viewport->titleWindow = viewTitleWindow;

  viewAttrib.event_mask = viewportMASK;
  viewSizeHints.flags   = PPosition | PSize;
  viewSizeHints.x       = -(viewBorderWidth+3);
  viewSizeHints.x       = 0; /* lose this */
  viewSizeHints.y       = titleHeight;
  viewSizeHints.width   = titleSizeHints.width;
  viewSizeHints.height  = titleSizeHints.height-(titleHeight+appendixHeight);

  viewGraphWindow = XCreateWindow(dsply,                  /* display */
                                  viewTitleWindow,        /* parent */
                                  viewSizeHints.x,        /* x */
                                  viewSizeHints.y,        /* y */
                                  viewSizeHints.width,    /* width */
                                  viewSizeHints.height,   /* height */
                                  /* viewBorderWidth+3*/0,      /* border width */
                                  CopyFromParent,         /* depth */
                                  InputOutput,            /* class */
                                  CopyFromParent,         /* visual */
                                  viewportCreateMASK,     /* valuemask */
                                  &viewAttrib             /* attributes */);
  XSetNormalHints(dsply,viewGraphWindow,&viewSizeHints);
  XSetStandardProperties(dsply,viewGraphWindow,"","",None,NULL,0,
                         &viewSizeHints);
  viewport->viewWindow = viewGraphWindow;
  graphWindowAttrib.width = viewSizeHints.width;
  graphWindowAttrib.height = viewSizeHints.height;

  if (viewport->hueOffset != viewport->hueTop) {
    multiColorFlag = yes;
    redoColor = no;
  } else {
    if (viewport->hueTop < 11)
      smoothHue = viewport->hueTop*6;
    else {
      if (viewport->hueTop > 10 && viewport->hueTop < 16)
        smoothHue = viewport->hueTop*20 - 140;
      else smoothHue = viewport->hueTop*12 - 12;
    }
    redoColor = yes;
  }

            /**** Make the control panel for the viewport. ****/

  XSync(dsply,0);

  control = viewport->controlPanel = makeControlPanel();
  makeLightingPanel();
  makeVolumePanel();
  makeSavePanel();
  makeQuitPanel();

  if ((viewport->haveControl = viewData.showCP))
    putControlPanelSomewhere(anywhere);

  firstTime = yes;
  return(viewport);

} /* makeViewport() */


/*****************************
 * void postMakeViewport()   *
 *                           *
 * post processing when      *
 * creating a viewport.      *
 *  1) assign min,max values *
 *     for the box volume    *
 *****************************/

void
postMakeViewport (void)
{

  corners[0].x = viewData.xmin; corners[0].y = viewData.ymin;
  corners[0].z = viewData.zmin;
  corners[1].x = viewData.xmax; corners[1].y = viewData.ymin;
  corners[1].z = viewData.zmin;
  corners[2].x = viewData.xmax; corners[2].y = viewData.ymin;
  corners[2].z = viewData.zmax;
  corners[3].x = viewData.xmin; corners[3].y = viewData.ymin;
  corners[3].z = viewData.zmax;
  corners[4].x = viewData.xmin; corners[4].y = viewData.ymax;
  corners[4].z = viewData.zmin;
  corners[5].x = viewData.xmax; corners[5].y = viewData.ymax;
  corners[5].z = viewData.zmin;
  corners[6].x = viewData.xmax; corners[6].y = viewData.ymax;
  corners[6].z = viewData.zmax;
  corners[7].x = viewData.xmin; corners[7].y = viewData.ymax;
  corners[7].z = viewData.zmax;

  box[2].pointsPtr[0] = &(corners[0]);
  box[2].pointsPtr[1] = &(corners[1]);
  box[2].pointsPtr[2] = &(corners[2]);
  box[2].pointsPtr[3] = &(corners[3]);

  box[3].pointsPtr[0] = &(corners[1]);
  box[3].pointsPtr[1] = &(corners[5]);
  box[3].pointsPtr[2] = &(corners[6]);
  box[3].pointsPtr[3] = &(corners[2]);

  box[0].pointsPtr[0] = &(corners[4]);
  box[0].pointsPtr[1] = &(corners[7]);
  box[0].pointsPtr[2] = &(corners[6]);
  box[0].pointsPtr[3] = &(corners[5]);

  box[1].pointsPtr[0] = &(corners[0]);
  box[1].pointsPtr[1] = &(corners[3]);
  box[1].pointsPtr[2] = &(corners[7]);
  box[1].pointsPtr[3] = &(corners[4]);

  box[5].pointsPtr[0] = &(corners[3]);
  box[5].pointsPtr[1] = &(corners[2]);
  box[5].pointsPtr[2] = &(corners[6]);
  box[5].pointsPtr[3] = &(corners[7]);

  box[4].pointsPtr[0] = &(corners[0]);
  box[4].pointsPtr[1] = &(corners[4]);
  box[4].pointsPtr[2] = &(corners[5]);
  box[4].pointsPtr[3] = &(corners[1]);

   /* clip box */

  clipBox[0].pointsPtr[0] = &(clipCorners[0]);
  clipBox[0].pointsPtr[1] = &(clipCorners[1]);
  clipBox[0].pointsPtr[2] = &(clipCorners[2]);
  clipBox[0].pointsPtr[3] = &(clipCorners[3]);

  clipBox[1].pointsPtr[0] = &(clipCorners[1]);
  clipBox[1].pointsPtr[1] = &(clipCorners[5]);
  clipBox[1].pointsPtr[2] = &(clipCorners[6]);
  clipBox[1].pointsPtr[3] = &(clipCorners[2]);

  clipBox[2].pointsPtr[0] = &(clipCorners[4]);
  clipBox[2].pointsPtr[1] = &(clipCorners[7]);
  clipBox[2].pointsPtr[2] = &(clipCorners[6]);
  clipBox[2].pointsPtr[3] = &(clipCorners[5]);

  clipBox[3].pointsPtr[0] = &(clipCorners[0]);
  clipBox[3].pointsPtr[1] = &(clipCorners[3]);
  clipBox[3].pointsPtr[2] = &(clipCorners[7]);
  clipBox[3].pointsPtr[3] = &(clipCorners[4]);

  clipBox[4].pointsPtr[0] = &(clipCorners[3]);
  clipBox[4].pointsPtr[1] = &(clipCorners[2]);
  clipBox[4].pointsPtr[2] = &(clipCorners[6]);
  clipBox[4].pointsPtr[3] = &(clipCorners[7]);

  clipBox[5].pointsPtr[0] = &(clipCorners[0]);
  clipBox[5].pointsPtr[1] = &(clipCorners[4]);
  clipBox[5].pointsPtr[2] = &(clipCorners[5]);
  clipBox[5].pointsPtr[3] = &(clipCorners[1]);


}


/*****************************************
 *** int keepDrawingViewport() ***
 *****************************************/




int
keepDrawingViewport(void)
{

  XEvent peekEvent;
  int retVal;

  if (XPending(dsply)) {
    XPeekEvent(dsply,&peekEvent);
    if (((peekEvent.type == Expose) &&
         ((peekEvent.xany).window == viewport->viewWindow)) ||
        ((peekEvent.type == Expose) &&
         ((peekEvent.xany).window == viewport->titleWindow)) ||
        ((peekEvent.type == Expose) &&
         ((peekEvent.xany).window == control->controlWindow))) {
      retVal = firstTime;
    } else if ((peekEvent.xbutton.type == ButtonRelease) ||
               ((peekEvent.type == LeaveNotify) && !(followMouse)) ||
               ((peekEvent.type == MotionNotify) && !(followMouse)) ||
               (peekEvent.type == ResizeRequest)) {
      XNextEvent(dsply,&peekEvent);
      followMouse = no;
      retVal = yes;
    } else if ((peekEvent.xany).window == (control->buttonQueue[hideControl]).self) {
      viewport->haveControl = no;
      XUnmapWindow(dsply,control->controlWindow);
      retVal = yes;
    } else {
      retVal = no;
    }
  } else {
    retVal = !followMouse;
  }

  if (writeImage) retVal = yes;
  drawMore = no;
  return(retVal);

}
