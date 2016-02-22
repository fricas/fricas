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

#define _LIGHTING3D_C
#include "fricas_c_macros.h"

#include <math.h>
#include <string.h>

#include "light11.bitmap"
#include "light11.mask"


#include "header.h"
#include "static.h"
#include "draw.h"
#include "cpanel.h"
#include "volume.h"

#include "Gfun.H1"
#include "XSpadFill.H1"
#include "all_3d.H1"

#define ucharp_to_charp(x) ((char *)x)

#define lightMASK ExposureMask
#define lightCursorForeground lightingTitleColor
#define lightCursorBackground foregroundColor

#define lightFontHeight (lightingFont->max_bounds.ascent+lightingFont->max_bounds.descent)

#define lightingAxesColor  monoColor(52)
#define lightingLabelColor monoColor(12)
#define lightingBoxColor   monoColor(138)
#define lightingLightColor monoColor(7)
#define lightingTitleColor monoColor(69)
#define lightingButtonColor monoColor(140)
#define lightingTransColor monoColor(140)
#define lightingTransArrowColor monoColor(100)
#define lightingTransLabelColor monoColor(207)

#define lightingAxesSize 175
#define lightingAxesX    61
#define lightingAxesY    28

#define lightAxesScale 110      /* the extent of the axes in object space */
#define lightScale 0.63         /* projected scale factor */

#define arrowHead (control->buttonQueue[lightTranslucent].buttonX + 5)
static viewTriple point0 = {0,0,0};


/***************************
 * int makeLightingPanel() *
 ***************************/

int
makeLightingPanel(void)
{

  int i;
  XSetWindowAttributes cwAttrib, controlAttrib;
  XSizeHints sizehint;
  Pixmap lightbits,lightmask;
  XColor foreColor, backColor;

  lightbits = XCreateBitmapFromData(dsply,rtWindow,
                                    ucharp_to_charp(lightBitmap_bits),
                                    lightBitmap_width,lightBitmap_height);
  lightmask = XCreateBitmapFromData(dsply,rtWindow,
                                    ucharp_to_charp(lightMask_bits),
                                    lightMask_width,lightMask_height);
  cwAttrib.background_pixel = backgroundColor;
  cwAttrib.border_pixel = foregroundColor;
  cwAttrib.event_mask = lightMASK;
  cwAttrib.colormap = colorMap;
  cwAttrib.override_redirect = overrideManager;
  foreColor.pixel = lightCursorForeground;
  XQueryColor(dsply,colorMap,&foreColor);
  backColor.pixel = lightCursorBackground;
  XQueryColor(dsply,colorMap,&backColor);
  cwAttrib.cursor = XCreatePixmapCursor(dsply,lightbits,lightmask,
                                        &foreColor,&backColor,
                                        lightBitmap_x_hot,lightBitmap_y_hot);
  lightingWindow = XCreateWindow(dsply,control->controlWindow,
                                 -3,-3,controlWidth,controlHeight,3,
                                 CopyFromParent,InputOutput,CopyFromParent,
                                 controlCreateMASK,&cwAttrib);

  sizehint.flags  = USPosition | USSize;
  sizehint.x      = 0;
  sizehint.y      = 0;
  sizehint.width  = controlWidth;
  sizehint.height = controlHeight;
          /*** the None stands for icon pixmap. ***/
  XSetNormalHints(dsply,lightingWindow,&sizehint);
  XSetStandardProperties(dsply,lightingWindow,"Lighting Panel 3D",
                         "Lighting Panel",None,NULL,0,&sizehint);

      /*** lighting axes window ***/
  cwAttrib.event_mask = 0;
  lightingAxes = XCreateWindow(dsply,lightingWindow,
                               lightingAxesX,lightingAxesY,
                               lightingAxesSize,lightingAxesSize,
                               0,CopyFromParent,InputOutput,CopyFromParent,
                               controlCreateMASK,&cwAttrib);

  sizehint.flags  = USPosition | USSize;
  sizehint.x      = lightingAxesX;
  sizehint.y      = lightingAxesY;
  sizehint.width  = lightingAxesSize;
  sizehint.height = lightingAxesSize;
          /*** the None stands for icon pixmap ***/
  XSetNormalHints(dsply,lightingAxes,&sizehint);
  XSetStandardProperties(dsply,lightingAxes,"Lighting Axes","Lighting Axes",
                         None,NULL,0,&sizehint);
  XMapWindow(dsply,lightingAxes);

    /*** draw lighting buttons ***/
  initLightButtons(control->buttonQueue);
/*
  controlAttrib.event_mask = (control->buttonQueue[lightingButtonsStart]).mask;
  (control->buttonQueue[lightingButtonsStart]).self =
                  XCreateWindow(dsply, lightingWindow,
                    (control->buttonQueue[lightingButtonsStart]).buttonX,
                    (control->buttonQueue[lightingButtonsStart]).buttonY,
                    (control->buttonQueue[lightingButtonsStart]).buttonWidth,
                    (control->buttonQueue[lightingButtonsStart]).buttonHeight,
                    0,0,InputOnly,CopyFromParent,
                    buttonCreateMASK,&controlAttrib);
  XMakeAssoc(dsply,table,(control->buttonQueue[lightingButtonsStart]).self,
             &((control->buttonQueue[lightingButtonsStart]).buttonKey));
  XMapWindow(dsply,(control->buttonQueue[lightingButtonsStart]).self);
*/
  for (i=(lightingButtonsStart + 1); i<(lightingButtonsEnd); i++) {
    controlAttrib.event_mask = (control->buttonQueue[i]).mask;
    (control->buttonQueue[i]).self =
                    XCreateWindow(dsply,lightingWindow,
                      (control->buttonQueue[i]).buttonX,
                      (control->buttonQueue[i]).buttonY,
                      (control->buttonQueue[i]).buttonWidth,
                      (control->buttonQueue[i]).buttonHeight,
                      0,0,InputOnly,CopyFromParent,
                      buttonCreateMASK,&controlAttrib);
    XMakeAssoc(dsply,table,(control->buttonQueue[i]).self,
               &((control->buttonQueue[i]).buttonKey));
    XMapWindow(dsply,(control->buttonQueue[i]).self);
  }

     /* assign global direction variables for light projections */
  sinTheta  = sin(-viewport->theta);
  cosTheta  = cos(-viewport->theta);
  sinPhi    = sin(viewport->phi);
  cosPhi    = cos(viewport->phi);

  return(0);

}  /* makeLightingPanel() */

/***************************
 * void drawLightingAxes() *
 ***************************/

void
drawLightingAxes(void)
{

  XWindowAttributes laInfo;
  int i,xCenter,yCenter;
  float Px0,Py0;
  int vPx0,vPy0,vPx1,vPy1;
  viewTriple pointX,pointY,pointXY,pointXYZ;

  XGetWindowAttributes(dsply,lightingAxes,&laInfo);
  XClearWindow(dsply,lightingAxes);
  xCenter = laInfo.width / 2;
  yCenter = laInfo.height / 2;

  sinTheta = sin(-viewport->theta);
  cosTheta = cos(-viewport->theta);
  sinPhi   = sin(viewport->phi);
  cosPhi   = cos(viewport->phi);

  GSetForeground(lightingGC,(float)monoColor(buttonColor),Xoption);
  for (i=0; i < 3; i++) {
    Px0 = proj2PX(axes[i][0],axes[i][1]);
    Py0 = proj2PY(axes[i][0],axes[i][1],axes[i][2]);
    vPx0 =                   Px0 * lightScale + xCenter;
    vPy0 = laInfo.height - (Py0 * lightScale + yCenter);
    Px0 = proj2PX(axes[i][3],axes[i][4]);
    Py0 = proj2PY(axes[i][3],axes[i][4],axes[i][5]);
    vPx1 =                   Px0 * lightScale + xCenter;
    vPy1 = laInfo.height - (Py0 * lightScale + yCenter);
    GDrawLine(lightingGC,lightingAxes,vPx0,vPy0,vPx1,vPy1,Xoption);
  }

  GSetForeground(lightingGC,(float)lightingLabelColor,Xoption);
  for (i=0; i < basicScreen; i++) {
    Px0 = proj2PX(labels[i][0],labels[i][1]);
    Py0 = proj2PY(labels[i][0],labels[i][1],labels[i][2]);
    vPx0 =                   Px0 * lightScale + xCenter;
    vPy0 = laInfo.height - (Py0 * lightScale + yCenter);
    Px0 = proj2PX(labels[i][3],labels[i][4]);
    Py0 = proj2PY(labels[i][3],labels[i][4],labels[i][5]);
    vPx1 =                   Px0 * lightScale + xCenter;
    vPy1 = laInfo.height - (Py0 * lightScale + yCenter);
    GDrawLine(lightingGC,lightingAxes,vPx0,vPy0,vPx1,vPy1,Xoption);
  }

  GSetForeground(lightingGC,(float)lightingBoxColor,Xoption);
  pointX.x = tempLightPointer[0] * lightAxesScale;
  pointX.y = 0;
  pointX.z = 0;

  pointY.x = 0;
  pointY.y = tempLightPointer[1] * lightAxesScale;
  pointY.z = 0;

  pointXY.x = tempLightPointer[0] * lightAxesScale;
  pointXY.y = tempLightPointer[1] * lightAxesScale;
  pointXY.z = 0;

  pointXYZ.x = tempLightPointer[0] * lightAxesScale;
  pointXYZ.y = tempLightPointer[1] * lightAxesScale;
  pointXYZ.z = tempLightPointer[2] * lightAxesScale;

  Px0 = proj2PX(pointXY.x,pointXY.y);
  Py0 = proj2PY(pointXY.x,pointXY.y,pointXY.z);
  vPx0 =                   Px0 * lightScale + xCenter;
  vPy0 = laInfo.height - (Py0 * lightScale + yCenter);

  Px0 = proj2PX(pointX.x,pointX.y);
  Py0 = proj2PY(pointX.x,pointX.y,pointX.z);
  vPx1 =                   Px0 * lightScale + xCenter;
  vPy1 = laInfo.height - (Py0 * lightScale + yCenter);
  GDrawLine(lightingGC,lightingAxes,vPx0,vPy0,vPx1,vPy1,Xoption);

  Px0 = proj2PX(pointY.x,pointY.y);
  Py0 = proj2PY(pointY.x,pointY.y,pointY.z);
  vPx1 =                   Px0 * lightScale + xCenter;
  vPy1 = laInfo.height - (Py0 * lightScale + yCenter);
  GDrawLine(lightingGC,lightingAxes,vPx0,vPy0,vPx1,vPy1,Xoption);

  Px0 = proj2PX(pointXYZ.x,pointXYZ.y);
  Py0 = proj2PY(pointXYZ.x,pointXYZ.y,pointXYZ.z);
  vPx1 =                   Px0 * lightScale + xCenter;
  vPy1 = laInfo.height - (Py0 * lightScale + yCenter);
  GDrawLine(lightingGC,lightingAxes,vPx0,vPy0,vPx1,vPy1,Xoption);

  GSetForeground(lightingGC,(float)lightingLightColor,Xoption);
  Px0 = proj2PX(point0.x,point0.y);
  Py0 = proj2PY(point0.x,point0.y,point0.z);
  vPx0 =                   Px0 * lightScale + xCenter;
  vPy0 = laInfo.height - (Py0 * lightScale + yCenter);
  GDrawLine(lightingGC,lightingAxes,vPx0,vPy0,vPx1,vPy1,Xoption);

}   /* drawLightingAxes */


/******************************
 * void drawLightTransArrow() *
 ******************************/

void
drawLightTransArrow(void)
{

  int i;
  float f;

  /*** Draw the intensity potentiometer window. ***/
  XClearArea(dsply,lightingWindow,
             (control->buttonQueue[lightTranslucent]).buttonX,
             (control->buttonQueue[lightTranslucent]).buttonY-5,
             (control->buttonQueue[lightTranslucent]).buttonWidth,
             (control->buttonQueue[lightTranslucent]).buttonHeight+10,
             False);
  GDrawLine(controlMessageGC,lightingWindow,
            (control->buttonQueue[lightTranslucent]).buttonX,
            (control->buttonQueue[lightTranslucent]).buttonY,
            (control->buttonQueue[lightTranslucent]).buttonX,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).buttonHeight,Xoption);
  GDrawLine(controlMessageGC,lightingWindow,
            (control->buttonQueue[lightTranslucent]).buttonX + 1,
            (control->buttonQueue[lightTranslucent]).buttonY,
            (control->buttonQueue[lightTranslucent]).buttonX +
            (control->buttonQueue[lightTranslucent]).buttonWidth * 3/10,
            (control->buttonQueue[lightTranslucent]).buttonY,Xoption);
  GDrawLine(controlMessageGC,lightingWindow,
            (control->buttonQueue[lightTranslucent]).buttonX + 1,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).yHalf/2,
            (control->buttonQueue[lightTranslucent]).buttonX +
            (control->buttonQueue[lightTranslucent]).buttonWidth * 2/10,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).yHalf/2,Xoption);
  GDrawLine(controlMessageGC,lightingWindow,
            (control->buttonQueue[lightTranslucent]).buttonX + 1,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).yHalf,
            (control->buttonQueue[lightTranslucent]).buttonX +
            (control->buttonQueue[lightTranslucent]).buttonWidth * 3/10,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).yHalf,Xoption);
  GDrawLine(controlMessageGC,lightingWindow,
            (control->buttonQueue[lightTranslucent]).buttonX + 1,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).buttonHeight*3/4,
            (control->buttonQueue[lightTranslucent]).buttonX +
            (control->buttonQueue[lightTranslucent]).buttonWidth * 2/10,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).buttonHeight*3/4,Xoption);
  GDrawLine(controlMessageGC,lightingWindow,
            (control->buttonQueue[lightTranslucent]).buttonX + 1,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).buttonHeight,
            (control->buttonQueue[lightTranslucent]).buttonX +
            (control->buttonQueue[lightTranslucent]).buttonWidth * 3/10,
            (control->buttonQueue[lightTranslucent]).buttonY +
            (control->buttonQueue[lightTranslucent]).buttonHeight,Xoption);

  /*** Draw the intensity selection arrow ***/
  GSetForeground(lightingGC,(float)lightingTransArrowColor,Xoption);

  f = (control->buttonQueue[lightTranslucent].buttonY +
       control->buttonQueue[lightTranslucent].buttonHeight) -
       (tempLightIntensity *
        control->buttonQueue[lightTranslucent].buttonHeight);
  i = f;
  GDrawLine(lightingGC, lightingWindow, arrowHead + 10, i,
            arrowHead + 22, i + 2, Xoption);
  GDrawLine(lightingGC, lightingWindow, arrowHead + 22, i + 2,
            arrowHead + 22, i - 2, Xoption);
  GDrawLine(lightingGC, lightingWindow, arrowHead + 22, i - 2,
            arrowHead + 10, i, Xoption);

}  /* drawLightTransArrow() */





/****************************
 * void drawLightingPanel() *
 ****************************/

void
drawLightingPanel(void)
{

  char *s;
  int i,strlength;

  /* Draw border lines to separate the lighting window, potentiometers,
     and button regions of the lightng subpanel. */
  GSetForeground(trashGC,(float)foregroundColor,Xoption);
  GSetLineAttributes(trashGC,3,LineSolid,CapButt,JoinMiter,Xoption);
  GDrawLine(trashGC, lightingWindow, 0,  potA, controlWidth, potA, Xoption);

  GSetLineAttributes(trashGC,2,LineSolid,CapButt,JoinMiter,Xoption);
  GDrawLine(trashGC, lightingWindow, 0, lightB, controlWidth, lightB, Xoption);
  GDrawLine(trashGC, lightingWindow, 0, lightPotA, controlWidth,
            lightPotA, Xoption);
  GDrawLine(trashGC, lightingWindow, 0, lightPotB, controlWidth,
            lightPotB, Xoption);
  GDrawLine(trashGC, lightingWindow, lightTransL, lightPotA,
            lightTransL, lightPotB, Xoption);

  writeControlTitle(lightingWindow);
  s = "Lighting Control Panel";
  strlength = strlen(s);
  GSetForeground(anotherGC,(float)lightingTitleColor,Xoption);
  GDrawString(anotherGC, lightingWindow,
              centerX(anotherGC, s, strlength, controlWidth),
              lightB+18, s, strlength, Xoption);

  for (i=lightingButtonsStart; i<(lightingButtonsEnd); i++) {
    switch (i) {
    case lightMove:
      GSetForeground(lightingGC,(float)lightingButtonColor,Xoption);
      GDraw3DButtonOut(lightingGC,lightingWindow,
                     (control->buttonQueue[i]).buttonX,
                     (control->buttonQueue[i]).buttonY,
                     (control->buttonQueue[i]).buttonWidth,
                     (control->buttonQueue[i]).buttonHeight,Xoption);
      GSetForeground(lightingGC,(float)monoColor(buttonColor),Xoption);
      GDrawLine(lightingGC,lightingWindow,
                (control->buttonQueue[i]).buttonX +
                (control->buttonQueue[i]).xHalf,
                (control->buttonQueue[i]).buttonY,
                (control->buttonQueue[i]).buttonX +
                (control->buttonQueue[i]).xHalf,
                (control->buttonQueue[i]).buttonY +
                2*(control->buttonQueue[i]).yHalf,Xoption);
      GDrawLine(lightingGC,lightingWindow,
                (control->buttonQueue[i]).buttonX,
                (control->buttonQueue[i]).buttonY +
                (control->buttonQueue[i]).yHalf,
                (control->buttonQueue[i]).buttonX +
                2*(control->buttonQueue[i]).xHalf,
                (control->buttonQueue[i]).buttonY +
                (control->buttonQueue[i]).yHalf,Xoption);
      break;
    case lightMoveXY:
      GSetForeground(lightingGC,(float)lightingButtonColor,Xoption);
      GDraw3DButtonOut(lightingGC,lightingWindow,
                     (control->buttonQueue[i]).buttonX,
                     (control->buttonQueue[i]).buttonY,
                     (control->buttonQueue[i]).buttonWidth,
                     (control->buttonQueue[i]).buttonHeight,Xoption);
      GSetForeground(lightingGC,(float)monoColor(buttonColor),Xoption);
      GDrawLine(lightingGC,lightingWindow,
                (control->buttonQueue[i]).buttonX +
                (control->buttonQueue[i]).xHalf,
                (control->buttonQueue[i]).buttonY,
                (control->buttonQueue[i]).buttonX +
                (control->buttonQueue[i]).xHalf,
                (control->buttonQueue[i]).buttonY +
                2*(control->buttonQueue[i]).yHalf,Xoption);
      GDrawLine(lightingGC,lightingWindow,
                (control->buttonQueue[i]).buttonX,
                (control->buttonQueue[i]).buttonY +
                (control->buttonQueue[i]).yHalf,
                (control->buttonQueue[i]).buttonX +
                2*(control->buttonQueue[i]).xHalf,
                (control->buttonQueue[i]).buttonY +
                (control->buttonQueue[i]).yHalf,Xoption);
      break;
    case lightMoveZ:
      GSetForeground(lightingGC,(float)lightingButtonColor,Xoption);
      GDraw3DButtonOut(lightingGC,lightingWindow,
                     (control->buttonQueue[i]).buttonX,
                     (control->buttonQueue[i]).buttonY,
                     (control->buttonQueue[i]).buttonWidth,
                     (control->buttonQueue[i]).buttonHeight,Xoption);
      GSetForeground(lightingGC,(float)monoColor(buttonColor),Xoption);
      GDrawLine(lightingGC,lightingWindow,
                (control->buttonQueue[i]).buttonX +
                (control->buttonQueue[i]).xHalf,
                (control->buttonQueue[i]).buttonY,
                (control->buttonQueue[i]).buttonX +
                (control->buttonQueue[i]).xHalf,
                (control->buttonQueue[i]).buttonY +
                2*(control->buttonQueue[i]).yHalf,Xoption);
      GDrawLine(lightingGC,lightingWindow,
                (control->buttonQueue[i]).buttonX +
                (control->buttonQueue[i]).xHalf -
                (control->buttonQueue[i]).xHalf/2,
                (control->buttonQueue[i]).buttonY +
                (control->buttonQueue[i]).yHalf,
                (control->buttonQueue[i]).buttonX +
                (control->buttonQueue[i]).xHalf +
                (control->buttonQueue[i]).xHalf/2,
                (control->buttonQueue[i]).buttonY +
                (control->buttonQueue[i]).yHalf,Xoption);
      break;
    case lightTranslucent:
      drawLightTransArrow();
      break;
    default:
      GDraw3DButtonOut(lightingGC,lightingWindow,
                     (control->buttonQueue[i]).buttonX,
                     (control->buttonQueue[i]).buttonY,
                     (control->buttonQueue[i]).buttonWidth,
                     (control->buttonQueue[i]).buttonHeight,Xoption);
      s = (control->buttonQueue[i]).text;
      strlength = strlen(s);
      GSetForeground(trashGC,
                     (float)monoColor((control->buttonQueue[i]).textColor),Xoption);
      GDrawString(trashGC, lightingWindow,
                  (control->buttonQueue[i]).buttonX +
                  centerX(processGC,s,strlength,
                          (control->buttonQueue[i]).buttonWidth),
                  (control->buttonQueue[i]).buttonY +
                  centerY(processGC,(control->buttonQueue[i]).buttonHeight),
                  s,strlen(s),Xoption);
    }  /* switch */
  }  /* for i in control->buttonQueue */

  GSetForeground(lightingGC,(float)monoColor(labelColor),Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightMoveXY].buttonX +
              control->buttonQueue[lightMoveXY].buttonWidth + 3,
              control->buttonQueue[lightMoveXY].buttonY +
              control->buttonQueue[lightMoveXY].yHalf,
              "x",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightMoveXY].buttonX +
              control->buttonQueue[lightMoveXY].xHalf - 2,
              control->buttonQueue[lightMoveXY].buttonY - 4,
              "y",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightMoveZ].buttonX +
              control->buttonQueue[lightMoveZ].xHalf - 2,
              control->buttonQueue[lightMoveZ].buttonY - 4,
              "z",1,Xoption);

  /** Draw the title for the intensity potentiometer. */
  GSetForeground(lightingGC,(float)lightingTransColor,Xoption);

  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY,
              "I",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY +
              lightFontHeight,
              "n",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY +
              lightFontHeight*2,
              "t",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY +
              lightFontHeight*3,
              "e",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY +
              lightFontHeight*4,
              "n",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY +
              lightFontHeight*5,
              "s",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY +
              lightFontHeight*6,
              "i",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY +
              lightFontHeight*7,
              "t",1,Xoption);
  GDrawString(lightingGC,lightingWindow,
              control->buttonQueue[lightTranslucent].buttonX +
              control->buttonQueue[lightTranslucent].buttonWidth + 3,
              control->buttonQueue[lightTranslucent].buttonY +
              lightFontHeight*8,
              "y",1,Xoption);
  drawLightingAxes();
  drawLightTransArrow();

}  /* drawLightingPanel */
