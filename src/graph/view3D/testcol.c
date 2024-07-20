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

#define _MAIN3D_C
#include "fricas_c_macros.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <math.h>

#include "header.h"
#include "cpanel.h"
#include "process.h"
#include "bsdsignal.h"

#include "bsdsignal.H1"
#include "util.H1"
#include "Gfun.H1"
#include "XSpadFill.H1"
#include "XShade.H1"
#include "all_3d.H1"

viewPoints      *viewport;
GCptr           GChead=NULL;    /* ptr to head of ps GC linked list */
char            *PSfilename;    /* output file name used in user directory */
int             psInit=no;      /* need to call globaInitPs() each run */
char            * env_fricas;   /* used for ps file paths */
int             maxGreyShade=0;
GC              globalGC1, globalGC2, anotherGC, globGC, trashGC,
                controlMessageGC, lightingGC, volumeGC, quitGC, processGC,
                saveGC, graphGC, componentGC, opaqueGC, renderGC;
unsigned long   foregroundColor, backgroundColor;
int             Socket=1, ack=1;
Colormap        colorMap;
viewTriple      *splitPoints;
Display         *dsply;
int             scrn;
Window          rtWindow;
HashTable       *table;
int             mono, totalColors, totalSolid, totalDithered, totalHues,
                totalSolidShades, totalDitheredAndSolids,totalShades;
XFontStruct     *globalFont, *buttonFont, *headerFont, *titleFont, *graphFont,
                *lightingFont, *volumeFont, *quitFont, *saveFont,*serverFont;
XGCValues       gcVals;
unsigned long * spadColors;
float           transform[4][4], transform1[4][4],
                R[4][4], R1[4][4], S[4][4], T[4][4], I[4][4];
float           A[4][4], B[4][4], D[4], E[4][4], F[4], array[4][4];

int             followMouse=no,
                viewportKeyNum=0;
                                 /**********************/
                                 /** global variables **/
                                 /**********************/

char            scaleReport[5];
char            deltaXReport[5], deltaYReport[5];
XSizeHints      viewSizeHints;

GC              processGC;
viewPoints      *viewport;
controlPanelStruct      *control;
char            *s;
int             someInt;

/* check /usr/include/X11 for current implementation of
   pixels (e.g. BlackPixel()) */

           /** totalShades is initially set to totalShadesConst.
               If X cannot allocate 8 shades for each hue, total-
               Shades is decremented. there is currently only a check for
               this value to be positive. --> something to add: change over
               to monochrome if totalShades=0. just modify the spadcolors.c
               file. spadcolors.c has been modified so that it returns the
               value for totalShades. since the return value had previously
               been unused, a modification in this way ensures continued
               support of other routines calling this function (e.g.
               hypertex stuff). **/


int             drawMore;

int             spadMode=no,            /* yes if receiving FriCAS command and
                                           calling drawViewport */
                spadDraw=no;            /* yes if drawing viewport for
                                           a FriCAS command */
int             spadSignalReceived=0;  /* yes if current state is a result of
                                           a signal from FriCAS */
int             inNextEvent=no;         /* true just before a call to
                                           XNextEvent */
jmp_buf         jumpFlag;

char            errorStr[80];

view3DStruct    viewData;

Window          quitWindow, saveWindow;

                /** variables below assume only one viewport per process **/

Window          lightingWindow, lightingAxes;
float           lightPointer[3], tempLightPointer[3];

int             axesXY[3][4];
float           axesZ[3][2];

float           lightIntensity=1.0, tempLightIntensity;
float           backLightIntensity = 1.0;

                /** used for writing viewport info out to a file **/
char            filename[256];


                /** used for draw viewport routines */
float           sinTheta, sinPhi, cosTheta, cosPhi, viewScale,
                viewScaleX, viewScaleY, viewScaleZ, reScale;
int             xCenter, yCenter;

XWindowAttributes       vwInfo;
XWindowAttributes       graphWindowAttrib;

XPoint          *quadMesh;
XImage          *imageX;
int             *xPts;   /* pointer to projected points (x followed by y) */
float           transform[4][4], transform1[4][4],
                R[4][4], R1[4][4], S[4][4], T[4][4], I[4][4];
float           A[4][4], B[4][4], D[4], E[4][4], F[4], array[4][4];


int             scanline, polyCount;
polyList        *scanList[ARRAY_HEIGHT];
float           xleft = (float)0 ,xright = (float)ARRAY_WIDTH;

colorBuffer      cBuffer[ARRAY_WIDTH];
float            zBuffer[ARRAY_WIDTH];

float           zC, dzdx, lum, point_norm[3];
float           intersectColor[2], dcolor;
triple          dpt, dnorm;

                /** eyePoint **/
float           eyePoint[3];

                /** tube stuff **/
XPoint          polygonMesh[20];

                /* bypass the hidden surface algorithm if no rotations, etc */
int             saveFlag=no;
int             firstTime=yes, noTrans = yes, startup = yes;
int             redrawView = no;   /* set to yes when returning from
                                      subpanels */
int             redoColor = no, pixelSetFlag = no, redoDither = no;
int             redoSmooth = no, multiColorFlag = no;

/* In order to set recalc to true (see draw.h) */
int             finishedList=no, zoomed=yes, translated = yes,
                changedIntensity, movingLight = no, writeImage = no,
                rotated=yes, switchedPerspective, changedEyeDistance,
                gotToggle = no;
poly            *quickList;

                /* if not connected to FriCAS */
int             viewAloned;

                /** for drawing the box **/
viewTriple      corners[8], clipCorners[8];
boxSideStruct   box[6], clipBox[6];

                /** for freeing up points created from split polygons **/
int             resMax=0;  /* number of points in the split point reservoir */

                /** view volume stuff **/
Window          volumeWindow;
int             frustrumVertex;
int             doingPanel=CONTROLpanel; /* rewrite titles in proper panel */
int             doingVolume;

int             screenX;  /* global floating point indicating mouse position
                             on frustrum screen */
float           xClipMinN, xClipMaxN,   /* normalized values for
                                           clip volume */
                yClipMinN, yClipMaxN,
                zClipMinN, zClipMaxN,
                clipValue;              /* mouse input */
float           pzMin, pzMax;   /* for a given (theta,phi): calculated in
                                   drawViewport(), used in drawFrustrum() */

                /** B/W shades **/
                /** events from the viewport manager **/
char            propertyName[14];
char            propertyBuffer[256];

                /* global ps variables */

               /** Resource database **/
XrmDatabase rDB;

               /** variables used for smooth shading **/
int    smoothError = no;
Pixmap viewmap;
float  Cspec = 0.30;
float  Cdiff = 0.4;
float  Camb = 0.3;
float  coeff = 35.0;
float  saturation = 0.8;
int    smoothHue;
int    smoothConst = 50;




void
main(void)
{

  XGCValues     controlGCVals;
  int           i, code;

  char property[256];
  char *prop = &property[0];
  char *str_type[20];
  XrmValue value;

  Atom wm_delete_window;
  XColor               foreColor, backColor;
  XSizeHints           titleSizeHints;
  Window               viewTitleWindow, viewGraphWindow;
  XSetWindowAttributes viewAttrib;
  Pixmap spadbits,spadmask;

  /**** Global inits ****/
  splitPoints = NIL(viewTriple);

  /**** Set up display ****/
  if ((dsply = XOpenDisplay(getenv("DISPLAY"))) == NULL)
    {fprintf(stderr,"Could not open display.\n");exit (-1);}
  scrn = DefaultScreen(dsply);
  rtWindow = RootWindow(dsply,scrn);

  /**** link Xwindows to viewports - X10 feature ****/
  table = XCreateAssocTable(nbuckets);

  /**** Create FriCAS color map ****/
  totalShades = 0;
  totalColors = XInitSpadFill(dsply,scrn,&colorMap,
                              &totalHues,&totalSolidShades,
                              &totalDitheredAndSolids,&totalShades);
  if (totalColors < 0) {
    fprintf(stderr,"ERROR: Could not allocate all the necessary colors.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }



  PSGlobalInit();
  /* must initiate before using any G/PS functions */
  /* need character name: used as postscript GC variable */
  /* need to create ps GCs for all GCs used by drawing in viewWindow */

                     /* globalGC1 */
  controlGCVals.foreground  = monoColor(axesColor);
  controlGCVals.background  = backgroundColor;
  globalGC1  = XCreateGC(dsply,rtWindow,GCForeground |
                         GCBackground ,&controlGCVals);
  carefullySetFont(globalGC1,globalFont);
  PSCreateContext(globalGC1, "globalGC1", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

                     /* controlMessageGC */
  controlGCVals.foreground  = controlMessageColor;
  controlGCVals.background  = backgroundColor;
  controlMessageGC      = XCreateGC(dsply,rtWindow,GCForeground |
                                    GCBackground ,&controlGCVals);
  carefullySetFont(controlMessageGC,globalFont);

                     /* globalGC2 */
  controlGCVals.foreground = monoColor(labelColor);
  globalGC2 = XCreateGC(dsply,rtWindow,GCForeground,&controlGCVals);
  carefullySetFont(globalGC2,buttonFont);
  PSCreateContext(globalGC2, "globalGC2", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

                     /* trashGC */
  controlGCVals.function = GXcopy;
  trashGC  = XCreateGC(dsply,rtWindow,0 ,&controlGCVals);
  carefullySetFont(trashGC,buttonFont);
  PSCreateContext(trashGC, "trashGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

                    /* componentGC */
  componentGC  = XCreateGC(dsply,rtWindow,0 ,&controlGCVals);
  carefullySetFont(componentGC,buttonFont);
  PSCreateContext(componentGC, "componentGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

                   /* opaqueGC */
  opaqueGC  = XCreateGC(dsply,rtWindow,0 ,&controlGCVals);
  carefullySetFont(opaqueGC,buttonFont);
  PSCreateContext(opaqueGC, "opaqueGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

                   /* renderGC */
  renderGC  = XCreateGC(dsply,rtWindow,0,&controlGCVals);
  carefullySetFont(renderGC,buttonFont);
  PSCreateContext(renderGC, "renderGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

                  /* globGC */
  globGC = XCreateGC(dsply,rtWindow,0,&controlGCVals);
  carefullySetFont(globGC,headerFont);
  PSCreateContext(globGC, "globGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

                /* anotherGC */
  controlGCVals.line_width = colorWidth;
  anotherGC  = XCreateGC(dsply,rtWindow,GCBackground | GCLineWidth |
                         GCFunction ,&controlGCVals);
  carefullySetFont(anotherGC,titleFont);
  PSCreateContext(anotherGC, "anotherGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

  /* also create one for rendering (grayscale only for now) */
  /* assign arbitrary number to renderGC as 9991 - see header.h */
  PSCreateContext(GC9991, "renderGC", psNormalWidth, psButtCap,
                  psRoundJoin, psWhite, psBlack );


              /* processGC */
  gcVals.background  = backgroundColor;
  processGC = XCreateGC(dsply,rtWindow,GCBackground |
                        GCFillStyle,&gcVals);
  carefullySetFont(processGC,buttonFont);

              /* lightingGC */
  controlGCVals.foreground = monoColor(axesColor);
  controlGCVals.background = backgroundColor;
  lightingGC  = XCreateGC(dsply,rtWindow,GCForeground | GCBackground
                          ,&controlGCVals);
  carefullySetFont(lightingGC,lightingFont);


             /* volumeGC */
  volumeGC = XCreateGC(dsply,rtWindow,GCForeground | GCBackground
                       ,&controlGCVals);
  carefullySetFont(volumeGC,volumeFont);

              /* quitGC */
  quitGC  = XCreateGC(dsply,rtWindow,GCForeground | GCBackground
                      ,&controlGCVals);
  carefullySetFont(quitGC,buttonFont);

              /* saveGC */
  saveGC  = XCreateGC(dsply,rtWindow,GCForeground | GCBackground
                      ,&controlGCVals);
  carefullySetFont(saveGC,buttonFont);


              /* graphGC */
  graphGC  = XCreateGC(dsply,rtWindow,GCForeground | GCBackground
                       ,&controlGCVals);
  carefullySetFont(graphGC,buttonFont);
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

  viewData.title = "untitled";
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

  viewTitleWindow = XCreateWindow(dsply,rtWindow,viewData.vX,viewData.vY,
                                  viewData.vW,viewData.vH,viewBorderWidth+3,
                                  CopyFromParent,InputOutput,CopyFromParent,
                                  viewportTitleCreateMASK,&viewAttrib);

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
  viewSizeHints.y       = titleHeight;
  viewSizeHints.width   = titleSizeHints.width;
  viewSizeHints.height  = titleSizeHints.height-(titleHeight+appendixHeight);
  viewGraphWindow = XCreateWindow(dsply,viewTitleWindow,
                                  viewSizeHints.x,viewSizeHints.y,
                                  viewSizeHints.width,viewSizeHints.height,
                                  viewBorderWidth+3,
                                  CopyFromParent,InputOutput,CopyFromParent,
                                  viewportCreateMASK,&viewAttrib);
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


}     /* main() */
