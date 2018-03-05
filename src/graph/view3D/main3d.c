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
#include "strutil.h"

                                 /**********************/
                                 /** global variables **/
                                 /**********************/

unsigned long * spadColors;
Display         *dsply;
int             scrn;
XFontStruct     *globalFont, *buttonFont, *headerFont, *titleFont, *graphFont,
                *lightingFont, *volumeFont, *quitFont, *saveFont,*serverFont;
char            scaleReport[5];
char            deltaXReport[5], deltaYReport[5];
int             followMouse=no,
                viewportKeyNum=0;
Window          rtWindow;
GC              globalGC1, globalGC2, anotherGC, globGC, trashGC,
                controlMessageGC, lightingGC, volumeGC, quitGC,
                saveGC, graphGC, componentGC, opaqueGC, renderGC;
XSizeHints      viewSizeHints;
HashTable       *table;
Colormap        colorMap;
int             Socket=1, ack=1;

GC              processGC;
viewPoints      *viewport;
controlPanelStruct      *control;
XGCValues       gcVals;
char            *s;
int             someInt;

/* check /usr/include/X11 for current implementation of
   pixels (e.g. BlackPixel()) */
unsigned long   foregroundColor, backgroundColor;

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

int             mono, totalColors, totalSolid, totalDithered, totalHues,
                totalSolidShades, totalDitheredAndSolids,totalShades;

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

                /** for freeing up points created frrom split polygons **/
viewTriple      *splitPoints;
int             resMax=0;  /* number of points in the split point resevoir */

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
int             maxGreyShade=0;
                /** events from the viewport manager **/
char            propertyName[14];
char            propertyBuffer[256];

                /* global ps variables */
int             psInit=no;      /* need to call globaInitPs() each run */
GCptr           GChead=NULL;    /* ptr to head of ps GC linked list */
char PSfilename[280];    /* used for ps file path */
char *env_fricas;

               /** Resource database **/
XrmDatabase rDB;

               /** variables used for smooth shading **/
int    smoothError = no;
Pixmap viewmap;
int    viewmap_valid = 0;
float  Cspec = 0.30;
float  Cdiff = 0.4;
float  Camb = 0.3;
float  coeff = 35.0;
float  saturation = 0.8;
int    smoothHue;
int    smoothConst = 50;



int
the_handler(Display *display,XErrorEvent *event)
{
  char buffer[512];
  XGetErrorText(display,event->error_code,buffer,511);
  fprintf(stderr,"%s\n",buffer);
  return(0);
}

int
main(void)
{

  XGCValues     controlGCVals;
  int           i, code;

  char property[256];
  char *prop = &property[0];
  char *str_type[20];
  XrmValue value;


  /**** Global inits ****/
  splitPoints = NIL(viewTriple);

  /**** Set up display ****/
  if ((dsply = XOpenDisplay(getenv("DISPLAY"))) == NULL)
    {fprintf(stderr,"Could not open display.\n");exit (-1);}
  scrn = DefaultScreen(dsply);
  rtWindow = RootWindow(dsply,scrn);
  XSetErrorHandler(the_handler);
  /*   XSynchronize(dsply,False); */

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


  mergeDatabases();

  /*** Determine whether monochrome or color is used ***/
  if (XrmGetResource(rDB, "FriCAS.3D.monochrome", "", str_type,
                     &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
      (void) strcpy(prop, "off");
  }


  mono = ((totalSolid == 2) || (strcmp(prop,"on") == 0));
  if (mono) maxGreyShade=XInitShades(dsply,scrn) ;

  if (XrmGetResource(rDB, "FriCAS.3D.inverse", "", str_type,
                     &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
      (void) strcpy(prop, "off");
  }

  if (mono) {
    if (strcmp(prop,"on") == 0) {  /* 0 if equal - inverse video */
      foregroundColor = white;
      backgroundColor = black;
    } else {  /* off - no inverse video */
      foregroundColor = black;
      backgroundColor = white;
    }
  } else {  /* inverse of inverse in color (for some strange reason) */
    if (strcmp(prop,"on") == 0) {  /* 0 if equal - inverse video */
      foregroundColor = white;
      backgroundColor = black;
    } else {  /* off - no inverse video */
      foregroundColor = black;
      backgroundColor = white;
    }
  }

  strcpy(PSfilename, "fricas3D.ps");

  XSync(dsply,0);

  /**** Open global fonts ****/
   serverFont = XQueryFont(dsply,XGContextFromGC(DefaultGC(dsply,scrn)));

  if (XrmGetResource(rDB, "FriCAS.3D.messageFont", "FriCAS.3D.Font",
                     str_type, &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
      (void) strcpy(prop, messageFontDefault);
  }
  if ((globalFont = XLoadQueryFont(dsply, prop)) == NULL) {
     fprintf(stderr, "Warning:  could not get the %s font for messageFont\n",prop);
     globalFont = serverFont;
  }

  if (XrmGetResource(rDB, "FriCAS.3D.buttonFont", "FriCAS.3D.Font",
                     str_type, &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
      (void) strcpy(prop, buttonFontDefault);
  }
  if ((buttonFont = XLoadQueryFont(dsply, prop)) == NULL) {
     fprintf(stderr, "Warning:  could not get the %s font for buttonFont\n",prop);
     buttonFont = serverFont;
  }

  if (XrmGetResource(rDB, "FriCAS.3D.headerFont", "FriCAS.3D.Font",
                     str_type, &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
      (void) strcpy(prop, headerFontDefault);
  }
  if ((headerFont = XLoadQueryFont(dsply, prop)) == NULL) {
     fprintf(stderr, "Warning:  could not get the %s font for headerFont\n",prop);
     headerFont = serverFont;
  }

  if (XrmGetResource(rDB, "FriCAS.3D.titleFont", "FriCAS.3D.Font",
                     str_type, &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
     (void) strcpy(prop, titleFontDefault);
  }
  if ((titleFont = XLoadQueryFont(dsply, prop)) == NULL) {
     fprintf(stderr, "Warning:  could not get the %s font for titleFont\n",prop);
     titleFont = serverFont;
  }

  if (XrmGetResource(rDB, "FriCAS.3D.lightingFont", "FriCAS.3D.Font",
                     str_type, &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
      (void) strcpy(prop, lightingFontDefault);
  }
  if ((lightingFont = XLoadQueryFont(dsply, prop)) == NULL) {
     fprintf(stderr, "Warning:  could not get the %s font for lightingFont\n",prop);
     lightingFont = serverFont;
  }

  if (XrmGetResource(rDB, "FriCAS.3D.volumeFont", "FriCAS.3D.Font",
                     str_type, &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
      (void) strcpy(prop, volumeFontDefault);
  }
  if ((volumeFont = XLoadQueryFont(dsply, prop)) == NULL) {
     fprintf(stderr, "Warning:  could not get the %s font for volumeFont\n",prop);
     volumeFont = serverFont;

  }
 /**** Create widely used Graphic Contexts ****/


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

  /**** Get Data from the Viewport Manager ****/

  i    = 123;   /* Used in viewman, what is this for? */
  code = check(write(Socket,&i,intSize));

  /* Check if I am getting stuff from FriCAS or, if I am viewAlone. */
  readViewman(&viewAloned,intSize);
  readViewman(&viewData,sizeof(view3DStruct));
  readViewman(&i,intSize);

  if (!(viewData.title = (char *)saymem("main.c",i,sizeof(char)))) {
    fprintf(stderr,"VIEW3D: Fatal Error>> Ran out of memory trying to receive\
                         the title.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }
  readViewman(viewData.title,i);

  readViewman(&(viewData.lightVec[0]),floatSize);
  readViewman(&(viewData.lightVec[1]),floatSize);
  readViewman(&(viewData.lightVec[2]),floatSize);

  viewData.scaleDown = yes;

  switch (viewData.typeOf3D) {

  /* Currently, the view3DType information doesn't get sent from
     FriCAS - all surfaces are alike regardless of how they
     were created. We may revert back to receiving this information
     in case we want to take advantage of certain properties of
     certain surfaces (e.g. z=f(x,y)). */

        case view3DType:
        case viewTubeType:
                viewport = make3DComponents();
                viewData.box = no;
                viewData.pointSize = 3;
                break;
  }; /* switch typeOf3D */


  /*************************************************
   ** Do some temporary assignments that would    **
   ** later be coded in the makeViewport routines **
   ** when the corresponding code has been put    **
   ** into the viewAlone, viewman and spad files. **
   *************************************************/

  viewData.distortX     = viewData.distortY = viewData.distortZ = 1;
  viewData.clipPlane    = clipPlaneMin;
  viewData.clipStuff    = yes;

  xClipMinN = yClipMinN = zClipMinN = 0.0;
  xClipMaxN = yClipMaxN = zClipMaxN = 1.0;

  control = viewport->controlPanel;

  bsdSignal(SIGTERM,goodbye,DontRestartSystemCalls);
  bsdSignal(SIGSEGV,goodbye,DontRestartSystemCalls);

  /** send acknowledgement to viewport manager**/
  i = 345;

  fricas_sprintf_to_buf1(errorStr, "%s",
                         "sending window info to viewport manager");
  check(write(Socket,&(viewport->viewWindow),sizeof(Window)));

  viewmap = XCreatePixmap(dsply,viewport->viewWindow,
                          vwInfo.width,vwInfo.height,
                          DisplayPlanes(dsply,scrn));
  viewmap_valid = 1;

  processEvents();

  goodbye(-1);
  return(0); /* control never gets here but compiler complains */
}     /* main() */



void
mergeDatabases(void)
{
  XrmDatabase homeDB,serverDB,applicationDB;
  char filenamebuf[1024];
  char *filename = &filenamebuf[0];
  char *classname = "FriCAS";
  char name[255];

  (void) XrmInitialize();
  (void) strcpy(name, "/usr/lib/X11/app-defaults/");
  (void) strcat(name, classname);
  applicationDB = XrmGetFileDatabase(name);
  (void) XrmMergeDatabases(applicationDB, &rDB);

  if (XResourceManagerString(dsply) != NULL){
    serverDB = XrmGetStringDatabase(XResourceManagerString(dsply));
  }
  else {
    (void) strcpy(filename,getenv("HOME"));
    (void) strcat(filename,"/.Xdefaults");
    serverDB = XrmGetFileDatabase(filename);
  }
  XrmMergeDatabases(serverDB,&rDB);
  if ( getenv ("XENVIRONMENT") == NULL) {
    int len;
    (void) strcpy(filename,getenv("HOME"));
    (void) strcat(filename,"/.Xdefaults-");
    len = strlen(filename);

    (void) gethostname(filename+len,1024-len);

  }
  else {
    (void) strcpy (filename,getenv ("XENVIRONMENT"));
  }
  homeDB = XrmGetFileDatabase(filename);
  XrmMergeDatabases(homeDB,&rDB);
}
