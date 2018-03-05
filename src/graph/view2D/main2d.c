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

#define _MAIN2D_C
#include "fricas_c_macros.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <signal.h>

#include "header2.h"
#include "bsdsignal.h"
#include "globals2.h"
#include "bsdsignal.H1"
#include "all_2d.H1"
#include "Gfun.H1"
#include "util.H1"
#include "XSpadFill.H1"


/**********************/
/** global variables **/
/**********************/
Window        rtWindow,viewman;
Display       *dsply;
XFontStruct   *globalFont,
  *buttonFont,
  *headerFont,
  *titleFont,
  *graphFont,
  *unitFont,
  *serverFont;
GC            globalGC1,
  globalGC2,
  anotherGC,
  globGC,
  trashGC,
  controlMessageGC,
  graphGC,
  unitGC,
  processGC;
XGCValues     gcVals;
HashTable     *table;
Colormap      colorMap;

XrmDatabase rDB; /* Resource database */


char          scaleXReport[5],
  scaleYReport[5],
  deltaXReport[5],
  deltaYReport[5],
  *s,
  errorStr[80],
  filename[256], /** For writing viewport data out to a file **/
  *xDefault;     /** used for accessing .XDefaults **/


unsigned long *spadColors;
unsigned long foregroundColor, backgroundColor;

int           followMouse = no,
  viewportKeyNum = 0,
  scrn,
  Socket = 1,
  ack = 1,
  someInt,
  drawMore,
  spadMode=no, /* yes if receiving FriCAS command and calling drawViewport */
  spadDraw=no, /* yes if drawing viewport because of a FriCAS command */
  pointsON  = yes,  /* these would affect the choices in buttons.c */
  connectON = yes,
  splineON  = no,
  axesON    = yes,
  unitsON   = no,
  zoomXON   = yes,
  zoomYON   = yes,
  transXON  = yes,
  transYON  = yes,
  currentGraph = 0,  /* last graph selected */
  queriedGraph = 0,  /* current graph queried */
  picking=0,
  dropping=0,
  viewAloned,  /* if not connected to FriCAS */
  mono,
  totalColors,
  totalSolid,
  totalDithered,
  maxGreyShade,
  totalHues,
  totalSolidShades,
  totalDitheredAndSolids,
  totalShades;
/* totalShades is initially set to totalShadesConst (probably 8).
   If X cannot allocate 8 shades for each hue, totalShades is
   decremented. There is currently only a check for this value
   to be positive. ---> something to add: change over to monochrome
   if totalShades=0. Just modify the spadcolors.c file.
   spadcolors.c has been modified so that it returns the value for
   totalShades. Since the return value had previously been unused,
   a modification in this way ensures continued support of other
   routines calling this function (e.g. hyperDoc stuff). */

viewPoints    *viewport;
controlPanelStruct *control;
jmp_buf jumpFlag;
graphStruct      graphArray[maxGraphs];
graphStateStruct graphStateArray[maxGraphs],
  graphStateBackupArray[maxGraphs];
xPointStruct     xPointsArray[maxGraphs];
float aspectR = 1.0;
/* global ps variables */
int     psInit=no;      /* need to call globalInitPs() each run */
GCptr   GChead=NULL;    /* ptr to head of ps GC linked list */
char PSfilename[280];   /* used as ps file pathname */
char *env_fricas;

int
main(void)
{

  XGCValues       controlGCVals;
  int             i,code;
  view2DStruct    viewData;

  char property[256];
  char *prop = &property[0];
  char *str_type[20];
  XrmValue value;


  /**** Set up display ****/
  if ((dsply = XOpenDisplay(getenv("DISPLAY"))) == NULL)
    fprintf(stderr,"Could not open the display.\n");
  scrn  = DefaultScreen(dsply);
  rtWindow  = RootWindow(dsply,scrn);

  /**** link Xwindows to viewports - X10 feature ****/
  table        = XCreateAssocTable(nbuckets);

  /**** Create FriCAS color map ****/
  totalColors = XInitSpadFill(dsply,scrn,&colorMap,
                              &totalHues,&totalSolidShades,
                              &totalDitheredAndSolids,&totalShades);

  if (totalColors < 0) {
    fprintf(stderr,">>Error: Could not allocate all the necessary colors.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }

  mergeDatabases();


  /*** Determine whether monochrome or color is used ***/
  if (XrmGetResource(rDB, "FriCAS.2D.monochrome", "", str_type,
                     &value) == True) {
    (void) strncpy(prop, value.addr, (int)value.size);
  } else {
    (void) strcpy(prop, "off");
  }

  mono = ((totalSolid == 2) || (strcmp(prop,"on") == 0));

  if (XrmGetResource(rDB, "FriCAS.2D.inverse", "", str_type,
                     &value) == True) {
      (void) strncpy(prop, value.addr, (int)value.size);
  } else {
      (void) strcpy(prop, "off");
  }

  if (mono)
    if (strcmp(prop,"on") == 0) {             /* 0 if equal (inverse video) */
      foregroundColor = WhitePixel(dsply,scrn);
      backgroundColor = BlackPixel(dsply,scrn);
    } else {                                  /* off (no inverse video) */
      foregroundColor = BlackPixel(dsply,scrn);
      backgroundColor = WhitePixel(dsply,scrn);
    }
  else   /* inverse of inverse in color (for some strange reason) */
    if (strcmp(prop,"on") == 0) {         /* 0 if equal (inverse video) */
      foregroundColor = WhitePixel(dsply,scrn);
      backgroundColor = BlackPixel(dsply,scrn);
    } else {                                  /* off (no inverse video) */
      foregroundColor = BlackPixel(dsply,scrn);
      backgroundColor = WhitePixel(dsply,scrn);
    }

  strcpy(PSfilename, "fricas2D.ps");

  /**** Open global fonts ****/
  serverFont = XQueryFont(dsply,XGContextFromGC(DefaultGC(dsply,scrn)));

  if (XrmGetResource(rDB,
                     "FriCAS.2D.messageFont",
                     "FriCAS.2D.Font",
                     str_type, &value) == True)
    (void) strncpy(prop,value.addr,(int)value.size);
  else
    (void) strcpy(prop,messageFontDefault);
  if ((globalFont = XLoadQueryFont(dsply, prop)) == NULL) {
    fprintf(stderr,
            "Warning:  could not get the %s font for messageFont\n",prop);
    globalFont = serverFont;
  }

  if (XrmGetResource(rDB,
                     "FriCAS.2D.buttonFont",
                     "FriCAS.2D.Font",
                     str_type, &value) == True)
    (void) strncpy(prop,value.addr,(int)value.size);
  else
    (void) strcpy(prop,buttonFontDefault);
  if ((buttonFont = XLoadQueryFont(dsply, prop)) == NULL) {
    fprintf(stderr,
            "Warning:  could not get the %s font for buttonFont\n",prop);
    buttonFont = serverFont;
  }

  if (XrmGetResource(rDB,
                     "FriCAS.2D.headerFont",
                     "FriCAS.2D.Font",
                     str_type, &value) == True)
     (void) strncpy(prop,value.addr,(int)value.size);
  else
    (void) strcpy(prop,headerFontDefault);

  if ((headerFont = XLoadQueryFont(dsply, prop)) == NULL) {
    fprintf(stderr,
            "Warning:  could not get the %s font for headerFont\n",prop);
    headerFont = serverFont;
  }

  if (XrmGetResource(rDB,
                     "FriCAS.2D.titleFont",
                     "FriCAS.2D.Font",
                     str_type,&value) == True)
    (void) strncpy(prop,value.addr,(int)value.size);
  else
    (void) strcpy(prop,titleFontDefault);

  if ((titleFont = XLoadQueryFont(dsply, prop)) == NULL) {
    fprintf(stderr,
            "Warning:  could not get the %s font for titleFont\n",prop);
    titleFont = serverFont;
  }

  if (XrmGetResource(rDB,
                     "FriCAS.2D.graphFont",
                     "FriCAS.2D.Font",
                     str_type,&value) == True)
    (void) strncpy(prop,value.addr,(int)value.size);
  else
    (void) strcpy(prop,graphFontDefault);

  if ((graphFont = XLoadQueryFont(dsply, prop)) == NULL) {
    fprintf(stderr,
            "Warning:  could not get the %s font for graphFont\n",prop);
    graphFont = serverFont;
  }

  if (XrmGetResource(rDB,
                     "FriCAS.2D.unitFont",
                     "FriCAS.2D.Font",
                     str_type,&value) == True)
    (void) strncpy(prop,value.addr,(int)value.size);
  else
    (void) strcpy(prop,unitFontDefault);

  if ((unitFont = XLoadQueryFont(dsply, prop)) == NULL) {
     fprintf(stderr,
             "Warning:  could not get the %s font for unitFont\n",prop);
     unitFont = serverFont;
  }


  /**** Create widely used Graphic Contexts ****/
  PSGlobalInit();
  /* must initiate before using any G/PS functions
     need character name: used as postscript GC variable
     need to create ps GCs for all GCs used by drawings in viewWindow */

  /* globalGC1 */

  controlGCVals.foreground = monoColor(axesColorDefault);
  controlGCVals.background = backgroundColor;
  globalGC1 = XCreateGC(dsply,rtWindow,GCForeground | GCBackground ,
                        &controlGCVals);
  carefullySetFont(globalGC1,globalFont);


  /* create the equivalent GCs for ps */
  PSCreateContext(globalGC1, "globalGC1", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

  /* controlMessageGC */

  controlGCVals.foreground = controlMessageColor;
  controlMessageGC = XCreateGC(dsply,rtWindow,GCForeground | GCBackground
                               ,&controlGCVals);
  carefullySetFont(controlMessageGC,globalFont);

  /* globalGC2 */

  controlGCVals.foreground = monoColor(labelColor);
  controlGCVals.background = backgroundColor;
  globalGC2 = XCreateGC(dsply,rtWindow,GCForeground | GCBackground,
                        &controlGCVals);
  carefullySetFont(globalGC2,buttonFont);
  PSCreateContext(globalGC2, "globalGC2", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

  /* trashGC  */

  trashGC = XCreateGC(dsply,rtWindow,0,&controlGCVals);
  carefullySetFont(trashGC,buttonFont);
  PSCreateContext(trashGC, "trashGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

  /* globGC */

  globGC = XCreateGC(dsply,rtWindow,0,&controlGCVals);
  carefullySetFont(globGC,headerFont);
  PSCreateContext(globGC, "globGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

  /* anotherGC  */

  controlGCVals.line_width = colorWidth;
  anotherGC  = XCreateGC(dsply,rtWindow,GCBackground,&controlGCVals);
  carefullySetFont(anotherGC,titleFont);
  PSCreateContext(anotherGC, "anotherGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

  /* processGC */

  gcVals.background = backgroundColor;
  processGC         = XCreateGC(dsply,rtWindow,GCBackground ,&gcVals);
  carefullySetFont(processGC,buttonFont);

  /* graphGC */

  graphGC           = XCreateGC(dsply,rtWindow,GCBackground,&gcVals);
  carefullySetFont(graphGC,graphFont);
  PSCreateContext(graphGC, "graphGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

  /* unitGC */

  unitGC            = XCreateGC(dsply,rtWindow,GCBackground ,&gcVals);
  carefullySetFont(unitGC,unitFont);
  PSCreateContext(unitGC, "unitGC", psNormalWidth, psButtCap,
                  psMiterJoin, psWhite, psBlack);

  /**** Initialize Graph States ****/

  for (i=0; i<maxGraphs; i++) {
    graphStateArray[i].scaleX = 0.9;
    graphStateArray[i].scaleY = 0.9;
    graphStateArray[i].deltaX = 0.0;
    graphStateArray[i].deltaY = 0.0;
    graphStateArray[i].centerX = 0.0;
    graphStateArray[i].centerY = 0.0;
    graphStateArray[i].pointsOn  = yes;
    graphStateArray[i].connectOn = yes;
    graphStateArray[i].splineOn  = no;
    graphStateArray[i].axesOn    = yes;
    graphStateArray[i].unitsOn   = no;
    graphStateArray[i].showing   = no;
    graphStateArray[i].selected  = no;
    graphStateBackupArray[i] = graphStateArray[i];
  }

  /**** Get Data from the Viewport Manager ****/

  i = 123;
  code=check(write(Socket,&i,intSize));

  /* Check if I am getting stuff from FriCAS or, if I am viewAlone. */
  readViewman(&viewAloned,intSize);
  readViewman(&viewData,sizeof(view2DStruct));
  readViewman(&i,intSize);

  if (!(viewData.title = (char *)malloc(i))) {
    fprintf(stderr,
            "ERROR: Ran out of memory trying to receive the title.\n");
    exitWithAck(RootWindow(dsply,scrn),Window,-1);
  }
  readViewman(viewData.title,i);

  for (i=0; i<maxGraphs; i++) {
    readViewman(&(graphArray[i].key),intSize);
    if (graphArray[i].key) {            /** this graph slot has data **/
      getGraphFromViewman(i);
    } /* if graph exists (graphArray[i].key is not zero) */
  } /* for i in graphs */

  viewport = makeView2D(&viewData);
  control = viewport->controlPanel;

  bsdSignal(SIGTERM,goodbye,DontRestartSystemCalls);

  /* send acknowledgement to viewport manager */
  i = 345;
  check(write(Socket,&(viewport->viewWindow),sizeof(Window)));

  processEvents();

  goodbye(-1);
  return(0);  /* control never reaches here but compiler complains */
} /* main() */

void
mergeDatabases(void)
{
  /* using global
     rDB
     dsply
     */
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

  if (XResourceManagerString(dsply) != NULL)
    serverDB = XrmGetStringDatabase(XResourceManagerString(dsply));
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
  else
    (void) strcpy (filename,getenv ("XENVIRONMENT"));

  homeDB = XrmGetFileDatabase(filename);
  XrmMergeDatabases(homeDB,&rDB);
}
