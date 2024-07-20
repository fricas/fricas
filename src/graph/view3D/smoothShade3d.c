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

#define _SMOOTHSHADE_C
#include "fricas_c_macros.h"

#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "header.h"
#include "draw.h"
#include "volume.h"
#include "mode.h"   /* for #define components */

#include "spadcolors.H1"
#include "Gfun.H1"
#include "util.H1"
#include "XSpadFill.H1"
#include "all_3d.H1"

#define SAFE_VALUE 892347






char
get_cBuffer_axes(int ix)
{
        if( ix >=0 && ix <ARRAY_WIDTH) return (cBuffer[ix].axes);
        return ('0');
}

void
put_cBuffer_axes(int ix,char val)
{
        if( ix >=0 && ix <ARRAY_WIDTH) cBuffer[ix].axes = val;
}

int
get_cBuffer_indx(int ix)
{
        if( ix >=0 && ix <ARRAY_WIDTH) return (cBuffer[ix].indx);
        return (-1);
}

void
put_cBuffer_indx(int ix,int val)
{
        if( ix >=0 && ix <ARRAY_WIDTH) cBuffer[ix].indx = val;
}

void
put_zBuffer(int ix,float val)
{
        if (ix >=0 && ix <ARRAY_WIDTH) zBuffer[ix] = val;
}

float
get_zBuffer(int ix)
{
        return (zBuffer[ix]);
}

void
put_imageX(int ix,char val)
{
  if (ix <=0 && ix <vwInfo.width) imageX->data[ix] = val;
}




/***************************
 * void drawPhongSpan()    *
 *                         *
 * This routine sets the   *
 * buffer values for each  *
 * span of pixels which    *
 * intersect the current   *
 * scanline.               *
 ***************************/
void
drawPhongSpan(triple pt,float N[3],int dFlag)
{
  int                xpixel,hue,shade;
  float              colorindx, col;
  triple             hs;


  /* negative values of xleft and xright have been pushed to machine0 */

  xpixel = (int)xleft;


  while (xpixel <= (int)xright) {
    /* if z is closer to viewer than value in zBuffer continue */
    if ( (zC < get_zBuffer(xpixel)) ) {
      /* get the intensity for current point */
      col = phong(pt,N);
      put_cBuffer_axes(xpixel,'0');
      put_zBuffer(xpixel,zC);
      /* if mono (bw dsply) do black and white semi-random dithering */
      if (mono || viewport->monoOn) {
        if (get_random() < 100.0*exp((double)-1.3*(pi_sq*(col-.2)*(col-.2))))  {
          put_cBuffer_indx(xpixel,black);
        } else {
          put_cBuffer_indx(xpixel,white);
        }
      } else {
        /* glossy shading for one hue else dithered for many hues */
        if (viewport->hueOffset == viewport->hueTop && !smoothError) {
          colorindx = (float)(smoothConst+1) * col;
          if (colorindx > (smoothConst+1)) colorindx = smoothConst+1;
          put_cBuffer_indx(xpixel,XPixelColor((int)colorindx-1));
        } else { /* probabilistic multi-hued dithering */
          hs = norm_dist();
          hue = (int)(intersectColor[0]+hs.x/20.0);
          /* cannot dither out of color map range */
          if (viewport->hueOffset < viewport->hueTop) {
            if (hue < viewport->hueOffset)
              hue = viewport->hueOffset;
            else {
              if (hue > viewport->hueTop)
                hue = viewport->hueTop;
            }
          } else {
            if (hue < viewport->hueTop)
              hue = viewport->hueTop;
            else {
              if (hue > viewport->hueOffset)
                hue = viewport->hueOffset;
            }
          }
          col += hs.y/6.0;  /* perturb intensity */
          if (col > 1.0) put_cBuffer_indx(xpixel,white);
          else {
            if (col < 0.0) put_cBuffer_indx(xpixel,black);
            else {
              shade = (int)(col * 4.0);
              put_cBuffer_indx(xpixel,XSolidColor(hue,shade));
            }
          }
        }
      }
    } /* zC < zBuffer */
    zC += dzdx;
    if (viewport->hueOffset != viewport->hueTop || smoothError ||
        viewport->monoOn)
      intersectColor[0] += dcolor;
    N[0] += dnorm.x;  N[1] += dnorm.y;  N[2] += dnorm.z;
    pt.x += dpt.x;  pt.y += dpt.y;  pt.z += dpt.z;
    xpixel++;
  } /* while each pixel */

}


/***************************
 * void scanPhong()      *
 *                         *
 * This routine takes all  *
 * polygons that intersect *
 * with the current scan-  *
 * line and calculates the *
 * intersecting x and z    *
 * points as well as the   *
 * color at each point.    *
 * Interpolation is done   *
 * according to Phong.     *
 ***************************/

void
scanPhong(int dFlag)
{
  viewTriple *p1, *p2;
  polyList   *polygon;
  poly       *p;
  int        i,num,xtemp,numttt;
  int        *anIndex, *start, *end;
  float      x1,x2,y1,y2,z2,zright,wx1,wx2,wy1,wy2,wz1,wz2;
  float      intersectionx[2], intersectionz[2];
  float      c1,c2,colortemp,ztemp,dY,diffy,diffx,n1[3],n2[3],NV[3];
  triple     ntemp, intersectPt[2], ptemp, pt, intersectN[2];

  /* polygon list intersecting the current scanline, will be modified to
     edge list structure */
  polygon = scanList[scanline];
  while (polygon != NIL(polyList) && polygon->polyIndx != NIL(poly) ) {
    /* for each polygon in the list */
    p = polygon->polyIndx;
    /* don't include clipped polygons */
    if ( ! ( p->partialClipPz ||
             p->totalClipPz   ||
             (viewData.clipStuff && (p->partialClip  || p->totalClip ) ) ) ) {
      num = 0; /* 0 & 1, for the 2 edges of polygon that intersect scanline */
      numttt =0;

      if ((scanline >= (int)p->pymin) && (scanline <= (int)p->pymax)) {
        /* which edges of the polygon intersect the scanline */
        for (i=0, anIndex=p->indexPtr; i<p->numpts; i++) {
          start = anIndex + i;
          p1 = refPt3D(viewData,*(start));
          x1 = p1->px;  y1 = p1->py;  zC = p1->pz;  c1 = p1->sc;
/*           if (x1 < machine0){ x1 = machine0; } */
          wx1 = p1->wx; wy1 = p1->wy; wz1 = p1->wz;
          n1[0] = p1->norm[0]; n1[1] = p1->norm[1]; n1[2] = p1->norm[2];
          end = (i != (p->numpts - 1)) ? anIndex + (i + 1) : anIndex;
          p2 = refPt3D(viewData,*(end));
          x2 = p2->px; y2 = p2->py; z2 = p2->pz; c2 = p2->sc;
/*          if (x2 < machine0){ x2 = machine0; } */
          wx2 = p2->wx; wy2 = p2->wy; wz2 = p2->wz;
          n2[0] = p2->norm[0]; n2[1] = p2->norm[1]; n2[2] = p2->norm[2];
          /* find beginning and end for intersecting edge */
          if ((scanline < y1 && scanline >= y2) ||
              (scanline >= y1 && scanline < y2)) {
            dY = (float)scanline - y1;
            diffy = y2 - y1;
            if (absolute(diffy) < 0.01) diffy = 1.0;
            intersectionx[num] = x1 + ((x2-x1)/diffy) * dY;
            intersectionz[num] = zC + ((z2-zC)/diffy) * dY;
            if (viewport->hueOffset != viewport->hueTop || smoothError ||
                viewport->monoOn)
              intersectColor[num] = c1 + ((c2 - c1)/diffy) * dY;
            intersectN[num].x = n1[0] + ((n2[0] - n1[0])/diffy)*dY;
            intersectN[num].y = n1[1] + ((n2[1] - n1[1])/diffy)*dY;
            intersectN[num].z = n1[2] + ((n2[2] - n1[2])/diffy)*dY;
            intersectPt[num].x = wx1 + ((wx2 - wx1)/diffy)*dY;
            intersectPt[num].y = wy1 + ((wy2 - wy1)/diffy)*dY;
            intersectPt[num].z = wz1 + ((wz2 - wz1)/diffy)*dY;
            num = 1-num;
            numttt++;
          } /* if edge intersects scanline */
        } /* for each edge */
     if (numttt>=2) { /* if numttt 0 or 1 something has gone wrong */
        xleft = intersectionx[0];  xright = intersectionx[1];
        zC  = intersectionz[0];     zright = intersectionz[1];
        /* edges are drawn from left to right, so switch if necessary */
        if (xright < xleft) {
          xtemp = xright;  xright = xleft;  xleft = xtemp;
          ztemp = zright;  zright = zC;  zC = ztemp;
          if (viewport->hueOffset != viewport->hueTop || smoothError ||
              viewport->monoOn) {
            colortemp = intersectColor[1];
            intersectColor[1] = intersectColor[0];
            intersectColor[0] = colortemp;
          }
          ntemp = intersectN[1];  intersectN[1] = intersectN[0];
          intersectN[0] = ntemp;
          ptemp = intersectPt[1];
          intersectPt[1] = intersectPt[0];
          intersectPt[0] = ptemp;
        }
        diffx = xright - xleft;
        if (absolute(diffx) > .01) {
          if (viewport->hueOffset != viewport->hueTop || smoothError ||
              viewport->monoOn)
            dcolor = (intersectColor[1] - intersectColor[0]) / diffx;
          dnorm.x = (intersectN[1].x - intersectN[0].x) / diffx;
          dnorm.y = (intersectN[1].y - intersectN[0].y) / diffx;
          dnorm.z = (intersectN[1].z - intersectN[0].z) / diffx;
          dpt.x = (intersectPt[1].x - intersectPt[0].x) / diffx;
          dpt.y = (intersectPt[1].y - intersectPt[0].y) / diffx;
          dpt.z = (intersectPt[1].z - intersectPt[0].z) / diffx;
          dzdx = (zright - zC) / diffx;
        } else {
          if (viewport->hueOffset != viewport->hueTop || smoothError ||
              viewport->monoOn)
            dcolor = intersectColor[1];
          dnorm.x = 0.0;  dnorm.y = 0.0;  dnorm.z = 0.0;
          dpt.x = 0.0;  dpt.y = 0.0;  dpt.z = 0.0;
          dzdx = 0.0;
        }
        NV[0] = intersectN[0].x;
        NV[1] = intersectN[0].y;
        NV[2] = intersectN[0].z;
        pt.x = intersectPt[0].x;
        pt.y = intersectPt[0].y;
        pt.z = intersectPt[0].z;
        drawPhongSpan(pt,NV,dFlag);
     } /* numttt guard */
      } /* if scanline intersect */
    } /* clipped */
    polygon = polygon->next;
  } /* while still polygons */

}

/********************************************
 * boxTObuffer() writes the projection of   *
 * the x,y  bounding box to the z-buffer.   *
 ********************************************/

void
boxTObuffer(void)
{
  int    xpix,i,j,k,count,decision;
  int    xA,xB,yA,yB;
  float  x,xend,y,yend,diffy,dX,dY,dXY,intersectionx;

  for (i=0;i<6;i++) {
    if (box[i].inside) {
      for (j=0; j<3; j++) {
        quadMesh[j].x = box[i].pointsPtr[j]->px;
        quadMesh[j].y = box[i].pointsPtr[j]->py;
      }

      intersectionx = 0.0;
      for (k=0; k<2; k++) {
        xA = quadMesh[k].x; yA = quadMesh[k].y;
        xB = quadMesh[k+1].x; yB = quadMesh[k+1].y;

/*
        if (xA > graphWindowAttrib.width+1) xA = graphWindowAttrib.width+1;
        if (xB > graphWindowAttrib.width+1) xB = graphWindowAttrib.width+1;
        if (yA > graphWindowAttrib.height) yA = graphWindowAttrib.height;
        if (yB > graphWindowAttrib.height) yB = graphWindowAttrib.height;
        if (xA < 0) xA = 0;  if (xB < 0) xB = 0;
        if (yA < 0) yA = 0;  if (yB < 0) yB = 0;
*/
        x = xA;  xend = xB;  y = yA;  yend = yB;
        diffy = (float)scanline - y;
        dX = xend - x;  dY = yend - y;
        if (absolute(dY) > machine0) {
          dXY = dX/dY;
        } else {
          dXY = dX;
        }

        if (dXY < 0.0) dXY = -dXY;

        if ((scanline == (int)y) && (absolute(dY) <= 1.0)) {
          if (x <= xend) {
            for (xpix = (int)x; xpix <= (int)xend; xpix++) {
              put_cBuffer_axes(xpix,'b');
            }
          } else {
            for (xpix = (int)x; xpix >= (int)xend; xpix--) {
              put_cBuffer_axes(xpix,'b');
            }
          }
        } else {
          if (xend < x)
            decision = (scanline < y && scanline >= yend) ||
                       (scanline > y && scanline <= yend);
          else
            decision = (scanline <= y && scanline > yend) ||
                       (scanline >= y && scanline < yend);
          if (decision) {
            intersectionx = x + dX/dY * diffy;
            for (count = (int)intersectionx;
                 count <= (int)intersectionx + (int)dXY; count++) {
              put_cBuffer_axes(count,'b');
            }
          }
        }
      }

    }
  }

}

/********************************************
 * clipboxTObuffer() writes the projection  *
 * of the x,y,z clipping region box to the  *
 * z-buffer.                                *
 ********************************************/

void
clipboxTObuffer(void)
{
  int    xpix,i,j,k,count,decision;
  int    xA,xB,yA,yB;
  float  x,xend,y,yend,diffy,dX,dY,dXY,intersectionx;

  for (i=0;i<6;i++) {
    if (clipBox[i].inside) {
      for (j=0; j<3; j++) {
        quadMesh[j].x = clipBox[i].pointsPtr[j]->px;
        quadMesh[j].y = clipBox[i].pointsPtr[j]->py;
      }

      intersectionx = 0.0;
      for (k=0; k<2; k++) {
        xA = quadMesh[k].x; yA = quadMesh[k].y;
        xB = quadMesh[k+1].x; yB = quadMesh[k+1].y;
/*

        if (xA > graphWindowAttrib.width+1) xA = graphWindowAttrib.width+1;
        if (xB > graphWindowAttrib.width+1) xB = graphWindowAttrib.width+1;
        if (yA > graphWindowAttrib.height) yA = graphWindowAttrib.height;
        if (yB > graphWindowAttrib.height) yB = graphWindowAttrib.height;
        if (xA < 0) xA = 0;  if (xB < 0) xB = 0;
        if (yA < 0) yA = 0;  if (yB < 0) yB = 0;
*/
        x = xA;  xend = xB;  y = yA;  yend = yB;
        diffy = (float)scanline - y;
        dX = xend - x;  dY = yend - y;
        if (absolute(dY) > machine0) {
          dXY = dX/dY;
        } else {
          dXY = dX;
        }
        if (dXY < 0.0) dXY = -dXY;

        if ((scanline == (int)y) && (absolute(dY) <= 1.0)) {
          if (x <= xend) {
            for (xpix = (int)x; xpix <= (int)xend; xpix++) {
              put_cBuffer_axes(xpix,'c');
            }
          } else {
            for (xpix = (int)x; xpix >= (int)xend; xpix--) {
              put_cBuffer_axes(xpix,'c');
            }
          }
        } else {
          if (xend < x)
            decision = (scanline < y && scanline >= yend) ||
                       (scanline > y && scanline <= yend);
          else
            decision = (scanline <= y && scanline > yend) ||
                       (scanline >= y && scanline < yend);
          if (decision) {
            intersectionx = x + dX/dY * diffy;
            for (count = (int)intersectionx;
                 count <= (int)intersectionx + (int)dXY; count++) {
              put_cBuffer_axes(count,'c');
            }
          }
        }
      }

    }
  }

}



/********************************************
 * axesTObuffer() writes the projection of  *
 * the x,y,z axes to the z-buffer.          *
 ********************************************/

void
axesTObuffer(void)
{
  int    xpix,i,count,decision;
  int    xA,xB,yA,yB;
  float  x,xend,y,yend,diffy,dX,dY,dXY,intersectionx;
  float  zA,zB,z,zend;
  float  dZ,dZX,dZY,intersectionz;

  intersectionz = 0.0;  intersectionx = 0.0;
  for (i=0; i<3; i++) {
    xA = axesXY[i][0]; yA = axesXY[i][1]; zA = axesZ[i][0];
    xB = axesXY[i][2]; yB = axesXY[i][3]; zB = axesZ[i][1];
/*
    if (xA > graphWindowAttrib.width+1) xA = graphWindowAttrib.width+1;
    if (xB > graphWindowAttrib.width+1) xB = graphWindowAttrib.width+1;
    if (yA > graphWindowAttrib.height) yA = graphWindowAttrib.height;
    if (yB > graphWindowAttrib.height) yB = graphWindowAttrib.height;
    if (xA < 0) xA = 0;  if (xB < 0) xB = 0;
    if (yA < 0) yA = 0;  if (yB < 0) yB = 0;
*/
    x = xA;  xend = xB;  y = yA;  yend = yB;  z = zA;  zend = zB;
    diffy = (float)scanline - y;
    dX = xend - x;  dY = yend - y;  dZ = zend - z;
      dZY = dZ/dY;
      dXY = dX/dY;
    if (dXY < 0.0) dXY = -dXY;
    dZX = dZ/dX;

    if ((scanline == (int)y) && (absolute(dY) <= 1.0)) {
      if (x <= xend) {
        for (xpix = (int)x; xpix <= (int)xend; xpix++) {
          put_cBuffer_axes(xpix,'a');
          put_zBuffer(xpix,z + dZY * diffy);
        } /* for x */
      } else {
        for (xpix = (int)x; xpix >= (int)xend; xpix--) {
          put_cBuffer_axes(xpix,'a');
          put_zBuffer(xpix,z + dZY * diffy);
        } /* for x */
      }
    } else {
      if (xend < x)
        decision = (scanline < y && scanline >= yend) ||
                   (scanline > y && scanline <= yend);
      else
        decision = (scanline <= y && scanline > yend) ||
                   (scanline >= y && scanline < yend);
      if (decision) {
        intersectionx = x + dX/dY * diffy;
        intersectionz = z + dZY * diffy;
        for (count = (int)intersectionx;
             count <= (int)intersectionx + (int)dXY; count++) {
          put_cBuffer_axes(count,'a');
          put_zBuffer(count,intersectionz);
          intersectionz += dZX;
        }
      } /* if edge intersects scanline */
    }
  } /* for each axes */

}

/********************************************
 * scanLines() scanline z-buffer algorithm  *
 * initialize z-buffer and color buffer for *
 * all scanlines.                           *
 ********************************************/

void
scanLines(int dFlag)
{
  unsigned long pixColor;
  int           i;
  char          tempA;

  if (dFlag == Xoption) {
    if (viewmap_valid) {
      XFreePixmap(dsply,viewmap);
      viewmap_valid=0;
    }
    viewmap = XCreatePixmap(/* display */     dsply,
                            /* drawable */    viewport->viewWindow,
                            /* width */       vwInfo.width,
                            /* height */      vwInfo.height,
                            /* depth */       DefaultDepth(dsply,scrn));
    viewmap_valid =1;
    GSetForeground(trashGC,(float)backgroundColor,dFlag);
    XFillRectangle(dsply,viewmap,trashGC,0,0,vwInfo.width,vwInfo.height);
    XFillRectangle(dsply,viewport->viewWindow,trashGC,0,0,
                   vwInfo.width,vwInfo.height);
  } else {
    if (mono || viewport->monoOn) {
      GSetForeground(GC9991,
                   1.0-(float)((int)(psShadeMax-0.3*psShadeMax)-1)*psShadeMul,dFlag);
    } else {
      GSetForeground(GC9991, psWhite, dFlag);
    }
    quadMesh[0].x = 0;  quadMesh[0].y = 0;
    quadMesh[1].x = graphWindowAttrib.width+2;
    quadMesh[1].y = 0;
    quadMesh[2].x = graphWindowAttrib.width+2;
    quadMesh[2].y = graphWindowAttrib.height;
    quadMesh[3].x = 0;
    quadMesh[3].y = graphWindowAttrib.height;
    quadMesh[4].x = 0;  quadMesh[4].y = 0;
    PSFillPolygon(GC9991, quadMesh, 5);
  }

  if (graphWindowAttrib.height >= physicalHeight)
    graphWindowAttrib.height = physicalHeight - 1;
  if (graphWindowAttrib.width >= physicalWidth)
    graphWindowAttrib.width = physicalWidth - 1;
  if (dFlag == Xoption)
    strcpy(control->message,"         Display Scanlines          ");
  else
    strcpy(control->message,"          Writing Output            ");
  writeControlMessage();

  scanline = graphWindowAttrib.height-1;

  imageX = XCreateImage(/* display */        dsply,
                        /* visual */         DefaultVisual(dsply,scrn),
                        /* depth */          DefaultDepth(dsply,scrn),
                        /* format */         ZPixmap,
                        /* offset */         0,
                        /* data */           0,
                        /* width */          vwInfo.width,
                        /* height */         1,
                        /* bitmap_pad */     32,
                        /* bytes_per_line */ 0);
  imageX->data = (char *)malloc(imageX->bytes_per_line);


  while (scanline >= 0 && keepDrawingViewport()) {
    /* initialize buffer values for scanline */
    pixColor = backgroundColor;
    for (i=0; i < (int)graphWindowAttrib.width; i++) {
      put_zBuffer(i,10000.0);
      put_cBuffer_indx(i,-1);
      put_cBuffer_axes(i,'0');
      if (mono || viewport->monoOn)
        if ((scanline % 2) == 0)
          if ((i % 2) == 0) {
            if (i>=0 && i<vwInfo.width) XPutPixel(imageX,i,0,backgroundColor);
          }
          else {
            if (i>=0 && i<vwInfo.width) XPutPixel(imageX,i,0,foregroundColor);
          }
        else
          if ((i % 2) == 0) {
            if (i>=0 && i<vwInfo.width) XPutPixel(imageX,i,0,foregroundColor);
          }
          else {
            if (i>=0 && i<vwInfo.width) XPutPixel(imageX,i,0,backgroundColor);
          }
      else {
        if (i>=0 && i<vwInfo.width) XPutPixel(imageX,i,0,backgroundColor);
      }
    }

    /* writes the axes info to the buffers */
    if (viewData.box) boxTObuffer();
    if (viewData.clipbox) clipboxTObuffer();
    if (viewport->axesOn) axesTObuffer();

    /* fill buffers for current scanline */
    scanPhong(dFlag);

    for (i=0; i < (int)graphWindowAttrib.width; i++) {
      /* include bounding region info */
      if (viewData.box) {
        if (get_cBuffer_axes(i) == 'b') {
          if (dFlag==Xoption) {
            if (mono || (viewport->monoOn)) pixColor = foregroundColor;
            else pixColor = boxInline;
            if (i >=0 && i<vwInfo.width) XPutPixel(imageX,i,0,pixColor);
          } else {
            GDrawPoint(viewport->viewWindow, GC9991,
                       (int) psBlack, i, scanline, dFlag);
          }
        }
      }
      /* include clipping box info */
      if (viewData.clipbox) {
        if (get_cBuffer_axes(i)== 'c') {
          if (dFlag==Xoption) {
            if (mono || (viewport->monoOn)) pixColor = foregroundColor;
            else pixColor = clipBoxInline;
            if (i >=0 && i<vwInfo.width) XPutPixel(imageX,i,0,pixColor);
          } else {
            GDrawPoint(viewport->viewWindow, GC9991,
                       (int) psBlack, i, scanline, dFlag);
          }
        }
      }
      /* include axes info */
      if (viewport->axesOn) {
        if (get_cBuffer_axes(i) == 'a') {
          if (dFlag == Xoption) {
            if (mono || (viewport->monoOn)) pixColor = foregroundColor;
            else pixColor = monoColor(axesColor);
            if (i >=0 && i<vwInfo.width)  XPutPixel(imageX,i,0,pixColor);
          } else {
            GDrawPoint(viewport->viewWindow, GC9991,
                       (int) psBlack, i, scanline, dFlag);
          }
        } /* if buffer slot is an axes point */
        tempA = get_cBuffer_axes(i);
      } else tempA = '0';  /* else axes not on */

      if (get_cBuffer_indx(i) >= 0 && (tempA == '0')) {
        if (dFlag == Xoption) {
          GSetForeground(trashGC,(float)get_cBuffer_indx(i),dFlag);
          pixColor = get_cBuffer_indx(i);
          if (i >=0 && i<vwInfo.width) XPutPixel(imageX,i,0,pixColor);
        }
        else {
          GDrawPoint(viewport->viewWindow, GC9991,
                     get_cBuffer_indx(i), i, scanline, dFlag);
        }
      }
    } /* for each pixel in scanline */

    if (dFlag == Xoption) {
      XPutImage(dsply,viewport->viewWindow,trashGC,imageX,0,0,0,
                scanline,vwInfo.width,1);
      XPutImage(dsply,viewmap,trashGC,imageX,0,0,0,
                scanline,vwInfo.width,1);
    }

    scanline--;

  } /* while each scanline */
  XDestroyImage(imageX);

}

/*************************************
 * void freePolyList();              *
 *                                   *
 * frees up the global scanList l-l  *
 *************************************/

void
freePolyList (void)
{
  polyList *P, *nextP;
  int  i;

  for (i = 0; (i < ARRAY_HEIGHT); i++) {
    P = scanList[i];
    while((P != NIL(polyList))) {
      nextP = P->next;
      free(P);
      P = nextP;
    }
  }

} /* freePolyList() */


/********************************************
 * showAxesLabels() writes the axes labels  *
 * onto the viewmap of a graph.             *
 ********************************************/

void
showAxesLabels(int dFlag)
{
  int  xcoord2,ycoord2;

  if (dFlag == Xoption)
    if (mono || (viewport->monoOn))
      GSetForeground(globGC,(float)foregroundColor,dFlag);
    else
      GSetForeground(globGC,(float)monoColor(labelColor),dFlag);
  else GSetForeground(GC9991,psBlack,dFlag);

  /* axes label for X */
  if ((int)axesZ[0][0] >= (int)axesZ[0][1]) {
    if (axesXY[0][2] < axesXY[0][0]) xcoord2 = axesXY[0][2]-5;
    else xcoord2 = axesXY[0][2] + 5;
    if (axesXY[0][3] < axesXY[0][1]) ycoord2 = axesXY[0][3]-5;
    else ycoord2 = axesXY[0][3] + 5;
    if (!viewport->yzOn) {
      if (dFlag == Xoption)
        GDrawString(globGC,viewmap,xcoord2,ycoord2,"X",1,dFlag);
      else
        GDrawString(GC9991,viewport->viewWindow,xcoord2,ycoord2,"X",1,dFlag);
    }
  }

  /* axes label for Y */
  if ((int)axesZ[1][0] >= (int)axesZ[1][1]) {
    if (axesXY[1][2] < axesXY[1][0]) xcoord2 = axesXY[1][2]-5;
    else xcoord2 = axesXY[1][2] + 5;
    if (axesXY[1][3] < axesXY[1][1]) ycoord2 = axesXY[1][3]-5;
    else ycoord2 = axesXY[1][3] + 5;
    if (!viewport->xzOn) {
      if (dFlag == Xoption)
        GDrawString(globGC,viewmap,xcoord2,ycoord2,"Y",1,dFlag);
      else
       GDrawString(GC9991,viewport->viewWindow,xcoord2,ycoord2,"Y",1,dFlag);
    }
  }

  /* axes label for Z */
  if ((int)axesZ[2][0] >= (int)axesZ[2][1]) {
    if (axesXY[2][2] < axesXY[2][0]) xcoord2 = axesXY[2][2]-5;
    else xcoord2 = axesXY[2][2] + 5;
    if (axesXY[2][3] < axesXY[2][1]) ycoord2 = axesXY[2][3]-5;
    else ycoord2 = axesXY[2][3] + 5;
    if (!viewport->xyOn) {
      if (dFlag == Xoption)
        GDrawString(globGC,viewmap,xcoord2,ycoord2,"Z",1,dFlag);
      else
       GDrawString(GC9991,viewport->viewWindow,xcoord2,ycoord2,"Z",1,dFlag);
    }
  }
}



/********************************************
 * changeColorMap() modifies the color map  *
 * for moving in and out of smooth shading. *
 ********************************************/

void
changeColorMap(void)
{
  int         okay, i, hue, *index;
  poly        *cp;
  viewTriple  *pt;

  strcpy(control->message,"         Make New Color Map         ");
  writeControlMessage();
  if ((viewport->hueOffset == viewport->hueTop) &&
      !mono && !viewport->monoOn) {

     /* colormap is not an even distribution across spectrum */
     /* see spadcolors.c code to understand why this is done */

    if (viewport->hueTop < 11) smoothHue = viewport->hueTop * 6;
    else
      if (viewport->hueTop > 10 && viewport->hueTop < 16) {
        smoothHue = viewport->hueTop*20 - 140;
      }
      else {
        smoothHue = viewport->hueTop*12 - 12;
      }

    if (redoColor) {
       /* reallocate colormap for new hue */
      redoColor = no;
      if (pixelSetFlag) {
        FreePixels(dsply,colorMap,smoothConst+1);
      }
      okay = makeNewColorMap(dsply,colorMap,smoothHue);
      if (okay) {
        pixelSetFlag = yes;
        smoothError = no; }
      else {
        pixelSetFlag = no;
        smoothError = yes; }
    } /* if redoColor */
  } else {
    redoDither = no;
    if (pixelSetFlag && !mono) {
      FreePixels(dsply,colorMap,smoothConst);
      pixelSetFlag = no;
      redoColor = no;
      multiColorFlag = yes;
    }
    if (!mono && !viewport->monoOn) {
      cp = quickList;
      while (cp != NIL(poly) && keepDrawingViewport()) {
        for (i = 0, index = cp->indexPtr;
             i < cp->numpts; i++, index++) {
          pt = refPt3D(viewData,*(index));
          /* get hue for each point if multi-dithering is used */
          if (absolute(cp->color) > 1.0)
            hue = floor(absolute(cp->color));
          else
            hue = floor(absolute(cp->color) * viewport->numberOfHues) +
                  viewport->hueOffset;
          pt->sc = (float)hue;
        } /* for each point in polygon */
        cp = cp->next;
      }
    } /* multi-color dither */
  } /* else hueOffset != hueTop */
}


/***********************
 * void drawPhong()    *
 *                     *
 * A general routine   *
 * for displaying a    *
 * list of polygons    *
 * using a simple      *
 * scanline z-buffer   *
 * algorithm with      *
 * phong shading.      *
 ***********************/

void
drawPhong(int dFlag)
{

    poly          *p, *head;
    polyList      *s;
    int           i,j,hue;
    int           *anIndex, redo;
    viewTriple    *aPoint, *polyPt;

    redo = (recalc || redoSmooth);
    if (redo || redoColor || redoDither) {
      rotated = no;  zoomed = no;  translated = no;
      switchedPerspective = no;  changedEyeDistance = no;
      redoSmooth = no;  movingLight = no;

      /* If only a color change don't recalculate polygon info. */
      if (!redo) {
        /* glossy shading if a single hue is indicated */
        changeColorMap();
        scanLines(dFlag);
        /* if axes are on then show axes labels */
        if (viewport->axesOn) showAxesLabels(dFlag);

        /* show pixmap of image */
        XCopyArea(dsply,viewmap,viewport->viewWindow,trashGC,0,0,
                  vwInfo.width,vwInfo.height,0,0);
      } else {
        if (keepDrawingViewport()) {
          if (!firstTime && !(scanline > 0)) {
            strcpy(control->message,"          Freeing Polygons          ");
            writeControlMessage();
            freeListOfPolygons(quickList);
            freePointResevoir();
          }
          if (keepDrawingViewport()) {
            strcpy(control->message,"         Collecting Polygons        ");
            writeControlMessage();
            quickList = copyPolygons(viewData.polygons);

            if (keepDrawingViewport()) {
              strcpy(control->message,"         Projecting Polygons        ");
              writeControlMessage();
              projectAllPolys(quickList);
              if (keepDrawingViewport()) {
                strcpy(control->message,
                       "       Setting Polygon Extremes      ");
                writeControlMessage();
                minMaxPolygons(quickList);
                if (keepDrawingViewport()) {
                  strcpy(control->message,
                         "          Sorting Polygons          ");
                  writeControlMessage();
                  quickList = msort(quickList,0,viewData.numPolygons,
                                    polyCompare);
                  calcEyePoint();
                  head = p = quickList;

                  /* glossy shading if a single hue is indicated */
                  changeColorMap();

                  for (i=0, aPoint=viewData.points;
                       i<viewData.numOfPoints; i++,aPoint++) {
                    aPoint->norm[0]= 0.0;
                    aPoint->norm[1]= 0.0;
                    aPoint->norm[2]= 0.0;
                  }
                  freePolyList();
                  for (i = 0; i < ARRAY_HEIGHT; i++)
                    scanList[i] = NIL(polyList);
                  /* for each polygon  */
                  /* calculate average normal for each vertex  */
                  strcpy(control->message,
                         "         Build Polygon Lists        ");
                  writeControlMessage();
                  p = head;
                  while ((p != NIL(poly)) && keepDrawingViewport()) {

                    for (j = 0, anIndex = p->indexPtr;
                         j < p->numpts; j++, anIndex++) {
                      polyPt = refPt3D(viewData,*(anIndex));
                      polyPt->norm[0] += p->N[0];
                      polyPt->norm[1] += p->N[1];
                      polyPt->norm[2] += p->N[2];
                      normalizeVector(polyPt->norm);
                      /* get hue for each point if multi-dithering is used */
                      if ((viewport->hueOffset != viewport->hueTop ||
                           smoothError) && !mono) {
                        if (absolute(p->color) > 1.0) {
                          hue = floor(absolute(p->color));
                        } else {
                          hue = floor(absolute(p->color) *
                                      viewport->numberOfHues) +
                                      viewport->hueOffset;
                        }
                        polyPt->sc = (float)hue;
                      } /* multi-color dither */
                    } /* for each point in polygon */

                    if ( ! ( p->partialClipPz ||
                             p->totalClipPz   ||
                             (viewData.clipStuff && (p->partialClip  || p->totalClip ) ) ) ) {
                      /* put polygon in each scanline list it intersects */
                      for (i=(int)p->pymin; i<= (int)p->pymax; i++) {
                        if ( (i>=0) && (i<ARRAY_HEIGHT ) ){
                           s = (polyList *)saymem("smoothShade.c",1,sizeof(polyList));
                           s->polyIndx = p;
                           s->next = scanList[i];
                           scanList[i] = s;
                        }
                      } /* put polygon in each scanline it intersects */
                    } /* if polygon not clipped */
                    p = p->next;
                  } /* while still polygons */

                  scanLines(dFlag);

                  /* if axes are on then show axes labels */
                  if (viewport->axesOn) showAxesLabels(dFlag);

                  /* show pixmap of image */
                  XCopyArea(dsply,viewmap,viewport->viewWindow,trashGC,0,0,
                          vwInfo.width,vwInfo.height,0,0);
                  /* freePolyList(scanList);   */

                } /* keepDrawingViewport() after setting extreme values */
              } /* keepDrawingViewport() after projecting all polygons */
            } /* keepDrawingViewport() after collecting polygons */
          } /* keepDrawingViewport() after freeing polygons */
        } /* keepDrawingViewport() after recalc */
        finishedList = !(scanline>0);
        if (firstTime) firstTime = no;
      } /* not only a color change */

    } else { /* else just redisplay current pixmap of image */
      XCopyArea(dsply,viewmap,viewport->viewWindow,trashGC,0,0,
                vwInfo.width,vwInfo.height,0,0);
    }
    clearControlMessage();
    strcpy(control->message,viewport->title);
    writeControlMessage();

} /* drawPhong */
