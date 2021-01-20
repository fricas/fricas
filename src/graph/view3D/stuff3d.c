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

#define _STUFF3D_C
#include "fricas_c_macros.h"

#include "header.h"

#include  <stdlib.h>
#include  <unistd.h>
#include  <math.h>



#include "Gfun.H1"
#include "spadcolors.H1"
#include "util.H1"

#include "all_3d.H1"
/*****************************
 ***      traverse(n)      ***
 *** returns the nth point ***
 *** in a point reservoir  ***
 *****************************/

viewTriple *
traverse (int n)
{

  int i;
  viewTriple *v;

  v = splitPoints;
  for (i=0; i<n; i++) v = v->next;
  return(v);

} /* traverse */


/**************************/
/***  float absolute(x) ***/
/**************************/

float
absolute (float x)
{

  if (x<0.0) return(-x);
  else return(x);

}




/****************************/
/***  float get_random(x) ***/
/****************************/

float
get_random(void)
{

  float x;

  x = (float)(rand() % 100);
  return(x);

}




/****************************/
/***   float norm_dist()  ***/
/****************************/

triple
norm_dist(void)
{

  float   u1, u2, v1, v2, ss, rad;
  triple  pert;

  ss = 2.0;
  while (ss >= 1.0) {
    u1 = get_random()/100.0;
    u2 = get_random()/100.0;
    v1 = 2.0*u1 - 1.0;  v2 = 2.0*u2 - 1.0;
    ss = v1*v1 + v2*v2;
  }
  if (ss == 0.0) ss += .1;
  rad = -2.0*log(ss)/ss;
  pert.x = v1 * sqrt(rad);
  pert.y = v2 * sqrt(rad);
  pert.z = 0.0;

  return(pert);
}



/************************/
/***  void goodbye()  ***/
/************************/

void
goodbye(int sig)
{

  int Command;

  PSClose(); /* free PS file and data structure space */

  if (pixelSetFlag) FreePixels(dsply,colorMap,smoothConst);
  if (!viewAloned) {
     Command = viewportClosing;
     check(write(Socket,&Command,intSize));
     }

  XCloseDisplay(dsply);
  exit(0);
}           /* goodbye */
