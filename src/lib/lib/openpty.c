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
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>

#if defined(HAVE_OPENPTY_DECL) && defined(HAVE_OPENPTY)
   #ifdef HAVE_UTIL_H
      #include <util.h>
   #else
      #ifdef HAVE_PTY_H
         #include <pty.h>
      #endif
   #endif
   #define FRICAS_USE_OPENPTY 1
#else
   #define FRICAS_USE_OPENPTY 0
#endif

#include "openpty.H1"


/*
 * The main function is ptyopen. It simply opens up both sides of a
 * pseudo-terminal. It uses and saves the pathnames for
 * the devices which were actually opened.
 *
 * If it fails it simply exits the program.
 *
 *
 * ptyopen(controller, server, controllerPath, serverPath)
 * int *controller;     The file descriptor for controller side of the pty
 * int *server;         The file descriptor for the server side
 * char *controllerPath;  actually , this is not used anywhere on return
                          and can be taken out of the call sequence
 * char *serverPath;
 *
 * The path name  vars should be declared of size 11 or more
 */


int
ptyopen(int *controller,int * server, char *controllerPath,char * serverPath)
{
#if FRICAS_USE_OPENPTY
   return openpty(controller, server, serverPath, 0, 0);
#else
   return -1;
#endif
}
