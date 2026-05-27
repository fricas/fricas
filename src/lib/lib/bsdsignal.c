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
#include "bsdsignal.h"

#include <signal.h>

#include "bsdsignal.H1"


SignalHandlerFunc
bsdSignal(int sig, SignalHandlerFunc action, int restartSystemCall)
{
#if HAVE_DECL_SIGACTION

  struct sigaction in,out;
  in.sa_handler = action;
  /* handler is reinstalled - calls are restarted if restartSystemCall */
#ifdef SA_RESTART
  if(restartSystemCall) in.sa_flags = SA_RESTART;
  else in.sa_flags = 0;
#elif defined(SA_INTERRUPT)
  if (restartSystemCall) in.sa_flags = 0;
  else in.sa_flags = SA_INTERRUPT;
#else
  in.sa_flags = 0;
#endif

  return (sigaction(sig, &in, &out) ? (SignalHandlerFunc) -1 :
          (SignalHandlerFunc) out.sa_handler);
#else /* !HAVE_DECL_SIGACTION */
  return (SignalHandlerFunc) -1;
#endif /* HAVE_DECL_SIGACTION */
}
