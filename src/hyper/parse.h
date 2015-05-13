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

#ifndef _PARSE_H_
#define _PARSE_H_ 1

#include "fricas_c_macros.h"
#include "hterror.h"

#include <sys/stat.h>
#include <fcntl.h>
#include <sys/errno.h>
#include <ctype.h>
#include "hyper.h"

#include <setjmp.h>

extern jmp_buf  jmpbuf;
extern int      vbuff;

extern TextNode *cur_spadcom;   /* spad command being parsed *** */
extern TextNode *curr_node;
extern long     page_start_fpos;/* tells the character position of the start
                                 * of the page, needed to find the current
                                 * position when restoring the scanner */

/*
 * Default sizes of hash tables
 */

#define LinkHashSize    25
#define DependHashSize  20

extern HashTable *gLinkHashTable;    /* the hash table of active link windows */

/*
 * Flags and defines for the modes the parser can be in
 */

#define AllMode            0
#define NoVerticalMode     1
#define SimpleMode         2

extern short int gParserMode;

/*
 * Flags and defines for telling us what part of the page is being parsed.
 */

extern short int gParserRegion;
extern short int gStringValueOk;
extern boolean   gEndedPage;

extern int      line_number;

/*
 * Things for handling macro parameters
 */



extern ParameterList parameters;


/*
 * The error buffer
 */

extern char     ebuffer[];

#endif
