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

/*
 * hthits pattern htdb-file
 *
 * Scan HyperDoc files for a given pattern.
 *
 * The output contains lines of the form:
 *
 * page-name`title`n
 *
 * The title and body of each page are scanned but the name is not. It is
 * possible that the title matches but not any lines. The number of matches
 * in the page (n) is given last.
 *
 * SMW Feb 91
 */
#define _HTHITS_C
#include "fricas_c_macros.h"

#include "debug.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <regex.h>

/*
 * For fixed-size arrays.
 */
#define MAX_HTDB_LINE   1024
#define MAX_ENTRY_TYPE  30      /* I.e. \page \newcommand \patch ... */
#define MAX_ENTRY_NAME  1024    /* E.g. DifferentialCalculusPage */
#define MAX_COMP_REGEX  1024

typedef struct pgInfo {
    char name[MAX_ENTRY_NAME];
    long start, size;
} PgInfo ;

#include "hthits.H1"

/*
 * Global variables set according to the command line.
 */

char *progName;
char *pattern;
char *htdbFName;
int gverifydates=0;
regex_t reg_pattern;

int
main(int argc,char ** argv)
{
    cmdline(argc, argv);

    regcomp(&reg_pattern, pattern, REG_NEWLINE);

    handleHtdb();
    return(0);
}

void
cmdline(int argc,char ** argv)
{
    progName = argv[0];

    if (argc != 3) {
        fprintf(stderr, "Usage: %s pattern htdb-file\n", progName);
        exit(1);
    }

    pattern = argv[1];
    htdbFName = argv[2];
}

void
handleHtdb(void)
{
    FILE *htdbFile;
    int c;

    htdbFile = fopen(htdbFName, "r");
    if (htdbFile == NULL)
        badDB();

    while ((c = getc(htdbFile)) != EOF) {
        if (c != '\t')
            badDB();
        ungetc(c, htdbFile);

        handleFile(htdbFile);
    }
    fclose(htdbFile);
}


void
handleFile(FILE *htdbFile)
{
    static PgInfo *pgInfoV = 0;
    static int pgInfoC = 0;

    char htdbLine[MAX_HTDB_LINE];
    char htfname[MAX_HTDB_LINE];
    time_t httime;
    long htsize;
    struct stat htstat;

    long fstart, fend;
    int rc, i, npages;

    char entname[MAX_ENTRY_NAME], enttype[MAX_ENTRY_TYPE];
    long entoffset, entlineno;

    fgets(htdbLine, MAX_HTDB_LINE, htdbFile);

    sscanf(htdbLine, " %s %ld", htfname, &httime);

    /*
     * 1. Verify file: get size and check modification time.
     */
    rc = stat(htfname, &htstat);
    if (rc == -1) {
        fprintf(stderr, "%s: Cannot access %s\n", progName, htfname);
        exit(1);
    }
    if (gverifydates && (htstat.st_mtime != httime)) {

        fprintf(stderr, "%s: Out of date file %s\n", progName, htfname);
        exit(1);
    }
    htsize = htstat.st_size;

    /*
     * 2. Count the pages in the file.
     */
    npages = 0;
    fstart = ftell(htdbFile);
    fend = ftell(htdbFile);

    while (fgets(htdbLine, MAX_HTDB_LINE, htdbFile) != NULL) {
        if (htdbLine[0] == '\t')
            break;
        if (!strncmp(htdbLine, "\\page", 5))
            npages++;
        fend = ftell(htdbFile);
    }

    /*
     * 3. Find offset and size of each \page (skipping \newcommands etc.)
     */
    if (npages > pgInfoC) {
        if (pgInfoV)
            free(pgInfoV);

        pgInfoC = npages;
        pgInfoV = (PgInfo *)
            malloc(npages * sizeof(PgInfo));

        if (!pgInfoV) {
            fprintf(stderr, "%s: out of memory\n", progName);
            exit(1);
        }
    }

    fseek(htdbFile, fstart, 0);

    for (i = 0; fgets(htdbLine, MAX_HTDB_LINE, htdbFile) != NULL;) {
        if (htdbLine[0] == '\t')
            break;

        sscanf(htdbLine, "%s %s %ld %ld",
               enttype, entname, &entoffset, &entlineno);

        if (i > 0 && pgInfoV[i - 1].size == -1)
            pgInfoV[i - 1].size = entoffset - pgInfoV[i - 1].start;

        if (!strcmp(enttype, "\\page")) {
            strncpy(pgInfoV[i].name, entname, MAX_ENTRY_NAME);
            pgInfoV[i].start = entoffset;
            pgInfoV[i].size = -1;

            i++;
        }
    }
    if (i > 0 && pgInfoV[i - 1].size == -1)
        pgInfoV[i - 1].size = htsize - pgInfoV[i - 1].start;

    if (i != npages)
        badDB();

    /*
     * 4. Position database input to read next file-description
     */
    fseek(htdbFile, fend, 0);

    /*
     * 5. Process the pages of the file.
     */
    handleFilePages(htfname, npages, pgInfoV);
}

void
handleFilePages(char *fname, int pgc, PgInfo *pgv)
{
    FILE *infile;
    int i;

    infile = fopen(fname, "r");
    if (infile == NULL) {
        fprintf(stderr, "%s: Cannot read file %s\n", progName, fname);
        exit(1);
    }


    for (i = 0; i < pgc; i++)
        handlePage(infile, pgv + i);

    fclose(infile);

}

void
handlePage(FILE *infile,PgInfo * pg)
{
    static char *pgBuf = 0;
    static int pgBufSize = 0;

    char *title, *body;

    if (pg->size > pgBufSize - 1) {
        if (pgBuf)
            free(pgBuf);
        pgBufSize = pg->size + 20000;
        pgBuf = (char *)malloc(pgBufSize);

        if (!pgBuf) {
            fprintf(stderr,"%s: Out of memory\n", progName);
            exit(1);
        }
    }

    fseek(infile, pg->start, 0);
    fread(pgBuf, pg->size, 1, infile);
    pgBuf[pg->size] = 0;

    splitpage(pgBuf, &title, &body);
    /*untexbuf(title);*/
    untexbuf(body);

#ifdef DEBUG
    printf("-------------- %s -------------\n%s", pg->name, pgBuf);
    printf("============== %s =============\n", title);
    printf("%s", body);
#endif

    searchPage(pg->name, title, body);

}

void
searchPage(char *pgname,char * pgtitle,char * pgbody)
{
    char *bodyrest;
    regmatch_t match_pos;
    int nhits = 0;

    if (!regexec(&reg_pattern, pgtitle, 1, &match_pos, 0))
        nhits++;

    bodyrest = pgbody;
    while (!regexec(&reg_pattern, bodyrest, 1, &match_pos, 0)) {
        nhits++;
        bodyrest += match_pos.rm_eo;
    }
    if (nhits) {
        printf("\\newsearchresultentry{%d}{%s}",nhits, pgtitle);
        squirt(pgname, strlen(pgname));
        printf("\n");
    }
}

/*
 * Given string s and length n, output ` followed by the first n characters
 * of s with ` and newline converted to blanks. This function destructively
 * modifies s.
 */

void
squirt(char *s, int n)
{
    char *t, *e;
    int c;

    c = s[n];

    for (t = s, e = s + n; t < e; t++)
        if (*t == '`' || *t == '\n')
            *t = ' ';

    if (s[n] != 0) {
        s[n] = 0;
    }
    printf("{%.*s}", n, s);
    s[n] = c;
}

/*
 * Any newlines and separator characters in the title are changed to blanks.
 */
void
splitpage(char *buf, char **ptitle, char **pbody)
{
    int n, depth, tno;
    char *s;

    switch (buf[1]) {
      case 'p':
        tno = 2;
        break;                  /* \page{Name}{Title} */
      case 'b':
        tno = 3;
        break;                  /* \begin{page}{Name}{Title} */
      default:
        fprintf(stderr, "%s: Invalid page format: %s\n", progName, buf);
        exit(1);
    }

    n = 0;
    depth = 0;

    for (s = buf; *s; s++) {
        if (*s == '{')
            if (++depth == 1 && ++n == tno)
                *ptitle = s + 1;
        if (*s == '}')
            if (depth-- == 1 && n == tno) {
                *s = 0;
                *pbody = s + 1;
                break;
            }
    }
}


void
untexbuf(char *s)
{
    char *d = s;

    while (*s)
        switch (*s) {
          case '\\':
            *d++ = ' ';
            s++;
            if (*s != '%')
                while (isalpha(*s))
                    s++;
            break;
          case '%':
            *d++ = ' ';
            s++;
            while (*s && *s != '\n')
                s++;
            break;
          case '{':
          case '}':
          case '#':
            *d++ = ' ';
            s++;
            break;
          default:
            *d++ = *s++;
        }
    *d = 0;
}

void
badDB(void)
{
    fprintf(stderr, "%s:  bad database file %s\n", progName, htdbFName);
    exit(1);
}

void
regerr(int code)
{
    fprintf(stderr, "%s: regular expression error %d for \"%s\"\n",
            progName, code, pattern);
}
