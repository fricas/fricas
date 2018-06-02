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

/* ex2ht creates a cover page for structured HyperDoc example pages */


#define _EX2HT_C
#include "fricas_c_macros.h"
#include "debug.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>


#define MaxLineLength 512
#define MaxFiles      100

char *files[MaxFiles];
int numFiles = 0;
struct timeval latest_date[2] ={{0,0},{0,0}};

#include "ex2ht.H1"


int
main(int argc, char **argv)
{
    int i;

    if (argc == 1) {
        fprintf(stderr, "usage: %s exfile.ht ...\n", argv[0]);
        return (-1);
    }
    openCoverPage();
    for (i = 1; i < argc; i++)
        exToHt(argv[i]);
    closeCoverPage();
    for (i = 0; i < numFiles; i++)
        addFile(files[i]);
    closeCoverFile();
    return 0;
}

char *
allocString(char *s)
{
    char *t = (char *) malloc(strlen(s) + 1);

    strcpy(t, s);
    return t;
}

char *
strPrefix(char *prefix, char *s)
{
    while (*prefix != '\0' && *prefix == *s) {
        prefix++;
        s++;
    }
    if (*prefix == '\0')
        return s;
    return NULL;
}

char *
getExTitle(FILE *inFile, char *line)
{
    char *title;

    while (fgets(line, MaxLineLength, inFile) != NULL)
        if ((title = strPrefix("% Title: ", line))) {
            title[strlen(title) - 1] = '\0';
            return title;
        }
    fprintf(stderr, "No Title title line in the file!\n");
    return NULL;
}

void
exToHt(char *filename)
{
    char line[MaxLineLength], *line2;
    char *title, *pagename;
    FILE *inFile = fopen(filename, "r");
    FILE *outFile;
    int len, i;
    struct timeval  tvp;
    struct stat buf;

    if (inFile == NULL) {
        fprintf(stderr, "couldn't open %s for reading.\n", filename);
        return;
    }
    strcpy(line, "Menu");
    strcat(line, filename);
    len = strlen(line);
    for (i = 0; i < len; i++)
        if (line[i] == '.') {
            line[i] = '\0';
            break;
        }
    outFile = fopen(line, "w");
    if (outFile == NULL) {
        fprintf(stderr, "couldn't open %s for writing.\n", line);
        return;
    }
    pagename = allocString(line);
    title = getExTitle(inFile, line);
    if (title == NULL) {
        return;
    }
    files[numFiles++] = pagename;
    emitCoverLink(pagename, title);
    emitHeader(outFile, pagename, title);
    while (fgets(line, MaxLineLength, inFile) != NULL) {
        if ((line2 = strPrefix("\\begin{page}{", line)))
            emitMenuEntry(line2, outFile);
        else if ((line2 = strPrefix("\\spadcommand{", line)))
            emitSpadCommand(line2, "\\spadcommand{", outFile);
        else if ((line2 = strPrefix("\\spadpaste{", line)))
            emitSpadCommand(line2, "\\spadpaste{", outFile);
        else if ((line2 = strPrefix("\\example{", line)))
            emitSpadCommand(line2, "\\example{", outFile);
        else if ((line2 = strPrefix("\\graphpaste{", line)))
            emitSpadCommand(line2, "\\graphpaste{", outFile);
    }
    emitFooter(outFile);
    fclose(inFile);
    fclose(outFile);
    stat(filename,&buf);
    tvp.tv_sec =buf.st_mtime;
    tvp.tv_usec =0;
    if timercmp(&tvp,&latest_date[1],>){
        latest_date[1].tv_sec=buf.st_mtime;
        }
}

void
emitHeader(FILE *outFile, char *pageName, char *pageTitle)
{
    fprintf(outFile, "\\begin{page}{%s}{%s}\n", pageName, pageTitle);
    fprintf(outFile, "\\beginscroll\\beginmenu\n");
}

void
emitFooter(FILE *outFile)
{
    fprintf(outFile, "\\endmenu\\endscroll\\end{page}\n");
}

/* s is pageName}{title} */
void
emitMenuEntry(char *line, FILE *outFile)
{
    char pageName[MaxLineLength], title[MaxLineLength];
    char *p = pageName, *t = title;

    while (*line != '}')
        *p++ = *line++;
    *p = '\0';
    line++;
    while (*line != '}')
        *t++ = *line++;
    *t = '\0';
    fprintf(outFile, "\\menudownlink%s}{%s}\n", title, pageName);
}

void
emitSpadCommand(char *line, char *prefix, FILE *outFile)
{
    int braceCount = 1;
    char command[MaxLineLength], *t = command;

    while (1) {
        if (*line == '}')
            braceCount--;
        if (braceCount == 0)
            break;
        if (*line == '{')
            braceCount++;
        *t++ = *line++;
    }
    *t = '\0';
    fprintf(outFile, "%s%s}\n", prefix, command);
}

/* cover page functions */

FILE *coverFile;

void
openCoverPage(void)
{
    coverFile = fopen("coverex.ht", "w");
    if (coverFile == NULL) {
        fprintf(stderr, "couldn't open coverex.ht for writing\n");
        exit(-1);
    }
    fprintf(coverFile, "%% DO NOT EDIT! Created by ex2ht.\n\n");
    fprintf(coverFile, "\\begin{page}{ExampleCoverPage}{Examples Of FriCAS Commands}\n");
    fprintf(coverFile, "\\beginscroll\\table{\n");
}

void
closeCoverPage(void)
{
    fprintf(coverFile, "}\\endscroll\\end{page}\n\n");
}

void
closeCoverFile(void)
{
    fclose(coverFile);
    utimes("coverex.ht",latest_date);
}

void
emitCoverLink(char *name, char *title)
{
    fprintf(coverFile, "{\\downlink{%s}{%s}}\n", title, name);
}

void
addFile(char *filename)
{
    FILE *file = fopen(filename, "r");
    int c;

    if (file == NULL) {
        fprintf(stderr, "Couln't open %s for reading\n", filename);
        exit(-1);
    }
    while ((c = getc(file)) != EOF)
        putc(c, coverFile);
    putc('\n', coverFile);
    fclose(file);
    unlink(filename);
}
