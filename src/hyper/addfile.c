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

#include "debug.h"

#include "hyper.h"

#include <sys/stat.h>
#include <errno.h>

#include "all_hyper_proto.H1"


char *gDatabasePath = NULL;

static int pathname(char * name);

static int
strpostfix(char *s, char *t)
{
    int slen = strlen(s), tlen = strlen(t);
    return (tlen < slen) && !strcmp(s+slen-tlen, t);
}

/* extend_ht : just checks the name and adds a .ht if needed */

void
extend_ht(char *name)
{

    if (!strpostfix(name, ".ht") && !strpostfix(name, ".pht"))
        strcat(name, ".ht");
    return;
}

#define cwd(n) ((n[0] == '.' && n[1] == '/')?(1):(0))

/*
 * This procedure is sent a filename, and from it tries to build the full
 * filename, this it returns in the fullname variable. If the file is not
 * found, then it returns a -1. The fname is the fullpath name for the file,
 * including the .ht extension. The aname is the filename minus the added .ht
 * extension, and the pathname.
 */

static int
build_ht_filename(char *fname, char *aname, char *name)
{
    char cdir[2048];
    char *c_dir;
    char *HTPATH;
    char *trace;
    char *trace2;
    int ht_file;

    if (cwd(name)) {
        /* user wants to use the current working directory */
        c_dir = (char *) getcwd(cdir, 2046);
        strcpy(fname, c_dir);

        /* Now add the rest of the filename */
        strcat(fname, "/");
        strcat(fname, &name[2]);

        /** now copy the actual file name to addname **/
        for (trace = &name[strlen(name)]; trace != name &&
             (*trace != '/'); trace--);
        if (trace == name) {
            fprintf(stderr, "ht_open_file: Didn't expect a filename like %s\n",
                    name);
            exit(-1);
        }
        trace++;
        strcpy(aname, trace);

        /** add  the .ht extension if needed **/
        extend_ht(aname);
        extend_ht(fname);

        /* Now just try to access the file */
        return (access(fname, R_OK));
    }
    else if (pathname(name)) {
        /* filename already has the path specified */
        strcpy(fname, name);

        /** now copy the actual file name to addname **/
        for (trace = &name[strlen(name)]; trace != name &&
             (*trace != '/'); trace--);
        if (trace == name) {
            fprintf(stderr, "ht_open_file: Didn't expect a filename like %s\n",
                    name);
            exit(-1);
        }
        trace++;
        strcpy(aname, trace);

        /** add  the .ht extension if needed **/
        extend_ht(aname);
        extend_ht(fname);

        /* Now just try to access the file */
        return (access(fname, R_OK));
    }
    else {/** If not I am going to have to append path names to it **/
        HTPATH = (char *) getenv("HTPATH");
        if (HTPATH == NULL) {
        /** The user does not have a HTPATH, so I will use the the directory
        $AXIOM/share/hypertex/pages/ as the default path ***/
          char *spad = (char *) getenv("AXIOM");
          if (spad == NULL) {
            fprintf(stderr,
            "ht_file_open:Cannot find ht data base: setenv HTPATH or AXIOM\n");
             exit(-1);
          }
          HTPATH = (char *) halloc(1024 * sizeof(char), "HTPATH");
          strcpy(HTPATH, spad);
          strcat(HTPATH, "/share/hypertex/pages");
        }

        /** Now that I have filled HTPATH, I should try to open a file by the
          given name **/
        strcpy(aname, name);
        extend_ht(aname);
        for (ht_file = -1, trace2 = HTPATH;
             ht_file == -1 && *trace2 != '\0';) {
            for (trace = fname; *trace2 != '\0' && (*trace2 != ':');)
                *trace++ = *trace2++;
            *trace++ = '/';
            *trace = 0;
            if (!strcmp(fname, "./")) {
                /** The person wishes me to check the current directory too **/
                getcwd(fname, 256);
                strcat(fname, "/");
            }
            if (*trace2)
                trace2++;
            strcat(fname, aname);
            ht_file = access(fname, R_OK);
        }
        return (ht_file);
    }
}

static int
pathname(char *name)
{
    while (*name)
        if (*name++ == '/')
            return 1;

    return 0;
}

/** This procedure opens the proper HT file **/

FILE *
ht_file_open(char *fname, char *aname, char *name)
{
    FILE *ht_fp;
    int ret_value;

    ret_value = build_ht_filename(fname, aname, name);
    if (ret_value == -1) {
        fprintf(stderr, "ht_file_open: Unknown file %s\n", fname);
        exit(-1);
    }

    ht_fp = fopen(fname, "r");
    if (ht_fp == NULL) {
        perror("ht_file_open");
        exit(-1);
    }
    return (ht_fp);
}

/*
 * This function is responsible for actually opening the database file. For the
 * moment it gets the $AXIOM environment variable, and appends to it
 * "share/hypertex/ht.db", and then opens it
 */

/*
 * Modified on 12/3/89 to take a second argument. This argument tells the
 * open routine whether it is reading the db file, or writing it. If writing
 * is true, then I should check to insure I have proper write access.
 * -JMW
 */

/*
 * Modified again on 12/9/89 so that it now uses HTPATH as the path name. Now
 * it initially loads up the path name into a static variable. Then upon
 * every trip, it gets the next ht.db found. It returns NULL when no ht.db is
 * found. -JMW
 */


FILE *
db_file_open(char *db_file)
{
    static char *db_path_trace = NULL;
    char *db_file_trace;
    FILE *db_fp;
    char *spad;

    /*
     * The first time through is the only time this could be true. If so, then
     * create the default HTPATH for gDatabasePath.
     */
/*    fprintf(stderr,"addfile:db_file_open: entered db_file=%s\n",db_file);*/
    if (gDatabasePath == NULL) {
        gDatabasePath = (char *) getenv("HTPATH");
        if (gDatabasePath == NULL) {
            spad = (char *) getenv("AXIOM");
            if (spad == NULL) {
/*                fprintf(stderr,
                   "addfile:db_file_open: Cannot find ht data base path:\n");*/
                exit(-1);
            }
            gDatabasePath = (char *) halloc(sizeof(char) * 1024, "db_file_open");
            strcpy(gDatabasePath, spad);
            strcat(gDatabasePath, "/share/hypertex/pages");
        }
        db_path_trace = gDatabasePath;
    }
/*fprintf(stderr,"addfile:db_file_open: db_path_trace=%s\n",db_path_trace);*/
    /*
     * Now Loop until I find one with okay filename
     */

    for (db_fp = NULL; db_fp == NULL && *db_path_trace != '\0';) {
        for (db_file_trace = db_file; *db_path_trace != ':' &&
             *db_path_trace != '\0'; db_path_trace++)
            *db_file_trace++ = *db_path_trace;
        *db_file_trace = '\0';
        strcat(db_file_trace, "/ht.db");
/*fprintf(stderr,"addfile:db_file_open: db_file_trace=%s\n",db_file_trace);*/
/*fprintf(stderr,"addfile:db_file_open: db_file=%s\n",db_file);*/

        db_fp = fopen(db_file, "r");

        if (*db_path_trace != '\0')
            db_path_trace++;
    }
/*    if (db_fp == NULL)
      fprintf(stderr,"addfile:db_file_open: exit (null)\n");
    else
      fprintf(stderr,"addfile:db_file_open: exit opened\n");
*/
    return (db_fp);
}


FILE *
temp_file_open(char *temp_db_file)
{
    FILE *temp_db_fp;

    /** Just make the name and open it **/

    strcpy(temp_db_file, temp_dir);
    strcat(temp_db_file, "ht2.db" /* db_file_name */ );
    temp_db_fp = fopen(temp_db_file, "w");

    if (temp_db_fp == NULL) {
        perror("temp_file_open");
        exit(-1);
    }
    return temp_db_fp;
}
