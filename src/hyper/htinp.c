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
#include "debug.h"

#include <sys/stat.h>
#include <sys/signal.h>
#include <setjmp.h>

#include "hyper.h"
#include "parse.h"
#include "bsdsignal.h"

#include "all_hyper_proto.H1"
#include "sockio-c.H1"
#include "bsdsignal.H1"

static void make_input_file_list(void);
static void make_the_input_file(UnloadedPage * page);
static void make_input_file_from_page(HyperDocPage * page);
static int inListAndNewer(char * inputFile, char * htFile);
static void print_paste(FILE * pfile, char * realcom, char * command,
                        char * pagename, int com_type);
static void print_graph_paste(FILE * pfile, char * realcom, char * command,
                              char * pagename, int com_type);
static void send_command(char * command, int com_type);

extern char **input_file_list;
extern int input_file_count;
extern int make_patch_files;
extern int kill_spad;
extern jmp_buf jmpbuf;


#define MaxInputFiles 256
char *active_file_list[MaxInputFiles];
int num_active_files = 0;
char *inactive_file_list[MaxInputFiles];
int num_inactive_files = 0;
int include_bf = 0;
char buf_for_record_commands[256];



void
make_record(void)
{
  int i;
  for (i=0;i<input_file_count;i++){
    send_lisp_command("(|clearCmdCompletely|)");
    send_lisp_command("(setq |$testingSystem| T)");
    send_lisp_command("(setq |$printLoadMsgs| NIL)");
    send_lisp_command("(setq |$BreakMode| '|resume|)");
    sprintf(buf_for_record_commands,"(|inputFile2RecordFile| '\"%s\")",input_file_list[i]);
    fprintf(stderr,"%s\n",buf_for_record_commands);
    send_lisp_command(buf_for_record_commands);
  }
  if (kill_spad){
    i = connect_spad();
    if (i != NotConnected && i != SpadBusy)
      send_int(spad_socket, KillLispSystem);
  }

}

void
verify_record(void)
{
  int i;
  for (i=0;i<input_file_count;i++){
    send_lisp_command("(|clearCmdCompletely|)");
    send_lisp_command("(setq |$testingSystem| T)");
    send_lisp_command("(setq |$printLoadMsgs| NIL)");
    send_lisp_command("(setq |$BreakMode| '|resume|)");
    sprintf(buf_for_record_commands,"(|verifyRecordFile| '\"%s\")",input_file_list[i]);
    fprintf(stderr,"%s\n",buf_for_record_commands);
    send_lisp_command(buf_for_record_commands);
  }
  if (kill_spad) {
    i = connect_spad();
    if (i != NotConnected && i != SpadBusy)
      send_int(spad_socket, KillLispSystem);
  }
}


void
ht2_input(void)
{
  HashTable *table;
  HashEntry *entry;
  int i;

  bsdSignal(SIGUSR2, SIG_IGN,RestartSystemCalls);
  gWindow = alloc_hd_window();
  init_group_stack();
  table = gWindow->fPageHashTable;
  make_input_file_list();
  for (i = 0; i < table->size; i++)
    for (entry = table->table[i]; entry != NULL; entry = entry->next)
      make_the_input_file((UnloadedPage *) entry->data);
  if (kill_spad){
    i = connect_spad();
    if (i != NotConnected && i != SpadBusy)
      send_int(spad_socket, KillLispSystem);
  }
}

static char *
make_input_file_name(char *buf, char *filename)
{
    char *b, *c;

    strcpy(buf, filename);
    for (b = buf + strlen(buf) - 1; b != buf && *b != '/'; b--);
    if (b != buf)
        b = b + 1;
    for (c = b; *c != '.' || c[1] != 'h' || c[2] != 't'; c++);
    strcpy(c, ".input");
    return b;
}

static char *
make_paste_file_name(char *buf, char *filename)
{
    char *b, *c;

    strcpy(buf, filename);
    for (b = buf + strlen(buf) - 1; b != buf && *b != '/'; b--);
    if (b != buf)
        b = b + 1;
    for (c = b; *c != '.' || c[1] != 'h' || c[2] != 't'; c++);
    strcpy(c, ".pht");
    return b;
}

static void
make_the_input_file(UnloadedPage *page)
{
    char buf[2048], *b;

    if (!page->fpos.name)
        return;
    b = make_input_file_name(buf, page->fpos.name);
    if (inListAndNewer(b, page->fpos.name)) {
        printf("parsing: %s\n", page->name);
        if (setjmp(jmpbuf)) {
            printf("Syntax error!\n");
        }
        else {
            load_page((HyperDocPage *)page);
            make_input_file_from_page(gWindow->page);
        }
    }
}

int example_number;

static void
make_input_file_from_page(HyperDocPage *page)
{
  TextNode *node;
  int starting_file = 1,/* i,*/ /*len,*/ ret_val;
  char *buf, buf2[1024], buf3[1024];
  char *b, *c, *com;
  FILE *file = NULL;
  FILE *pfile = NULL;
  static HyperDocPage *op = NULL;

  if (op == page)
    return;
  op = page;
  if (page == NULL)
    return;
  b = make_input_file_name(buf2, page->filename);
  c = make_paste_file_name(buf3, page->filename);
  if (inListAndNewer(b, page->filename)) {
    /* open and prepare the input file */
    file = fopen(b, "a");
    if (file == NULL) {
      fprintf(stderr, "couldn't open output file %s\n", b);
      exit(-1);
    }
    fprintf(file, "\n-- Input for page %s\n", page->name);
    fprintf(file, ")clear all\n\n");

    for (node = page->scrolling; node != NULL; node = node->next)
      if (node->type == Spadcommand || node->type == Spadgraph
          || node->type == Spadsrc) {
        if (starting_file) {
          example_number = 1;
          if (make_patch_files) {
            send_lisp_command("(|clearCmdAll|)");
            send_lisp_command("(|resetWorkspaceVariables|)");
            send_lisp_command("(setq $linelength 55)");
            send_lisp_command("(setq |$printLoadMsgs| NIL)");
            send_lisp_command("(setq |$UserLevel| '|development|)");
            pfile = fopen(c, "a");
            if (pfile == NULL) {
              fprintf(stderr, "couldn't open output file %s\n", c);
              exit(-1);
            }
          }
          starting_file = 0;
        }
        else
          example_number++;
        buf = print_to_string(node->next);
        com = alloc_string(buf);
        fprintf(file, "%s\n", buf);
        fflush(file);
        fprintf(stderr, "writing:\t%s\n", buf);
        include_bf = 1;
        buf = print_to_string(node->next);
        include_bf = 0;
        if (make_patch_files) {
          if (node->type == Spadcommand || node->type == Spadsrc)
            print_paste(pfile, com, buf, page->name, node->type);
          else
            print_graph_paste(pfile, com, buf, page->name, node->type);
        }
      }
    if (!starting_file && make_patch_files) {
      ret_val = fclose(pfile);
      if (ret_val == -1) {
        fprintf(stderr, "couldn't close file %s\n", b);
        exit(-1);
      }
    }
    ret_val = fclose(file);
    if (ret_val == -1) {
      fprintf(stderr, "couldn't close file %s\n", b);
      exit(-1);
    }
  }
}

char *
strCopy(char *s)
{
    char *b = halloc(strlen(s) + 1,"String");

    strcpy(b, s);
    return b;
}

static int
inListAndNewer(char *inputFile, char *htFile)
{
    int ret_val, found = 0, i;
    struct stat htBuf, inputBuf;

    for (i = 0; i < num_active_files; i++) {
        if (strcmp(active_file_list[i], inputFile) == 0) {
            found = 1;
            break;
        }
    }
    if (found)
        return 1;
    found = 0;
    for (i = 0; i < num_inactive_files; i++)
        if (strcmp(inactive_file_list[i], inputFile) == 0) {
            found = 1;
            break;
        }
    if (found)
        return 0;
    found = 0;
    for (i = 0; i < input_file_count; i++)
        if (strcmp(input_file_list[i], inputFile) == 0) {
            found = 1;
            break;
        }
    if (!found) {
        inactive_file_list[num_inactive_files++] = strCopy(inputFile);
        return 0;
    }
    ret_val = stat(inputFile, &inputBuf);
    if (ret_val == -1) {
        active_file_list[num_active_files++] = input_file_list[i];
        printf("making %s\n", inputFile);
        return 1;
    }
    ret_val = stat(htFile, &htBuf);
    if (ret_val == -1) {
        inactive_file_list[num_inactive_files++] = strCopy(inputFile);
        return 0;
    }
    ret_val = htBuf.st_mtime > inputBuf.st_mtime;
    ret_val = 1;
    if (ret_val) {
        active_file_list[num_active_files++] = input_file_list[i];
        printf("making %s\n", inputFile);
        unlink(inputFile);
    }
    else
        inactive_file_list[num_inactive_files++] = input_file_list[i];
    return ret_val;
}

static void
make_input_file_list(void)
{
    int i;
    char buf[2048], *name;

    for (i = 0; i < input_file_count; i++) {
        name = make_input_file_name(buf, input_file_list[i]);
        input_file_list[i] = (char *) halloc(strlen(name) + 1,"Input Filename");
        strcpy(input_file_list[i], name);
    }
}

void
print_paste_line(FILE *pfile,char *str)
{
    char *free = "\\free", *bound = "\\bound", *f = free, *b = bound;
    int justSaw = 0;

    for (; *str; str++) {
        if (*f == '\0')
            justSaw = 2;
        if (*b == '\0')
            justSaw = 2;
        if (*b == *str)
            b++;
        else
            b = bound;
        if (*f == *str)
            f++;
        else
            f = free;
        if (*str == '%' || *str == '{' || *str == '}' || *str == '#') {
            if (*str == '{' && justSaw)
                justSaw--;
            else if (*str == '}' && justSaw)
                justSaw--;
            else
                putc('\\', pfile);
        }
        putc(*str, pfile);
    }
}



void
get_spad_output(FILE *pfile,char *command,int com_type)
{
    int n, i;
    char buf[1024];

    send_command(command, com_type);
    n = get_int(spad_socket);
    for (i = 0; i < n; i++) {
        get_string_buf(spad_socket, buf, 1024);
        fprintf(pfile, "%s\n", buf);
    }
    unescape_string(command);
}

/*
 * THEMOS says: There is a problem here in that we issue the (|close|) and
 * then go on. If this is the last command, we will soon send a SIGTERM and
 * the whole thing will collapse maybe BEFORE the writing out has finished.
 * Fix: Call a Lisp function that checks (with \spadop{key} ps and grep) the
 * health of the viewport. We do this after the (|close|).
 */
void
get_graph_output(char *command,char *pagename,int com_type)
{
    int n, i;
    char buf[1024];

    send_command(command, com_type);
    n = get_int(spad_socket);
    /* FIXME: this looks crazy */
    for (i = 0; i < n; i++) {
        get_string_buf(spad_socket, buf, 1024);
    }
    sprintf(buf, "(|processInteractive| '(|write| |%s| \"%s%d\" \"image\") NIL)", "%",
            pagename, example_number);
    send_lisp_command(buf);
    send_lisp_command("(|setViewportProcess|)");
    send_lisp_command("(|processInteractive| '(|close| (|%%| -3)) NIL)");
    send_lisp_command("(|waitForViewport|)");
    get_int(spad_socket);
}
static void
send_command(char *command,int com_type)
{
    char buf[1024];

    if (com_type != Spadsrc) {
        escape_string(command);
        sprintf(buf, "(|parseAndEvalToHypertex| '\"%s\")", command);
        send_lisp_command(buf);
    }
    else {
        FILE *f;
        char name[512], str[512]/*, *c*/;

        sprintf(name, "/tmp/hyper%s.input", getenv("SPADNUM"));
        f = fopen(name, "w");
        if (f == NULL) {
            fprintf(stderr, "Can't open temporary input file %s\n", name);
            return;
        }
        fprintf(f, "%s", command);
        fclose(f);
        sprintf(str, "(|parseAndEvalToHypertex| '\")read %s\")", name);
        send_lisp_command(str);
    }
}

static void
print_paste(FILE *pfile,char *realcom,char *command,char *pagename,int com_type)
{
    fprintf(pfile, "\\begin{patch}{%sPatch%d}\n", pagename, example_number);
    fprintf(pfile, "\\begin{paste}{%sFull%d}{%sEmpty%d}\n",
            pagename, example_number, pagename, example_number);
    fprintf(pfile, "\\pastebutton{%sFull%d}{\\hidepaste}\n",
            pagename, example_number);
    fprintf(pfile, "\\tab{5}\\spadcommand{");
    print_paste_line(pfile, command);
    fprintf(pfile, "}\n");
    fprintf(pfile, "\\indentrel{3}\\begin{verbatim}\n");
    get_spad_output(pfile, realcom, com_type);
    fprintf(pfile, "\\end{verbatim}\n");
    fprintf(pfile, "\\indentrel{-3}\\end{paste}\\end{patch}\n\n");

    fprintf(pfile, "\\begin{patch}{%sEmpty%d}\n", pagename, example_number);
    fprintf(pfile, "\\begin{paste}{%sEmpty%d}{%sPatch%d}\n",
            pagename, example_number, pagename, example_number);
    fprintf(pfile, "\\pastebutton{%sEmpty%d}{\\showpaste}\n",
            pagename, example_number);
    fprintf(pfile, "\\tab{5}\\spadcommand{");
    print_paste_line(pfile, command);
    fprintf(pfile, "}\n");
    fprintf(pfile, "\\end{paste}\\end{patch}\n\n");
    fflush(pfile);
}
static void
print_graph_paste(FILE *pfile,char *realcom,char *command,char *pagename,int com_type)
{
    fprintf(pfile, "\\begin{patch}{%sPatch%d}\n", pagename, example_number);
    fprintf(pfile, "\\begin{paste}{%sFull%d}{%sEmpty%d}\n",
            pagename, example_number, pagename, example_number);
    fprintf(pfile, "\\pastebutton{%sFull%d}{\\hidepaste}\n",
            pagename, example_number);
    fprintf(pfile, "\\tab{5}\\spadgraph{");
    print_paste_line(pfile, command);
    fprintf(pfile, "}\n");
    fprintf(pfile, "\\center{\\unixcommand{\\inputimage{\\env{AXIOM}/share/viewports/%s%d.VIEW/image}}{\\env{AXIOM}/bin/viewAlone\\space{1} \\env{AXIOM}/share/viewports/%s%d}}\n", pagename, example_number, pagename, example_number);
    get_graph_output(realcom, pagename, com_type);
    fprintf(pfile, "\\end{paste}\\end{patch}\n\n");

    fprintf(pfile, "\\begin{patch}{%sEmpty%d}\n", pagename, example_number);
    fprintf(pfile, "\\begin{paste}{%sEmpty%d}{%sPatch%d}\n",
            pagename, example_number, pagename, example_number);
    fprintf(pfile, "\\pastebutton{%sEmpty%d}{\\showpaste}\n",
            pagename, example_number);
    fprintf(pfile, "\\tab{5}\\spadgraph{");
    print_paste_line(pfile, command);
    fprintf(pfile, "}\n");
    fprintf(pfile, "\\end{paste}\\end{patch}\n\n");
    fflush(pfile);
}
