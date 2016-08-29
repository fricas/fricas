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

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <limits.h>
#include <pwd.h>
#include <fcntl.h>
#include <termios.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <signal.h>

#include "fricas_c_macros.h"

#include "com.h"
#include "bsdsignal.h"
#include "sman.h"

#include "bsdsignal.H1"
#include "sockio-c.H1"
#include "openpty.H1"

char *ws_path = "$AXIOM/bin/AXIOMsys";
int start_clef = 1;                 /* start clef under spad */
int start_graphics = 1;             /* start the viewman */
int start_ht = 1;                   /* start hypertex */
int start_spadclient = 0;           /* Start the client spad buffer */
int start_local_spadclient = 1;     /* Start the client spad buffer */
int use_X;                      /* Use the X windows environment */
int server_num;                 /* FriCAS server number */
int tpd=0;                      /* to-print-debug information */

/************************************************/
/* definitions of programs which sman can start */
/************************************************/

char    *GraphicsProgram        = "$AXIOM/lib/viewman";
char    *HypertexProgram        = "$AXIOM/bin/hypertex -s";
char    *ClefProgram            =
           "$AXIOM/bin/clef -f $AXIOM/lib/command.list -e ";
char    *SessionManagerProgram  = "$AXIOM/lib/session";
char    *SpadClientProgram      = "$AXIOM/lib/spadclient";
char    *PasteFile              = NULL;
char    *MakeRecordFile         = NULL;
char    *VerifyRecordFile       = NULL;

SpadProcess *spad_process_list = NULL;
/***************************/
/* sman defaults file name */
/***************************/

#define SpadDefaultFile "spadprof.input"

char ClefCommandLine[256];

#define BufSize      4096       /* size of communication buffer */
char big_bad_buf[BufSize];      /* big I/O buffer */
char *eval_code;
const char *eval_fmt = " -eval \"\"";
size_t eval_buf_size = 1024;

Sock *session_io = NULL;        /* socket connecting to session manager */

/***********************************************************/
/* Some characters used and externally defined in edible.h */
/***********************************************************/

unsigned char  _INTR, _QUIT, _ERASE, _KILL, _EOF, _EOL, _RES1, _RES2;

/*************************************/
/* Stuff for opening pseudo-terminal */
/*************************************/

int ptsNum, ptcNum;
char ptsPath[20], ptcPath[20];

char **new_envp;                /* new environment for FriCAS */
int child_pid;                  /* child's process id */
struct termios oldbuf;           /* the original settings */
struct termios childbuf;         /* terminal structure for user i/o */


int death_signal = 0;

/* Concatenate str to buff escaping all potentially unsafe characters */
void
protcat(char * buff, char * str)
{
    int c;
    while(*buff) buff++;
    for(; (c = *str); str++) {
        if (isalnum(c) || c == '\n') {
            *buff = c;
            buff++;
        } else {
            *buff = '\\';
            buff[1] = c;
            buff += 2;
        }
    }
    *buff = 0;
}

static void
process_arguments(int argc,char ** argv)
{
  int arg;
  if (tpd == 1) fprintf(stderr,"sman:process_arguments entered\n");
  for (arg = 1; arg < argc; arg++) {
    if      (strcmp(argv[arg], "-debug")      == 0)
      tpd = 1;
    else if (strcmp(argv[arg], "-noclef")      == 0)
      start_clef = 0;
    else if (strcmp(argv[arg], "-clef")        == 0)
      start_clef = 1;
    else if (strcmp(argv[arg], "-gr")          == 0)
      start_graphics = 1;
    else if (strcmp(argv[arg], "-nogr")        == 0)
      start_graphics = 0;
    else if (strcmp(argv[arg], "-ht")          == 0)
      start_ht = 1;
    else if (strcmp(argv[arg], "-noht")        == 0)
      start_ht = 0;
    else if (strcmp(argv[arg], "-iw")          == 0)
      start_spadclient = 1;
    else if (strcmp(argv[arg], "-ihere")       == 0)
      start_local_spadclient = 1;
    else if (strcmp(argv[arg], "-noihere")     == 0)
      start_local_spadclient = 0;
    else if (strcmp(argv[arg], "-noiw")        == 0)
      start_spadclient = 0;
    else if (strcmp(argv[arg], "-ws")          == 0)
      ws_path = argv[++arg];
    else if (strcmp(argv[arg], "-comp")        == 0)
      ws_path = "$AXIOM/etc/images/comp";
    else if (strcmp(argv[arg], "-nox")         == 0)
      {
        use_X = 0;
        start_local_spadclient = 1;
        start_spadclient = 0;
        start_ht = 0;
        start_graphics = 0;
      }
    else if (strcmp(argv[arg], "-grprog")      == 0)
      GraphicsProgram = argv[++arg];
    else if (strcmp(argv[arg], "-htprog")      == 0)
      HypertexProgram = argv[++arg];
    else if (strcmp(argv[arg], "-clefprog")    == 0) {
      strcpy(ClefCommandLine,argv[++arg]);
      ClefProgram =
        strcat(ClefCommandLine, " -f $AXIOM/lib/command.list -e ");
    }
    else if (strcmp(argv[arg], "-sessionprog") == 0)
      SessionManagerProgram = argv[++arg];
    else if (strcmp(argv[arg], "-clientprog")  == 0)
      SpadClientProgram = argv[++arg];
    else if (strcmp(argv[arg], "-rm")  == 0)
      MakeRecordFile = argv[++arg];
    else if (strcmp(argv[arg], "-rv")  == 0)
      VerifyRecordFile = argv[++arg];
    else if (strcmp(argv[arg], "-paste")  == 0)
      PasteFile = argv[++arg];
    else if (strcmp(argv[arg], "-eval") == 0) {
      size_t elen = strlen(argv[++arg]);
      size_t olen = strlen(eval_code);
      size_t slen;
      if (elen > INT_MAX/10) {
          fprintf(stderr, "Code to eval is too big (size %ld)\n", elen);
          exit(1);
      }
      slen = olen + 2*elen + strlen(eval_fmt);
      if (slen >= INT_MAX/2) {
          fprintf(stderr, "Code to eval is too big (size %ld)\n", elen);
          exit(1);
      }
      if (slen > eval_buf_size) {
        eval_buf_size = 2*slen ;
        eval_code = (char *)realloc(eval_code, eval_buf_size + 1);
      }
      strcat(eval_code, " -eval ");
      protcat(eval_code + olen, argv[arg]);
    }
    else {
      fprintf(stderr, "Usage: sman <-clef|-noclef> \
<-gr|-nogr> <-ht|-noht> <-iw|-noiw> <-nox> <-comp> <-ws spad_workspace> \
<-grprog path> <-htprog path> <-clefprog path> <-sessionprog path> \
<-clientprog path>\n");
      exit(-1);
    }
  }
  if (tpd == 1)
  { fprintf(stderr,"  sman ");
    if (start_clef == 0)
      fprintf(stderr,"-noclef ");
    else
      fprintf(stderr,"-clef ");
    if (start_graphics == 0)
      fprintf(stderr,"-nogr ");
    else
      fprintf(stderr,"-gr ");
    if (start_ht == 0)
      fprintf(stderr,"-noht ");
    else
      fprintf(stderr,"-ht ");
    if (start_spadclient == 0)
      fprintf(stderr,"-noiw ");
    else
      fprintf(stderr,"-iw ");
    if (start_local_spadclient == 0)
      fprintf(stderr,"-noihere ");
    else
      fprintf(stderr,"-ihere ");
    if (start_local_spadclient == 0)
      fprintf(stderr,"-noihere ");
    else
      fprintf(stderr,"-ihere ");
    if (use_X == 0)
      fprintf(stderr,"-nox ");
    fprintf(stderr,"-ws ");
    fprintf(stderr,"'%s' ",ws_path);
    fprintf(stderr,"-grprog ");
    fprintf(stderr,"'%s' ",GraphicsProgram);
    fprintf(stderr,"-htprog ");
    fprintf(stderr,"'%s' ",HypertexProgram);
    fprintf(stderr,"-clefprog ");
    fprintf(stderr,"'%s' ",ClefCommandLine);
    fprintf(stderr,"-sessionprog ");
    fprintf(stderr,"'%s' ",SessionManagerProgram);
    fprintf(stderr,"-clientprog ");
    fprintf(stderr,"'%s' ",SpadClientProgram);
    fprintf(stderr,"-rm ");
    fprintf(stderr,"'%s' ",MakeRecordFile);
    fprintf(stderr,"-rv ");
    fprintf(stderr,"'%s' ",VerifyRecordFile);
    fprintf(stderr,"-paste ");
    fprintf(stderr,"'%s' ",PasteFile);
    fprintf(stderr,"\n");
  }
  if (tpd == 1) fprintf(stderr,"sman:process_arguments exit\n");
}

static int
in_X(void)
{
  if (getenv("DISPLAY")) return 1;
  return 0;
}

static void
process_options(int argc, char **argv)
{
    if (tpd == 1) fprintf(stderr,"sman:process_options entered\n");
    use_X = isatty(0) && in_X();
    process_arguments(argc, argv);
    if (tpd == 1) fprintf(stderr,"sman:process_options exit\n");
}

static void
death_handler(int sig)
{
  death_signal = 1;
}

static void
sman_catch_signals(void)
{

  /* Set up the signal handlers for sman */
  bsdSignal(SIGINT,  SIG_IGN,RestartSystemCalls);
  bsdSignal(SIGTERM, death_handler,RestartSystemCalls);
  bsdSignal(SIGQUIT, death_handler,RestartSystemCalls);
  bsdSignal(SIGHUP,  death_handler,RestartSystemCalls);
  bsdSignal(SIGILL,  death_handler,RestartSystemCalls);
  bsdSignal(SIGTRAP, death_handler,RestartSystemCalls);
#ifdef SIGABRT
  bsdSignal(SIGABRT, death_handler,RestartSystemCalls);
#else
 #ifdef SIGIOT
  bsdSignal(SIGIOT,  death_handler,RestartSystemCalls);
 #endif
#endif
  bsdSignal(SIGBUS,  death_handler,RestartSystemCalls);
  bsdSignal(SIGSEGV, death_handler,RestartSystemCalls);
  /* don't restart wait call on SIGUSR1  */

}

static void
fix_env(char **envp, int spadnum)
{
  int len, i;
  char *sn;
  for(len = 0; envp[len] != NULL; len++);
  new_envp = (char **) malloc((len + 3) * sizeof(char *));
  new_envp[0] = "SPADSERVER=TRUE";
  sn = (char *) malloc(20 * sizeof(char));
  sprintf(sn, "SPADNUM=%d", spadnum);
  new_envp[1] = sn;
  for(i=0; i<=len; i++)
    new_envp[i+2] = envp[i];
}

static void
init_term_io(void)
{
  if(!isatty(0)) return;
  if( tcgetattr(0, &oldbuf) == -1) {
    perror("getting termios");
    return ;                    /*  exit(-1); */
  }
  if( tcgetattr(0, &childbuf) == -1) {
    perror("getting termios");
    return ;                    /*   exit(-1); */
  }
  _INTR = oldbuf.c_cc[VINTR];
  _QUIT = oldbuf.c_cc[VQUIT];
  _ERASE = oldbuf.c_cc[VERASE];
  _KILL = oldbuf.c_cc[VKILL];
  _EOF = oldbuf.c_cc[VEOF];
  _EOL = oldbuf.c_cc[VEOL];
}

static char *
strPrefix(char *prefix,char * s)
{
  while (*prefix != '\0' && *prefix == *s) {
    prefix++;
    s++;
  }
  if (*prefix == '\0') return s;
  return NULL;
}

static void
check_spad_proc(char *file, char *prefix)
{
  char *num;
  int pid;
  if ((num = strPrefix(prefix, file))) {
    pid = atoi(num);
    if (pid > 2) {
      kill(pid, 0);
      if (kill(pid, 0) == -1 && errno == ESRCH) {
        unlink(file);
      }
    }
  }
}

static void
clean_up_old_sockets(void)
{
  char com[512], tmp_file[128];
  FILE *file;
  int len;
  sprintf(tmp_file, "/tmp/socks.%d", server_num);
  sprintf(com, "ls /tmp/.d* /tmp/.s* /tmp/.i* /tmp/.h* 2> %s > %s",
          tmp_file, tmp_file);
  system(com);
  file = fopen(tmp_file, "r");
  if (file == NULL) {
    fprintf(stderr, "Can't open socket listing file\n");
    return;
  }
  while(fgets(com, 512, file) != NULL) {
    len = strlen(com);
    if (len) com[len-1] = '\0';
    else break;
    check_spad_proc(com, "/tmp/.d");
    check_spad_proc(com, "/tmp/.s");
    check_spad_proc(com, "/tmp/.i");
    check_spad_proc(com, "/tmp/.h");
  }
  fclose(file);
  unlink(tmp_file);
}

static SpadProcess *
fork_you(int death_action)
{
  /* fork a new process, giving it a default death action */
  /* return NULL in child, SpadProcess in parent          */
  int child_pid = fork();
  SpadProcess *proc;
  if (!child_pid) return NULL;
  proc = (SpadProcess *) malloc(sizeof(SpadProcess));
  proc->proc_id = child_pid;
  proc->death_action = death_action;
  proc->command = NULL;
  proc->next = spad_process_list;
  spad_process_list = proc;
  return proc;
}

static void
exec_command_env(char *command,char ** env)
{
  char new_command[512];
  sprintf(new_command, "exec %s", command);
  execle("/bin/sh","/bin/sh", "-c", new_command, NULL, env);
}

static SpadProcess *
spawn_of_hell(char *command, int death_action)
{
  SpadProcess *proc = fork_you(death_action);
  if (proc != NULL) {
    proc->command = command;
    return proc;
  }
  exec_command_env(command, new_envp);
  return NULL;
}

static void
start_the_spadclient(void)
{
  char command[256];
  if (start_clef)
    sprintf(command,
          "xterm -sb -sl 500 -name fricasclient -n FriCAS -T FriCAS -e %s %s",
          ClefProgram, SpadClientProgram);
  else
    sprintf(command,
          "xterm -sb -sl 500 -name fricasclient -n FriCAS -T FriCAS -e %s",
          SpadClientProgram);
  if (tpd == 1)
    fprintf(stderr,"sman:start_the_spadclient: %s\n",command);
  spawn_of_hell(command, NadaDelShitsky);
}

static void
start_the_local_spadclient(void)
{
  char command[256];
  if (start_clef)
    sprintf(command, "%s  %s", ClefProgram, SpadClientProgram);
  else
    sprintf(command, "%s", SpadClientProgram);
  if (tpd == 1)
    fprintf(stderr,"sman:start_the_local_spadclient: %s\n",command);
  spawn_of_hell(command, NadaDelShitsky);
}

static void
start_the_session_manager(void)
{
  spawn_of_hell(SessionManagerProgram, Die);
}

static void
start_the_hypertex(void)
{
  char prog[512];

  if (PasteFile){
    sprintf(prog, "%s -k -ip %s", HypertexProgram, PasteFile);
    spawn_of_hell(prog, NadaDelShitsky);
  }
  else if (MakeRecordFile){
    sprintf(prog, "%s -k -rm %s", HypertexProgram,MakeRecordFile );
    spawn_of_hell(prog, NadaDelShitsky);
  }
  else if (VerifyRecordFile){
    sprintf(prog, "%s -k -rv %s", HypertexProgram, VerifyRecordFile);
    spawn_of_hell(prog, NadaDelShitsky);
  }
  else  spawn_of_hell(HypertexProgram, CleanHypertexSocket);
}

static void
start_the_graphics(void)
{
  spawn_of_hell(GraphicsProgram, DoItAgain);
}

/* Start the FriCAS session in a separate process, */
/* using a pseudo-terminal to catch all input and output */
static void
fork_FriCAS(void)
{
  char *augmented_ws_path;  /* will append directory path */
  char *tmp_pointer;
  SpadProcess *proc;

  proc =  fork_you(Die);
  child_pid = (proc == NULL ? 0 : proc->proc_id);
  switch(child_pid) {
  case -1 :
    fprintf(stderr, "Can't create a new process \n");
    exit(0);
  case 0:
    /* Dissasociate from my parents group so all my child processes */
    /* look at my terminal as the controlling terminal for the      */
    /* group                                                        */

    if(setsid() < 0) {
      perror("Dissassociating from parents group");
      exit(-1);
    }

    close(ptsNum);
    /* Now reopen the server side, so that pg, su, etc. work properly */

    if ((ptsNum =  open(ptsPath, O_RDWR)) < 0 ) {
      perror("fork_FriCAS: Failed to reopen server");
      fprintf(stderr, "ptsPath=%s\n", ptsPath);
      exit(-1);
    }

    /* since I am the child, I can close ptc, and dup pts for all its */
    /* standard descriptors                                           */

    if( (dup2(ptsNum, 0) == -1) ||
        (dup2(ptsNum, 1) == -1) ||
        (dup2(ptsNum, 2) == -1)  ) {
      perror("trying to dupe the child");
      exit(-1);
    }
    close(ptcNum);
    close(ptsNum);


    /* I also have to turn off echoing, since I am echoing all the */
    /* input myself                  */

    childbuf.c_lflag &= ~ECHO;
    if( tcsetattr(0, TCSAFLUSH, &childbuf) == -1) {
      perror("setting the term buffer");
      exit(-1);
    }
        augmented_ws_path = (char *)malloc(2 * strlen(ws_path) + strlen(eval_code) + strlen(" -- ") + 1);
    strcpy(augmented_ws_path,ws_path);          /* write the name    */
    /* Pass '--' to make sure that argument(s) passed to AXIOMsys
       do not cause trouble from host Lisp (Closure CL would
       treat argument as Lisp kernel name, Clisp signals error
       if it can not recognize the option. */
    strcat(augmented_ws_path," -- ");
    /* pass eval code directly to interpreter */
    strcat(augmented_ws_path,eval_code);
    strcat(augmented_ws_path, " ");
    strcat(augmented_ws_path,ws_path);          /* name again        */
    tmp_pointer = (char *)
      strrchr(augmented_ws_path,'/');      /*pointer to last /  */
    *(++tmp_pointer) = '\0';
    exec_command_env(augmented_ws_path, new_envp);

    /*    fprintf(stderr, "Cannot execute the %s system.\n", ws_path); */

        free(augmented_ws_path);
    exit(0);
  }
}

static void
start_the_FriCAS(char **envp)
{
  server_num = make_server_number();
  clean_up_old_sockets();
  if (server_num == -1) {
    fprintf(stderr, "could not get an FriCAS server number\n");
    exit(-1);
  }
  if (ptyopen(&ptcNum, &ptsNum, ptcPath, ptsPath) == -1) {
    perror("start_the_FriCAS: ptyopen failed");
    exit(-1);
  }
  fix_env(envp, server_num);
  fork_FriCAS();
  close(ptsNum);
}

static void
clean_hypertex_socket(void)
{
   char name[256];
   sprintf(name, "%s%d", MenuServerName, server_num);
   unlink(name);
}

static void
clean_up_sockets(void)
{
  char name[256];
  sprintf(name, "%s%d", SpadServer, server_num);
  unlink(name);
  sprintf(name, "%s%d", SessionServer, server_num);
  unlink(name);
  sprintf(name, "%s%d", SessionIOName, server_num);
  unlink(name);
  clean_hypertex_socket();
}

static void
read_from_spad_io(int ptcNum)
{
  int ret_code = 0, i=0;
  static int mes_len =0;
  ret_code = read(ptcNum, big_bad_buf, BufSize);
  if (ret_code == -1) {
    clean_up_sockets();
    exit(-1);
  }
  if (session_io == NULL) {
    if (ret_code < mes_len)
      mes_len -= ret_code;
    else {
      if (mes_len > 0) {
        i = mes_len;
        mes_len = 0;
      }
      else
        i = 0;
      ret_code = write(1, big_bad_buf+i, ret_code-i);
    }
  }
  else
    ret_code = swrite(session_io, big_bad_buf, ret_code,
                      "writing to session man");
  if (ret_code == -1) {
    perror("writing output to session manager");
    clean_up_sockets();
    exit(-1);
  }
}

static void
read_from_manager(int ptcNum)
{
  int ret_code;
  ret_code = sread(session_io, big_bad_buf, BufSize, "reading session io");
  if (ret_code == -1) {
    return;
  }
  ret_code = write(ptcNum, big_bad_buf, ret_code);
  if (ret_code == -1) {
    return;
  }
}

static void
manage_spad_io(int ptcNum)
{
  int ret_code, i, p;
  fd_set rd;
  while (1) {
    rd = socket_mask;
    FD_SET(ptcNum, &rd);
    if (session_io != NULL)
      FD_SET(session_io->socket, &rd);
    ret_code = sselect(FD_SETSIZE, &rd, 0, 0, NULL);
    if (ret_code == -1) {
      perror("Session manager select");
      clean_up_sockets();
      exit(-1);
    }
    if (FD_ISSET(ptcNum, &rd)) {
      read_from_spad_io(ptcNum);
    }
    for(i=0; i<2; i++) {
      if (server[i].socket > 0 && FD_ISSET(server[i].socket, &rd)) {
        p = fricas_accept_connection(server+i);
        switch(p) {
        case SessionIO:
          session_io = purpose_table[SessionIO];
          /*  printf("connected session manager\n\r");*/
          printf("\n");
          break;
        default:
          printf("sman: Unknown connection request type: %d\n", p);
          break;
        }
      }
    }
    if (session_io != NULL && FD_ISSET(session_io->socket, &rd)) {
      read_from_manager(ptcNum);
    }
  }
}

static void
init_spad_process_list(void)
{
  spad_process_list = NULL;
}

static SpadProcess *
find_child(int proc_id)
{
  SpadProcess *proc;
  for(proc = spad_process_list; proc != NULL; proc = proc->next)
    if (proc->proc_id == proc_id) return proc;
  return NULL;
}

static void
kill_all_children(void)
{
  char name[256];
  SpadProcess *proc;


  for(proc = spad_process_list; proc != NULL; proc = proc->next) {
    kill(proc->proc_id, SIGTERM);
  }
  sprintf(name, "/tmp/hyper%d.input",server_num);
  unlink(name);

}

static void
monitor_children(void)
{
  int dead_baby, stat;
  SpadProcess *proc;
  while (1) {
    stat = 0;
    dead_baby = wait(&stat);
    /* Check the value of dead_baby, since wait may have returned
       a pid but subsequently we have received a signal.  Yeuch! */
    if (dead_baby == -1 && death_signal) {
      kill_all_children();
      clean_up_sockets();
      fricas_sleep(200);
      exit(0);
    }

    if (dead_baby == -1) {
      fprintf(stderr, "sman: wait returned -1\n");
      continue;
    }
    proc = find_child(dead_baby);
    if (proc == NULL) {
      /*      fprintf(stderr, "sman: %d is not known to be a child process\n",
              dead_baby);
              */
      continue;
    }
    switch(proc->death_action) {
    case Die:
      kill_all_children();
      clean_up_sockets();
      fricas_sleep(200);
      exit(0);
    case NadaDelShitsky:
      break;
    case DoItAgain:
      spawn_of_hell(proc->command, DoItAgain);
      break;
    case CleanHypertexSocket:
      clean_hypertex_socket();
      break;
    }
  }
}

int
main(int argc, char *argv[],char *envp[])
{
  if (tpd == 1) fprintf(stderr,"sman:main entered\n");
  bsdSignal(SIGINT,  SIG_IGN,RestartSystemCalls);
  *(eval_code = (char *)malloc(eval_buf_size + 1)) = '\0';
  process_options(argc, argv);

  init_term_io();
  init_spad_process_list();
  start_the_FriCAS(envp);
  if (open_server(SessionIOName) == -2) {
    fprintf(stderr, "Fatal error opening I/O socket\n");
    clean_up_sockets();
    exit(-1);
  }
  start_the_session_manager();
  if (start_spadclient)       start_the_spadclient();
  if (start_local_spadclient) start_the_local_spadclient();
  if (start_ht)               start_the_hypertex();
  if (start_graphics)         start_the_graphics();
  fricas_sleep(100);

  if (fork_you(Die) != NULL) {
    sman_catch_signals();
    monitor_children();
    exit(0);
  }
  manage_spad_io(ptcNum);
  if (tpd == 1) fprintf(stderr,"sman:main exit\n");
  free(eval_code);
  return(0);
}
