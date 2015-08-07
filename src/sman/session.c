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

/* #define DEBUG */

#include "fricas_c_macros.h"
#include <stdlib.h>
#include <sys/time.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#ifdef SGIplatform
#include <bstring.h>
#endif
#include "com.h"
#include "bsdsignal.h"
#include "sockio-c.H1"
#include "bsdsignal.H1"

#define BufSize         4096    /* size of communication buffer */

typedef struct sock_list {      /* linked list of Sock */
  Sock Socket;
  struct sock_list *next;
} Sock_List;

Sock *spad_io = (Sock *) 0;        /* to_server socket for SessionIO */
Sock *spad_server = (Sock *) 0;    /* to_server socket for SpadServer    */
Sock *menu_client = (Sock *) 0;    /* to_client socket for MenuServerName  */
Sock *active_session = (Sock *) 0; /* pointer to currently active session */

Sock_List *plSock = (Sock_List *) 0;

char big_bad_buf[BufSize];      /* big I/O buffer */
int num_active_clients = 0;     /* number of InterpWindows attached */
int reading_output = 0;
fd_set session_socket_mask;

static void
usr1_handler(int sig)
{
  return;
}

static void
usr2_handler(int sig)
{
  send_signal(spad_server, SIGINT);
  return;
}

static void
term_handler(int sig)
{
  exit(1);
}

static void
pr()
{
  Sock_List *pSock;

  fprintf(stderr,"The socket list:\n");
  for(pSock=plSock;pSock!=(Sock_List *)0;pSock=pSock->next){
    fprintf(stderr,"(%d,%d,%d)\t",pSock->Socket.pid,2<<(pSock->Socket.socket),pSock->Socket.frame);
  }
  fprintf(stderr,"\n");
}

static void
close_client(int frame)
{
  Sock_List *pSock,*locSock;
  int socket_fd;

  /* we will check for frame equality,
     kill with send_signal,
     notify HyperTex so that it updates its list (if it's a spadbuf),
     repair the list,
     unset the active_session,
     update num_active_clients
     */


  /* first check head */
#ifdef DEBUG
fprintf(stderr,"close_client(%d)\n",frame);
#endif

  if ( (plSock) && (plSock->Socket.frame == frame) ){
    socket_fd = plSock->Socket.socket;
    send_signal((Sock *)plSock, SIGTERM);
    if ( menu_client != (Sock *) 0){
      send_int(menu_client,CloseClient);
      send_int(menu_client,(*plSock).Socket.pid);
    }
#ifdef DEBUG
fprintf(stderr,"trying to clear %u\n",socket_fd);
#endif
    FD_CLR(socket_fd,&session_socket_mask);
    locSock = plSock;
    if ((*plSock).next == (Sock_List *) 0)
      {plSock = (Sock_List *) 0;}
    else
      {plSock = plSock->next;}
    active_session = (Sock *) 0;
    num_active_clients--;
    free(locSock);
  }

  /* now check the rest */

  else {
    for (pSock=plSock; pSock->next != (Sock_List *) 0 ; pSock=pSock->next)
      if (pSock->next->Socket.frame == frame){
        socket_fd = pSock->next->Socket.socket;
        send_signal((Sock *)pSock->next, SIGTERM);
        if ( menu_client != (Sock *) 0){
          send_int(menu_client,CloseClient);
          send_int(menu_client,(*plSock).Socket.pid);
        }
#ifdef DEBUG
fprintf(stderr,"trying to clear %u\n",socket_fd);
#endif
        FD_CLR(socket_fd,&session_socket_mask);
        locSock = pSock->next;
        if (  pSock->next->next == (Sock_List *) 0  )
          { pSock->next= (Sock_List *) 0;}
        else
          { pSock->next = pSock->next->next;}
        num_active_clients--;
        active_session = (Sock *) 0;
        free(locSock);
        break;
      }
  }
#ifdef DEBUG
pr();
#endif
}

static void
read_SpadServer_command(void)
{
  int cmd, frame, num;
  cmd  = get_int(spad_server);
  switch (cmd) {
  case EndOfOutput:
    if (menu_client != (Sock *) 0) send_signal(menu_client, SIGUSR2);
    if (reading_output != 0) reading_output = 0;
    break;
  case QueryClients:
    /*  don't count MenuServer */
    num =  num_active_clients ;
    send_int(spad_server, num);
    break;
  case CloseClient:
    frame = get_int(spad_server);
    if (frame != -1) close_client(frame);
    break;
  case SendXEventToHyperTeX:
    break;
  default:
    fprintf(stderr, "session : unknown command from SpadServer %d\n", cmd);
    break;
  }
}

static int
test_sock_for_process(Sock *sock)
{
  if (sock == (Sock *)0 ) return -1;
  return kill(sock->pid, 0);
}

static void
read_menu_client_command(void)
{
  int cmd,frame, i,socket_fd;
  Sock_List *pSock;

  /* save it for possible clearing */
  socket_fd =  menu_client->socket;

  if (test_sock_for_process(menu_client) == -1) {
    FD_CLR(socket_fd,&session_socket_mask);
    menu_client = (Sock *) 0;
    reading_output = 0;
    return;
  }
  cmd = get_int(menu_client);
  switch(cmd) {
  case -1:              /* socket closed */
    FD_CLR(socket_fd,&session_socket_mask);
    menu_client = (Sock *) 0;
    reading_output = 0;
    break;
  case SwitchFrames:
#ifdef DEBUG
fprintf(stderr,"menu:SwitchFrames\n");
#endif
    frame = get_int(menu_client);
    send_int(spad_server, SwitchFrames);
    send_int(spad_server, frame);
    for(i=0,pSock=plSock; pSock != (Sock_List *) 0 ; i++,pSock=pSock->next)
      if (pSock->Socket.frame == frame) {
        active_session = (Sock *)pSock;
        reading_output = 1;
        break;
      }
    if (i == num_active_clients) {
      /* fprintf(stderr, "Couldn't find socket for frame %d\n", frame); */
    }
    break;
  case QuerySpad:
#ifdef DEBUG
fprintf(stderr,"menu:QuerySpad\n");
#endif
    send_int(menu_client, reading_output);
    break;
  default:
    fprintf(stderr, "session : unknown command from MenuServer: %d\n", cmd);
    menu_client = (Sock *) 0;
    break;
  }
}

static void
read_from_spad_io(void)
{
  int ret_code;
  ret_code = sread(spad_io, big_bad_buf, BufSize, "session: stdout socket");
  if (ret_code == -1) return;
  if(active_session != (Sock *) 0) {
    ret_code = swrite(active_session, big_bad_buf, ret_code,
                      NULL);
  }
}

static void
kill_spad(void)
{
  int i;
  Sock_List *pSock;

  send_signal(spad_server, SIGTERM);
  for  (pSock=plSock,i=0;
        (i<num_active_clients) && (pSock != (Sock_List *) 0);
        i++,pSock=pSock->next) {
    if ((pSock->Socket).socket != 0)
      send_signal((Sock *)pSock, SIGTERM);
  }
  if (menu_client != (Sock *) 0) send_signal(menu_client, SIGTERM);
  exit(0);
}

static int
accept_session_connection(Sock *server_sock)
{
  int sock_fd, ret_code;
  Sock_List *pls;

  /* Could be three things : KillSpad MenuServer InterpWindow  */

  pls = (Sock_List *) malloc(sizeof (Sock_List));
  sock_fd = accept(server_sock->socket, 0, 0);
  if (sock_fd == -1) {
    perror("session : accepting connection");
    return -1;
  }
  (pls->Socket).socket = sock_fd;
    get_socket_type((Sock *)pls);

    switch((pls->Socket).purpose) {
    case KillSpad:
      kill_spad();
      return KillSpad;
      free(pls);
    case MenuServer:
#ifdef DEBUG
      fprintf(stderr,"session: accepted MenuServer , fd = %d\n",sock_fd);
#endif
      menu_client = &(pls->Socket);
      FD_SET(menu_client->socket, &session_socket_mask);
      return MenuServer;
    case InterpWindow:
#ifdef DEBUG
      fprintf(stderr,"session: accepted InterpWindow , fd = %d\n",sock_fd);
#endif

      /* new Sock is put at the head of the list */
      if (plSock == (Sock_List *)0 ) {
        plSock = pls;
        plSock->next = (Sock_List *)0 ;
      }
      else{
        pls->next = plSock;
        plSock = pls;
      }

      /* we need to maintain session_socket_mask here since we roll our own accept */

      FD_SET(plSock->Socket.socket, &session_socket_mask);
      send_int(spad_server, CreateFrame);
      {
          int command = get_int(spad_server);
          /* XXX hack -- the whole protocol looks broken, we just
          try to detect losage */
          if (command != CreateFrameAnswer) {
              fprintf(stderr, "session: fatal, got out of sync "
                               "with Spad server\n  (lost race)\n");
              exit(1);
          }
      }
      plSock->Socket.frame = get_int(spad_server);
      active_session = (Sock *)plSock;
      get_string_buf(spad_server, big_bad_buf, BufSize);
      ret_code = swrite((Sock *)plSock, big_bad_buf, strlen(big_bad_buf)+1,
                        "session: writing to InterpWindow");
      if (ret_code == -1)
        return -1;
      num_active_clients++;
#ifdef DEBUG
pr();
#endif
      return plSock->Socket.purpose;
    }
    return (-1);
}

static void
read_from_session(Sock *sock)
{
  int ret_code;
  if (sock != active_session) {
    send_int(spad_server, SwitchFrames);
    send_int(spad_server, sock->frame);
  }
  active_session = sock;
  ret_code = sread(sock, big_bad_buf, BufSize,
                   "session: reading InterpWindow");
  if (ret_code == -1) {
    active_session = (Sock *) 0;
    reading_output = 0;
    return;
  }
  ret_code = swrite(spad_io, big_bad_buf, ret_code,
                    "session: writing SessionIO");
  if (ret_code == -1) {
    active_session = (Sock *)0 ;
    reading_output = 0;
    return;
  }
  reading_output = 1;
}

static void
manage_sessions(void)
{
  int ret_code;
  fd_set rd, wr, ex;
  Sock_List  *pSock;

  reading_output = 0;
  while (1) {
    FD_ZERO(&rd);
    FD_ZERO(&wr);
    FD_ZERO(&ex);

    /* Allow server socket and all connections if not waiting for output
       socket_mask is maintained by libspad.a  */
#ifdef DEBUG
fprintf(stderr,"session_socket_mask=%u ",*((long *)session_socket_mask.fds_bits));
#endif
    rd = session_socket_mask;
    if (!reading_output) {
      rd = session_socket_mask;
    }

    /* Allow the active_session if set */
    if (active_session) FD_SET(active_session->socket, &rd);
#ifdef DEBUG
fprintf(stderr,"[rd=%u ",*((long *)rd.fds_bits));
#endif

    ret_code = sselect(FD_SETSIZE, &rd, &wr, &ex, NULL);
    if (ret_code == -1) {
        break;
    }
#ifdef DEBUG
fprintf(stderr,"rd=%u]\n",*((long *)rd.fds_bits));
#endif

    if ((menu_client != (Sock *) 0)  && FD_ISSET(menu_client->socket, &rd)) {
      /* MenuServer wants to talk */
      read_menu_client_command(); }


    if (FD_ISSET(spad_io->socket, &rd)) {
      /* Lisp has output */
      read_from_spad_io(); }


    if (FD_ISSET(server[1].socket, &rd)) {
      /* Someone wants to connect to our server socket */
      accept_session_connection(server+1); }


    for(pSock=plSock; pSock != (Sock_List *) 0 ; pSock=pSock->next) {
      if ((active_session == (Sock *)pSock || !reading_output) &&
          (pSock->Socket).socket>0 && FD_ISSET(pSock->Socket.socket, &rd)) {
        /* An InterpWindow */
        read_from_session((Sock *)pSock); }
    }


    if (FD_ISSET(spad_server->socket, &rd)) {
      /* The Lisp socket */
      read_SpadServer_command(); }
  }
}

int
main(void)
{

#ifdef DEBUG2
  /* delay for attaching with debugger before interesting things happen */
  sleep(30);
#endif

 /* spad_server connects to Lisp server socket
    read_SpadServer_command handles requests */
  spad_server = connect_to_local_server(SpadServer, SessionManager, Forever);
  if (spad_server == (Sock *) 0) {
    fprintf(stderr, "session: Cannot connect to FriCAS server!\n");
    exit(0);
  }
  else {
#ifdef DEBUG
    fprintf(stderr, "session: connected SpadServer , fd = %d\n",
            spad_server->socket);
#endif
    FD_SET(spad_server->socket, &session_socket_mask);
  }


  /* spad_io connects to SessionIOName server socket
    this is Lisp std IO read_from_spad_io handles requests */
  spad_io = connect_to_local_server(SessionIOName, SessionIO, Forever);
  if (spad_io == (Sock *) 0) {
    fprintf(stderr, "session: Cannot connect to FriCAS IO!\n");
    exit(0);
  }
  else {
#ifdef DEBUG
    fprintf(stderr,"session: connected SessionIOName , fd = %d\n",
            spad_io->socket);
#endif
    FD_SET(spad_io->socket, &session_socket_mask);
  }
  bsdSignal(SIGUSR2, usr2_handler,DontRestartSystemCalls);
  bsdSignal(SIGUSR1, usr1_handler,RestartSystemCalls);
  bsdSignal(SIGINT,  SIG_IGN,RestartSystemCalls);
  bsdSignal(SIGTERM, term_handler,RestartSystemCalls);

  /* open_server opens the server socket so that we can accept connections
    we expect connections from spadbuf/spadclient(purpose:InterpWindow)
    and hypertex (MenuServer) */

  if (open_server(SessionServer) == -2) {
    fprintf(stderr, "session: Cannot make server socket!\n");
    exit(-1);
  }
  else {
#ifdef DEBUG
    fprintf(stderr, "session: opened SessionServer , fd = %d\n",
            server[1].socket);
#endif
    FD_SET(server[1].socket,&session_socket_mask);
  }
  manage_sessions();
  return(0);
}
