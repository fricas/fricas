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

/* Still a problem with close_client */

/* Communication interface for external FriCAS buffers */

#include "fricas_c_macros.h"
#include "debug.h"

#include <signal.h>

#include "hyper.h"
#include "parse.h"
#include "bsdsignal.h"

#include "all_hyper_proto.H1"
#include "sockio-c.H1"
#include "bsdsignal.H1"

static void start_user_buffer(HyperDocPage * page);
static void clear_execution_marks(HashTable * depend_hash);
static void issue_dependent_commands(HyperDocPage * page, TextNode * command,
                                     int type);
static void send_pile(Sock * sock, char * str);
static void mark_as_executed(HyperDocPage * page, TextNode * command,
                             int type);
static void accept_menu_server_connection(HyperDocPage * page);
static void switch_frames(void);
static void close_client(int pid);

typedef struct sock_list {      /* linked list of Sock */
    Sock Socket;
    struct sock_list *next;
}   Sock_List;

Sock_List *plSock = (Sock_List *) 0;
Sock *spad_socket = (Sock *) 0; /* to_server socket for SpadServer */

char *p2sBuf = NULL;
int p2sBufSize = 0;

/* issue a FriCAS command to the buffer associated with a page */
void
issue_spadcommand(HyperDocPage *page, TextNode *command, int immediate,
                  int type)
{
    char *buf;
    int ret_val;

    ret_val = connect_spad();
    if (ret_val == NotConnected || ret_val == SpadBusy)
        return;

    if (page->sock == NULL)
        start_user_buffer(page);
    ret_val = send_int(page->sock, TestLine);
    if (ret_val == -1) {
        page->sock = NULL;
        clear_execution_marks(page->depend_hash);
        issue_spadcommand(page, command, immediate, type);
        return;
    }
    issue_dependent_commands(page, command, type);
    ret_val = send_int(page->sock, ReceiveInputLine);
    buf = print_to_string(command);
    if (immediate) {
        int len = strlen(buf);
        buf = p2sBuf = resizeBuffer(len + 2, p2sBuf, &p2sBufSize);
        buf[len + 1] = '\0';
        buf[len] = '\n';
    }
    if (type == Spadsrc)
        send_pile(page->sock, buf);
    else
        send_string(page->sock, buf);
    mark_as_executed(page, command, type);
    gIsEndOfOutput = 0;
}
static void
send_pile(Sock *sock,char * str)
{
    FILE *f;
    char name[512], command[512];

    sprintf(name, "/tmp/hyper%s.input", getenv("SPADNUM"));
    f = fopen(name, "w");
    if (f == NULL) {
        fprintf(stderr, "Can't open temporary input file %s\n", name);
        return;
    }
    fprintf(f, "%s", str);
    fclose(f);
    sprintf(command, ")read %s\n", name);
    send_string(sock, command);
}
static void
issue_dependent_commands(HyperDocPage *page, TextNode *command,int type)
{
    TextNode *node, *depend_label;
    SpadcomDepend *depend;
    int end_type = (type == Spadcommand || type == Spadgraph) ?
    (Endspadcommand) : (Endspadsrc);

    for (node = command->next; node->type != end_type;
         node = node->next)
        if (node->type == Free)
            for (depend_label = node->data.node; depend_label != NULL;
                 depend_label = depend_label->next)
                if (depend_label->type == Word) {
                    depend = (SpadcomDepend *)
                        hash_find(page->depend_hash, depend_label->data.text);
                    if (depend == NULL) {
                        fprintf(stderr, "Error: dependency on undefined label: %s\n",
                                depend_label->data.text);
                        continue;
                    }
                    if (!depend->executed) {
                        issue_spadcommand(page, depend->spadcom->next, 1,
                                          depend->spadcom->type);
                        while (!gIsEndOfOutput)
                            pause();
                        sleep(1);
                    }
                }
}
static void
mark_as_executed(HyperDocPage *page, TextNode *command,int type)
{
    TextNode *node, *depend_label;
    SpadcomDepend *depend;
    int end_type = (type == Spadcommand || type == Spadgraph)
    ? (Endspadcommand) : (Endspadsrc);

    for (node = command; node->type != end_type; node = node->next)
        if (node->type == Bound)
            for (depend_label = node->data.node; depend_label != NULL;
                 depend_label = depend_label->next)
                if (depend_label->type == Word) {
                    depend = (SpadcomDepend *)
                        hash_find(page->depend_hash, depend_label->data.text);
                    if (depend == NULL) {
                        fprintf(stderr, "No dependency entry for label: %s\n",
                                depend_label->data.text);
                        continue;
                    }
                    depend->executed = 1;
                }
}

/* start a spad buffer for the page associated with the give */
static void
start_user_buffer(HyperDocPage *page)
{
    char buf[1024], *title;
    char *SPAD;
    char spadbuf[250];
    char complfile[250];
    int ret_val;

    SPAD = (char *) getenv("AXIOM");
    if (SPAD == NULL) {
        sprintf(SPAD, "/spad/mnt/rios");
    }
    sprintf(spadbuf, "%s/lib/spadbuf", SPAD);
    sprintf(complfile, "%s/lib/command.list", SPAD);
    title = print_to_string(page->title);
    if (access(complfile, R_OK) == 0)

        /*
         * TTT says : why not invoke with "-name fricasclient" and set any
         * defaults in the usual way
         */
        sprintf(buf,
        "xterm -sb -sl 500 -name fricasclient -n '%s' -T '%s' -e  %s %s %s&",
                title, title, spadbuf, page->name, complfile);
    else
        sprintf(buf,
         "xterm -sb -sl 500 -name fricasclient -n '%s' -T '%s' -e  %s '%s'&",
                title, title, spadbuf, page->name);
    ret_val = system(buf);
    if (ret_val == -1 || ret_val == 127) {

        /*
         * perror("running the xterm spadbuf program"); exit(-1);
         */
    }
    accept_menu_server_connection(page);
    sleep(2);
}

/* Clears the execution marks in a hash table when a buffer has been killed */
static void
clear_execution_marks(HashTable *depend_hash)
{
    int i;
    HashEntry *h;
    SpadcomDepend *depend;

    if (depend_hash == 0)
        return;
    for (i = 0; i < depend_hash->size; i++)
        for (h = depend_hash->table[i]; h != NULL; h = h->next) {
            depend = (SpadcomDepend *) h->data;
            depend->executed = 0;
        }
}

Sock *
accept_menu_connection(Sock *server_sock)
{
    int sock_fd /*, session, ret_code*/;
    Sock_List *pls;
    /*Sock local_sock;*/

    /* Could only be InterpWindow */

    pls = (Sock_List *) halloc(sizeof(Sock_List),"SockList");
    sock_fd = accept(server_sock->socket, 0, 0);
    if (sock_fd == -1) {
        perror("session : accepting connection");
        return 0;
    }
    (pls->Socket).socket = sock_fd;
    get_socket_type((Sock *) pls);

#ifdef DEBUG
    fprintf(stderr,
            "session: accepted InterpWindow , fd = %d\n", sock_fd);
#endif


    /* put new item at head of list */

    if (plSock == (Sock_List *) 0) {
        plSock = pls;
        plSock->next = (Sock_List *) 0;
    }
    else {
        pls->next = plSock;
        plSock = pls;
    }

    /* need to maintain socket_mask since we roll our own accept */

    FD_SET(plSock->Socket.socket, &socket_mask);
    return (Sock *) plSock;
}

static void
accept_menu_server_connection(HyperDocPage *page)
{

    /*
     * TTT thinks this code should just provide a Sock to the page. The only
     * client assumed is a spadbuf. Since spadbuf was invoked with the page
     * name, it just passes it back here as a check (get_string line).
     */
    int ret_code/*, i*/;
    fd_set rd;
    Sock *sock;
    char *buf_name;
    HyperDocPage *npage;

    if (page->sock != NULL)
        return;
    while (1) {
        rd = server_mask;
        ret_code = sselect(FD_SETSIZE, &rd, 0, 0, NULL);
        if (ret_code == -1) {
            perror("Session manager select");
            continue;
        }

        if (server[1].socket > 0 && FD_ISSET(server[1].socket, &rd)) {
            sock = accept_menu_connection(server + 1);
            if (sock == 0)
                return;
            if (sock->purpose == InterpWindow) {
                buf_name = get_string(sock);
                npage = (HyperDocPage *)
                    hash_find(gWindow->fPageHashTable, buf_name);
                if (npage == NULL) {

                    /*
                     * Lets just try using the current page TTT doesn't know
                     * why this could be detrimental
                     *
                     * fprintf(stderr, "connecting spadbuf to unknown page:
                     * %s\n", buf_name);
                     */
                    page->sock = sock;
                    return;
                }
                else {

                    /*
                     * For some reason npage and page may be different TTT
                     * thinks this happens when a dynamic page has the same
                     * name as an existing static page.
                     */
                    npage->sock = sock;
                    page->sock = sock;
                }
                if (!strcmp(buf_name, page->name)) {
                    return;
                }
            }
        }
    }
}


/*
 * This procedure takes a HyperDoc node, and expands it into string
 */

/*
 * This routine takes a text node and creates a string out of it. This is for
 * use with things such as spad commands. There are  a very limited set of
 * node types it can handle, so be careful
 */

char *
print_to_string(TextNode *command)
{
    int len = 0;

    print_to_string1(command, &len);
    p2sBuf = resizeBuffer(len, p2sBuf, &p2sBufSize);
    return print_to_string1(command, NULL);
}

/*
see the code in ht-util.boot
        $funnyQuote := char 127
        $funnyBacks := char 128
*/
#define funnyEscape(c)  ((c) == '"' ? '\177' : ((c) == '\\' ? '\200' : c))
#define funnyUnescape(c) ((c) == '\177' ? '"' : ((c) == '\200' ? '\\' : c))
#define storeChar(ch) \
  do { if (sizeBuf) { (*sizeBuf)++;} else {*c++ = (ch);} } while(0)
#define storeString(str) for (s=str;*s;s++) {storeChar(*s);}

extern int include_bf;

char *
print_to_string1(TextNode *command,int * sizeBuf)
{
    char *c = p2sBuf;
    char *s;
    InputItem *item;
    LineStruct *curr_line;
    int lcount;
    InputBox *box;
    int num_spaces;
    int count;
    TextNode *node;

    /*
     * Init the stack of text nodes, things are pushed on here when I trace
     * through a nodes data.node. This way we always know where the next is.
     */

    for (node = command; node != NULL;) {
        switch (node->type) {
          case Newline:
            storeChar('\n');
            node = node->next;
            break;
          case Ifcond:
            if (check_condition(node->data.ifnode->cond))
                node = node->data.ifnode->thennode;
            else
                node = node->data.ifnode->elsenode;
            break;
          case Endarg:
          case Endspadcommand:
          case Endspadsrc:
          case Endpix:
            storeChar('\0');
            return p2sBuf;
          case Endverbatim:
          case Endif:
          case Fi:
          case Endmacro:
          case Endparameter:
          case Rbrace:
          case Endgroup:
            node = node->next;
            break;
          case Punctuation:

            /*
             * Simply copy the piece of text
             */
            if (node->space & FRONTSPACE) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case WindowId:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            storeChar(' ');
            node = node->next;
            break;
          case Verbatim:
          case Spadsrctxt:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }

            /*
             * now add the eol
             */

            /*
             * if(node->next && node->next->type != Endspadsrc)
             * storeChar('\n');
             */
            node = node->next;
            break;
          case Dash:
          case Rsquarebrace:
          case Lsquarebrace:
          case Word:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case BoxValue:
            box =
             (InputBox *) hash_find(gWindow->page->box_hash, node->data.text);
            if (box == NULL) {
                fprintf(stderr,
                        "Print_to_string:Box %s Has no symbol table entry\n",
                        node->data.text);
                exit(-1);
            }
            storeChar(' ');
            if (box->picked) {
                storeChar('t');
            }
            else {
                storeChar('n');
                storeChar('i');
                storeChar('l');
            }
            node = node->next;
            break;
          case StringValue:
            item = return_item(node->data.text);
            if (item != NULL) {
                if (node->space)
                    storeChar(' ');
                curr_line = item->lines;
                while (curr_line != NULL) {
                    for (lcount = 0, s = curr_line->buffer; *s && lcount < item->size;
                         s++, lcount++) {
                        storeChar(funnyEscape(*s));
                    }
                    if (curr_line->len <= item->size && curr_line->next)
                        storeChar('\n');
                    curr_line = curr_line->next;
                }
            }
            else if ((box = (InputBox *) hash_find(gWindow->page->box_hash,
                                                node->data.text)) != NULL) {
                if (node->space) { storeChar(' '); }
                if (box->picked) {
                    storeChar('t');
                }
                else {
                    storeChar('n');
                    storeChar('i');
                    storeChar('l');
                }
            }
            else {
                fprintf(stderr, "Error, Symbol %s has no symbol table entry\n",
                        node->data.text);
                exit(-1);
            }
            node = node->next;
            break;
          case Space:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case Titlenode:
          case Endtitle:
          case Center:
          case Endcenter:
          case BoldFace:
          case Emphasize:
          case Indentrel:
            node = node->next;
            break;
          case Bound:
            if (include_bf) {
                int len, i;
                TextNode *n2 = node->data.node;

                storeChar('\\');
                storeChar('b');
                storeChar('o');
                storeChar('u');
                storeChar('n');
                storeChar('d');
                storeChar('{');
                for (; n2->type != Endarg; n2 = n2->next) {
                    if (n2->type == Word) {
                        len = strlen(n2->data.text);
                        for (i = 0; i < len; i++)
                            storeChar(n2->data.text[i]);
                        storeChar(' ');
                    }
                }
                storeChar('}');
            }
            node = node->next;
            break;
          case Free:
            if (include_bf) {
                int len, i;
                TextNode *n2 = node->data.node;

                storeChar('\\');
                storeChar('f');
                storeChar('r');
                storeChar('e');
                storeChar('e');
                storeChar('{');
                for (; n2->type != Endarg; n2 = n2->next) {
                    if (n2->type == Word) {
                        len = strlen(n2->data.text);
                        for (i = 0; i < len; i++)
                            storeChar(n2->data.text[i]);
                        storeChar(' ');
                    }
                }
                storeChar('}');
            }
            node = node->next;
            break;
          case Macro:
            node = node->next;
            break;
          case Pound:
            if (node->space) { storeChar(' '); }
            node = node->next;
            break;
          case Group:
            node = node->next;
            break;
          case Indent:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          default:
            fprintf(stderr,
                    "Print_to_string: Unrecognized Keyword Type %d\n",
                    node->type);
            node=node->next;
            break;
        }
    }
    storeChar('\0');
    return p2sBuf;
}

/*
 * Send a lisp or spad command to the FriCAS server for execution , if
 * type is link, then we wait for a HyperDoc card to be returned
 */

HyperDocPage *
issue_server_command(HyperLink *link)
{
    TextNode *command = (TextNode *) link->reference.node;
    int ret_val;
    char *buf;
    HyperDocPage *page;

    ret_val = connect_spad();
    if (ret_val == NotConnected) {
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "SpadNotConnectedPage");
        if (page == NULL)
            fprintf(stderr, "No SpadNotConnectedPage found\n");
        return page;
    }
    if (ret_val == SpadBusy) {
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "SpadBusyPage");
        if (page == NULL)
            fprintf(stderr, "No SpadBusyPage found\n");
        return page;
    }
    switch_frames();
    switch (link->type) {
      case Qspadcall:
      case Qspadcallquit:
      case Spadlink:
      case Spaddownlink:
      case Spadmemolink:
        send_int(spad_socket, QuietSpadCommand);
        break;
      case Spadcall:
      case Spadcallquit:
        send_int(spad_socket, SpadCommand);
        break;
      default:
        send_int(spad_socket, LispCommand);
        break;
    }
    buf = print_to_string(command);
    send_string(spad_socket, buf);
    if (link->type == Lispcommand || link->type == Spadcall
        || link->type == Spadcallquit || link->type == Qspadcallquit
        || link->type == Qspadcall || link->type == Lispcommandquit)
        return NULL;
    page = parse_page_from_socket();
    return page;
}

int
issue_serverpaste(TextNode *command)
{
    char *buf;
    int ret_val;

    ret_val = connect_spad();
    if (ret_val == NotConnected || ret_val == SpadBusy)
        return 1;
    switch_frames();
    send_int(spad_socket, LispCommand);
    buf = print_to_string(command);
    send_string(spad_socket, buf);
    return 1;
}

/*
 * issue a unix command
 */
void
issue_unixcommand(TextNode *node)
{
    char *buf;
    char *copy;


    buf = print_to_string(node);
    copy =(char *) halloc((strlen(buf)+2)*sizeof(char),"Unixcommand");
    strcpy(copy,buf);
    copy[strlen(buf) + 1] = '\0';
    copy[strlen(buf)] = '&';
    system(copy);
    free(copy);
    return;
}

HyperDocPage *
issue_unixlink(TextNode *node)
{
    HyperDocPage *page;
    char *buf;

    buf = print_to_string(node);
    if ((unixfd = popen(buf, "r")) == NULL) {
        fprintf(stderr, "Error popening %s\n", buf);
        exit(-1);
    }
    bsdSignal(SIGUSR2,SIG_IGN,0);
    page = parse_page_from_unixfd();
    bsdSignal(SIGUSR2,sigusr2_handler,0);
    return page;
}

int
issue_unixpaste(TextNode *node)
{
    char *buf;

    buf = print_to_string(node);
    if ((unixfd = popen(buf, "r")) == NULL) {
        fprintf(stderr, "Error popening %s\n", buf);
        exit(-1);
    }
    return 1;
}


/*
 * called when session_server selects
 */
void
service_session_socket(void)
{
    int cmd, pid;

    cmd = get_int(session_server);
    switch (cmd) {
      case CloseClient:
        pid = get_int(session_server);
        if (pid != -1)
            close_client(pid);
        break;
      default:
        fprintf(stderr,
                "(HyperDoc) Unknown command from SessionServer %d\n", cmd);
        break;
    }
}


/*
 * let spad know which frame to issue command via
 */
static void
switch_frames(void)
{
    if (session_server == NULL) {
        fprintf(stderr, "(HyperDoc) No session manager connected!\n");
        return;
    }
    if (gWindow->fricas_frame == -1) {
        fprintf(stderr, "(HyperDoc) No FriCAS frame associated with top level window!\n");
        return;
    }
    send_int(session_server, SwitchFrames);
    send_int(session_server, gWindow->fricas_frame);
}
void
send_lisp_command(char *command)
{
    int ret_val;

    ret_val = connect_spad();
    if (ret_val == NotConnected || ret_val == SpadBusy) {
        return;
    }
    send_int(spad_socket, LispCommand);
    send_string(spad_socket, command);
}
void
escape_string(char *s)
{
    char *st;

    for (st = s; *st; st++)
        *st = funnyEscape(*st);
}
void
unescape_string(char *s)
{
    char *st;

    for (st = s; *st; st++)
        *st = funnyUnescape(*st);
}
static void
close_client(int pid)
{
    /*int i;*/
    Sock_List *pSock, *locSock;

    /*
     * just need to drop the list item
     */

    if (plSock == (Sock_List *) 0)
        return;

    /*
     * first check head
     */

    if ((plSock->Socket.pid == pid)) {
        locSock = plSock;
        if ((*plSock).next == (Sock_List *) 0) {
            plSock = (Sock_List *) 0;
        }
        else {
            plSock = plSock->next;
        }
        free(locSock);
    }

    /*
     * now check the rest
     */

    else {
        for (pSock = plSock; pSock->next != (Sock_List *) 0; pSock = pSock->next)
            if (pSock->next->Socket.pid == pid) {
                locSock = pSock->next;
                if (pSock->next->next == (Sock_List *) 0) {
                    pSock->next = (Sock_List *) 0;
                }
                else {
                    pSock->next = pSock->next->next;
                }
                free(locSock);
                break;
            }
    }
}



char *
print_source_to_string(TextNode *command)
{
    int len = 0;

    print_source_to_string1(command, &len);
    p2sBuf = resizeBuffer(len, p2sBuf, &p2sBufSize);
    return print_source_to_string1(command, NULL);
}
char *
print_source_to_string1(TextNode *command,int * sizeBuf)
{
    char *c = p2sBuf;
    char *s;
    InputItem *item;
    LineStruct *curr_line;
    int lcount;
    InputBox *box;
    int num_spaces;
    int count;
    TextNode *node;

    /*
        print out HyperDoc source for what you see
     */

    for (node = command; node != NULL;) {
        switch (node->type) {
          case Newline:
            storeString("\\newline\n");
            node = node->next;
            break;
          case Par:
            storeString("\n\n");
            node = node->next;
            break;
          case Indentrel:
            storeString("\\indentrel{");
            storeString(node->data.node->data.text);
            storeChar('}');
            node = node->next;
            break;
          case Tab:
            storeString("\\tab{");
            storeString(node->data.node->data.text);
            storeChar('}');
            node = node->next;
            break;
          case Ifcond:
            if (check_condition(node->data.ifnode->cond))
                node = node->data.ifnode->thennode;
            else
                node = node->data.ifnode->elsenode;
            break;
          case Endarg:
          case Endspadsrc:
          case Endpix:
          case Endbutton:
            storeChar('}');
            node = node->next;
            break;
          case Endverbatim:
          case Endif:
          case Fi:
          case Endmacro:
          case Endparameter:
          case Rbrace:
            node = node->next;
            break;
          case Punctuation:

            /*
             * Simply copy the piece of text
             */
            if (node->space & FRONTSPACE) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case WindowId:
            storeString("\\windowid ");
            node = node->next;
            break;
          case Verbatim:
          case Spadsrctxt:
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case Dash:
          case Rsquarebrace:
          case Lsquarebrace:
          case Word:
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case BoxValue:
            box = (InputBox *) hash_find(gWindow->page->box_hash, node->data.text);
            if (box == NULL) {
                fprintf(stderr, "Print_to_string:Box %s Has no symbol table entry\n",
                        node->data.text);
                exit(-1);
            }
            storeChar(' ');
            if (box->picked) {
                storeChar('t');
            }
            else {
                storeChar('n');
                storeChar('i');
                storeChar('l');
            }
            node = node->next;
            break;
          case StringValue:
            item = return_item(node->data.text);
            if (item != NULL) {
                if (node->space) {  storeChar(' '); }
                curr_line = item->lines;
                while (curr_line != NULL) {
                    for (lcount = 0, s = curr_line->buffer;
                         *s && lcount < item->size;
                         s++, lcount++) {
                        storeChar(funnyUnescape(*s));
                    }
                    if (curr_line->len <= item->size && curr_line->next)
                        storeChar('\n');
                    curr_line = curr_line->next;
                }
            }
            else if ((box = (InputBox *) hash_find(gWindow->page->box_hash,
                                                node->data.text)) != NULL) {
                if (node->space) { storeChar(' '); }
                if (box->picked) {
                    storeChar('t');
                }
                else {
                    storeChar('n');
                    storeChar('i');
                    storeChar('l');
                }
            }
            else {
                fprintf(stderr, "Error, Symbol %s has no symbol table entry\n",
                        node->data.text);
                exit(-1);
            }
            node = node->next;
            break;
          case Space:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case Emphasize:
            storeString("\\em ");
            node = node->next;
            break;
          case BoldFace:
            storeString("\\bf ");
            node = node->next;
            break;
          case Sl:
            storeString("\\it ");
            node = node->next;
            break;
          case Rm:
            storeString("\\rm ");
            node = node->next;
            break;
          case It:
            storeString("\\it ");
            node = node->next;
            break;
          case Tt:
            storeString("\\tt ");
            node = node->next;
            break;
          case Group:
/* skip {} */
            if (node->next->type==Endgroup){
               node=node->next->next;
               break;
                }
            storeChar('{');
            node = node->next;
            break;
          case Endgroup:
            storeChar('}');
            node = node->next;
            break;
          case Box:
            storeString("\\box{");
            node = node->next;
            break;
          case Endbox:
            storeChar('}');
            node = node->next;
            break;
          case Center:
            storeString("\\center{");
            node = node->next;
            break;
          case Endcenter:
            storeString("}");
            storeChar('\n');
            node = node->next;
            break;
          case Titlenode:
          case Endtitle:
            node = node->next;
            break;
          case Bound:
            {
                TextNode *n2 = node->data.node;

                storeString("\\bound{");
                for (; n2->type != Endarg; n2 = n2->next) {
                    if (n2->type == Word) {
                        storeString(n2->data.text);
                        storeChar(' ');
                        }
                    }
                storeChar('}');
            }
            node = node->next;
            break;
          case Free:
            {
                TextNode *n2 = node->data.node;

                storeString("\\free{");
                for (; n2->type != Endarg; n2 = n2->next) {
                    if (n2->type == Word) {
                        storeString(n2->data.text);
                        storeChar(' ');
                        }
                    }
                storeChar('}');
                }
            node = node->next;
            break;
          case Macro:
            storeChar(' ');
            node = node->next;
            break;
          case Pound:
            if (node->space) { storeChar(' '); }
            node = node->next;
            break;
          case Indent:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case Inputbitmap:
            storeString("\\inputbitmap{");
            storeString(node->data.text);
            storeString("}\n");
            node = node->next;
            break;
          case Endscrolling:
            storeString("\\end{scroll}\n");
            node = node->next;
            break;
          case Scrollingnode:
            storeString("\\begin{scroll}\n");
            storeString("% This is the scrolling area\n");
            node = node->next;
            break;
          case Horizontalline:
            storeString("\\horizontalline\n");
            node = node->next;
            break;
          case Endtable:
            storeChar('}');
            node = node->next;
            break;
          case Table:
            storeString("\\table{");
            node = node->next;
            break;
          case Tableitem:
            storeChar('{');
            node = node->next;
            break;
          case Endtableitem:
            storeChar('}');
            node = node->next;
            break;
          case Beginitems:
            storeString("\\begin{items}");
            node = node->next;
            break;
          case Item:
            storeString("\n\\item");
            node = node->next;
            break;
          case Enditems:
            storeString("\n\\end{items}");
            node = node->next;
            break;
/*** LINKS ***/
/* all these guys are ended by Endbutton
we close the brace then */
          case Spadlink:
            storeString("\\fauxspadlink{");
            node = node->next;
            break;
          case Unixlink:
            storeString("\\fauxunixlink{");
            node = node->next;
            break;
          case Lisplink:
            storeString("\\fauxlisplink{");
            node = node->next;
            break;
          case Link:
            storeString("\\fauxlink{");
            node = node->next;
            break;
          case LispDownLink:
            storeString("\\fauxlispdownlink{");
            node = node->next;
            break;
          case LispMemoLink:
            storeString("\\fauxlispmemolink{");
            node = node->next;
            break;
          case Memolink:
            storeString("\\fauxmemolink{");
            node = node->next;
            break;
          case Windowlink:
            storeString("\\fauxwindowlink{");
            node = node->next;
            break;
          case Downlink:
            storeString("\\fauxdownlink{");
            node = node->next;
            break;
/** END OF LINKS **/
          case Unixcommand:
            storeString("\\unixcommand{");
            node = node->next;
            break;
          case Lispcommand:
            storeString("\\lispcommand{");
            node = node->next;
            break;
          case Spadgraph:
            storeString("\\spadgraph{");
            node = node->next;
            break;
          case Spadcommand:
            storeString("\\spadcommand{");
            node = node->next;
            break;
          case Endspadcommand:
            storeChar('}');
            node = node->next;
            break;
          case Footernode:
            storeString("% This is the footer\n");
            node = node->next;
            break;
          case Endfooter:
            storeString("% This is the end of the footer\n");
            node = node->next;
            break;
          case Endheader:
            storeString("% This is the end of the header\n");
            node = node->next;
            break;
          case Headernode:
            storeString("% This is the header\n");
            node = node->next;
            break;
          default:
            fprintf(stderr,
                    "Print_source_to_string: Unrecognized Keyword Type %d\n",
                    node->type);
            node=node->next;
            break;
        }
    }
    storeChar('\0');
    return p2sBuf;
}
