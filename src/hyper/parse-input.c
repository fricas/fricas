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

/***
  Contains all the code needed to parse input items,
  InputString
  SimpleBox
  RadioBox.
  ****/

#include "fricas_c_macros.h"

#include "debug.h"

#include "parse.h"
#include "lex.h"
#include "hyper.h"

#include "all_hyper_proto.H1"

static void insert_item(InputItem * item);
static void add_box_to_rb_list(char * name, InputBox * box);
static int check_others(InputBox * list);

/* create an unmapped input window for getting strings * */
extern int make_input_file;

HyperLink *
make_input_window(InputItem * item)
{
  HyperLink *link;
  XSetWindowAttributes at;

  if (!make_input_file) {
    link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");
    if (link == NULL) {
      fprintf(stderr, "Ran out of memory allocating a hyper link!\n");
      exit(-1);
    }
    at.cursor = gActiveCursor;
    at.background_pixel = gInputBackgroundColor;
    at.border_pixel = gActiveColor;
    link->win = XCreateWindow(gXDisplay, gWindow->fDisplayedWindow, 0, 0, 100, 100, 0,
                              0, InputOutput, CopyFromParent,
                              CWCursor | CWBackPixel | CWBorderPixel, &at);
    XSelectInput(gXDisplay, link->win, ButtonPressMask);
    link->type = Inputstring;
    link->x = link->y = 0;
    /** This way when I click in an input window, I need only use reference
      to get a pointer to the item                             ***/
    link->reference.string = item;
    hash_insert(gLinkHashTable,(char *) link,(char *) &link->win);

    return link;
  }
  return 0;
}

/* create an unmapped input window for boxes */
HyperLink *
make_box_window(InputBox * box, int type)
{
  HyperLink *link = 0;
  XSetWindowAttributes at;

  if (!make_input_file) {
    link = (HyperLink *) halloc(sizeof(HyperLink), "Make_box_window");
    if (link == NULL) {
      fprintf(stderr, "Ran out of memory allocating a hyper link!\n");
      exit(-1);
    }
    at.cursor = gActiveCursor;
    at.background_pixel = gInputBackgroundColor;
    link->win = XCreateWindow(gXDisplay, gWindow->fDisplayedWindow,
                              0, 0, 100, 100, 0,
                              0, InputOutput, CopyFromParent,
                              CWCursor | CWBackPixel, &at);
    XSelectInput(gXDisplay, link->win, ButtonPressMask);
    link->type = type;
    link->x = link->y = 0;
    /** This way when I click in an input window, I need only use reference
      to get a pointer to the item                             ***/
    link->reference.box = box;
    hash_insert(gLinkHashTable, (char *)link,(char *) &link->win);
  }

  return link;
}

void
initialize_default(InputItem *item,char * buff)
{
  LineStruct *newline;
  LineStruct *curr_line;
  int size = item->size;
  int bp;

  item->curr_line = item->lines = alloc_inputline(size);
  curr_line = item->lines;
  item->num_lines = 1;
  curr_line->line_number = 1;
  /* while I still have lines to fill */
  for (bp = 0; *buff;) {
    if (*buff == '\n') {
      curr_line->len = bp;
      curr_line->buffer[bp] = 0;
      newline = alloc_inputline(size);
      newline->line_number = ++(item->num_lines);
      curr_line->next = newline;
      newline->prev = curr_line;
      curr_line = newline;
      bp = 0;
      buff++;
    }
    else if (bp == size) {
      curr_line->len = size + 1;
      curr_line->buffer[size] = '_';
      curr_line->buffer[size + 1] = 0;
      newline = alloc_inputline(size);
      newline->line_number = ++(item->num_lines);
      curr_line->next = newline;
      newline->prev = curr_line;
      bp = 0;
      curr_line = newline;
    }
    else {
      curr_line->buffer[bp++] = *buff++;
    }
  }
  curr_line->buff_pntr = curr_line->len = bp;
  item->curr_line = curr_line;
}



/* Parse the input string statement * */
void
parse_inputstring(void)
{
  TextNode *input_node = curr_node;
  char *name;
  InputItem *item;
  int size;
  char *default_value;

  gStringValueOk = 0;

  /* first get the name */
  input_node->type = token.type;
  get_expected_token(Lbrace);
  name = get_input_string();
  input_node->data.text = alloc_string(name);
  /* now get the width */
  get_expected_token(Lbrace);
  get_expected_token(Word);
  get_expected_token(Rbrace);
  size = atoi(token.id);
  if (size < 0) {
    fprintf(stderr, "Illegal size in Input string\n");
    longjmp(jmpbuf, 1);
  }

  /* get the default value */
  get_expected_token(Lbrace);
  default_value = get_input_string();

  /** now I need to malloc space for the input stuff **/
  item = (InputItem *) halloc(sizeof(InputItem), "InputItem");

  /* Now store all the string info */
  item->name = (char *)
    halloc((strlen(input_node->data.text) + 1) * (sizeof(char)),"parse_inputstring");
  strcpy(item->name, input_node->data.text);
  item->size = size;
  item->entered = 0;
  item->next = NULL;
  initialize_default(item, default_value);

  /** Now that I have all the structures made, lets make the window, and
    add the item to the list                                 ****/

  input_node->link = make_input_window(item);
  if (!make_input_file)
    item->win = input_node->link->win;      /* TTT */
  insert_item(item);
  gStringValueOk = 1;
  curr_node = input_node;
  return ;
}

void
parse_simplebox(void)
{
  InputBox *box;
  char *name;
  short int picked = 0;
  char *filename;
  TextNode *input_box = curr_node;

  gStringValueOk = 0;

  /* set the type and space fields  */
  input_box->type = SimpleBox;
  input_box->space = token.id[-1];

  /* IS it selected? */
  get_token();
  if (token.type == Lsquarebrace) {
    get_expected_token(Word);
    if (!is_number(token.id)) {
      fprintf(stderr,
              "parse_simple_box: Expected a value not %s\n", token.id);
      print_page_and_filename();
      jump();
    }
    else if (!strcmp(token.id, "1"))
      picked = 1;
    else if (!strcmp(token.id, "0"))
      picked = 0;
    else {
      fprintf(stderr, "parse_simple_box: Unexpected Value %s\n", token.id);
      print_page_and_filename();
      jump();
    }
    get_expected_token(Rsquarebrace);
    get_token();
  }

  if (token.type != Lbrace) {
    token_name(token.type);
    fprintf(stderr, "parse_inputbox was expecting a { not a %s\n", ebuffer);
    print_page_and_filename();
    jump();
  }

  name = get_input_string();
  if (gPageBeingParsed->box_hash && hash_find(gPageBeingParsed->box_hash, name)) {
    fprintf(stderr, "Input box name %s is not unique \n", name);
    print_page_and_filename();
    jump();
  }

  box = alloc_inputbox();
  box->name = alloc_string(name);
  input_box->data.text = alloc_string(name);
  box->picked = picked;

  /* Get the filename for the selected and unselected bitmaps */
  get_expected_token(Lbrace);
  filename = get_input_string();
  if (!make_input_file)
    box->selected = insert_image_struct(filename);
  get_expected_token(Lbrace);
  filename = get_input_string();
  if (!make_input_file) {
    box->unselected = insert_image_struct(filename);
    /* set the width and height for the maximaum of the two */
    input_box->height = max(box->selected->height, box->unselected->height);
    input_box->width = max(box->selected->width, box->unselected->width);
    /* Make the window and stuff */
    input_box->link = make_box_window(box, SimpleBox);
    box->win = input_box->link->win;

    /* Now add the box to the box_has table for this window */
    if (gPageBeingParsed->box_hash == NULL) {
      gPageBeingParsed->box_hash = (HashTable *) halloc(sizeof(HashTable),
                                                        "Box Hash");
      hash_init(
                gPageBeingParsed->box_hash,
                BoxHashSize,
                (EqualFunction) string_equal,
                (HashcodeFunction) string_hash);
    }
    hash_insert(gPageBeingParsed->box_hash, (char *)box, box->name);
  }

  /* reset the curr_node and then return */
  curr_node = input_box;
  gStringValueOk = 1;
  return;
}
void
parse_radiobox(void)
{
  InputBox *box;
  char *name;
  char *group_name;
  short int picked = 0;
  TextNode *input_box = curr_node;

  gStringValueOk = 0;

  /* set the type and space fields  */
  input_box->type = Radiobox;
  input_box->space = token.id[-1];

  /* IS it selected? */
  get_token();
  if (token.type == Lsquarebrace) {
    get_expected_token(Word);
    if (!is_number(token.id)) {
      fprintf(stderr,
              "parse_simple_box: Expected a value not %s\n", token.id);
      print_page_and_filename();
      jump();
    }
    else if (!strcmp(token.id, "1"))
      picked = 1;
    else if (!strcmp(token.id, "0"))
      picked = 0;
    else {
      fprintf(stderr, "parse_simple_box: Unexpected Value %s\n", token.id);
      print_page_and_filename();
      jump();
    }
    get_expected_token(Rsquarebrace);
    get_token();
  }

  if (token.type != Lbrace) {
    token_name(token.type);
    fprintf(stderr, "parse_inputbox was expecting a { not a %s\n", ebuffer);
    print_page_and_filename();
    jump();
  }

  name = get_input_string();
  if (gPageBeingParsed->box_hash && hash_find(gPageBeingParsed->box_hash, name)) {
    fprintf(stderr, "Input box name %s is not unique \n", name);
    print_page_and_filename();
    jump();
  }

  box = alloc_inputbox();
  box->name = alloc_string(name);
  input_box->data.text = alloc_string(name);
  box->picked = picked;

  /* Now what I need to do is get the group name */
  get_token();
  if (token.type != Lbrace) {
    token_name(token.type);
    fprintf(stderr, "parse_inputbox was expecting a { not a %s\n", ebuffer);
    print_page_and_filename();
    jump();
  }
  group_name = get_input_string();

  /*
   * Now call a routine which searches the radio box list for the current
   * group name, and if found adds this box to it
   */
  add_box_to_rb_list(group_name, box);

  input_box->width = box->rbs->width;
  input_box->height = box->rbs->height;
  /* Make the window and stuff */
  input_box->link = make_box_window(box, Radiobox);
  if (!make_input_file)
    box->win = input_box->link->win;        /* TTT */


  /* Now add the box to the box_has table for this window */
  if (gPageBeingParsed->box_hash == NULL) {
    gPageBeingParsed->box_hash = (HashTable *) halloc(sizeof(HashTable),
                                                      "Box Hash");
    hash_init(
              gPageBeingParsed->box_hash,
              BoxHashSize,
              (EqualFunction) string_equal,
              (HashcodeFunction) string_hash);
  }
  hash_insert(gPageBeingParsed->box_hash, (char *)box, box->name);

  /* reset the curr_node and then return */
  curr_node = input_box;
  gStringValueOk = 1;
  return;
}
static void
add_box_to_rb_list(char *name,InputBox *box)
{
  RadioBoxes *trace = gPageBeingParsed->radio_boxes;
  InputBox *list;
  /*int found = 0;*/

  while (trace != NULL && strcmp(trace->name, name))
    trace = trace->next;

  if (!trace) {
    fprintf(stderr, "Tried to add a radio box to a non-existent group %s\n",
            name);
    print_page_and_filename();
    jump();
  }

  /* now add the box to the list */
  list = trace->boxes;
  box->next = list;
  trace->boxes = box;

  if (box->picked && check_others(box->next)) {
    fprintf(stderr, "Only a single radio button can be picked\n");
    print_page_and_filename();
    box->picked = 0;
  }
  box->selected = trace->selected;
  box->unselected = trace->unselected;
  box->rbs = trace;

  return;
}
static int
check_others(InputBox *list)
{
  InputBox *trace = list;

  while (trace != NULL && !trace->picked)
    trace = trace->next;

  if (trace != NULL)
    return 1;
  else
    return 0;
}


/* inserts an item into the current input list */
static void
insert_item(InputItem *item)
{
  InputItem *trace = gPageBeingParsed->input_list;

  if (gPageBeingParsed->current_item == NULL) {
    gPageBeingParsed->current_item = item;
  }
  if (trace == NULL) {
    /** Insert at the front of the list **/
    gPageBeingParsed->input_list = item;
    return;
  }
  else {
    /** find the end of the list **/
    while (trace->next != NULL)
      trace = trace->next;
    trace->next = item;
    return;
  }
}

InputItem *save_item;

void
init_paste_item(InputItem *item)
{
  InputItem *trace = gPageBeingParsed->input_list;

  if (!item) {
    gPageBeingParsed->input_list = NULL;
    gPageBeingParsed->current_item = NULL;
    save_item = NULL;
  }
  else {
    save_item = item->next;
    trace->next = NULL;
  }
}
void
repaste_item(void)
{
  InputItem *trace;

  if (save_item) {
    for (trace = gPageBeingParsed->input_list; trace && trace->next != NULL;
         trace = trace->next);
    if (trace) {
      trace->next = save_item;
    }
    else {
      gWindow->page->input_list = save_item;
      gWindow->page->current_item = save_item;
    }
  }
  save_item = NULL;
}

InputItem *
current_item(void)
{
  InputItem *trace = gPageBeingParsed->input_list;

  if (trace) {
    for (; trace->next != NULL; trace = trace->next);
    return trace;
  }
  else
    return NULL;
}
int
already_there(char *name)
{
  RadioBoxes *trace = gPageBeingParsed->radio_boxes;

  while (trace && strcmp(trace->name, name))
    trace = trace->next;

  if (trace)
    return 1;
  else
    return 0;
}
void
parse_radioboxes(void)
{
  TextNode *return_node = curr_node;
  RadioBoxes *newrb;
  char *fname;

  /* I really don't need this node, it just sets up some parsing stuff */
  return_node->type = Noop;

  newrb = alloc_rbs();

  get_token();
  if (token.type != Lbrace) {
    token_name(token.type);
    fprintf(stderr, "\\radioboxes was expecting a name not %s\n", ebuffer);
    print_page_and_filename();
    jump();
  }

  newrb->name = alloc_string(get_input_string());

  /* quick search for the name in the current list */
  if (already_there(newrb->name)) {
    free(newrb->name);
    free(newrb);
    fprintf(stderr, "Tried to redefine radioboxes %s\n", newrb->name);
    print_page_and_filename();
    jump();
  }
  /* now I have to get the selected and unslected bitmaps */
  get_token();
  if (token.type != Lbrace) {
    token_name(token.type);
    fprintf(stderr, "\\radioboxes was expecting a name not %s\n", ebuffer);
    print_page_and_filename();
    jump();
  }
  fname = get_input_string();
  if (!make_input_file)
    newrb->selected = insert_image_struct(fname);

  get_token();
  if (token.type != Lbrace) {
    token_name(token.type);
    fprintf(stderr, "\\radioboxes was expecting a name not %s\n", ebuffer);
    print_page_and_filename();
    jump();
  }
  fname = get_input_string();
  if (!make_input_file) {
    newrb->unselected = insert_image_struct(fname);
    newrb->height = max(newrb->selected->height, newrb->unselected->height);
    newrb->width = max(newrb->selected->width, newrb->unselected->width);
    /* now add the thing to the current list of radio boxes */
  }
  newrb->next = gPageBeingParsed->radio_boxes;
  gPageBeingParsed->radio_boxes = newrb;

  curr_node = return_node;
  return;
}
