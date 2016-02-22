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

/******************************************************************************
 *
 * keyin.c:
 *
 * Copyright The Numerical Algorithms Group Limited 1991, 1992, 1993.
 *
 ****************************************************************************/

#include "fricas_c_macros.h"
#include "debug.h"


#include "hyper.h"
#include "keyin.h"
#include "parse.h"

#include "all_hyper_proto.H1"
#include <X11/keysym.h>


#define min(x,y)     ( (x<y)?(x):(y))

int in_cursor_height;
int in_cursor_width;
int out_cursor_height;
int out_cursor_width;
int in_cursor_y;
int out_cursor_y;
int start_x;
int start_y;
int simple_box_width;
int gInInsertMode = 0;

unsigned int ModifiersMask = ShiftMask | LockMask | ControlMask
    | Mod1Mask | Mod2Mask | Mod3Mask
    | Mod4Mask | Mod5Mask;

unsigned int UnsupportedModMask = LockMask | ControlMask
    | Mod1Mask | Mod2Mask | Mod3Mask
    | Mod4Mask | Mod5Mask;


/*
 * Since the user can't tell me directly what name to use here, I am going to
 * let it be a default property. This way the user can link to whatever page
 * he/she wants. If it is a link right to the quit  page, then I will just
 * quit right away. Otherwise I will try to find the page, and display it.
 */

static char *protected_quit;

HyperLink *quitLink;            /** the global link to the quit page ***/

void
handle_key(XEvent *event)
{
  char key_buffer[20];
  int key_buffer_size = 20;
  KeySym keysym;
  XComposeStatus compstatus;
  int charcount;
  int display_again = 0;
  char *name;
  char *filename;
  /*char *head = "echo htadd -l ";*/
  /*char *blank1 = "                                        ";*/
  /*char *blank2 = "                                       \n";*/
  char buffer[180];
  FILE *filehandle;

  charcount = XLookupString((XKeyEvent *)event, key_buffer, key_buffer_size, &keysym ,&compstatus); /* 5 args */

  key_buffer[charcount] = '\0';
  switch (keysym) {
  case XK_Prior:
  case XK_F29:
    scrollUpPage();
    break;
  case XK_Next:
  case XK_F35:
    scrollDownPage();
    break;
  case XK_F3:
  case XK_F12:
    quitHyperDoc();
    break;
  case XK_F5:
    if (event->xkey.state & ShiftMask) {
      name = gWindow->page->name;
      filename = gWindow->page->filename;
      sprintf(buffer, "htadd -l %s\n", filename);
      system(buffer);
      filehandle = (FILE *) hash_find(&gFileHashTable, filename);
      fclose(filehandle);
      hash_delete(&gFileHashTable, filename);
      gWindow->fMacroHashTable =
        (HashTable *) halloc(sizeof(HashTable), "macro hash");
      hash_init(
                gWindow->fMacroHashTable,
                MacroHashSize,
                (EqualFunction ) string_equal,
                (HashcodeFunction) string_hash);
      gWindow->fPatchHashTable = (HashTable *) halloc(sizeof(HashTable), "patch hash");
      hash_init(
                gWindow->fPatchHashTable,
                PatchHashSize,
                (EqualFunction ) string_equal,
                (HashcodeFunction) string_hash);
      gWindow->fPasteHashTable = (HashTable *) halloc(sizeof(HashTable), "paste hash");
      hash_init(gWindow->fPasteHashTable,
                PasteHashSize,
                (EqualFunction ) string_equal,
                (HashcodeFunction) string_hash);
      gWindow->fCondHashTable = (HashTable *) halloc(sizeof(HashTable), "cond hash");
      hash_init(
                gWindow->fCondHashTable,
                CondHashSize,
                (EqualFunction ) string_equal,
                (HashcodeFunction) string_hash);
      gWindow->fPageHashTable = (HashTable *) halloc(sizeof(HashTable), "page hash");
      hash_init(
                gWindow->fPageHashTable,
                PageHashSize,
                (EqualFunction ) string_equal,
                (HashcodeFunction) string_hash);
      make_special_pages(gWindow->fPageHashTable);
      read_ht_db(
                 gWindow->fPageHashTable,
                 gWindow->fMacroHashTable,
                 gWindow->fPatchHashTable);
      gWindow->page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, name);
      if (gWindow->page == NULL) {
        fprintf(stderr, "lose...gWindow->page for %s is null\n", name);
        exit(-1);
      }
      display_again = 1;
    }
    break;
  case XK_F9:
    make_window_link(KeyDefsHelpPage);
    break;
  case XK_Tab:
    if (event->xkey.state & ShiftMask)
      prev_input_focus();
    else if (event->xkey.state & ModifiersMask)
      BeepAtTheUser();
    else
      next_input_focus();
    break;
  case XK_Return:
    if (!(event->xkey.state & ShiftMask)) {
      next_input_focus();
      break;
    }

    /* next ones fall through to input area handling */

  case XK_Escape:
    if (!gWindow->page->current_item)
      break;
  case XK_F1:
    if (!gWindow->page->current_item) {
      gWindow->page->helppage = alloc_string(NoMoreHelpPage);
      helpForHyperDoc();
      break;
    }
  case XK_Home:
    if (!gWindow->page->current_item) {
      scrollToFirstPage();
      break;
    }
  case XK_Up:
    if (!gWindow->page->current_item) {
      scrollUp();
      break;
    }
  case XK_Down:
    if (!gWindow->page->current_item) {
      scrollDown();
      break;
    }

  default:
    display_again = 0;
    dialog(event, keysym, key_buffer);
    XFlush(gXDisplay);
    break;
  }

  if (display_again) {
    display_page(gWindow->page);
    gWindow->fWindowHashTable = gWindow->page->fLinkHashTable;
  }
}

/*
 * This routine returns the modifier mask associated
 * to a key symbol.
 */

static unsigned int
get_modifier_mask(KeySym sym)
{
    unsigned int       i, mask;
    XModifierKeymap    *mod;
    KeyCode            kcode;
    const int          masks[8] = {
        ShiftMask, LockMask, ControlMask,
            Mod1Mask, Mod2Mask, Mod3Mask, Mod4Mask, Mod5Mask
    };

    mod = XGetModifierMapping(gXDisplay);
    kcode = XKeysymToKeycode(gXDisplay,sym);

    if (mod) {
        for (i = 0; i < (8 * mod->max_keypermod); i++){
            if (!mod->modifiermap[i]) continue;
            else if (kcode == mod->modifiermap[i]){
                mask = masks[i / mod->max_keypermod];
                XFreeModifiermap(mod);
                return mask;
            }
        }
        XFreeModifiermap(mod);
    }
    return 0;
}



/*
 * This routine initializes some of the variables needed by the input
 * strings, and boxes.
 */

void
init_keyin(void)
{
    char *prop;
    unsigned int nlm;


    nlm = get_modifier_mask(XK_Num_Lock);
    UnsupportedModMask &= ~nlm;
    ModifiersMask &= ~nlm;

    /*
     * First set all the values for when the active cursor is in the window
     */

    in_cursor_height = 2;
    in_cursor_y = gInputFont->max_bounds.ascent +
        gInputFont->max_bounds.descent;
    in_cursor_width = gInputFont->max_bounds.width;

    /*
     * Now for when the cursor is empty
     */

    out_cursor_height = gInputFont->max_bounds.ascent +
        gInputFont->max_bounds.descent;
    out_cursor_y = 2;
    out_cursor_width = in_cursor_width;

    start_x = 5;

    start_y = gInputFont->max_bounds.ascent;

    /*
     * Find out How big I should make the simple boxes
     */

    simple_box_width = XTextWidth(gInputFont, "X", 1) + 5;

    prop = XGetDefault(gXDisplay, gArgv[0], "ProtectedQuit");

    if (prop == NULL) {
        protected_quit = (char *) halloc(strlen("ProtectedPage") + 1,
                                         "protected_quit");
        strcpy(protected_quit, "ProtectedPage");
    }
    else {
        protected_quit = (char *) halloc(strlen(prop) + 1, "protected_quit");
        strcpy(protected_quit, prop);
    }


}
