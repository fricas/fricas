/* edin.c */
extern void init_reader(void);
extern void do_reading(void);
extern void send_function_to_child(void);
/* fnct_key.c */
extern void define_function_keys(void);
extern void handle_function_key(int key);
/* wct.c */
extern void load_wct_file(char * fn);
extern void skim_wct(void);
extern void rescan_wct(void);
extern void find_wct(void);
/* prt.c */
extern void myputchar(char c);
extern void clear_buff(void);
extern void move_end(void);
extern void move_home(void);
extern void move_fore_word(void);
extern void move_back_word(void);
extern void delete_current_char(void);
extern void delete_to_end_of_line(void);
extern void delete_line(void);
extern void printbuff(int start, int cnt);
extern void ins_print(int start, int cnt);
extern void reprint(int start);
extern void back_up_and_blank(int bp);
extern int back_up(int bp);
extern void print_whole_buff(void);
extern void move_ahead(void);
extern void move_back(void);
extern void back_over_current_char(void);
