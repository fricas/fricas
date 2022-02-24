/* All the previous commands are stored in a double linked list. */
typedef struct que_struct {
   char buff[1024];
   char flags[1024];
   struct que_struct *prev, *next;
} QueStruct;

extern int buff_pntr;        /* present length of  buff       */
extern int curr_pntr;        /* the current position in buff  */

extern int get_buff(int i);
extern int get_flag(int i);

extern int prt_char(int i);
extern int dist_left(int i);
extern int dist_right(int i);

extern void re_init_buff(void);
extern int store_buff_char(int pos, char c, int f);
extern void store_buff_string(int pos, int cnt, char * cp, int f);
extern void shift_buff_forward(int curr_pntr, int buff_pntr, int diff);
extern void shift_buff_backward(int curr_pntr, int buff_pntr, int diff);
extern void shift_buff(int cp, int bp, int diff);

extern void store_final_buff_char(char c);

extern void null_termiante_buff(void);
extern int buff_length(void);
extern int buff_cmp(char *s);

extern void write_buff(int fd);

extern void store_buff_to_ring(QueStruct *ring, int buff_pntr);
extern void restore_ring_to_buff(QueStruct * ring);
