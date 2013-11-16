/* Process control definitions.  Used by fork_you and spawn_of_hell */

/* When a process dies it kills off everything else */
#define Die 1
/* When a process dies, do nothing */
#define NadaDelShitsky  2
/* When a process dies start it up again */
#define DoItAgain       3
/* When hypertex dies, clean its socket */
#define CleanHypertexSocket 4

typedef struct spad_proc {
  int   proc_id;        /* process id of child */
  int   death_action;   /* one of the above constants */
  char  *command;       /* sh command line to restart the process */
  struct spad_proc *next;
} SpadProcess;
