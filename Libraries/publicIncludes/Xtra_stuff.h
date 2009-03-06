/* 	$Id: Xtra_stuff.h 2 2002-07-02 14:57:33Z oye $	 */

# ifndef XTRA_STUFF_H
# define XTRA_STUFF_H

struct dd_extra_stuff {		/* extra container for non DORADE structs */
  char name_struct[4];		/* "XSTF" */
  long sizeof_struct;

  long one;			/* always set to one (endian flag) */
  long source_format;		/* as per ../include/dd_defines.h */

  long offset_to_first_item;	/* bytes from start of struct */
  long transition_flag;
} ;

typedef struct dd_extra_stuff XTRA_STUFF;

# endif
