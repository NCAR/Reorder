/* 	$Id: run_sum.h 2 2002-07-02 14:57:33Z oye $	 */

# ifndef RUN_SUM_H
# define RUN_SUM_H
/* c------------------------------------------------------------------------ */

struct run_sum {
    double sum;
    double short_sum;
    double *vals;
    int count;
    int index_next;
    int index_lim;
    int run_size;
    int short_size;
};
/* c------------------------------------------------------------------------ */
# endif
