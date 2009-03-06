/* 	$Id: Comment.h 2 2002-07-02 14:57:33Z oye $	 */

struct comment_d {
    char comment_des[4];	/* Comment descriptor identifier: ASCII */
				/* characters "COMM" stand for Comment */
				/* Descriptor. */
    long  comment_des_length;	/* Comment descriptor length in bytes. */
    char  comment[500];	        /* comment*/

}; /* End of Structure */



typedef struct comment_d comment_d;
typedef struct comment_d COMMENT;

