/* 	$Id: Pdata.h 2 2002-07-02 14:57:33Z oye $	 */

# ifndef INCPdatah
# define INCPdatah

struct paramdata_d {
    char pdata_desc[4];	      /* parameter data descriptor identifier: ASCII */
			      /* characters "RDAT" stand for sweep info */
			      /* block Descriptor. */
    long  pdata_length;       /* parameter data descriptor length in bytes. */
    char  pdata_name[8];      /*name of parameter*/
}; /* End of Structure */



typedef struct paramdata_d paramdata_d;
typedef struct paramdata_d PARAMDATA;

# endif /* INCPdatah */
