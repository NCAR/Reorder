/* 	$Id: ground_echo.c 2 2002-07-02 14:57:33Z oye $	 */

#ifndef lint
static char vcid[] = "$Id: ground_echo.c 2 2002-07-02 14:57:33Z oye $";
#endif /* lint */

/*
 * This file contains the following routines
 * 
 */

# include <dorade_headers.h>
# include "input_limits.h"
# include "input_sweepfiles.h"
# include "dd_stats.h"
# include <dd_math.h>
# include <function_decl.h>
# include <dgi_func_decl.h>

static struct dd_input_sweepfiles_v3 *dis;
static struct dd_input_filters *difs;
static struct dd_stats *dd_stats=NULL;


int ge_init();
int dd_loop_ground_echo();
void dd_gecho();


/* c------------------------------------------------------------------------ */

int ge_init()
{
    int ii, ok=YES;
    struct dd_input_filters *dd_return_difs_ptr();
    struct dd_stats *dd_return_stats_ptr();
    struct dd_input_sweepfiles_v3 *dd_setup_sweepfile_structs_v3();
    struct unique_sweepfile_info_v3 *usi;

    /* do all the necessary initialization including
     * reading in the first ray
     */
    dis = dd_setup_sweepfile_structs_v3(&ok);
    dis->print_swp_info = YES;
    difs = dd_return_difs_ptr();
    dd_stats = dd_return_stats_ptr();

    /*
     * check to see if requested fields present
     */
    for(ii=0,usi=dis->usi_top; ii++ < dis->num_radars; usi=usi->next) {
	/* now try and find files for each radar by
	 * reading the first record from each file
	 */
	if(ddswp_next_ray_v3(usi) == END_OF_TIME_SPAN) {
	    printf( "Problems reading first record of %s/%s\n"
		   , usi->directory, usi->filename);
	    ok = NO;
	}
    }
    if(!ok)
	  return(-1);
    return(0);
}
/* c------------------------------------------------------------------------ */

int dd_loop_ground_echo()
{
    static int ii, rn, count=0, mark;
    struct unique_sweepfile_info_v3 *usi;
    struct dd_general_info *dgi, *dd_window_dgi();
    
    if((ii=ge_init()) == END_OF_TIME_SPAN ) {
	return(1);
    }
    else if( ii < 0 )
	  return(1);

    usi = dis->usi_top;
    /*
     * for each radar, write out the UF file
     */
    for(rn=0; rn++ < dis->num_radars; usi=usi->next) {
	dgi = dd_window_dgi(usi->radar_num);
	for(;;) {
	    if(!(count++ % 500))
		  dgi_interest_really(dgi, NO, "", "", dgi->dds->swib);
	    dd_gecho(dgi);
	    
	    dgi->new_vol = NO;
	    dgi->new_sweep = NO;
	    if(ddswp_next_ray_v3(usi) == END_OF_TIME_SPAN) {
		break;
	    }
# ifdef obsolete
	    if(usi->swp_count >= usi->num_swps)
		  break;
# endif
	}
    }

    dd_stats->rec_count = dis->rec_count;
    dd_stats->ray_count = dis->ray_count;
    dd_stats->sweep_count = dis->sweep_count;
    dd_stats->vol_count = dis->vol_count;
    dd_stats->file_count = dis->file_count;
    dd_stats->MB = dis->MB;

    return(1);
}
/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */




