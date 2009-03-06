/* 	$Id: dda_common.c 244 2006-04-13 15:47:55Z dennisf $	 */

#ifndef lint
static char vcid[] = "$Id: dda_common.c 244 2006-04-13 15:47:55Z dennisf $";
#endif /* lint */
/*
 * This file contains the following routines
 * 
 * 
 * dd_assign_radar_num
 * dd_catalog_only_flag
 * dd_cchandl
 * 
 * dd_clean_tdfs
 * dd_close
 * dd_control_c
 * dd_dump_headers
 * dd_first_comment
 * dd_get_structs
 * dd_intset
 * 
 * dd_last_comment
 * dd_min_rays_per_sweep
 * dd_next_comment
 * dd_kill_tdf
 * dd_num_radars
 * dd_open
 * dd_output_flag
 * dd_position
 * dd_rename_swp_file
 * dd_reset_control_c
 * dd_return_difs_ptr
 * dd_return_radar_num
 * dd_return_stats_ptr
 * dd_time_dependent_fixes
 * 
 * dd_unlink
 * dd_window_dgi
 * dd_write
 * dd_vol_reset
 * dgi_last
 * difs_terminators
 * 
 * 
 * return_dd_open_inf
 * 
 * do_print_dd_open_info
 * dont_print_dd_open_info
 * 
 */

# include "time_dependent_fixes.h"
# include <dorade_headers.h>
# include "input_limits.h"
# include "dd_stats.h"
# include <time.h>
# include <sys/time.h>
# include <signal.h>

# ifndef PISP
# include <point_in_space.h>
# endif

# include <dd_math.h>
# include <function_decl.h>

# define PMODE 0666
# define MIN_RAYS_PER_SWEEP 1

# ifndef SOLO_HALT
# define SOLO_EXIT 0x1
# define SOLO_HALT 0x2
# endif

int Solo_state=0;

static struct time_dependent_fixes *tdf=NULL;
static struct dd_general_info *dgi_ptrs[MAX_SENSORS], *last_dgi=NULL;
static struct dd_general_info *window_dgi[MAX_SENSORS];
static struct dd_stats *dd_stats=NULL;
static struct dd_input_filters *keep_difs=NULL;
static struct dd_comm_info *first_comment=NULL;
static int Num_radars=0, Num_windows=0;
static int Min_rays_per_sweep=0;
static int Max_rays_per_sweep=0;
static int Max_sweeps_per_volume=0;
static int Min_sweeps_per_volume=0;
static int Output_flag=NO;
static int Catalog_only_flag=NO, print_dd_open_info=YES;
static char dd_open_info[128];
static int Control_C=NO;
static int solo_flag=0;
void dd_unlink();



/* c------------------------------------------------------------------------ */

void solo_set_halt_flag()
{
    Solo_state |= SOLO_HALT;
}
/* c------------------------------------------------------------------------ */

void solo_clear_halt_flag()
{
    int ii=~SOLO_HALT;
    Solo_state &= ii;
    Control_C = NO;
}
/* c------------------------------------------------------------------------ */

int solo_halt_flag()
{
    return(SOLO_HALT SET_IN Solo_state);
}
/* c------------------------------------------------------------------------ */

void dd_set_control_c()
{
    Control_C = YES;
    return;
}
/* c------------------------------------------------------------------------ */

void dd_reset_control_c()
{
    Control_C = NO;
    return;
}
/* c------------------------------------------------------------------------ */

int dd_control_c()
{
    return(Control_C);
}
/* c------------------------------------------------------------------------ */

#if defined (SYSV) && !defined (SVR4)
void dd_cchandl()
#else
int dd_cchandl()
#endif
{
    printf("\nCaught Cntrl-C!\n\n");
    Control_C = YES;
    solo_set_halt_flag();

# ifdef SYSV
# ifdef SVR4
    signal (SIGINT, (void *) dd_cchandl);
# else
    (void) signal (SIGINT, dd_cchandl);
# endif
# endif
}
/* c------------------------------------------------------------------------ */

int dd_catalog_only_flag()
{
    return(Catalog_only_flag);
}
/* c---------------------------------------------------------------------- */

struct dd_stats *dd_stats_reset()
{
   if(!dd_stats) {
      return(dd_return_stats_ptr());
   }
   memset((char *)dd_stats, 0, sizeof(struct dd_stats));
   return(dd_stats);
}
/* c---------------------------------------------------------------------- */

char * 
dd_stats_sprintf()
{
    static char stats[88];

    sprintf(stats, " r:%d b:%d s:%d v:%d f:%d %.2f MB"
	    , dd_stats->rec_count
	    , dd_stats->ray_count
	    , dd_stats->sweep_count
	    , dd_stats->vol_count
	    , dd_stats->file_count
	    , dd_stats->MB
	    );
    return(stats);
}
/* c------------------------------------------------------------------------ */

int dd_solo_flag()
{
    return(solo_flag);
}
/* c------------------------------------------------------------------------ */

int dd_set_solo_flag(val)
  int val;
{
    return(solo_flag = val);
}
/* c------------------------------------------------------------------------ */

int dd_assign_radar_num(radar_name)
  char *radar_name;
{
    int i, rn;
    struct dd_general_info *dd_ini();
    /*
     * return the set of structures associated with
     * this radar
     */
    if(Num_radars == 0) {	/* Initialize some stuff */
	for(i=0; i < MAX_SENSORS; i++ ) dgi_ptrs[i] = NULL;
    }
    else {
	if(!strlen(radar_name)) {
	    return(-1);
	}
	for(rn=0; rn < Num_radars; rn++) {
	    if(strncmp(radar_name, dgi_ptrs[rn]->radar_name, 8 ) == 0 ) {
		return(rn);
	    }
	}
    }
    /* Haven't dealt with this radar yet
     * initialize for this radar
     */
    last_dgi = dgi_ptrs[Num_radars] = dd_ini(Num_radars, radar_name);
    return(Num_radars++);
}
/* c------------------------------------------------------------------------ */

struct time_dependent_fixes *
dd_clean_tdfs(fx_count)
  int *fx_count;
{
    int nn=0;
    struct time_dependent_fixes *fix, *dd_kill_tdf();

    *fx_count = 0;
    if(!tdf)
	  return(NULL);

    for(fix=tdf; fix; fix = fix->next) if(!fix->use_it) nn++;

    for(; nn--; ) {
	for(fix=tdf; fix; fix = fix->next) {
	    if(!fix->use_it) {
		dd_kill_tdf(fix, fx_count);
		break;
	    }
	}
    }
    /* count them again in case there were no kills
     */
    *fx_count = 0;
    for(fix=tdf; fix; fix = fix->next, (*fx_count)++);
    return(tdf);
}
/* c------------------------------------------------------------------------ */

void dd_close( fid )
  int fid;
{
    int mark;

    if( !Output_flag  )
	  return;
    mark = close( fid );
    return;
}
/* c------------------------------------------------------------------------ */

struct dd_comm_info *
dd_first_comment()
{
    return(first_comment);
}
/* c------------------------------------------------------------------------ */

struct dd_general_info *
dd_get_structs(radar_num)
  int radar_num;
{
    /*
     * return the set of structures associated with
     * this radar
     */
    if( radar_num < 0 || radar_num >= Num_radars) {
	printf("Bad radar number Ray: %d\n", dd_stats->beam_count);
	exit(1);
    }

    return(last_dgi = dgi_ptrs[radar_num]);
}
/* c------------------------------------------------------------------------ */

/*
 * Keyboard interrupt handling.
 */

void dd_intset()
{
#if defined (SYSV) && !defined (SVR4)
    void dd_cchandl();
    (void) signal (SIGINT, dd_cchandl);
#else
    int dd_cchandl();
    signal (SIGINT, (void *) dd_cchandl);
#endif
}
/* c------------------------------------------------------------------------ */

struct dd_comm_info *
dd_last_comment()
{
    if(first_comment)
	  return(first_comment->last);
    return(NULL);
}
/* c------------------------------------------------------------------------ */

int
dd_min_rays_per_sweep()
{
    char *aa, *get_tagged_string();
    int ii;

    if(!Min_rays_per_sweep) {
	if(aa=get_tagged_string("MIN_RAYS_PER_SWEEP")) {
	    ii = atoi(aa);
	    if(ii > 0)
		  Min_rays_per_sweep = ii;
	}
	else if(aa=getenv("MIN_RAYS_PER_SWEEP")) {
	    ii = atoi(aa);
	    if(ii > 0)
		  Min_rays_per_sweep = ii;
	}
	if(!Min_rays_per_sweep) {
	    Min_rays_per_sweep = 5;
	}
    }
    return(Min_rays_per_sweep);
}
/* c------------------------------------------------------------------------ */

int
dd_max_rays_per_sweep()
{
    char *aa, *get_tagged_string();
    int ii;

    if(!Max_rays_per_sweep) {
	if(aa=get_tagged_string("MAX_RAYS_PER_SWEEP")) {
	    ii = atoi(aa);
	    if(ii > 0)
		  Max_rays_per_sweep = ii;
	}
	if(!Max_rays_per_sweep) {
	    Max_rays_per_sweep = 999999999;
	}
    }
    return(Max_rays_per_sweep);
}
/* c------------------------------------------------------------------------ */

int
dd_max_sweeps_per_volume()
{
    char *aa, *get_tagged_string();
    int ii;

    if(!Max_sweeps_per_volume) {
	if(aa=get_tagged_string("MAX_SWEEPS_PER_VOLUME")) {
	    ii = atoi(aa);
	    if(ii > 0)
		  Max_sweeps_per_volume = ii;
	}
	if(!Max_sweeps_per_volume) {
	    Max_sweeps_per_volume = 999999999;
	}
    }
    return(Max_sweeps_per_volume);
}
/* c------------------------------------------------------------------------ */

int
dd_min_sweeps_per_volume()
{
    char *aa, *get_tagged_string();
    int ii;

    if(!Min_sweeps_per_volume) {
	if(aa=get_tagged_string("MIN_SWEEPS_PER_VOLUME")) {
	    ii = atoi(aa);
	    if(ii > 0)
		  Min_sweeps_per_volume = ii;
	}
	if(!Min_sweeps_per_volume) {
	    Min_sweeps_per_volume = 1;
	}
    }
    return(Min_sweeps_per_volume);
}
/* c------------------------------------------------------------------------ */

struct comment_d *
dd_return_comment(num)
  int num;
{
    int ii;
    struct dd_comm_info *this = first_comment;

    if(!first_comment || !first_comment->num_comments)
	  return(NULL);		/* no comments! */
    
    for(; num-- && this; this = this->next);
    return(this ? this->comm : NULL);
}
/* c------------------------------------------------------------------------ */

int
dd_return_num_comments()
{
    if(first_comment) {
	return(first_comment->num_comments);
    }
    return(0);
}
/* c------------------------------------------------------------------------ */

void
dd_reset_comments()
{
    if(first_comment) {
	first_comment->num_comments = 0;
    }
}
/* c------------------------------------------------------------------------ */

struct dd_comm_info *
dd_next_comment(comm, little_endian)
  struct comment_d *comm;
{
    /* returns a pointer to the next comment info struct
     * this routine expects the calling routine to malloc space for
     * the actual comment, set the pointer and copy the comment into it
     */
    int nn = 0, cdl=0;
    struct dd_comm_info *this=first_comment;

    if(!comm)
	  return(NULL);

    if(little_endian) {
       swack4((char *)comm +4, &cdl);
    }
    else {
       cdl = comm->comment_des_length;
    }

    if(cdl < 8 || cdl > MAX_REC_SIZE) {
	return(NULL);
    }

    if(first_comment)
	  nn = first_comment->num_comments;

    for(; nn-- ; this = this->next);

    if(!this) {
	this = (struct dd_comm_info *)malloc(sizeof(struct dd_comm_info));
	memset((char *)this, 0, sizeof(struct dd_comm_info));
	if(!first_comment) first_comment = this;
    }
    if(!this->comm ||
      this->comm->comment_des_length < cdl) {
	if(this->comm) free(this->comm);
	this->comm = (struct comment_d *)malloc(cdl);
	memset(this->comm, 0, cdl);
    }
    memcpy(this->comm, comm, cdl);
    this->comm->comment_des_length = cdl;

    if(first_comment->num_comments++)
	  first_comment->last->next = this;
    first_comment->last = this;	/* always points to last comment */
    return(this);
}
/* c------------------------------------------------------------------------ */

struct time_dependent_fixes *
dd_kill_tdf(fx, fx_count)
  struct time_dependent_fixes *fx;
  int *fx_count;
{
    /* remove this fix struct and returns the number of fixes left
     */
    struct time_dependent_fixes *fix, *keep_fix=NULL;

    *fx_count = 0;

    if(!(fix = tdf))
	  return(NULL);
    /* find this fix
     */
    for(; fix; fix = fix->next) {
	if(fix->use_it) (*fx_count)++;
	if(fx == fix) keep_fix = fix;
    }
    if(!keep_fix) {
	printf("Bogus time dependent fix struct: %d\n", fx);
	return(tdf);		/* bogus fix struct */
    }
    *fx_count = 0;

    printf("TDFK Killing node: %d\n", fx->node_num);
    if(fx == tdf && !tdf->next) { /* only one left */
	free(fx);
	return(tdf=NULL);
    }
    if(fx == tdf) {
	tdf = fx->next;
	tdf->last = fx->last;
    }
    else if(fx == tdf->last) {
	fx->last->next = NULL;
	tdf->last = fx->last;
    }
    else {
	fx->last->next = fx->next;
	fx->next->last = fx->last;
    }

    for(fix=tdf; fix; fix = fix->next) {
	if(fix->use_it) (*fx_count)++;
    }
    free(fx);
    return(tdf);
}
/* c------------------------------------------------------------------------*/

int dd_num_radars()
{
    return(Num_radars);
}
/* c------------------------------------------------------------------------ */

int dd_open(dir, file_name )
  char *dir, *file_name;
{
    int i = -99, mark;
    char name[256], *a;

    strcpy( name, dir );
    strcat( name, file_name );
    if( Output_flag )
	  i = creat( name, PMODE );

    if(i > 11) {
	mark = 0;
    }

    if(a=strrchr(name, '/')) {
	sprintf(dd_open_info,  " file %s:%d \n", a, i );
    }
    else {
	sprintf(dd_open_info, " file %s:%d \n", name, i );
    }
    if(print_dd_open_info)
	  printf("%s", dd_open_info);
    return(i);
}
/* c------------------------------------------------------------------------ */

int dd_output_flag( val )
  int val;
{
    if( val != EMPTY_FLAG )
	  Output_flag = val ? YES : NO;
    return(Output_flag);
}
/* c------------------------------------------------------------------------ */
# define FROM_BOF 0

int dd_position( fid, offset )
  int fid, offset;
{
    int i=-2;
    if( Output_flag)
	  i = lseek( fid, (long)offset, FROM_BOF );
    return(i);
}
/* c------------------------------------------------------------------------*/

void dd_rename_swp_file(dgi)
  struct dd_general_info *dgi;
{
    char *aa, *bb, str[256], str2[256], *qq=dgi->file_qualifier;
    char file_name[88], scan_mode_mne[16], *cc, *sptrs[32];
    int nn, num_us = 0, sm, vn;

    slash_path(str, dgi->directory_name);
    strcat(str, dgi->sweep_file_name);

    /* create the new sweep file name
     */
    strcpy(file_name, dgi->sweep_file_name);
    bb = strstr(file_name, ".tmp");
    sm = (dgi->prev_scan_mode < 0) ? dgi->dds->radd->scan_mode
      : dgi->prev_scan_mode;
    dd_scan_mode_mne(sm, scan_mode_mne );

    vn = (dgi->prev_vol_num < 0) ? dgi->dds->vold->volume_num
      : dgi->prev_vol_num;

    sprintf(bb, ".%.1f_%s_v%d"
	    , dgi->dds->swib->fixed_angle
	    , scan_mode_mne
	    , vn
	    );

    /* try to preserve legacy qualifiers */

    for( aa = qq;  *aa ; aa++ ) { /* count the underscores */
      if( *aa == '_' )
	{ sptrs[ num_us++ ] = aa+1; }
    }

    if( num_us == 2 && strstr( qq, "min_accum" )) { /* rainfall files */
      strcat( bb, "_" );
      strcat( bb, sptrs[0] );
    }
    else if( num_us > 2 ) {
       strcat( bb, "_" );
       strcat( bb, sptrs[2] );
    }

    strcpy(dgi->swp_que->file_name, file_name);

    slash_path(str2, dgi->directory_name);
    strcat(str2, dgi->swp_que->file_name);

    if(Output_flag) {
      nn = rename ( str, str2);
    }
}
/* c------------------------------------------------------------------------ */

struct dd_input_filters *
dd_return_difs_ptr()
{
    if(!keep_difs) {
	keep_difs = (struct dd_input_filters *)
	      malloc(sizeof(struct dd_input_filters));
	memset((char *)keep_difs, 0, sizeof(struct dd_input_filters));

    }
    return(keep_difs);
}
/* c------------------------------------------------------------------------ */

int dd_return_radar_num(radar_name)
  char *radar_name;
{
    /* see if a dgi struct has been established for this radar
     * and return the number if it has
     */
    int rn=0;

    if(!strlen(radar_name) || Num_radars == 0) {
	return(-1);
    }

    for(; rn < Num_radars ; rn++) {
	if(strstr(dgi_ptrs[rn]->radar_name, radar_name)) {
	    return(rn);
	}
    }
    return(-1);
}
/* c------------------------------------------------------------------------ */

struct dd_stats *
dd_return_stats_ptr()
{
    if(!dd_stats) {
	dd_stats = (struct dd_stats *)malloc(sizeof(struct dd_stats));
	memset((char *)dd_stats, 0, sizeof(struct dd_stats));
    }
    return(dd_stats);
}
/* c------------------------------------------------------------------------ */

struct time_dependent_fixes *
dd_time_dependent_fixes(fx_count)
  int *fx_count;
{
    int ii, jj, kk, nn, nt, arg_num=0, dd_tokens(), node_num=0;
    char *aa, *bb, *cc, *dd,  *get_tagged_string(), last_char=' ';
    char string_space[2048], *str_ptrs[256];
    DD_TIME dts;
    struct time_dependent_fixes *fix, *dd_clean_tdfs();
    float f;

    *fx_count = 0;

    if(tdf) {
	for(fix=tdf; fix; fix = fix->next) {
	    if(fix->use_it) (*fx_count)++;
	}
	return(tdf);
    }
    if(!(aa = get_tagged_string("TIME_DEPENDENT_FIXES"))) {
	return(NULL);
    }
    nn = strlen(aa);
    bb = aa;
    cc = string_space;
    dd = &last_char;

    for(; nn--; bb++) {		/* zap xtra blanks, tabs and new lines */
	if(*bb == '\t' || *bb == '\n') *bb = ' ';
	if(*bb != ' ' || *dd != ' ') {
	    *cc++ = *bb;
	}
	*dd = *bb;
    }
    *cc++ = '\0';
    cc = string_space;
    nt = dd_tokens(string_space, str_ptrs);
    dts.year = dts.month = dts.day = dts.day_seconds = 0;

    for(ii=0; ii < nt;) {

	if(strncmp(str_ptrs[ii], "FIX", 4) == 0) {
	    arg_num = 1; ii++;
	    fix = (struct time_dependent_fixes *)
		  malloc(sizeof(struct time_dependent_fixes));
	    memset(fix, 0, sizeof(struct time_dependent_fixes));
	    fix->node_num = node_num++;
	    if(!tdf) {
		tdf = fix;
	    }
	    else {
		fix->last = tdf->last;
		tdf->last->next = fix;
	    }
	    tdf->last = fix;
	    continue;
	}

	switch(arg_num) {
	case 1:			/* start time */
	    if(kk = dd_crack_datime
	       (str_ptrs[ii], strlen(str_ptrs[ii]), &dts)) {
		fix->start_time = d_time_stamp(&dts);
		arg_num++; ii++;
	    }
	    else { arg_num = 0; }
	    break;

	case 2:
	    arg_num++; ii++;
	    break;

	case 3:
	    if(kk = dd_crack_datime
	       (str_ptrs[ii], strlen(str_ptrs[ii]), &dts)) {
		fix->stop_time = d_time_stamp(&dts);
		arg_num++; ii++;
	    }
	    else { arg_num = 0; }
	    break;

	case 4:
	    arg_num = 0;
	    if(!strncmp(str_ptrs[ii], "CELL_SPACING", 6)) {
		if((ii + 2) >= nt)
		      break;
		if((kk = sscanf(str_ptrs[ii + 2], "%f", &f)) == 1) {
		    fix->cell_spacing = f;
		    fix->typeof_fix = FIX_CELL_SPACING;
		    fix->use_it = YES;
		    (*fx_count)++;
		    ii += 3;
		}
	    }

	    else if(!strncmp(str_ptrs[ii], "NUM_CELLS", 5)) {
		if((ii + 2) >= nt)
		      break;
		if((kk = sscanf(str_ptrs[ii + 2], "%d", &nn)) == 1) {
		    fix->num_cells = nn;
		    fix->typeof_fix = FIX_NUM_CELLS;
		    fix->use_it = YES;
		    (*fx_count)++;
		    ii += 3;
		}
	    }
	    else if(!strncmp(str_ptrs[ii], "POINTING_ANGLE", 5)) {
		if((ii + 2) >= nt)
		      break;
		if(!strncmp(str_ptrs[ii + 2], "DOWN", 4)) {
		    fix->pointing_angle = 180.;
		    fix->typeof_fix = FIX_POINTING_ANGLE;
		    fix->use_it = YES;
		    (*fx_count)++;
		    ii += 3;
		}
		else if(!strncmp(str_ptrs[ii + 2], "UP", 2)) {
		    fix->pointing_angle = 0.;
		    fix->typeof_fix = FIX_POINTING_ANGLE;
		    fix->use_it = YES;
		    (*fx_count)++;
		    ii += 3;
		}
	    }
	    else if(!strncmp(str_ptrs[ii], "RED_BASELINE", 5) ||
		    !strncmp(str_ptrs[ii], "GREEN_BASELINE", 7)) {
		if((ii + 2) >= nt)
		      break;
		if((kk = sscanf(str_ptrs[ii + 2], "%f", &f)) == 1) {
		    fix->baseline = f;
		    fix->use_it = YES;
		    fix->typeof_fix =
			  !strncmp(str_ptrs[ii], "RED_BASELINE", 5)
				? RED_BASELINE : GREEN_BASELINE;
		    (*fx_count)++;
		    ii += 3;
		}
	    }
	    else if(!strncmp(str_ptrs[ii], "RED_COUNTS_PER_DB", 5) ||
		    !strncmp(str_ptrs[ii], "GREEN_COUNTS_PER_DB", 7)) {
		if((ii + 2) >= nt)
		      break;
		if((kk = sscanf(str_ptrs[ii + 2], "%f", &f)) == 1) {
		    fix->counts_per_db = f;
		    fix->use_it = YES;
		    fix->typeof_fix =
			  !strncmp(str_ptrs[ii], "RED_COUNTS_PER_DB", 5)
				? RED_COUNTS_PER_DB : GREEN_COUNTS_PER_DB;
		    (*fx_count)++;
		    ii += 3;
		}
	    }
	    else if(!strncmp(str_ptrs[ii], "RED_END_RANGE", 5) ||
		    !strncmp(str_ptrs[ii], "GREEN_END_RANGE", 7)) {
		if((ii + 2) >= nt)
		      break;
		if((kk = sscanf(str_ptrs[ii + 2], "%f", &f)) == 1) {
		    fix->end_range = f;
		    fix->use_it = YES;
		    fix->typeof_fix =
			  !strncmp(str_ptrs[ii], "RED_END_RANGE", 5)
				? RED_END_RANGE : GREEN_END_RANGE;
		    (*fx_count)++;
		    ii += 3;
		}
	    }
	    else {
	    }
	    break;

	default:		/* error mode */
	    ii++;		/* keep looping till another "FIX" is found */
	    break;
	}
    }
    dd_clean_tdfs(fx_count);

    if(*fx_count) {	/* print fixes */
	for(fix=tdf; fix; fix = fix->next) {
	    bb = cc = string_space;
	    dts.time_stamp = fix->start_time;
	    sprintf(bb, "From %s to ", dts_print(d_unstamp_time(&dts)));
	    bb += strlen(bb);
	    dts.time_stamp = fix->stop_time;
	    sprintf(bb, "%s ", dts_print(d_unstamp_time(&dts)));
	    bb += strlen(bb);

	    switch(fix->typeof_fix) {

	    case FIX_POINTING_ANGLE:
		sprintf(bb, "Pointing angle: %.1f", fix->pointing_angle);
		break;
	    case FIX_CELL_SPACING:
		sprintf(bb, "Cell spacing: %.2f", fix->cell_spacing);
		break;
	    }
	    bb += strlen(bb);
	    dd_append_cat_comment(cc);
	    printf("%s\n", cc);
	}
	return(tdf);
    }
    return(NULL);
}
/* c------------------------------------------------------------------------ */

void dd_unlink(full_path_name)
  char *full_path_name;
{
    int nn;
    if(Output_flag && strlen(full_path_name))
	  nn = unlink(full_path_name);
}
/* c------------------------------------------------------------------------ */

DGI_PTR 
dd_window_dgi(window_num)
  int window_num;
{
    /* returns the dgi struct pointer for the requested window
     */
    struct dd_general_info *dd_ini();
    int i;

    if(Num_windows == 0) {		/* first time! */
	for(i=0; i < MAX_SENSORS; i++ ) window_dgi[i] = NULL;
    }
    
    if(!window_dgi[window_num]) {
	window_dgi[window_num] = dd_ini(window_num, "");
	Num_windows++;
    }
    return(window_dgi[window_num]);
}
/* c------------------------------------------------------------------------ */

void dd_write( fid, buf, size )
  int fid, size;
  char *buf;
{
    static int err_count=0;
    int i;

    if( !Output_flag )
	  return;

    if((i = write( fid, buf, size )) < size ) {
	printf( "Problem in dd_write--err=%d  fid:%d\n", i, fid );
	if(++err_count > 11)
	      exit(1);
    }
    return;
}
/* c------------------------------------------------------------------------ */

DGI_PTR dgi_last()
{
    return(last_dgi);
}
# ifdef obsolete
/* c------------------------------------------------------------------------ */

void difs_terminators(dgi, difs, dd_stats)
  DGI_PTR dgi;
  struct dd_input_filters *difs;
  struct dd_stats *dd_stats;
{
    int i, mark;
    double d, ddir_free();

    if(difs->min_free_MB &&
       ((d=ddir_free(dgi->directory_name)) <= difs->min_free_MB)) {
	difs->stop_flag = YES;
    }
}
# endif
/* c------------------------------------------------------------------------ */

void do_print_dd_open_info()
{
    print_dd_open_info = YES;
}
/* c------------------------------------------------------------------------ */

void dont_print_dd_open_info()
{
    print_dd_open_info = NO;
}
/* c------------------------------------------------------------------------ */

char *
return_dd_open_info()
{
    return(dd_open_info);
}
/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

