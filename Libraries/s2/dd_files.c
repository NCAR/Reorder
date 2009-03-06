/* 	$Id: dd_files.c 185 2003-10-09 17:30:43Z oye $	 */

#ifndef lint
static char vcid[] = "$Id: dd_files.c 185 2003-10-09 17:30:43Z oye $";
#endif /* lint */
/* c------------------------------------------------------------------------ */

/*
 * This file contains routines
 * 
 * new version 3 stuff!
 * 
 * craack_ddfn
 * ddfn_file_info_line
 * ddfn_file_name
 * ddfn_list_versions
 * ddfn_pop_spair
 * ddfn_ptr_list
 * ddfn_push_spair
 * ddfn_search
 * ddfn_sort_insert
 * ddfn_time
 * ddfnp_list
 * ddir_files_v3
 * ddir_radar_num_v3
 * ddir_rescan_urgent
 * d_mddir_file_info_v3
 * mddir_entire_list_v3
 * mddir_file_info_v3
 * mddir_file_list_v3
 * mddir_gen_swp_list_v3
 * mddir_num_radars_v3
 * mddir_radar_name_v3
 * mddir_radar_num_v3(
 * mddir_return_swp_list_v3
 * return_ddir
 * 
 * 
 * 
 * 
 */
# include <sys/types.h>
# include <dirent.h>
# include <time.h>
# include <sys/time.h>

# ifdef osx
#  include <sys/vstat.h>
# endif

# include <dd_defines.h>
# include "dd_files.h"
# include <dorade_share.h>
# include "solo_utils.h"
# include <function_decl.h>

# include <errno.h>

# ifdef obsolete
#ifdef SVR4
# include <sys/statvfs.h>
#else
# ifdef IRIX4
#  include <sys/statfs.h>
# else
#  include <sys/vfs.h>
# endif
#endif
# endif

# define MAX_DIRECTORIES 16
# define MAX_FILE_TYPES 16
# define MAX_EXTEND 222

/* c------------------------------------------------------------------------ */

# define LT -1
# define EQ  0
# define GT  1

static struct dd_file_name_v3 *ddfn_spairs=NULL;

struct dd_radar_name_info_v3 {
    int num_sweeps;
    int num_ddfnp;
    int prev_req_type;
    struct dd_file_name_v3 **first_ddfnp;

    struct dd_file_name_v3 *top_ddfn;
    struct dd_file_name_v3 *h_node;
    struct dd_file_name_v3 *z_node;
    struct dd_file_name_v3 *prev_ddfn;
};

static int ddir_num_dirs_v3=0;
static struct dd_file_name_v3 **ddfnpl;
static int count_spairs=0, num_ddfnp=0;
static struct ddir_info_v3 *ddir_list_v3[MAX_DIRECTORIES];

struct ddir_info_v3 {
    long rescan_time;
    int auto_rescan_hit_max;
    int dir_num;
    int no_auto_scan;
    int num_hits;
    int num_radars;
    int rescan_urgent;
    struct dd_radar_name_info_v3 *rni[MAX_SENSORS];
    char *radar_name[MAX_SENSORS];
    char directory[128];
};
/* c------------------------------------------------------------------------ */

struct dd_radar_name_info {
    int num_vols;
    int num_swps;
    struct dd_file_name **swp_list;
    struct dd_file_name **vol_list;
    char *radar_name;
};

struct ddir_info {
    int dir_num;
    int max_entries;
    int extend;
    int num_radars;
    int num_swps;
    int num_vols;

    struct dd_file_name *ddfn;
    struct dd_file_name **ddfnp;
    struct dd_radar_name_info *rni[MAX_SENSORS];
    char directory_name[128];
};

static char *ascii_file_type[MAX_FILE_TYPES];


int      craack_ddfn();
char *   ddfn_file_info_line();
void     ddfn_file_name();
void     ddfn_list_versions();
struct dd_file_name_v3 *ddfn_pop_spair();
void     ddfn_ptr_list();
void     ddfn_push_spair();
struct dd_file_name_v3 *ddfn_search();
void     ddfn_sort_insert();
double   ddfn_time();
int      ddfnp_list();
int      ddir_files_v3();
int      ddir_radar_num_v3();
void     ddir_rescan_urgent();
double   d_mddir_file_info_v3();
struct dd_file_name_v3 **mddir_entire_list_v3();
long     mddir_file_info_v3();
int      mddir_file_list_v3();
int      mddir_gen_swp_list_v3();
int      mddir_gen_swp_str_list_v3();
int      mddir_num_radars_v3();
char *   mddir_radar_name_v3();
int      mddir_radar_num_v3();
struct dd_file_name_v3 **mddir_return_swp_list_v3();
double   ddfnp_list_entry();
struct ddir_info_v3 *return_ddir();
double   ddir_free();
double d_time_stamp();


/* c------------------------------------------------------------------------ */
/* new stuff */
/* c------------------------------------------------------------------------ */

int craack_ddfn(fn, ddfn)
  char *fn;
  struct dd_file_name_v3 *ddfn;
{
    char *delim=DD_NAME_DELIMITER;
    char *sptr[4];
    char str[128], *last_field, number[8];
    int times[6];
    int ii, jj, v, ndots;
    int yy, mon, dd, hh, mm, ss, ms;
    char *a=fn, *b, *c;
    double d;
    DD_TIME dts;

    
    for(; *a && *a != *delim; a++); /* move over first field and dot */
    a++;

    for(c=str,ndots=0; *a && ndots < 3; a++) {
	if(*a == *delim) {
	    *c++ = '\0';
	    sptr[ndots++] = c;
	}
	else
	      *c++ = *a;
    }
    *c++ = '\0';
    if(ndots < 2)
      return(-1);

    /* code to deal with a year > 99 */
    b = str;
    c = b + strlen(b) -10;	/* pointing at the month */
    if((ii = sscanf(c, "%2d%2d%2d%2d%2d"
		    , &mon, &dd, &hh, &mm, &ss)) < 5) {
      return(-1);
    }
    *c = '\0';
    if((ii = sscanf(b, "%d", &yy)) != 1) {
      return(-1);
    }
    /*
     * version number
     */
    if(!strlen(sptr[1])) {
      return(-1);
    }
    v = atoi(sptr[1]);
    ms = v % 1000;
    ddfn->milliseconds = ms;
    ddfn->version = v/1000;

    dts.year = yy > 1900 ? yy : yy+1900;
    dts.month = mon;
    dts.day = dd;
    dts.day_seconds = D_SECS(hh, mm, ss, ms);
    d = d_time_stamp(&dts);
    ddfn->time_stamp = d;

    /*
     * instrument name
     */
    if(!strlen(sptr[0])) {
      return(-1);
    }
    strcpy(ddfn->radar_name, sptr[0]);

    if(ndots == 3) {		/* tacked on comments */
	c = ddfn->comment;
				/* copy only the first 80 characters
				 * of the comment */
	for(ii=0; *a && ii < 80; ii++, *c++ = *a++); *c++ = '\0';
    }
    else {
	*ddfn->comment = '\0';
    }
    return(ndots);
}
/* c------------------------------------------------------------------------ */

char *
ddfn_file_info_line(ddfn, str)
  struct dd_file_name_v3 *ddfn;
  char *str;
{
    /* c...mark */
    int ii, nn, mark;
    char *a, *b, *c;
    DD_TIME dts;

    dts.time_stamp = ddfn_time(ddfn);

    sprintf(str, " %s %s"
	    , dts_print(d_unstamp_time(&dts))
	    , ddfn->radar_name
	    );
    if(nn=strlen(ddfn->comment)) {
	b = ddfn->comment;
	if(nn > 24) nn = 24;
	strcat(str, " ");
	for(c=str+strlen(str); nn--; *c++ = *b++); *c++ = '\0';
    }
    return(str);
}
/* c------------------------------------------------------------------------ */

void ddfn_file_name(ddfn, name)
  struct dd_file_name_v3 *ddfn;
  char *name;
{
    int v;

    v = ddfn->version*1000 +ddfn->milliseconds;
    dd_file_name("swp", (long)ddfn->time_stamp
		 , ddfn->radar_name, v, name);
    /* be sure and tack on the comments
     */
    if(*ddfn->comment){
	strcat(name, ".");
	strcat(name, ddfn->comment);
    }
    return;
}
/* c------------------------------------------------------------------------ */

void ddfn_list_versions(this)
  struct dd_file_name_v3 *this;
{
    if(!this)
	  return;
    ddfn_list_versions(this->ver_left);
    *ddfnpl++ = this;
    num_ddfnp++;
    ddfn_list_versions(this->ver_right);
}
/* c------------------------------------------------------------------------ */

struct dd_file_name_v3 *
ddfn_pop_spair()
{
    struct dd_file_name_v3 *ddfns=ddfn_spairs;

    if(!ddfns) {
	if(!(ddfns = (struct dd_file_name_v3 *)
	      malloc(sizeof(struct dd_file_name_v3)))) {
	    printf("Unable to malloc more disk file name structs\n");
	    exit(1);
	}
	count_spairs++;
    }
    else {
	ddfn_spairs = ddfns->next;
    }
    memset(ddfns, 0, sizeof(struct dd_file_name_v3));
    return(ddfns);
}
/* c------------------------------------------------------------------------ */

void ddfn_ptr_list(this, list_type)
  struct dd_file_name_v3 *this;
  int list_type;  
{
    if(!this)
	  return;

    ddfn_ptr_list(this->left, list_type);

    if(list_type == EXHAUSTIVE_LIST) {
	ddfn_list_versions(this);
    }
    else {
	*ddfnpl++ = this;
	num_ddfnp++;
    }
    ddfn_ptr_list(this->right, list_type);
}
/* c------------------------------------------------------------------------ */

void ddfn_push_spair(ddfns)
struct dd_file_name_v3 *ddfns;
{
    ddfns->next = ddfn_spairs;
    ddfn_spairs = ddfns;
    return;
}
/* c------------------------------------------------------------------------ */

struct dd_file_name_v3 *
ddfn_search(dir_num, radar_num, d_target_time, req_type, version)
  double d_target_time;
  int dir_num, radar_num, req_type, version;
{
    int ii, mark, equal=NO;
				/*
    long target_time = d_target_time;
				 */
    char *deep6=NULL;
    struct dd_file_name_v3 *this, *last, *gt_node=NULL, *lt_node=NULL;
    struct ddir_info_v3 *ddir, *return_ddir();
    struct dd_radar_name_info_v3 *rni;

    if(!(ddir = return_ddir(dir_num)))
	  return(NULL);
    rni = ddir->rni[radar_num];

    if(req_type == TIME_AFTER && req_type == rni->prev_req_type) {
	this = rni->prev_ddfn;
    }
    else {
	this = rni->h_node->right;
    }
    this = rni->h_node->right;
    rni->prev_req_type = req_type;

    if(req_type == TIME_AFTER) {

	for(;this;) {
	    if(d_target_time >= this->time_stamp) {
		this = this->right;
	    }
	    else {		/* target time is LT this time */
		gt_node = this;
		this = this->left;
	    }
	}
	if(gt_node) {
	    this = gt_node;
	}
	else {			/* no time after target time */
	    rni->prev_req_type = TIME_NEAREST;
	    rni->prev_ddfn = NULL;
	    return(NULL);
	}
    }
    else if(req_type == TIME_NEAREST) {

	for(;this;) {
	    if(d_target_time > this->time_stamp) {
		lt_node = this;
		this = this->right;
	    }
	    else if(this->time_stamp == d_target_time) {
		equal = YES;
		break;
	    }
	    else {		/* target time LT this time */
		gt_node = this;
		this = this->left;
	    }
	}
	if(!equal) {
	    if(!lt_node) {	/* target time is LT the first time we have */
		this = gt_node;
	    }
	    else if(!gt_node) {	/* target time is GT the last time we have */
		this = lt_node;
	    }
	    else {
		this = (d_target_time -lt_node->time_stamp) <
		      (gt_node->time_stamp -d_target_time) ? lt_node : gt_node;
	    }
	}
    }
    else {			/* TIME_BEFORE */
	/* the goal is to return the node that is to the left of
	 * the node that is GE the target time
	 */
	for(;this;) {
	    if(d_target_time <= this->time_stamp) {
		this = this->left;
	    }
	    else {		/* target time GT this time */
		lt_node = this;
		this = this->right;
	    }
	}
	if(lt_node) {
	    this = lt_node;
	}
	else {			/* no time before target time */
	    rni->prev_req_type = TIME_NEAREST;
	    rni->prev_ddfn = NULL;
	    return(NULL);
	}
    }
    if(!this) {			/* something's wrong */
	mark = *deep6;
    }
    /* now try to match the version requested
     */ 
    rni->prev_ddfn = this;
    for(; this;) {
	last = this;
	if(this->version == version) {
	    return(this);
	}
	if(version < (int)this->version) {
	    this = this->ver_left;
	}
	else {
	    this = this->ver_right;
	}
    }
    return(last);
}
/* c------------------------------------------------------------------------ */

void ddfn_sort_insert(rni, ddfn)
  struct dd_radar_name_info_v3 *rni;
  struct dd_file_name_v3 *ddfn;
{
    int ii, mark, mode;
    struct dd_file_name_v3 *this=rni->h_node, *last;


    for(;this;) {
	last = this;
	if(ddfn->time_stamp > this->time_stamp) {
	    mode = GT;
	    this = this->right;
	}
	else if(ddfn->time_stamp < this->time_stamp) {
	    this = this->left;
	    mode = LT;
	}
	else {
	    mode = EQ;
	    break;
	}
    }
    if(mode == GT) {
	last->right = ddfn;
    }
    else if(mode == LT) {
	last->left = ddfn;
    }
    else {			/* EQ implies more than one version */
	for(this=last; this;) {
	    last = this;
	    if(ddfn->version < this->version) {
		mode = LT;
		this = this->ver_left;
	    }
	    else{
		mode = GT;
		this = this->ver_right;
	    }
	}
	if(mode == GT) {
	    last->ver_right = ddfn;
	}
	else {
	    last->ver_left = ddfn;
	}
    }
    ddfn->parent = last;
}
/* c------------------------------------------------------------------------ */

double
ddfn_time(ddfn)
  struct dd_file_name_v3 *ddfn;
{
    /* returns a double precision time stamp from the file name
    double d = (double)ddfn->time_stamp +.001 * ddfn->milliseconds;
     */
    double d =  ddfn->time_stamp;
    return(d);
}
/* c------------------------------------------------------------------------ */

int ddfnp_list(dir_num, radar_num, list_type)
  int dir_num, radar_num, list_type;
{
    /* routine to create a sequential list of pointers to all
     * the file structs for this radar
     */
    int ii, mark;
    struct dd_file_name_v3 *this;
    struct ddir_info_v3 *ddir, *return_ddir();
    struct dd_radar_name_info_v3 *rni;

    if(!(ddir = return_ddir(dir_num)))
	  return(-1);
    rni = ddir->rni[radar_num];
    this = rni->h_node->right;
    /*
     */
    if(!rni->first_ddfnp) {
	rni->first_ddfnp = (struct dd_file_name_v3 **)
	      malloc(rni->num_sweeps
		     * sizeof(struct dd_file_name_v3 *));
	if(!rni->first_ddfnp) {
	    printf("Unable to malloc list of sweeps\n");
	    exit(1);
	}
	rni->num_ddfnp = rni->num_sweeps;
    }
    else if(rni->num_sweeps >
	    rni->num_ddfnp) {
	rni->first_ddfnp = (struct dd_file_name_v3 **)
	      realloc(rni->first_ddfnp
		      , rni->num_sweeps
		      * sizeof(struct dd_file_name_v3 *));
	rni->num_ddfnp = rni->num_sweeps;
    }
    ddfnpl = rni->first_ddfnp;
    num_ddfnp = 0;

    ddfn_ptr_list(this, list_type);
    return(0);
}
/* c------------------------------------------------------------------------ */

int ddir_files_v3(dir_num, dir)
  char *dir;
  int dir_num;
{
    /* The purpose of this routine is to provide info
     * on dorade files in the directory
     * c...mark
     */
    DIR *dir_ptr;
    struct dirent *dp;
    struct dd_file_name_v3 *ddfn, *ddfn_pop_spair();
    static int counth=0, count=0;
    int ii, rn, mark, file_count=0;
    struct ddir_info_v3 *ddir, *return_ddir();
    struct dd_radar_name_info_v3 *rni;

    counth++;
    if(!(ddir = return_ddir(dir_num)))
	  return(0);

    if(ddir->num_hits && !ddir->rescan_urgent) {
	ddir->num_hits++;
	if(ddir->num_hits < ddir->auto_rescan_hit_max) {
	    return(1);
	}
    }

    if(!(dir_ptr = opendir( dir ))) {
	printf( "Cannot open directory %s\n", dir );
	return(-1);
    }
    strcpy(ddir->directory, dir);
    ddir->num_hits = 1;
    ddir->rescan_urgent = NO;

    /*
     * put all the ddfns for this directory back on the spairs queue
     */
    for(rn=0; rn < ddir->num_radars; rn++) {
	ddir->rni[rn]->top_ddfn->last->next = ddfn_spairs;
	ddfn_spairs = ddir->rni[rn]->top_ddfn;
    }
    ddir->num_radars = 0;
    /*
     * loop through the files in this directory
     */
    for(;;) {
	dp=readdir(dir_ptr);
	if(dp == NULL ) {
	    break;
	}

	if(strncmp(dp->d_name, "swp.", 4) == 0) { /* only want sweep files */
	    if(strstr(dp->d_name, ".tmp")) { /* not a complete file */
		continue;
	    }
	    count++;
	    file_count++;
	    ddfn = ddfn_pop_spair();
    	    if(strstr(dp->d_name, "ARMAR")) {
		mark = 0;
	    }
	    if(craack_ddfn( dp->d_name, ddfn ) < 1) /* not a valid name */
	      continue;
	    /*
	     * find out which radar
	     */
	    for(rn=0; rn < ddir->num_radars; rn++) {
		if(strcmp(ddir->radar_name[rn], ddfn->radar_name) == 0)
		      break;
	    }
	    if(rn == ddir->num_radars) { /* new radar */
		ddir->num_radars++;
		if(!ddir->rni[rn]) {
		    /* really new */
		    rni = ddir->rni[rn] = (struct dd_radar_name_info_v3 *)
			  malloc(sizeof(struct dd_radar_name_info_v3));
		    memset(rni, 0, sizeof(struct dd_radar_name_info_v3));
		    /*
		     * see Sedgewick's "Algorithms" for explantions
		     * of h and z nodes
		     */
		    rni->z_node = ddfn_pop_spair();
		    rni->z_node->time_stamp = -1;
		    rni->z_node->left = rni->z_node->right = rni->z_node;

		    rni->h_node = ddfn_pop_spair();
		    rni->h_node->time_stamp = 0;
		    rni->h_node->left = rni->z_node;
		}
		else {
		    rni = ddir->rni[rn];
		}
		ddir->radar_name[rn] = ddfn->radar_name;
		rni->top_ddfn = rni->h_node->right = NULL;
		rni->num_sweeps = 0;
		rni->prev_req_type = TIME_NEAREST;
	    }
	    else {
		rni = ddir->rni[rn];
	    }
	    rni->num_sweeps++;
	    if(!rni->top_ddfn) {
		rni->top_ddfn = ddfn;
	    }
	    else {
		rni->top_ddfn->last->next = ddfn;
# ifdef unnecessary
		ddfn->last = rni->top_ddfn->last;
# endif
	    }
	    rni->top_ddfn->last = ddfn;
	    /*
	     * send off to insert/sort
	     */
	    ddfn_sort_insert(rni, ddfn);
	}
    }
    closedir(dir_ptr);
    return(file_count);
}
/* c------------------------------------------------------------------------ */

int ddir_radar_num_v3(dir_num, radar_name)
  int dir_num;
  char *radar_name;
{
    int ii, rn;
    struct ddir_info_v3 *ddir, *return_ddir();

    if(!(ddir = return_ddir(dir_num)))
	  return(-1);

    for(rn=0; rn < ddir->num_radars; rn++) {
	if(strstr(ddir->radar_name[rn], radar_name)) {
	    return(rn);
	}
    }
    return(-1);
}
/* c------------------------------------------------------------------------ */

void ddir_rescan_urgent(dir_num)
  int dir_num;
{
    struct ddir_info_v3 *ddir, *return_ddir();
    if(!(ddir = return_ddir(dir_num)))
	  return;
    ddir->rescan_urgent = YES;
}
/* c------------------------------------------------------------------------ */

double
d_mddir_file_info_v3(dir_num, radar_num, target_time, req_type, version
		, true_version_num, info_line, file_name)
  double target_time;
  int dir_num, radar_num, req_type, version, *true_version_num;
  char *info_line, *file_name;
{
    int v;
    struct dd_file_name_v3 *ddfn, *ddfn_search();
    char *a, *ddfn_file_info_line();
    double dtime;

    if(version == LATEST_VERSION)
	  v = 99999;
    else if(version == EARLIEST_VERSION)
	  v = -1;
    else {
	v = version / 1000;
    }
    ddfn = ddfn_search(dir_num, radar_num, target_time, req_type, v);

    if(ddfn) {
	a = ddfn_file_info_line(ddfn, info_line);
	ddfn_file_name(ddfn, file_name);
    }
    else {
	*info_line = '\0';
	return(target_time);
    }
    *true_version_num = ddfn->version;
    dtime = ddfn_time(ddfn);
    return(dtime);
}
/* c------------------------------------------------------------------------ */

struct dd_file_name_v3 **
mddir_entire_list_v3(dir_num, radar_num, num_swps)
  int dir_num, radar_num, *num_swps;
{
    struct ddir_info_v3 *ddir, *return_ddir();

    if(!(ddir = return_ddir(dir_num)) || radar_num >= ddir->num_radars ||
       radar_num < 0) {
	*num_swps = 0;
	return(NULL);
    }
    ddfnp_list(dir_num, radar_num, (int)EXHAUSTIVE_LIST);
    *num_swps = num_ddfnp;

    return(ddir->rni[radar_num]->first_ddfnp);
}
/* c------------------------------------------------------------------------ */

long
mddir_file_info_v3(dir_num, radar_num, target_time, req_type, version
		, true_version_num, info_line, file_name)
  long target_time;
  int dir_num, radar_num, req_type, version, *true_version_num;
  char *info_line, *file_name;
{
    double d_mddir_file_info_v3();

    return((long)d_mddir_file_info_v3
	   (dir_num, radar_num, (double)target_time, req_type, version
	    , true_version_num, info_line, file_name));
}
/* c------------------------------------------------------------------------ */

int mddir_file_list_v3(dir_num, dir)
  int dir_num;
  char *dir;
{
    int nn = ddir_files_v3(dir_num, dir);
    return(nn);
}
/* c------------------------------------------------------------------------ */

int mddir_gen_swp_list_v3(dir_num, radar_num, lm)
  int dir_num, radar_num;
  struct solo_list_mgmt *lm;
{
    /* just get the info list
     */
    return(mddir_gen_swp_str_list_v3(dir_num, radar_num, NO, lm));
}
/* c------------------------------------------------------------------------ */

int mddir_gen_swp_str_list_v3(dir_num, radar_num, full_file_name, lm)
  int dir_num, radar_num, full_file_name;
  struct solo_list_mgmt *lm;
{
    /* generate a list of file names or lines of file info
     */
    int ii, mm, nn, mark, num_sweeps;
    char str[256], *aa, *ddfn_file_info_line();
    DD_TIME dts;
    struct dd_file_name_v3 *ddfn, **ddfnp, **mddir_entire_list_v3();
    struct ddir_info_v3 *ddir, *return_ddir();
    struct dd_radar_name_info_v3 *rni;

    if(!(ddir = return_ddir(dir_num)))
	  return(0);
    rni = ddir->rni[radar_num];
    ddfnp = mddir_entire_list_v3(dir_num, radar_num, &num_sweeps);
    mm = num_sweeps > 0 ? num_sweeps : 0;

    lm->num_entries = 0;

    for(; mm--;) {
	ddfn = *ddfnp++;
	if(full_file_name) {
	    ddfn_file_name(ddfn, str);
	}
	else {
	    ddfn_file_info_line(ddfn, str);
	}
	solo_add_list_entry(lm, str, strlen(str));
    }
    return(num_sweeps);
}
/* c------------------------------------------------------------------------ */

int mddir_num_radars_v3(dir_num)
  int dir_num;
{
    struct ddir_info_v3 *ddir, *return_ddir();

    if(!(ddir = return_ddir(dir_num)))
	  return(0);
    return(ddir->num_radars);
}
/* c------------------------------------------------------------------------ */

char *
mddir_radar_name_v3(dir_num, radar_num)
  int dir_num,  radar_num;
{
    struct ddir_info_v3 *ddir, *return_ddir();

    if(!(ddir = return_ddir(dir_num)))
	  return(NULL);

    if( radar_num < 0 || radar_num >= ddir->num_radars ) {
	return(NULL);
    }
    else
	  return(ddir->radar_name[radar_num]);
}
/* c------------------------------------------------------------------------ */

int mddir_radar_num_v3(dir_num, radar_name)
  int dir_num;
  char *radar_name;
{
    int ii, rn;
    struct ddir_info_v3 *ddir, *return_ddir();

    if(!(ddir = return_ddir(dir_num)))
	  return(-1);

    for(rn=0; rn < ddir->num_radars; rn++) {
	if(strstr(ddir->radar_name[rn], radar_name)) {
	    return(rn);
	}
    }
    return(-1);
}
/* c------------------------------------------------------------------------ */

struct dd_file_name_v3 **
mddir_return_swp_list_v3(dir_num, radar_num, num_swps)
  int dir_num, radar_num, *num_swps;
{
    struct ddir_info_v3 *ddir, *return_ddir();

    if(!(ddir = return_ddir(dir_num)) || radar_num >= ddir->num_radars ||
       radar_num < 0) {
	*num_swps = 0;
	return(NULL);
    }
    ddfnp_list(dir_num, radar_num, 0);
    *num_swps = num_ddfnp;

    return(ddir->rni[radar_num]->first_ddfnp);
}
/* c------------------------------------------------------------------------ */

double
ddfnp_list_entry(dir_num, radar_num, ent_num, version, line, file_name)
  int dir_num, radar_num, ent_num, *version;
  char *line, *file_name;
{
    /* return the time stamp and the name from the sweep list
     */
    int ii, nn, mark;
    struct dd_file_name_v3 *ddfn, **ddfnp, **mddir_entire_list_v3();
    struct ddir_info_v3 *ddir, *return_ddir();
    struct dd_radar_name_info_v3 *rni;
    char *a, *ddfn_file_info_line();
    double dtime;

    *file_name = '\0';

    if(!(ddir = return_ddir(dir_num)))
	  return(0);
    rni = ddir->rni[radar_num];
    if(ent_num < 0 || ent_num >= rni->num_sweeps || radar_num < 0 ||
       radar_num >= ddir->num_radars)
	  return(0);
    ddfnp = mddir_entire_list_v3(dir_num, radar_num, &nn);
    ddfn = *(rni->first_ddfnp +ent_num);
    *version = ddfn->version * 1000 + ddfn->milliseconds;
    a = ddfn_file_info_line(ddfn, line);
    ddfn_file_name(ddfn, file_name);
    dtime = ddfn_time(ddfn);
    return(dtime);
}
/* c------------------------------------------------------------------------ */

struct ddir_info_v3 *
return_ddir(dir_num)
  int dir_num;
{
    int ii;
    struct ddir_info_v3 *this;
    char *aa;

    if(!ddir_num_dirs_v3) {
	ascii_file_type[SWP_FILE] = "swp";
	ascii_file_type[VOL_FILE] = "vol";
	ascii_file_type[BND_FILE] = "bnd";
	ascii_file_type[PPT_FILE] = "ppt";
	ascii_file_type[PIC_FILE] = "pic";
	ascii_file_type[GDE_FILE] = "gde";

	for(ii=0; ii < MAX_DIRECTORIES; ii++ ) {
	    ddir_list_v3[ii] = NULL;
	}
    }
    if(dir_num < 0 || dir_num >= MAX_DIRECTORIES)
	  return(NULL);

    if(!ddir_list_v3[dir_num]) {
	ddir_num_dirs_v3++;

	ddir_list_v3[dir_num] = this = (struct ddir_info_v3 *)
	      malloc(sizeof(struct ddir_info_v3));
	memset((char *)this, 0, sizeof(struct ddir_info_v3));
	this->dir_num = dir_num;
	this->auto_rescan_hit_max = 5;
	if((aa = getenv("SOLO_AUTO_RESCAN"))) {
	  this->auto_rescan_hit_max = (ii = atoi(aa)) > 0 ? ii : 1;
	}
	return(this);
    }
    
    return(ddir_list_v3[dir_num]);
}
/* c------------------------------------------------------------------------ */
# ifdef obsolete

double ddir_free( dir )
  char *dir;
{

#ifdef SVR4
    struct statvfs fs;
#else
    struct statfs fs;
#endif
    int i;
    double d;
   
#ifdef SVR4 
    i = statvfs( dir, &fs );
#else
    i = statfs (dir, &fs);
#endif

    /* return remaining capacity in megabytes */
#ifdef IRIX4
    d = (double)fs.f_bsize*(double)fs.f_bfree*1.e-6;
#else
    d = (double)fs.f_bsize*(double)fs.f_bavail*1.e-6;
#endif

    return(d);
}
# endif
/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

