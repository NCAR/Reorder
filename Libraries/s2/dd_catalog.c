/* 	$Id: dd_catalog.c 244 2006-04-13 15:47:55Z dennisf $	 */
#ifndef lint
static char vcid[] = "$Id: dd_catalog.c 244 2006-04-13 15:47:55Z dennisf $";
#endif /* lint */

# include <dorade_headers.h>
# include "dd_stats.h"
# include <time.h>
# include <sys/time.h>
# include <function_decl.h>
# include <dgi_func_decl.h>
# include "piraq.h"

# define READ_AND_WRITE_ACCESS 2
# define CURRENT_POSTION_LSEEK 1
# define    BOF_RELATIVE_LSEEK 0
# define    EOF_RELATIVE_LSEEK 2
# define    DDCAT_NORMAL_WRITE 0x0001
# define     DDCAT_NUVOL_WRITE 0x0002
# define   DDCAT_NUVOL_REWRITE 0x0004
# define           DDCAT_FLUSH 0x0008

# define MAX_ROTANG_DELTA 4.
# define MAX_RANGE_SEGMENTS 64
# define MAX_TEXT_SIZE 4*BLOCK_SIZE
# define MAX_SWEEP_SPACE 8192
# define MAX_ARG_COUNT 16
# define MAX_ARG_SIZE 80
# define CAT_FLUSH_POINT BLOCK_SIZE
# define FLUSH_SWEEP_COUNT 5
# define FLUSH YES

# define VOLUME "VOLUME"
# define SENSOR "SENSOR"
# define PARAMETER "PARAMETER"
# define SCAN "SCAN"
# define RANGE_INFO "RANGE_INFO"

char *Unix_months[] = { "JAN", "FEB", "MAR", "APR", "MAY", "JUN",
			     "JUL", "AUG", "SEP", "OCT", "NOV", "DEC" };

char *Stypes[] = { "CAL", "PPI", "COP", "RHI", "VER",
			      "TAR", "MAN", "IDL", "SUR", "AIR", "???" };
/* for each possible radar
 * you need to check for a new volume
 * or a new sweep and keep interesting information
 * from the beginning and end of each sweep
 *
 */
struct catalog_info {
    double d_cos_heading;
    double d_sin_heading;
    double d_cos_drift;
    double d_sin_drift;
    double d_cos_pitch;
    double d_sin_pitch;
    double d_cos_roll;
    double d_sin_roll;
    double prev_MB_count;
    float f_last_rotation_angle;
    float f_delta_angles;
    float f_swp_stop_angle;
    float f_stop_latitude;
    float f_stop_longitude;
    float f_stop_altitude;
    float f_stop_EW_wind;
    float f_stop_NS_wind;
    float f_ew_velocity;
    float f_ns_velocity;
    float f_fixed_angle;
    time_t stop_time;
    time_t time_stamp;
    time_t trip_time;

    long lseek_vol_offset;
    int avg_rota_count;
    int vol_rec_mark;
    int prev_rec_count;
    int num_rays;
    int rays_per_volume;
    int num_good_cells;
    int tot_cells;
    int volumes_fid;
    int radar_type;
    int radar_num;
    int vol_num;
    int sweep_num;
    int flush_sweep_count;
    int vol_txt_size;
    int swp_txt_size;
    char *vol_stop_time;
    char *vol_txt_ptr;
    char *swp_txt_ptr;
    char volumes_file_name[256];
};

struct comment_mgmt {
    char *buf;
    char *at;
    int sizeof_buf;
    int sizeof_comments;
};

static struct comment_mgmt *cmt=NULL;
static int time_interval=0;		/* every n seconds */
static int flush_sweep_count=FLUSH_SWEEP_COUNT;
static int Catalog_flag=YES;
static struct dd_stats *dd_stats=NULL;

static int Cat_radar_count=0;
static struct catalog_info *ci[MAX_SENSORS];
static char *source_tape_id=NULL;

# ifdef obsolete
double dd_rotation_angle();
double dd_tilt_angle();
double dd_elevation_angle();
double dd_azimuth_angle();
double dd_drift();
double dd_heading();
double dd_pitch();
double dd_roll();
double dd_altitude();
double dd_altitude_agl();
double dd_latitude();
double dd_longitude();
# endif

void cat_volume();
void cat_sweep();
char *cat_time();
char *cat_date();
void cat_cat_att();
void cat_cat_att_args();
char *reverse_string();
char *c_deblank();
void ddcat_close();
int ddcat_open();
void ddcat_write();


/* c------------------------------------------------------------------------ */

void
dd_append_cat_comment(comm_str)
  char *comm_str;
{
    /* it is assumed each call represents a line of text in the catalog
     */
    int nc=strlen(comm_str);

    if(!nc || !cmt)
	  return;

    if(cmt->sizeof_comments +nc +4 >= cmt->sizeof_buf) {
	cmt->sizeof_buf = cmt->sizeof_comments +nc +4;
	cmt->buf = (char *)realloc(cmt->buf, cmt->sizeof_buf);
	cmt->at = cmt->buf +cmt->sizeof_comments;
    }
    strcat(cmt->at, "! ");
    strcat(cmt->at, comm_str);
    strcat(cmt->at, "\n");
    cmt->sizeof_comments += strlen(cmt->at);
    cmt->at += strlen(cmt->at);
}
/* c------------------------------------------------------------------------ */

void dd_enable_cat_comments()
{
    if(cmt)
	  return;

    cmt = (struct comment_mgmt *)malloc(sizeof(struct comment_mgmt));
    memset(cmt, 0, sizeof(struct comment_mgmt));
    cmt->sizeof_buf = 512;
    cmt->buf = cmt->at = (char *)malloc(cmt->sizeof_buf);
    memset(cmt->at, 0, cmt->sizeof_buf);
    
}
/* c------------------------------------------------------------------------ */

void dd_catalog(dgi, ignored_time, flush)
  struct dd_general_info *dgi;
  time_t ignored_time;
  int flush;
{
    int rn=dgi->radar_num;
    static int count=0;
    int i, j, new_vol, new_swp, mark;
    double d, fabs(), sin(), cos(), angdiff();
    char *a, *b, *strstr();
    char *dd_radar_name(), *get_tagged_string();
    float rotang;

    time_t current_time=dgi->time;
    struct dds_structs *dds=dgi->dds;
    struct radar_d *radd=dds->radd;
    struct sweepinfo_d *swib=dds->swib;
    struct platform_i *asib=dds->asib;
    struct catalog_info *cii;
    struct dd_stats *dd_return_stats_ptr();


    count++;

    if(!Cat_radar_count) {
	for(i=0; i < MAX_SENSORS; i++) {
	    ci[i] = NULL;
	}
# ifdef obsolete
	cmt = (struct comment_mgmt *)malloc(sizeof(struct comment_mgmt));
	memset(cmt, 0, sizeof(struct comment_mgmt));
	cmt->sizeof_buf = 256;
	cmt->buf = cmt->at = (char *)malloc(cmt->sizeof_buf);
	memset(cmt->at, 0, cmt->sizeof_buf);
# else
	dd_enable_cat_comments();
# endif
	dd_stats = dd_return_stats_ptr();

	if(a = get_tagged_string( "OUTPUT_FLAGS" )) {
# ifdef obsolete
	    if(b=strstr(a, "CATALOG")) {
		if(a == b)	/* first item */
		      Catalog_flag = YES;
		else if(*(b-1) == ' ') /* not preceeded by "NO_" */
		      Catalog_flag = YES;
	    }
# else
	    if(b=strstr(a, "NO_CATALOG")) {
		Catalog_flag = NO;
	    }
# endif
	}
	if(a=get_tagged_string("CATALOG_SWEEP_SUMMARY")) {
	    if((j = atoi(a)) >= 0 )
		  time_interval = j;
	}
	if(a=get_tagged_string("CATALOG_FLUSH_COUNT")) {
	    if((j = atoi(a)) > 0 )
		  flush_sweep_count = j;
	}
	if(a=get_tagged_string("SOURCE_TAPE_ID")) {
	    source_tape_id = a;
	}
    }

    /* get structure for this radar
     */
    if(ci[rn] == NULL) {
	/* create space for and initialize info for this radar
	 */
	cii = ci[rn] =
	      (struct catalog_info *)malloc(sizeof(struct catalog_info));
	memset((char *)cii, 0, sizeof(struct catalog_info));
	cii->volumes_fid = -1;
	Cat_radar_count++;
	cii->vol_txt_ptr = (char *)malloc(BLOCK_SIZE/2);
	cii->swp_txt_ptr = (char *)malloc(BLOCK_SIZE);
    }
    else {
	cii = ci[rn];
    }

    if( flush == YES ) {

	if(cii->vol_txt_size <= 0)
	      return;
	cat_volume(dgi, cii, cii->stop_time, flush);
	cii->prev_MB_count = dd_stats->MB;
	cat_sweep(dgi, cii, cii->stop_time, flush);
	ddcat_close( cii->volumes_fid );
	cii->volumes_fid = -1;
	return;
    }

    if(cii->volumes_fid == -1) { /* initialization or reinitialization
				  */
	a = cii->vol_txt_ptr;
	b = cii->swp_txt_ptr;
	memset((char *)cii, 0, sizeof(struct catalog_info));
	cii->vol_txt_ptr = a;
	cii->swp_txt_ptr = b;
	memset(cii->vol_txt_ptr, 0, BLOCK_SIZE/2);
	memset(cii->swp_txt_ptr, 0, BLOCK_SIZE);
	cii->vol_num = -1;
	cii->radar_num = dgi->radar_num;
	strcpy(cii->volumes_file_name, dgi->directory_name);
	dd_file_name("cat", current_time, dd_radar_name(dds), 0
		     , cii->volumes_file_name
		     +strlen(cii->volumes_file_name));
	cii->volumes_fid = ddcat_open( cii->volumes_file_name );
    }

    /*
     * process this ray
     */

    new_vol = new_swp = dgi->vol_num != cii->vol_num;
    if(!new_vol && dgi->sweep_num != cii->sweep_num) {
	i = (   radd->radar_type == GROUND
	     || radd->radar_type == SHIP
	     || radd->radar_type == AIR_LF);
	j = current_time > cii->trip_time;
	new_swp = i || j ? YES : NO;
    }
    if( new_vol && cii->vol_txt_size > 0 ) {
	cat_volume(dgi, cii, cii->stop_time, FLUSH);
    }
    if( new_swp && cii->swp_txt_size > 0 ) {
	cat_sweep(dgi, cii, cii->stop_time, FLUSH);
    }
    if(new_vol) {
	/* Initialize for a new volume
	 */
	cii->vol_num = dgi->vol_num;
	cat_volume(dgi, cii, current_time, NO);
	cii->trip_time = current_time +time_interval;
    }

    if(new_swp) {
	/* Initialize for a new sweep
	 * but don't do it for every sweep if aircraft data
	 */
	cii->sweep_num = dgi->sweep_num;
	cii->num_rays = 0;
	cat_sweep(dgi, cii, current_time, NO);

	if(current_time > cii->trip_time) {
	    cii->trip_time = current_time +time_interval;
	}
    }

    cii->stop_time = current_time;
    cii->num_rays++;
    cii->rays_per_volume++;
    cii->f_fixed_angle = swib->fixed_angle;
    cii->f_swp_stop_angle = swib->stop_angle;
    cii->f_stop_latitude = dd_latitude(dgi);
    cii->f_stop_longitude = dd_longitude(dgi);
    cii->f_stop_altitude = dd_altitude(dgi);
    cii->f_stop_EW_wind = asib->ew_horiz_wind;
    cii->f_stop_NS_wind = asib->ns_horiz_wind;
    cii->f_ew_velocity += asib->ew_velocity;
    cii->f_ns_velocity += asib->ns_velocity;
    d = dd_heading(dgi);
    cii->d_cos_heading += cos((double)RADIANS(90.-d));
    cii->d_sin_heading += sin((double)RADIANS(90.-d));
    d = dd_drift(dgi);
    cii->d_cos_drift += cos((double)RADIANS(d));
    cii->d_sin_drift += sin((double)RADIANS(d));
    d = dd_pitch(dgi);
    cii->d_cos_pitch += cos((double)RADIANS(d));
    cii->d_sin_pitch += sin((double)RADIANS(d));
    d = dd_roll(dgi);
    cii->d_cos_roll += cos((double)RADIANS(d));
    cii->d_sin_roll += sin((double)RADIANS(d));

    rotang = dgi->dds->radd->scan_mode == RHI ? dd_elevation_angle(dgi)
	: dd_rotation_angle(dgi);
    /* this is done so that upward (counter clockwise) rotation for RHIs
     * appears positive
     */
    d = angdiff(cii->f_last_rotation_angle, rotang );

    if(fabs(d) < MAX_ROTANG_DELTA) {
	cii->avg_rota_count++;
	cii->f_delta_angles += d;
    }
    cii->f_last_rotation_angle = rotang;
    cii->prev_MB_count = dd_stats->MB;
    cii->prev_rec_count = dd_stats->rec_count;
    cii->num_good_cells += dgi->num_good_cells;
    dgi->num_good_cells = 0;
    cii->tot_cells += dgi->dds->celv->number_cells;
    return;
}
/* c------------------------------------------------------------------------ */

void cat_volume( dgi, cii, current_time, finish )
  struct dd_general_info *dgi;
  struct catalog_info *cii;
  time_t current_time;
  int finish;
{
    int i, n;
    double d, fabs();
    char arglist[MAX_ARG_COUNT][MAX_ARG_SIZE], *arg;
    char *arg_ptr[MAX_ARG_COUNT];
    char str[256], *arg1=arglist[0];
    float f, gs, gate_spacing[MAX_RANGE_SEGMENTS];
    int num_gates[MAX_RANGE_SEGMENTS];
    short date_time[6];
    time_t t, todays_date();

    struct dds_structs *dds=dgi->dds;
    struct volume_d *vold=dds->vold;
    struct radar_d *radd=dds->radd;
    struct cell_d *celv=dds->celv;
    char *c_deblank(), *reverse_string();
    char *cat_date(), *cat_time();
    char *cat=cii->vol_txt_ptr;
    HEADERV *dwel;

    
    if( finish == YES ) {
	str[0] = '\0';
	cat_cat_att( "Stop_date", 1, cat_date(cii->stop_time, arglist), str );
	cat_cat_att( "Stop_time", 1, cat_time(cii->stop_time, arglist), str );
	strncpy( cii->vol_stop_time, str, strlen(str));
	ddcat_write(cii, cii->vol_txt_ptr, cii->vol_txt_size
		     , DDCAT_NUVOL_REWRITE);
	cii->vol_txt_size = 0;
	return;
    }

    cii->vol_rec_mark = cii->prev_rec_count;

    *cii->vol_txt_ptr = 0;
    cat_cat_att( VOLUME, 0, arglist, cat );

    sprintf(arg1, "%d", vold->volume_num );
    cat_cat_att( "Number", 1, arglist, cat );

    if(source_tape_id) {
	cat_cat_att( "Source_tape_id", 1, source_tape_id, cat );
    }

    sprintf(arg1, "%.2f", (float)cii->prev_MB_count);
    cat_cat_att( "MB_to_Vol", 1, arglist, cat );

    sprintf(arg1, "%d", current_time );
    cat_cat_att( "Unix_time_stamp", 1, arglist, cat );

    cat_cat_att( "Start_date", 1, cat_date(current_time, arglist), cat );
    cat_cat_att( "Start_time", 1, cat_time(current_time, arglist), cat );

    cii->vol_stop_time = (cat += strlen(cat));
    cat_cat_att( "Stop_date", 1, cat_date(current_time, arglist), cat );
    cat_cat_att( "Stop_time", 1, cat_time(current_time, arglist), cat );

    t = todays_date(date_time);
    cat_cat_att( "Production_date", 1, cat_date(t, arglist), cat );
    cat_cat_att( "Production_time", 1, cat_time(t, arglist), cat );
    

    c_deblank(vold->proj_name, 20, arglist);
    cat_cat_att( "Project", 1, arglist, cat );

    arg = "/RTF/ATD/NCAR/UCAR/NSF";
    cat_cat_att( "Facility", 1, arg, cat );

    arg = "GMT";
    cat_cat_att( "Time_zone", 1, arg, cat );

    if( dgi->source_fmt == APIRAQ_FMT ) {
      dwel = (HEADERV *)dgi->gpptr2;
      sprintf(arg1, "%d", dwel->clutterfilter );
      cat_cat_att( "Piraq_clutter_filter", 1, arglist, cat );
    }

    c_deblank(vold->flight_num, 8, arglist);
    cat_cat_att( "Flight/IOP_number", 1, arglist, cat );

    /* Begin sensor specific attributes
     */
    cat_cat_att( SENSOR, 0, arglist, cat );

    c_deblank(dds->radd->radar_name, 8, arglist);
    cat_cat_att( "Name", 1, arglist, cat );

    cii->radar_type = radd->radar_type;

    switch (radd->radar_type) {
	
    case GROUND:
	arg = "Ground_based";
	break;
    case AIR_FORE:
	arg = "Airborne_forward";
	break;
    case AIR_AFT:
	arg = "Airborne_aft";
	break;
    case AIR_TAIL:
	arg = "Airborne_tail";
	break;
    case AIR_LF:
	arg = "Airborne_lower_fuselage";
	break;
    case SHIP:
	arg = "Shipboard";
	break;
    case AIR_NOSE:
	arg = "Airborne_nose";
	break;
    case SATELLITE:
	arg = "Satellite";
	break;
    default:
	arg = "Airborne_forward";
	break;
    }
    cat_cat_att( "Platform_configuration", 1, arg, cat );

    cat += strlen(cat);
    arg = Stypes[radd->scan_mode];
    cat_cat_att( "Scan_mode", 1, arg, cat );

    sprintf(arg1, "%.3e", radd->radar_const);
    cat_cat_att( "Radar_constant", 1, arglist, cat );

    sprintf(arg1, "%.3e", radd->peak_power);
    cat_cat_att( "Peak_power", 1, arglist, cat );

    sprintf(arg1, "%.3e", radd->noise_power);
    cat_cat_att( "Noise_power", 1, arglist, cat );

    sprintf(arg1, "%.3e", radd->receiver_gain);
    cat_cat_att( "Receiver_gain", 1, arglist, cat );

    sprintf(arg1, "%.3e", radd->antenna_gain);
    cat_cat_att( "Antenna_gain", 1, arglist, cat );

    sprintf(arg1, "%.3e", radd->system_gain);
    cat_cat_att( "System_gain", 1, arglist, cat );

    sprintf(arg1, "%.2f", radd->horz_beam_width);
    cat_cat_att( "Horz_beam_width_(deg)", 1, arglist, cat );

    sprintf(arg1, "%.2f", radd->vert_beam_width);
    cat_cat_att( "Vert_beam_width_(deg)", 1, arglist, cat );

    sprintf(arg1, "%.2f", radd->req_rotat_vel);
    cat_cat_att( "Rotational_velocity_(deg/sec)", 1, arglist, cat );

    cat += strlen(cat);

    switch (radd->data_compress) {
    case NO_COMPRESSION:
	arg = "No_compression";
	break;
    case HRD_COMPRESSION:
	arg = "Hrd_compression";
	break;
    default:
	arg = "Unknown";
	break;
    }
    cat_cat_att("Data_compression", 1, arg, cat );

    switch (radd->data_reduction) {
    case NO_DATA_REDUCTION:
	arg_ptr[0] = "None";
	n = 1;
	break;
    case TWO_ANGLES:
	arg_ptr[0] = "Between_angles";
	n = 3;
	break;
    case TWO_CIRCLES:
	arg_ptr[0] = "Between_concentric_circles";
	n = 3;
	break;
    case TWO_ALTITUDES:
	arg_ptr[0] = "Above/below altitudes";
	n = 3;
	break;
    default:
	arg_ptr[0] = "Unknown";
	n = 1;
	break;
    }
    if( n > 1 ) {
	sprintf( arglist[1], "%.1f", radd->data_red_parm0);
	sprintf( arglist[2], "%.1f", radd->data_red_parm1);
	arg_ptr[1] = arglist[1];
	arg_ptr[2] = arglist[2];
	cat_cat_att_args( "Data_reduction", n, arg_ptr, cat );
    }
    else {
	cat_cat_att("Data_reduction", 1, arg_ptr[0], cat );
    }

    /* Summarize the range/gate spacing info
     */

    cat_cat_att( RANGE_INFO, 0, arglist, cat );
    gate_spacing[0] = gs = celv->dist_cells[1] - celv->dist_cells[0];
    num_gates[0] = 2;

    /* itemize the range segments
     */
    for(i=1,n=0; i < celv->number_cells-1; i++ ) {
	d = celv->dist_cells[i+1] - celv->dist_cells[i];
	if( fabs(d-gs) > .9 ) {
	    gs = d;
	    n++;
	    if(n+1 >= MAX_RANGE_SEGMENTS) {
		break;
	    }
	    gate_spacing[n] = gs;
	    num_gates[n] = 0;
	}
	num_gates[n]++;
    }
    n++;
    sprintf(arg1, "%d", n );
    cat_cat_att( "Num_segments", 1, arglist, cat );

    sprintf(arg1, "%.1f", celv->dist_cells[0]
	    +dds->cfac->range_delay_corr);
    cat_cat_att( "Range_gate1_center", 1, arglist, cat );

    sprintf(arg1, "%.1f", celv->dist_cells[celv->number_cells-1]
	    +dds->cfac->range_delay_corr);
    cat_cat_att( "Max_range", 1, arglist, cat );

    arg_ptr[0] = arglist[0];
    arg_ptr[1] = arglist[1];
    
    for(i=0; i < n; i++ ) {
	sprintf( arglist[0], "%d", num_gates[i]);
	sprintf( arglist[1], "%.1f", gate_spacing[i]);
	cat_cat_att_args( "Segment_pair", 2, arg_ptr, cat );
    }
    cat_cat_att(reverse_string( RANGE_INFO, arglist), 0, arglist, cat );


    /* create a list of the parameters present in this volume
     */
    if(strlen(dgi->source_field_mnemonics)) {
	arg = dgi->source_field_mnemonics;
	cat_cat_att( "Raw_fields_present", 1, arg, cat );
    }
    sprintf(arg1, "%d", dgi->num_parms );
    cat_cat_att( "Param_count", 1, arglist, cat );

    for(i=0; i < dgi->num_parms; i++ ) {
	cat_cat_att( PARAMETER, 0, arglist, cat );

	c_deblank( dds->parm[i]->parameter_name, 8, arglist);
	cat_cat_att( "Name", 1, arglist, cat );

	c_deblank( dds->parm[i]->param_description, 39, arglist);
	cat_cat_att( "Description", 1, arglist, cat );

	c_deblank( dds->parm[i]->param_units, 8, arglist);
	cat_cat_att( "Units", 1, arglist, cat );

	sprintf(arg1, "%.3f", dds->parm[i]->recvr_bandwidth);
	cat_cat_att( "Receiver_bandwidth_(mhz)", 1, arglist, cat );

	switch (dds->parm[i]->polarization) {

	case HORIZONTAL:
	    arg = "Horizontal";
	    break;
	case VERTICAL:
	    arg = "Vertical";
	    break;
	case CIRCULAR_RIGHT:
	    arg = "Circular_right";
	    break;
	case ELLIPTICAL:
	    arg = "Ellipitical";
	    break;
	default:
	    arg = "Unknown";
	    break;
	}
	cat_cat_att( "Polarization", 1, arg, cat );

	sprintf(arg1, "%d", dds->parm[i]->num_samples );
	cat_cat_att( "Num_samples", 1, arglist, cat );

	switch (dds->parm[i]->binary_format) {

	case DD_8_BITS:
	    arg = "8_bit_integers";
	    break;
	case DD_16_BITS:
	    arg = "16_bit_integers";
	    break;
	case DD_24_BITS:
	    arg = "24_bit_integers";
	    break;
	case DD_32_BIT_FP:
	    arg = "32_bit_floating_point";
	    break;
	case DD_16_BIT_FP:
	    arg = "16_bit_floating_point";
	    break;
	default:
	    arg = "Unknown";
	    break;
	}
	cat_cat_att( "Binary_format", 1, arg, cat );

	sprintf(arg1, "%.3e", dds->parm[i]->parameter_scale);
	cat_cat_att( "Param_scale", 1, arglist, cat );

	sprintf(arg1, "%.3e", dds->parm[i]->parameter_bias);
	cat_cat_att( "Param_bias", 1, arglist, cat );

	cat_cat_att(reverse_string( PARAMETER, arglist), 0, arglist, cat );
	cat += strlen(cat);
    }

    /* List the frequencies and PRTs
     */
    for(i=0; i < radd->num_freq_trans; i ++ ) {
	switch (i) {

	case 0:
	    f = radd->freq1;
	    break;
	case 1:
	    f = radd->freq2;
	    break;
	case 2:
	    f = radd->freq3;
	    break;
	case 3:
	    f = radd->freq4;
	    break;
	case 4:
	    f = radd->freq5;
	    break;
	}
	sprintf( arglist[i], "%.3e", f );
	arg_ptr[i] = arglist[i];
    }
    cat_cat_att_args( "Frequencies", radd->num_freq_trans, arg_ptr, cat );

    for(i=0; i < radd->num_ipps_trans; i ++ ) {
	switch (i) {

	case 0:
	    f = radd->interpulse_per1;
	    break;
	case 1:
	    f = radd->interpulse_per2;
	    break;
	case 2:
	    f = radd->interpulse_per3;
	    break;
	case 3:
	    f = radd->interpulse_per4;
	    break;
	case 4:
	    f = radd->interpulse_per5;
	    break;
	}
    	sprintf( arglist[i], "%.3e", f );
	arg_ptr[i] = arglist[i];
    }
    cat_cat_att_args( "Interpulse_periods", radd->num_ipps_trans
		, arg_ptr, cat );

    cat += strlen(cat);
    sprintf(arg1, "%.4f", dd_longitude(dgi));
    cat_cat_att( "Radar_longitude", 1, arglist, cat );

    sprintf(arg1, "%.4f", dd_latitude(dgi));
    cat_cat_att( "Radar_latitude", 1, arglist, cat );

    sprintf(arg1, "%.4f", dd_altitude(dgi));
    cat_cat_att( "Radar_altitude", 1, arglist, cat );

    sprintf(arg1, "%.2f", radd->eff_unamb_vel );
    cat_cat_att( "Unambiguous_velocity_(m/s)", 1, arglist, cat );

    sprintf(arg1, "%.3f", radd->eff_unamb_range);
    cat_cat_att( "Unambiguous_range_(km)", 1, arglist, cat );


    cat_cat_att(reverse_string(SENSOR,arglist), 0, arglist, cat );

    /* End sensor specific attributes
     */

    cat_cat_att(reverse_string(VOLUME,arglist), 0, arglist, cat );
    cat += strlen(cat);

    cii->vol_txt_size = strlen(cii->vol_txt_ptr);
    ddcat_write(cii, cii->vol_txt_ptr, cii->vol_txt_size
		     , DDCAT_NUVOL_WRITE);
    cii->rays_per_volume = 0;
}
/* c------------------------------------------------------------------------ */

void cat_sweep( dgi, cii, current_time, finish )
  struct dd_general_info *dgi;
  struct catalog_info *cii;
  time_t current_time;
  int finish;
{
    int i, n, nc, llen, max_llen=77;
    float e, f;
    double d, atan2(), fmod(), sqrt();
    char arglist[MAX_ARG_COUNT][MAX_ARG_SIZE], *aa;
    char *arg1 = arglist[0];

    struct dds_structs *dds=dgi->dds;
    struct sweepinfo_d *swib=dds->swib;
    struct platform_i *asib=dds->asib;
    char *mark_char, *c_deblank(), *reverse_string();
    char *cat_date(), *cat_time();
    char *cat;


    if( finish == YES ) {
	mark_char = cat = cii->swp_txt_ptr +cii->swp_txt_size;

	sprintf(arg1, "%.1f", cii->f_fixed_angle );
	cat_cat_att( "Fixed_angle", 1, arglist, cat );

	cat_cat_att( "Stop_date", 1, cat_date(cii->stop_time, arglist), cat );
	cat_cat_att( "Stop_time", 1, cat_time(cii->stop_time, arglist), cat );

	sprintf(arg1, "%.1f", cii->f_swp_stop_angle );
	cat_cat_att( "Stop_angle", 1, arglist, cat );

	f = cii->avg_rota_count > 1
	      ? cii->f_delta_angles/(float)(cii->avg_rota_count-1) : 0;
	sprintf(arg1, "%.2f", f );
	cat_cat_att( "Average_delta_rotation", 1, arglist, cat );

	f = cii->tot_cells > 0 ? (float)cii->num_good_cells/cii->tot_cells : 0;
	sprintf(arg1, "%.2f", f*100.);
	cat_cat_att( "Percent_good_cells", 1, arglist, cat );

	sprintf(arg1, "%d", cii->rays_per_volume );
	cat_cat_att( "Num_rays", 1, arglist, cat );

	sprintf(arg1, "%.2f", (float)cii->prev_MB_count);
	cat_cat_att( "MB_sofar", 1, arglist, cat );

	sprintf(arg1, "%d", cii->prev_rec_count-cii->vol_rec_mark);
	cat_cat_att( "Recs_sofar", 1, arglist, cat );

	if( cii->radar_type != GROUND ) {
	    cat += strlen(cat);

	    sprintf(arg1, "%.3f", cii->f_stop_latitude );
	    cat_cat_att( "Stop_latitude", 1, arglist, cat );

	    sprintf(arg1, "%.3f", cii->f_stop_longitude );
	    cat_cat_att( "Stop_longitude", 1, arglist, cat );

	    sprintf(arg1, "%.3f", cii->f_stop_altitude);
	    cat_cat_att( "Stop_altitude_(km)", 1, arglist, cat );

	    sprintf(arg1, "%.1f", cii->f_stop_EW_wind );
	    cat_cat_att( "Stop_EW_wind_(m/s)", 1, arglist, cat );

	    sprintf(arg1, "%.1f", cii->f_stop_NS_wind );
	    cat_cat_att( "Stop_NS_wind_(m/s)", 1, arglist, cat );

	    if(cii->num_rays > 0) {
		
		e = cii->f_ns_velocity/(float)cii->num_rays;
		f = cii->f_ew_velocity/(float)cii->num_rays;
		sprintf(arg1, "%.0f", sqrt((double)(e*e+f*f)));
		cat_cat_att( "Average_ground_speed_(m/s)", 1, arglist, cat );
	    }
	    d = 360.+90. -DEGREES(atan2(cii->d_sin_heading
			      , cii->d_cos_heading));
	    sprintf(arg1, "%.0f", fmod(d,(double)360.));
	    cat_cat_att( "Average_heading_(deg)", 1, arglist, cat );
	    
	    d = DEGREES(atan2(cii->d_sin_drift, cii->d_cos_drift));
	    sprintf(arg1, "%.1f", d );
	    cat_cat_att( "Average_drift_(deg)", 1, arglist, cat );

	    d = DEGREES(atan2(cii->d_sin_pitch, cii->d_cos_pitch));
	    sprintf(arg1, "%.1f", d );
	    cat_cat_att( "Average_pitch_(deg)", 1, arglist, cat );

	    d = DEGREES(atan2(cii->d_sin_roll, cii->d_cos_roll));
	    sprintf(arg1, "%.1f", d);
	    cat_cat_att( "Average_roll_(deg)", 1, arglist, cat );
	}

	if(cmt->sizeof_comments) { /* dump out the comments */
	    n = strlen(mark_char);
	    cii->swp_txt_size += n;
	    mark_char = cat += strlen(cat);

	    strcat(cat, cmt->buf);
	    *cmt->buf = '\0';
	    cmt->sizeof_comments = 0;
	    cmt->at = cmt->buf;
	}
	cat_cat_att(reverse_string(SCAN,arglist), 0, arglist, cat );
	n = strlen(mark_char);
	cii->swp_txt_size += n;
	mark_char = cat += strlen(cat);

	ddcat_write(cii, cii->swp_txt_ptr, cii->swp_txt_size
		     , DDCAT_NORMAL_WRITE);
	return;
    }


    mark_char = cat = cii->swp_txt_ptr;
    cii->swp_txt_size = 0;
    *cii->swp_txt_ptr = 0;
    cat_cat_att(SCAN, 0, arglist, cat );

    sprintf(arg1, "%d", swib->sweep_num );
    cat_cat_att( "Number", 1, arglist, cat );

    cat_cat_att( "Start_date", 1, cat_date(current_time, arglist), cat );
    cat_cat_att( "Start_time", 1, cat_time(current_time, arglist), cat );
# ifdef obsolete
    sprintf(arg1, "%.1f", swib->fixed_angle );
    cat_cat_att( "Fixed_angle", 1, arglist, cat );
# endif
    sprintf(arg1, "%.1f", swib->start_angle );
    cat_cat_att( "Start_angle", 1, arglist, cat );

    sprintf(arg1, "%.2f", dds->radd->eff_unamb_vel );
    cat_cat_att( "Unambiguous_velocity_(m/s)", 1, arglist, cat );

    if( cii->radar_type != GROUND ) {
	sprintf(arg1, "%.3f", dd_latitude(dgi));
	cat_cat_att( "Start_latitude", 1, arglist, cat );

	sprintf(arg1, "%.3f", dd_longitude(dgi));
	cat_cat_att( "Start_longitude", 1, arglist, cat );

	sprintf(arg1, "%.3f", dd_altitude(dgi));
	cat_cat_att( "Start_altitude_(km)", 1, arglist, cat );

	sprintf(arg1, "%.3f", dd_altitude_agl(dgi));
	cat_cat_att( "Start_altitude_agl(km)", 1, arglist, cat );

	sprintf(arg1, "%.1f", dd_heading(dgi));
	cat_cat_att( "Start_heading_(deg)", 1, arglist, cat );

	e = asib->ew_velocity;
	f = asib->ns_velocity;
	sprintf(arg1, "%.0f", sqrt((double)(e*e+f*f)));
	cat_cat_att( "Start_ground_speed_(m/s)", 1, arglist, cat );

	sprintf(arg1, "%.1f", dd_roll(dgi) );
	cat_cat_att( "Start_roll", 1, arglist, cat );

	sprintf(arg1, "%.1f", dd_pitch(dgi) );
	cat_cat_att( "Start_pitch", 1, arglist, cat );

	sprintf(arg1, "%.1f", dd_drift(dgi) );
	cat_cat_att( "Start_drift", 1, arglist, cat );

	sprintf(arg1, "%.1f", asib->ew_horiz_wind );
	cat_cat_att( "EW_horiz_wind_(m/s)", 1, arglist, cat );

	sprintf(arg1, "%.1f", asib->ns_horiz_wind );
	cat_cat_att( "NS_horiz_wind_(m/s)", 1, arglist, cat );
    }
    cat += strlen(cat);
    cii->swp_txt_size = strlen(cii->swp_txt_ptr);
    cii->f_ew_velocity = cii->f_ns_velocity = 0;
    cii->d_cos_heading = cii->d_sin_heading = 0;
    cii->d_cos_drift = cii->d_sin_drift = 0;
    cii->d_cos_pitch = cii->d_sin_pitch = 0;
    cii->d_cos_roll = cii->d_sin_roll = 0;
    cii->f_last_rotation_angle = asib->rotation_angle;
    cii->f_delta_angles = 0;
    cii->avg_rota_count = cii->num_rays = 0;
    cii->flush_sweep_count++;
    cii->num_good_cells = cii->tot_cells = 0;
}
/* c------------------------------------------------------------------------ */

char *cat_time( time, dst )
  time_t time;
  char *dst;
{
    DD_TIME dts;

    dts.time_stamp = time;
    d_unstamp_time(&dts);
    sprintf( dst, "%02d:%02d:%02d", dts.hour, dts.minute, dts.second);
    
    return(dst);
}
/* c------------------------------------------------------------------------ */

char *cat_date( time, dst )
  time_t time;
  char *dst;
{
    DD_TIME dts;

    dts.time_stamp = time;
    d_unstamp_time(&dts);

    sprintf( dst, "%02d-%s-%d"
	    , dts.day
	    , Unix_months[dts.month-1]
	    , dts.year );

    return(dst);
}
/* c------------------------------------------------------------------------ */

void cat_cat_att( name, num_args, arg, cat )
  char *name, *arg, *cat;
  int num_args;
{
    strcat(cat,name );
    if( num_args == 1 ) {
	strcat(cat, ":" );
	strcat(cat, arg);
    }
    strcat(cat, "\n");
}
/* c------------------------------------------------------------------------ */

void cat_cat_att_args( name, num_args, arglist, cat )
  char *name, *arglist[], *cat;
  int num_args;
{
    int i;

    strcat(cat,name );
    if( num_args > 0 )
	  strcat(cat, ":" );
    for(i=0; i < num_args; i++) {
	strcat(cat, arglist[i]);
	if( i < num_args-1 )
	      strcat(cat, "," );
    }
    strcat(cat, "\n");
}
/* c------------------------------------------------------------------------ */

char *reverse_string( str, rev )
  char *str, *rev;
{
    int n=strlen(str);
    char *a=str+n, *b=rev;

    for(; n-- > 0; )
	  *b++ = *(--a);
    *b = '\0';
    return(rev);
}
/* c------------------------------------------------------------------------ */

char *c_deblank( a, n, clean )
  char *a, *clean;
  int n;
{
    /* remove leading and trailing blanks from the
     * n character string
     */
    char *b;

    for(;*a == ' ' && n > 0; a++,n--); /* leading blanks */
    for(b=a+n; *(--b) == ' ' && n > 0; n--); /* trailing blanks */
    
    for(b=clean; n-- > 0;) {
	*b++ = *a++;
    }
    *b= '\0';
    return(clean);
}
/* c------------------------------------------------------------------------ */

void ddcat_close( fid )
  int fid;
{
    if( !Catalog_flag  )
	  return;
    close( fid );
    return;
}
/* c------------------------------------------------------------------------ */
# define PMODE 0666

int ddcat_open(file_name )
  char *file_name;
{
    int i = -99;

    if( Catalog_flag )
	  i = creat( file_name, PMODE );
    printf( " file %s:%d \n", file_name, i );
    return(i);
}
/* c------------------------------------------------------------------------ */

void ddcat_write( cii, buf, size, func )
  struct catalog_info *cii;
  int size, func;
  char *buf;
{
    int i, j, *blowit=0, mark, save_place, fid=cii->volumes_fid;

    if( !Catalog_flag )
	  return;

    if(func & DDCAT_NUVOL_REWRITE) {
	save_place = lseek( fid, 0L, CURRENT_POSTION_LSEEK);
	i = lseek(fid, (long)cii->lseek_vol_offset, BOF_RELATIVE_LSEEK);
    }
    else if(func & DDCAT_NUVOL_WRITE) {
	cii->lseek_vol_offset = lseek( fid, 0L, CURRENT_POSTION_LSEEK);
    }

    if((i = write( fid, buf, size )) < size ) {
	printf( "Problem in ddcat_write--err=%d\n", i );
	exit(1);
    }

    if(func & DDCAT_NUVOL_REWRITE) {
	i = lseek(fid, (long)save_place, BOF_RELATIVE_LSEEK);
    }

    if(func & DDCAT_FLUSH) {
	save_place = lseek( fid, 0L, CURRENT_POSTION_LSEEK);
	j = close(fid);
	fid = open(cii->volumes_file_name, READ_AND_WRITE_ACCESS);
	j = lseek(fid, 0L, EOF_RELATIVE_LSEEK);
	printf("Close & reopen %s : %d\n", cii->volumes_file_name, fid);
	j = lseek( fid, 0L, CURRENT_POSTION_LSEEK);
	mark = 0;
    }
    return;
}
/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */
