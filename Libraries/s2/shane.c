
# ifndef PMODE
# define PMODE 0666
# endif

# define      SHANES_HEADER_SIZE 30

/* SABL id qualifiers
 */
# define        SABL_GREEN_COUNTS 0
# define           SABL_GREEN_DBM 2
# define          SABL_RED_COUNTS 4
# define             SABL_RED_DBM 2

/* 2 micron id qualifiers
 */
# define             _2MICRON_VEL 88.
# define             _2MICRON_INT 89.
# define             _2MICRON_NCP 90.

# define            POINTING_DOWN 0
# define              POINTING_UP 1

/* Data IDs
 */
# define                NAILS_ID 0
# define              UV_DIAL_ID 20
# define             YAG_DIAL_ID 40
# define                 SABL_ID 60
/*
 * for SABL
 * 60 => green counts and pointing down
 * 61 => green counts and pointing up
 * 62 => green dbm and poiting down
 * 63 => green dbm and poiting down
 * 64 => red counts and pointing down
 * 65 => red counts and pointing up
 * 66 => red dbm and poiting down
 * 67 => red dbm and poiting up
 */
# define   TWO_MICRON_DOPPLER_ID 80
# define         TO_BE_DETERMIED 100

/*
Here is what we've defined so far for the BSCAN format.

9.0000: ' '
9.0001: 'NCAR Foothills Lab'
9.0002: 'RL3, Boulder, CO'
9.0003: 'Boulder, CO'
9.0004: 'Jeffco Airport'
9.0005: 'Marshall'
9.0006: 'Table Mountain'
9.0007: 'Arch Cape, OR'
9.0008: 'BAO Tower'
9.0009: 'Braman, OK'
9.0010: 'Lamont, OK'
9.0011: 'Flatland, IL'  <====== FOR LIFT

Remember, default of 9.0000 just means that the platform was
on the ground - but the site is undefined.  Also, since the
files are direct access, we can easily change things like this
value without rewriting the whole file.
 */

/* Platform IDs
 */
# define       UNKNOWN_MOVING_PFM 8.0000
# define             CASA_212_PFM 8.0001
# define            NCAR_C130_PFM 8.0002
# define         LIFT_2MICRON_PFM 9.0001

/* Location IDs
 */
# define        UNKNOWN_FIXED_LOC 9.0000
# define                 FLAB_LOC 9.0001
# define                  RL3_LOC 9.0002
# define              BOULDER_LOC 9.0003
# define               JEFFCO_LOC 9.0004
# define             MARSHALL_LOC 9.0005
# define            TABLE_MTN_LOC 9.0006
# define            ARCH_CAPE_LOC 9.0007
# define                 LIFT_LOC 9.0011

# define               GREEN_LIST "GREEN GDB"
# define                 RED_LIST "INFRARED RDB"
# define            VELOCITY_LIST "VE"
# define                 NCP_LIST "NCP"
# define       SHANES_DATA_FIELDS "GDB RDB VE"
# define       _2M_INTENSITY_LIST "INT"

# include <dorade_headers.h>
# include <time.h>
# include <stdio.h>
# include <dd_math.h>
# include "dd_files.h"
# include "input_sweepfiles.h"
# include "input_limits.h"
# include "dd_stats.h"
# include <function_decl.h>
# include <dgi_func_decl.h>


struct shane_header {
    float hour;
    float minute;
    float second;
    float gps_altitude;
    float first_alt_altitude;
    float second_alt_altitude;
    float azimuth;
    float elevation;
    float roll;
    float pitch;
    float yaw;
    float month;
    float day;
    float year;
    float prf;
    float bad_shot_count;
    float data_type_id;		/* fortran word 17 */
    float correction_to_GMT;	/* fortran word 18 */
    float latitude;		/* fortran word 19 */
    float longitude;		/* fortran word 20 */
    float range_bin_1;		/* fortran word 21 */
    float bin_spacing;		/* fortran word 22 */
    float platform_id;		/* fortran word 23 */
    float words_in_header;	/* fortran word 24 */
    float no_data_flag;		/* fortran word 25 */
    float word_26;
    float shots_per_ray;
    float intended_shot_count;
    float num_cells;
    float transect_num;
};

struct field_mgmt {
    struct field_mgmt *next;
    int fid_num;
    int parm_num;
    int sizeof_buf;
    int transect_num;
    char filename[64];
    char field_name[16];
    char *buf;
};

struct shane_info {
    int shots_per_ray;
    float data_id;
    float platform_id;
    struct field_mgmt *top_field;
    char directory[128];
    int num_cells;
    float bin_spacing;
};

static struct dd_stats *dd_stats=NULL;
static struct dd_input_filters *difs=NULL;
static struct dd_input_sweepfiles_v3 *dis=NULL;
static struct shane_info *spv=NULL;
static int shane_output=YES;

double dd_rotation_angle();
double dd_tilt_angle();
double dd_latitude();
double dd_longitude();
double dd_altitude();
double dd_earthr();
double dd_azimuth_angle();
double dd_elevation_angle();
double dd_heading();
double dd_drift();
double dd_roll();
double dd_pitch();

static void stuff_shanes_hsk();
void produce_shanes_data();






/* c------------------------------------------------------------------------ */

void dd_shane_conv()
{
    static int count=0;
    int ii, rn, flush=NO, ok;
    struct dd_input_filters *dd_return_difs_ptr();
    struct dd_stats *dd_return_stats_ptr();
    struct dd_input_sweepfiles_v3 *dd_setup_sweepfile_structs_v3();
    struct unique_sweepfile_info_v3 *usi;
    struct dd_general_info *dgi, *dd_window_dgi();

    /* do all the necessary initialization including
     * reading in the first ray
     */
    difs = dd_return_difs_ptr();
    dd_stats = dd_return_stats_ptr();
    if(!(dis = dd_setup_sweepfile_structs_v3(&ok)))
	  return;
    dis->print_swp_info = YES;

    for(ii=0,usi=dis->usi_top; ii++ < dis->num_radars; usi=usi->next) {
	/* now try and find files for each radar by
	 * reading the first record from each file
	 */
	if(ddswp_next_ray_v3(usi) == END_OF_TIME_SPAN) {
	    printf( "Problems reading first record of %s/%s\n"
		   , usi->directory, usi->filename);
	    return;
	}
    }
    usi = dis->usi_top;
    /*
     * for each radar, write out the UF file
     */
    for(rn=0; rn++ < dis->num_radars; usi=usi->next) {
	dgi = dd_window_dgi(usi->radar_num);
	strcpy(dgi->radar_name, usi->radar_name);
	for(;;) {
	    if(!(count++ % 500))
		  dgi_interest_really(dgi, NO, "", "", dgi->dds->swib);
	    produce_shanes_data(dgi, flush);
	    
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
    return;
}
/* c------------------------------------------------------------------------ */

void produce_shanes_data(dgi)
  DGI_PTR dgi;
{
    /* produce a set of lidar profiles in Shane Mayers format
     */
    int ii, mm, nc, nn, nt, pn, mark, bad, shots_per_ray = 1;
    int num_cells, something_changed=NO;
    char *aa, string_space[256], *str_ptrs[32], str[256];
    char *get_tagged_string();
    int dd_tokens(), dd_find_field();
    struct field_mgmt *fm, *fm_last;
    float *fp, rcp_scale, bias, bin_spacing;
    short *ss;


    if(!shane_output)
	  return;

    if(!spv) {
	shane_output = NO;

	if(!(aa=get_tagged_string("OUTPUT_FLAGS"))) {
	    return;
	}
	if(!strstr(aa, "SHANES_DATA")) {
	    return;
	}
	if(!(aa=get_tagged_string("SHANES_DATA_FIELDS"))) {
	    aa = SHANES_DATA_FIELDS;
	}
	strcpy(string_space, aa);
	if((nt = dd_tokens(string_space, str_ptrs)) < 1)
	      return;

	spv = (struct shane_info *)malloc(sizeof(struct shane_info));
	memset(spv, 0, sizeof(struct shane_info));
	
	if(aa=get_tagged_string("SHANES_DIRECTORY")) {
	    slash_path(spv->directory, aa);
	}
	else {
	    slash_path(spv->directory, dgi->directory_name);
	}
	/*
	 * assume each token is a field name
	 */
	for(nn=ii=0; ii < nt; ii++) {
	    if((pn = dd_find_field(dgi, str_ptrs[ii])) >= 0) {
		fm = (struct field_mgmt *)malloc
		      (sizeof(struct field_mgmt));
		memset(fm, 0, sizeof(struct field_mgmt));

		if(!ii) {
		    spv->top_field = fm;
		}
		else {
		    fm_last->next = fm;
		}
		fm_last = fm;
		if(!(fm->buf = (char *)malloc(K64))) {
		    printf("Unable to malloc buffer in shanes data\n");
		    exit(1);
		}
		fm->parm_num = pn;
		memset(fm->buf, 0, K64);
		fm->sizeof_buf = K64;
		    
		strcpy(fm->field_name, str_ptrs[ii]);
	    }
	    else {
		printf("Field: %s not present and will not be produced\n"
		       , str_ptrs[ii]);
	    }
	}
	if(!spv->top_field) 
	      return;
	shane_output = YES;
	
	aa = dgi->radar_name;
	if(strstr(aa, "SABL")) {
	    spv->data_id = SABL_ID;
	    if(dgi->dds->radd->radar_type == GROUND) {
		/* klooge for now 10/96 */
		spv->platform_id  = LIFT_LOC;
	    }
	    else {
		spv->platform_id  = NCAR_C130_PFM;
	    }
	}
	else if(strstr(aa, "NAILS")) {
	    spv->data_id = NAILS_ID;
	    spv->platform_id  = NCAR_C130_PFM;
	}
	else if(strstr(aa, "ETL2MI")) {
	    spv->data_id = TWO_MICRON_DOPPLER_ID;
	    spv->platform_id  = LIFT_2MICRON_PFM;
	}
	else {
	}
    }
    if(!shane_output)
	  return;
    
    /* produce the data
     */
    num_cells = dgi->dds->celv->number_cells;
    bin_spacing = dgi->dds->celvc->dist_cells[1] -
	  dgi->dds->celvc->dist_cells[0];
    if(num_cells != spv->num_cells || bin_spacing != spv->bin_spacing) {
	something_changed = YES;
	spv->num_cells = num_cells;
	spv->bin_spacing = bin_spacing;
    }

	
    for(fm=spv->top_field;  fm ; fm = fm->next) {

	if(something_changed) {
	    if(fm->fid_num) close(fm->fid_num);
	    dd_file_name("pvw", (long)dgi->time
			 , str_terminate(str, dgi->radar_name, 8)
			 , 0, fm->filename);
	    strcat(fm->filename, ".");
	    strcat(fm->filename, fm->field_name);
	    strcpy(str, spv->directory);
	    strcat(str, fm->filename);
	    printf("Opening: %s\n", str);
	    
	    if((fm->fid_num = creat(str, PMODE)) < 0) {
		printf("Unable to create file: %s\n", str);
		shane_output = NO;
		return;
	    }
	}
	pn = dd_find_field(dgi, fm->field_name);
	bad = dgi->dds->parm[pn]->bad_data;
	rcp_scale = 1./dgi->dds->parm[pn]->parameter_scale;
	bias = dgi->dds->parm[pn]->parameter_bias;
	nn = nc = dgi->dds->celv->number_cells;
	fp = (float *)fm->buf;
	stuff_shanes_hsk(dgi, fp, fm);
	fp += SHANES_HEADER_SIZE;
	ss = (short *)dgi->dds->qdat_ptrs[pn];

	for(; nn--; ss++, fp++) {
	    if(*ss == bad) *fp = bad;
	    else *fp = DD_UNSCALE((float)(*ss), rcp_scale, bias);
	}
	mm = write(fm->fid_num, fm->buf
		   , (SHANES_HEADER_SIZE + nc) * sizeof(float));
    }
}
/* c------------------------------------------------------------------------ */

static void
stuff_shanes_hsk(dgi, hsk, fm)
  DGI_PTR dgi;
  float *hsk;
  struct field_mgmt *fm;
{
    int ii, id, point, field;
    int pn = fm->parm_num;
    DD_TIME *d_unstamp_time();
    float details;
    double d;
    struct shane_header *shd = (struct shane_header *)hsk;
    

    d_unstamp_time(dgi->dds->dts);

    shd->hour = dgi->dds->dts->hour;
    shd->minute = dgi->dds->dts->minute;
    shd->second = dgi->dds->dts->second;
    shd->gps_altitude = KM_TO_M(dd_altitude(dgi));
    shd->first_alt_altitude = 0;
    shd->second_alt_altitude = 0;
    shd->azimuth = dd_azimuth_angle(dgi);
    d = dd_elevation_angle(dgi);
    shd->elevation = d;
    shd->roll = dd_roll(dgi);
    shd->pitch = dd_pitch(dgi);
    shd->yaw = dd_drift(dgi);
    shd->month = dgi->dds->dts->month;
    shd->day = dgi->dds->dts->day;
    shd->year = dgi->dds->dts->year;
    shd->prf = dgi->dds->radd->interpulse_per1
	  ? 1./dgi->dds->radd->interpulse_per1 : 0;

    shd->bad_shot_count = 0;
    id = shd->data_type_id = spv->data_id;

    switch(id) {

    case NAILS_ID:
	shd->data_type_id = 0;
	shd->platform_id = TABLE_MTN_LOC;
	shd->platform_id = ARCH_CAPE_LOC;
	if(shd->elevation > 0) shd->data_type_id += 1;
	break;

    case SABL_ID:
	if(strstr(GREEN_LIST, fm->field_name)) {
	    shd->data_type_id += SABL_GREEN_COUNTS;
	    if(strstr(fm->field_name, "GDB")) {
		shd->data_type_id += SABL_GREEN_DBM;
	    }
	}
	else if(strstr(RED_LIST, fm->field_name)) {
	    shd->data_type_id += SABL_RED_COUNTS;
	    if(strstr(fm->field_name, "RDB")) {
		shd->data_type_id += SABL_RED_DBM;
	    }
	}
	if(shd->elevation > 0) shd->data_type_id += 1;
	shd->platform_id = NCAR_C130_PFM;
	break;

    case TWO_MICRON_DOPPLER_ID:
	if(strstr(VELOCITY_LIST, fm->field_name)) {
	   shd->data_type_id = _2MICRON_VEL;
	}
	else if(strstr(_2M_INTENSITY_LIST, fm->field_name)) {
	   shd->data_type_id = _2MICRON_INT;
	}
	else if(strstr(NCP_LIST, fm->field_name)) {
	   shd->data_type_id = _2MICRON_NCP;
	}
	shd->platform_id = LIFT_2MICRON_PFM;
	break;

    default:
	break;
    }

    shd->correction_to_GMT = 0;

    shd->latitude = dd_latitude(dgi);
    shd->longitude = dd_longitude(dgi);
    shd->range_bin_1 = dgi->dds->celvc->dist_cells[0];
    shd->bin_spacing = dgi->dds->celvc->dist_cells[1] -
	  dgi->dds->celvc->dist_cells[0];
    shd->words_in_header = SHANES_HEADER_SIZE;
    shd->no_data_flag = EMPTY_FLAG;
    shd->word_26 = -999.;
    shd->shots_per_ray = dgi->dds->parm[pn]->num_samples;
    shd->intended_shot_count = 0;
    shd->num_cells = dgi->dds->celv->number_cells;
    shd->transect_num = -999.;
}
/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */






