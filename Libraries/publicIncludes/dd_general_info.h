/* 	$Id: dd_general_info.h 182 2003-10-09 17:24:01Z oye $	 */
#ifndef INCdd_general_infoh
#define INCdd_general_infoh

# ifndef GNERIC_DESC
# define GNERIC_DESC

struct generic_descriptor {
    char name_struct[4];	/* "????" */
    long sizeof_struct;
};
typedef struct generic_descriptor *GD_PTR;

# endif  /* GNERIC_DESC */


struct dd_ray_sector {
    double sector;
    double angle0;
    double angle1;
    double rotation_angle;
};

struct prev_rays {
    struct prev_rays *last;
    struct prev_rays *next;
    double time;
    float rotation_angle;
    float heading;
    float dH;
    float pitch;
    float dP;
    float folds;
    float vel_corr;
    long disk_offset;
    long sizeof_ray;
    int v0;
    int clip_gate;
    int source_sweep_num;
    long *data[MAX_PARMS];
};

struct prev_swps {
    double start_time;
    double end_time;
    struct prev_swps *last;
    struct prev_swps *next;
    long volume_time_stamp;
    long sweep_time_stamp;
    int listed;
    int new_vol;
    int num_rays;
    int segment_num;
    int source_vol_num;
    int source_sweep_num;
    int sweep_file_size;
    char file_name[88];
    int num_parms;
    int scan_mode;
};

# ifndef ROTANG_STRUCTS
# define ROTANG_STRUCTS

struct rot_table_entry {
    float rotation_angle;
    long offset;
    long size;
};

struct rot_ang_table {
    char name_struct[4];	/* "RKTB" */
    long sizeof_struct;
    float angle2ndx;
    long ndx_que_size;
    long first_key_offset;
    long angle_table_offset;
    long num_rays;
};

# endif /* ROTANG_STRUCTS */

struct null_d {
    char name_struct[4];	/* "NULL" */
    long sizeof_struct;		/* sizeof(struct null_record) */
};
typedef struct null_d null_d;
typedef struct null_d NULL_D;

struct dd_comm_info {
    double time;
    struct comment_d *comm;
    struct dd_comm_info *last;
    struct dd_comm_info *next;
    char radar_name[12];
    int num_comments;
    int sizeof_comment_space;
};

struct cfac_wrap {
  int ok_frib;
  char frib_file_name[80];
  char cfac_file_name[256];
  char radar_name[16];
  struct correction_d *cfac;
  struct cfac_wrap *next;
};


struct dds_structs {
    struct platform_i *asib;
    struct cell_d *celv;
    struct cell_d *celvc;
    struct cell_spacing_d *cspd;
    struct comment_d *comm;
    struct correction_d *cfac;
    struct d_time_struct *dts;
    float *dist_cells[MAX_PARMS];
    struct field_parameter_data *frad;
    struct parameter_d *parm[MAX_PARMS];
    struct paramdata_d *rdat[MAX_PARMS];
    struct radar_angles *ra;
    struct radar_d *radd;
    struct field_radar_i *frib;
    struct waveform_d *wave;
    struct ray_i *ryib;
    struct sweepinfo_d *swib;
    struct super_SWIB *sswb;
    struct volume_d *vold;
    struct null_d *NULL_d;

    int number_cells[MAX_PARMS];
    int field_id_num[MAX_PARMS];
    int field_present[MAX_PARMS];
    int last_cell_num[MAX_PARMS];

    float rcp_uniform_cell_spacing[MAX_PARMS];
    float uniform_cell_zero[MAX_PARMS];
    int uniform_cell_count[MAX_PARMS];
    int *uniform_cell_lut[MAX_PARMS];

    struct lidar_d *lidr;
    struct lidar_parameter_data *ldat;
    char *raw_data;

    int sizeof_frad;
    int sizeof_qdat[MAX_PARMS];
    int max_radar_desc;
    int radar_desc_count;
    struct radar_d **radd_list;
    struct qparamdata_d *qdat[MAX_PARMS];
    char *qdat_ptrs[MAX_PARMS];
    int its_8_bit[MAX_PARMS];
    struct cfac_wrap *first_cfac;
    int max_sizeof_xstf;
    struct dd_extra_stuff *xstf;
};

typedef struct dds_structs *DDS_PTR;

struct dd_general_info {

    double time0;
    double time;
    double vol_time0;

    float prev_rot_angle;

    long start_time;
    long stop_time;
    long timeof_first_sweep;
    long timeof_last_sweep;
    long timeof_next_sweep;
    long volume_time_stamp;
    long sweep_time_stamp;

    int beam_count;
    int buf_bytes_left;		/* applies to input only */
    int clip_gate;
    int compression_scheme;
    int disk_output;
    int file_byte_count;	/* applies to input only */
    int ignore_cfacs;
    int ignore_this_sweep;
    int in_swp_fid;		/* applies to input only */
    int source_ray_num;
    int last_sweep;
    int source_num_parms;
    int max_rat_entries;
    int nav_system;
    int new_sweep;
    int new_vol;
    int num_good_cells;
    int num_parms;
    int num_rdats;
    int offset_to_swib;
    int parm_type[MAX_PARMS];
    int radar_num;
    int ray_num;
    int ray_quality;
    int sizeof_comments;
    int sizeof_dd_buf;
    int sizeof_dgi;
    int sizeof_seds;
    int source_fmt;
    int source_sweep_num;
    int source_vol_num;
    int sweep_count;
    int sweep_fid;		/* output fid */
    int sweep_num;
    int source_num_rays;
    int version;		/* output version number */
    int vol_count;
    int vol_num;
    struct rot_ang_table *source_rat;

    struct dds_structs *dds;
    struct prev_rays *ray_que;
    struct prev_swps *swp_que;
    long *rat_angle_ndx1;
    struct rot_ang_table *rat;
    struct rot_table_entry *rat_entry1;

    char *next_block;
    char *dd_buf;
    char *dd_vol_buf;
    char *seds;			/* solo edit summary */

    char storm_name[20];
    char radar_name[12];
    char directory_name[88];
    char volume_file_name[88];
    char sweep_file_name[88];

    char *in_buf;		/* points to start of input buffer */
    char *in_next_block;	/* points to where in in_buf */
    char source_field_mnemonics[256];

    struct dd_comm_info *first_comment;

    /* general purpose pointers */

    void * gpptr1;
    void * gpptr2;
    void * gpptr3;
    void * gpptr4;
    void * gpptr5;
    void * gpptr6;
    void * gpptr7;

  char file_qualifier[80];
  char orig_sweep_file_name[88];
  int prev_scan_mode;
  int prev_vol_num;
};

typedef struct dd_general_info *DGI_PTR;

#endif /* INCdd_general_infoh */





