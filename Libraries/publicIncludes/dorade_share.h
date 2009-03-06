/* 	$Id: dorade_share.h 2 2002-07-02 14:57:33Z oye $	 */
# include <time.h>
# include <sys/time.h>

/*
 * A sweep file consists of a super_SWIB at the beginning
 * the requisite number of parameter descriptors "PARM"
 * and the requisite RYIB, ASIB and RDAT blocks.
 * last record in sweep should be a "NULL" record
 * the file might also contain additional structures to
 * describe the various processing steps
 */

# include "dd_time.h"

/* c------------------------------------------------------------------------ */

struct radar_angles {
    /* all angles are in radians
     */
    float azimuth;
    float elevation;
    float x;
    float y;
    float z;
    float psi;
    float rotation_angle;
    float tilt;
};
/* c------------------------------------------------------------------------ */

struct que_val {
    int val;
    float f_val;
    double d_val;
    struct que_val *last;
    struct que_val *next;
};
/* c------------------------------------------------------------------------ */

struct running_avg_que {
    double sum;
    double rcp_num_vals;
    int in_use;
    int num_vals;
    struct que_val *at;
    struct running_avg_que *last;
    struct running_avg_que *next;
};
/* c------------------------------------------------------------------------ */
# ifndef SOLO_LIST_MGMT
# define SOLO_LIST_MGMT
# define SLM_CODE

struct solo_list_mgmt {
    int num_entries;
    int sizeof_entries;
    int max_entries;
    char **list;
};
/* c------------------------------------------------------------------------ */

struct solo_str_mgmt {
    struct solo_str_mgmt *last;
    struct solo_str_mgmt *next;
    char *at;
};
# endif

/* c------------------------------------------------------------------------ */
struct io_stuff {
    int fid;
    int io_type;
    int rlen;
    int status;
    int whats_left;
    char *at;
};

# ifdef obsolete
struct point_in_space {
    char name_struct[4];	/* "PISP" */
    long sizeof_struct;
    double time;
    double earth_radius;
    double latitude;
    double longitude;
    double altitude;
    float roll;
    float pitch;
    float drift;
    float heading;
    float x;
    float y;
    float z;
    float azimuth;
    float elevation;
    float range;
    float rotation_angle;
    float tilt;
    long cell_num;
    long ndx_last;
    long ndx_next;
    long state;
    char id[16];
};

# define   PISP_EARTH   0x00000001
# define   PISP_XYZ     0x00000002
# define   PISP_AZELRG  0x00000004
# define   PISP_AIR     0x00000008

/*
 * nameing conventions for files
 * where the middle number is a Unix time stamp
 *
 * "swp.123456789.cp2.0"  sweep files
 * "vol.123456789.cp2.0"  volume headers
 * "cat.123456789.cp2.0"  catalog files
 * "ufd.123456789.cp2.0"  Universal Format data
 * "dor.123456789.cp2.0"  DORADE data
 * "gde.123456789.cp2.0"  Ground echo files
 * "cpi.123456789.cp2.0"  CAPPI files
 *
 * "ppt.123456789.cp2.0"  plot parameters
 * "bnd.123456789.cp2.0"  boundaries
 * "pic.123456789.cp2.0"  images
 *
 *
 *
 *
 */

/*
 * a window file consists of the window descriptor
 * followed by the window data
 */

struct window_descriptor {
    char name_struct[4];	/* "WDES" */
    int sizeof_struct;
    struct point_in_space screen_center;
    struct point_in_space radar_location;
    time_t window_time_stamp;
    time_t data_time_stamp;
    char variable_name[8];
    char radar_name[8];
    char scan_type[8];
    char units[8];
    float pixels_per_km;
    float fixed_angle;
    float center_color_bar;
    float color_bar_increment;
    int num_colors;
    float azimuth_line_interval;
    float azimuth_annotation_range;
    float range_ring_interval;
    float range_annotation_azimuth;
    float x_ticmark_interval;
    float y_ticmark_interval;
    /*
     * The data should be untainted by annotation, color bars or
     * other overlays
     */
    int sizeof_window;
    int width_in_pixels;
    int height_in_pixels;
    int depth_in_bits;
    /* What other usefule X parameters might there be? */
};

struct window_data {
    char name_struct[4];	/* "WDAT" */
    int sizeof_struct;
    /* followed by (width x height) pixels so many bits deep */
};

struct boundary_header {
    char name_struct[4];	/* "BDHD" */
    long sizeof_struct;
    char name_radar[12];
    char user_id[16];
    struct point_in_space radar_location;
    double time_stamp;
    long type_of_info;
    long sizeof_boundary_point;
    long num_points;
    long offset_to_first_point;
    long ndx_of_point1;
};

# endif


