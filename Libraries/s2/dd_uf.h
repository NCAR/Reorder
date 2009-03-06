/* 	$Id: dd_uf.h 2 2002-07-02 14:57:33Z oye $	 */
# include "uf_fmt.h"
# include <dorade_headers.h>
# include "input_limits.h"
# include "dd_stats.h"
# include <time.h>
# include <sys/time.h>

# define MAX_UF_GATES 2048

struct uf_out_dev {
    struct uf_out_dev *next;
    struct uf_out_dev *local;
    char *radar_name;
    char *dev_name;
    int io_type;
};

struct uf_production {
    RTIME time;
    UF_MANDITORY *man;
    UF_OPTIONAL *opt;
    UF_LOC_USE *lug;		/* local use generic */
    UF_LOC_USE_AC *luac;	/* RDP aircraft header */
    UCLA_LOC_USE_AC *luucla;	/* UCLA's aircraft header */
    UF_LOC_USE_SHIP *luship;	/* RDP shipboard loc use header */
    UF_DATA_HED *dhed;		/* first part of data header */
    UF_FLD_ID_ARRAY *fida;
    UF_FLD_HED *fhed[MAX_UF_FLDS];
    short *fdata[MAX_UF_FLDS];

    float uf_range1;
    float uf_range2;
    float uf_gate_spacing;
    float uf_range_g1;
    double MB;
    double MB_trip;

    int dd_gate1;
    int dd_gate_lut[MAX_UF_GATES];
    int dd_radar_num;
    int io_type;
    int last_scan_mode;
    int last_num_parms;
    int last_ray_num;
    int last_sweep_num;
    int last_vol_num;
    int uf_fid;
    int uf_file_size;
    int uf_num_gates;
    int new_sweep;
    int new_vol;
    int dev_count;
    int vol_sweep_count;

    short *uf_buf;
    char uf_dir[256];
    struct uf_out_dev *dev_list;
    struct uf_out_dev *prev_dev;
    char radar_name[16];
    double current_media_size;
    float last_fxd_ang;
};

