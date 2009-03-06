/* 	$Id: dd_files.h 2 2002-07-02 14:57:33Z oye $	 */

# ifndef DDFILESH
# define DDFILESH

/* file type id */
# define SWP_FILE 1
# define VOL_FILE 2
# define CAT_FILE 3
# define DOR_FILE 4
# define UFD_FILE 5
# define GDE_FILE 6
# define BND_FILE 7
# define XXX_FILE 8
# define PPT_FILE 9
# define PIC_FILE 10
# define CPI_FILE 11

# define FILTER_TIME_BEFORE  4
# define        TIME_BEFORE  2
# define       TIME_NEAREST  0
# define         TIME_AFTER  1
# define  FILTER_TIME_AFTER  3

# define    LATEST_VERSION -1
# define  EARLIEST_VERSION -2
# define   EXHAUSTIVE_LIST -3

struct dd_file_name {
    long time_stamp;
    short file_type;
    short radar_num;
    short version;
    short num_versions;
    char radar_name[12];
};

struct dd_file_name_v2 {
    long time_stamp;
    short file_type;
    short radar_num;
    short version;
    short num_versions;
    char radar_name[12];
    short milliseconds;
};

struct dd_file_name_v3 {
    double time;
    struct dd_file_name_v3 *last;
    struct dd_file_name_v3 *next;

    struct dd_file_name_v3 *parent;
    struct dd_file_name_v3 *right;
    struct dd_file_name_v3 *left;

    struct dd_file_name_v3 *ver_right;
    struct dd_file_name_v3 *ver_left;

    double time_stamp;
    char radar_name[12];
    unsigned char version;
    unsigned char red_link;
    short milliseconds;
    char comment[88];
};

# endif




