#ifndef INCfunction_declh
#define INCfunction_declh

# include <sys/types.h>
# include <sys/stat.h>
# include <fcntl.h>

# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <unistd.h>
# include <ctype.h>


double  angdiff();
void 	by_products();
int  	cdcode();
void 	ctypeu16();
void 	ezhxdmp();
void 	ezascdmp();
   
double 	d_time_stamp();
DD_TIME *d_unstamp_time();
   
void 	dd_append_cat_comment();
int  	dd_assign_radar_num();
char *  dd_blank_fill();
int     dd_catalog_only_flag();
struct time_dependent_fixes *dd_clean_tdfs();
void 	dd_clear_dts();
void    dd_close();
int     dd_compress();
int  	dd_control_c();
int  	dd_crack_datime();
int  	dd_datum_size();
char *  dd_delimit();
void    dd_enable_cat_comments();
void 	dd_file_name();
void 	dd_file_name_ms();
void 	dd_gen_packet_info();
void 	dd_get_difs();
void 	dd_get_lims();
int     dd_get_scan_mode();
int  	dd_hrd16_uncompressx();
void 	dd_input_read_close();
int  	dd_input_read_open();
void 	dd_input_strings();
void 	dd_intset();
void 	dd_io_reset_offset();
int  	dd_itsa_physical_dev();
struct time_dependent_fixes *dd_kill_tdf();
int  	dd_logical_read();
int     dd_min_rays_per_sweep();
int     dd_num_radars();
void 	dd_radar_selected();
void 	dd_reset_control_c();
int  	dd_return_id_num();
struct dd_input_filters *dd_return_difs_ptr();
int     dd_return_num_comments();
int     dd_return_radar_num();
struct dd_stats *dd_return_stats_ptr();
void 	dd_rewind_dev();
int     dd_get_scan_mode();
void 	dd_set_control_c();
int     dd_set_solo_flag();
void 	dd_set_uniform_cells();
void 	dd_skip_files();
int  	dd_skip_recs();
int  	dd_solo_flag();
void 	dd_stuff_ray();
struct dd_stats *dd_stats_reset();
char *  dd_stats_sprintf();
struct time_dependent_fixes *dd_time_dependent_fixes();
int  	dd_tokens();
int  	dd_tokenz();
void    dd_uniform_cell_spacing();
void    difs_terminators();
char *  dts_print();
int     fb_read();
int     fp_bin_search();
 
char *  get_tagged_string();
int  	getreply();
int  	gp_read();
int  	gp_write();
void 	gri_interest();
int 	gri_nab_input_filters();
void 	gri_print_list();
void    gri_set_max_lines();
int  	gri_start_stop_chars();
int  	gri_max_lines();
int  	HRD_recompress();
   
int  	in_sector();
char *  put_tagged_string();
   
char *  slash_path();
void 	slm_print_list();
void 	solo_add_list_entry();
void 	solo_reset_list_entries();
void 	solo_unmalloc_list_mgmt();
char *  str_terminate();
time_t    todays_date();
short   swap2();
long    swap4();
long    xswap4();
char *  dd_whiteout();

# endif  /* INCfunction_declh */
