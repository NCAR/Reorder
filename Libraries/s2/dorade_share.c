/* 	$Id: dorade_share.c 246 2006-06-19 22:36:09Z dennisf $	 */

# include <sys/types.h>
# include <dirent.h>


#ifndef lint
static char vcid[] = "$Id: dorade_share.c 246 2006-06-19 22:36:09Z dennisf $";
#endif /* lint */
/*
 * This file contains the following routines
 * 
 * angdiff
 * cascdmp
 * chexdmp
 * cpy_bfill_str
 * dcdatime
 * dd_blank_fill
 * dd_chop
 * dd_crack_file_name
 * dd_crack_file_name_ms
 * dd_file_name
 * dd_file_name_ms
 * 
 * dd_hrd16
 * dd_hrd16_uncompressx
 * dd_nab_floats
 * dd_nab_ints
 * dd_rdp_uncompress8
 * dd_scan_mode_mne
 * dd_unquote_string
 * dorade_time_stamp
 * dts_print
 * eldora_time_stamp
 * fb_read
 * fb_write
 * fgetsx
 * file_time_stamp
 * fstring
 * get_tagged_string
 * gp_read
 * gp_write
 * in_sector
 * put_tagged_string
 * 
 * 
 * se_free_raqs
 * se_return_ravgs
 * slash_path
 * slm_print_list
 * solo_add_list_entry
 * solo_list_entry
 * solo_list_remove_dups
 * solo_list_remove_entry 
 * solo_list_sort_file_names
 * 
 * solo_malloc_list_mgmt
 * solo_malloc_pisp
 * solo_modify_list_entry
 * solo_reset_list_entries
 * solo_unmalloc_list_mgmt

 * str_terminate
 * time_now
 * todays_date
 * 
 * 
 */

# include <dd_defines.h>
# include <dorade_headers.h>
# include <point_in_space.h>
# include <time.h>
# include <sys/time.h>

# include <function_decl.h>
# include <dgi_func_decl.h>

# include <sys/mtio.h>
# include <sys/ioctl.h>

# include "run_sum.h"

# define MAX_TAGS 512
# define MAX_TAG_SIZE 32
# define MAX_STRING_SIZE 128

static char *Stypes[] = { "CAL", "PPI", "COP", "RHI", "VER",
			  "TAR", "MAN", "IDL", "SUR", "AIR", "???" };


void solo_message();

static struct running_avg_que *top_raq=NULL;
static char *tags[MAX_TAGS], *tagged_strings[MAX_TAGS];
static int num_tags=0;
extern int LittleEndian;

/* c------------------------------------------------------------------------ */


/* c------------------------------------------------------------------------ */


/* c------------------------------------------------------------------------ */


/* c------------------------------------------------------------------------ */

int dd_text_to_slm (const char *txt, struct solo_list_mgmt *slm)
{
  char *buf = NULL;
  int ii, nc=0, nn, nt;
  char *aa, *lines[512], str[256], *sptrs[32];

  slm->num_entries = 0;
 
  nn = strlen (txt);
  buf = (char *)malloc (nn+1);
  strcpy (buf, txt);
  nn = dd_tokenz (buf, lines, "\n");
  
  for (ii=0; ii < nn; ii++) {
    if (aa = strchr (lines[ii], '#')) /* comment */
      { *aa = '\0'; }
    
    strcpy (str, lines[ii]);
    nt = dd_tokens (str, sptrs);
    if (nt < 1)		/* no tokens in line */
      { continue; }
    
    solo_add_list_entry(slm, lines[ii], strlen(lines[ii]));
    nc++;
  }
  free (buf);
  return nc;
}

/* c------------------------------------------------------------------------ */

int dd_strings_to_slm (const char **lines, struct solo_list_mgmt *slm, int nn)
{
  int ii, nc=0, nt;
  char *aa, str[256], *sptrs[32], str2[256];

  slm->num_entries = 0;
 
  for (ii=0; ii < nn; ii++) {
    strcpy (str, lines[ii]);
    if (aa = strchr (str, '#')) /* comment */
      { *aa = '\0'; }
    strcpy (str2, str);
    nt = dd_tokens (str2, sptrs);
    if (nt < 1)		/* no tokens in line */
      { continue; }
    
    solo_add_list_entry(slm, str, strlen (str));
    nc++;
  }
  return nc;
}

/* c------------------------------------------------------------------------ */

int dd_absorb_strings (char *path_name, struct solo_list_mgmt *slm)
{
    int ii, nn, nt, mark;
    char *aa, *bb, str[256], str2[256], *sptrs[64], *fgetz();
    FILE *stream;


    if(!(stream = fopen(path_name, "r"))) {
	printf("Unable to open %s", path_name);
	return(-1);
    }
    slm->num_entries = 0;

    /* read in the new strings
     */
    for(nn=0;; nn++) {
	if(!(aa = fgets(str, (int)128, stream))) {
	    break;
	}
	if (bb = strchr (str, '#')) /* comment */
	  { *bb = '\0'; }

	strcpy (str2, str);
	if ((nt = dd_tokens (str2, sptrs)) < 1)
	  { continue; }
	solo_add_list_entry(slm, str, strlen(str));
    }
    fclose(stream);
    return(nn);
}
/* c------------------------------------------------------------------------ */

void solo_sort_strings(sptr, ns)
  char **sptr;
  int ns;
{
    int ii, jj;
    char *keep;

    for(ii=0; ii < ns-1; ii++) {
	for(jj=ii+1; jj < ns; jj++) {
	    if(strcmp(*(sptr+jj), *(sptr+ii)) < 0) {
		keep = *(sptr+ii);
		*(sptr+ii) = *(sptr+jj);
		*(sptr+jj) = keep;
	    }
	}
    }
}
/* c------------------------------------------------------------------------ */

int generic_sweepfiles( dir, lm, prefix, suffix, not_this_suffix )
  char *dir, *prefix, *suffix;
  struct solo_list_mgmt *lm;
  int not_this_suffix;
{
    /* tries to create a list of files in a directory
     */
    DIR *dir_ptr;
    struct dirent *dp;
    char mess[256];
    char *aa, *bb, *cc, *pfx=prefix, *sfx="";
    int pfx_len = strlen( prefix );
    int sfx_len = strlen( suffix );
    int mark;

    if( sfx_len )
      { sfx = suffix; }

    lm->num_entries = 0;

    if(!(dir_ptr = opendir(dir))) {
	sprintf(mess, "Cannot open directory %s\n", dir);
	printf( "%s", mess );
	return(-1);
    }

    for(;;) {
	dp=readdir(dir_ptr);
	if(dp == NULL ) {
	    break;
	}
	aa = dp->d_name;
	if( *aa == '.' )
	   { continue; }

	if( pfx_len && strncmp(aa, pfx, pfx_len))
	   { continue; }	/* does not compare! */

	if( sfx_len ) {
	  bb = aa + strlen(aa) - sfx_len;
	  mark = strncmp( bb, sfx, sfx_len );

	  if( not_this_suffix && mark == 0 )
	    { continue; }	/* don't want files with this suffix */
	  else if( mark != 0 )
	    { continue; }	/* wrong suffix */
	}
	/* passes all tests */
	solo_add_list_entry(lm, dp->d_name, strlen(dp->d_name));
    }
    closedir(dir_ptr);
    if(lm->num_entries > 1)
	  solo_sort_strings(lm->list, lm->num_entries);

    return(lm->num_entries);
}
/* c------------------------------------------------------------------------ */

int dd_return_interpulse_periods( dgi, field_num, ipps )
struct dd_general_info *dgi;
int field_num;
double * ipps;
{
  int ii, nn = 0, mask = dgi->dds->parm[field_num]->interpulse_time;
  int probe = 1;
  float * fptr = &dgi->dds->radd->interpulse_per1;

  if( field_num < 0 || field_num >= dgi->dds->radd->num_parameter_des )
    { return 0; }
				/*
  // interpulse periods returned in milliseconds
				 */
# ifdef notyet
  for( ii = 0; ii < 5; ii++, probe <<= 1 ) {
    if( probe & mask ) {
      /*
      // in the header as milliseconds
       */
      *( ipps + nn ) = *( fptr + ii ) * .001; 
      nn++;
    }
  }
# endif

  nn = dgi->dds->radd->num_ipps_trans;
  for (ii=0; ii < nn; ii++) {
     ipps[ii] = fptr[ii];
  }
  return nn;
}

/* c------------------------------------------------------------------------ */

int dd_return_frequencies( dgi, field_num, freqs )
struct dd_general_info *dgi;
int field_num;
double * freqs;
{
  int ii, nn = 0, mask = dgi->dds->parm[field_num]->xmitted_freq;
  int probe = 1;
  float * fptr = &dgi->dds->radd->freq1;

  if( field_num < 0 || field_num >= dgi->dds->radd->num_parameter_des )
    { return 0; }
				/*
  // frequencies returned in GHz
				 */

  for( ii = 0; ii < 5; ii++, probe <<= 1 ) {
    if( probe & mask )
      { *( freqs + nn++ ) = *( fptr + ii ); }
  }
  return nn;
}
/* c------------------------------------------------------------------------ */

int dd_alias_index_num( dgi, name ) 
struct dd_general_info *dgi;
char * name;
{
  static char * known_aliases [] =
  {
      "DZ DB DBZ"
    , "VE VR"
    , "RH RX RHO RHOHV"
    , "PH DP PHI PHIDP"
    , "ZD DR ZDR"
    , "LD LC LDR"
    , "NC NCP"
    , "LV LVDR LDRV"
  };
  char str[256], * sptrs[64], *aa, *bb, *cc;
  int ii, jj, kk, mm, nn, nt, ndx;
  int num_alias_sets = sizeof( known_aliases )/sizeof( char *);

  if(( mm = strlen( name )) < 1 )
    { return -1; }

  for( ii = 0; ii < num_alias_sets; ii++ ) { /* for each set of aliases */
    if( !strstr( known_aliases[ii], name ))
      { continue; }		/* not even close */

    strcpy( str, known_aliases[ii] );
    nt = dd_tokens( str, sptrs );

    for( jj = 0; jj < nt; jj++ ) {
      nn = strlen( sptrs[jj] );
      if( mm != nn )
	{ continue; }		/* lengths of possible match not the same */
      if( !strcmp( name, sptrs[jj] )) {
	/* 
	 * we have a match; now see if this mapper responds to
	 * one of these aliases
	 */
	for( kk = 0; kk < nt; kk++ ) {
	  if(( ndx = dd_find_field( dgi, sptrs[kk] )) >= 0 )
	    { return ndx; }
	}
	return -1;
      }
    }
  }
  return -1;
}
/* c------------------------------------------------------------------------ */

double angdiff( a1, a2 )
  float a1, a2;
{
    double d=a2-a1;

    if( d < -270. )
	  return(d+360.);
    if( d > 270. )
	  return(d-360.);
    return(d);
}
/* c------------------------------------------------------------------------ */

double d_angdiff( a1, a2 )
  double a1, a2;
{
    double d=a2-a1;

    if( d < -180. )
	  return(d+360.);
    if( d > 180. )
	  return(d-360.);
    return(d);
}
/*c----------------------------------------------------------------------*/

void cascdmp( b, m, n )		/* ascii dump of n bytes */
  char *b;
  int m, n;
{
    int s=0;
    char *c=b+m;
    
    for(; 0 < n--; c++,m++) {
	if( s == 0 ) {
	    printf("%5d) ", m );	/* new line label */
	    s = 7;
	}
	if( *c > 31 && *c < 127 )
	     putchar(*c);	/* print actual char */
	else
	     putchar('.');
	s++;
	
	if( s > 76 ) {
	    putchar('\n');	/* start a new line */
	    s = 0;
	}
    }
    if( s > 0 )
	 printf( "\n" );
}
/*c----------------------------------------------------------------------*/

void chexdmp( b, m, n )		/* hexdump of n bytes */
  char *b;
  int m, n;
{
    int c, i, s;
    char h[20];
    sprintf( h, "0123456789abcdef" );   /* adds a '\0'to the string */
    
    for(i=1,s=0; 0 < n--; i++,m++) {
	if( s == 0 ) {
	    printf("%5d) ", m/4);	/* new line label */
	    s = 7;
	}
	/* get upper and lower nibble */
	c = ((long int) *(b+m) >>4) & 0xf;
	putchar(*(h+c));	/* print actual hex char */
	c = (long int) *(b+m) & 0xf; 
	putchar(*(h+c));
	s += 2;
	
	if( s > 76 ) {
	    putchar('\n');	/* start a new line */
	    s = 0;
	}
	else if( i%4 == 0 ) {
	    putchar(' ');	/* space every 8 char */
	    s++;
	}
    }
    if( s > 0 ) printf( "\n" );
}
/* c------------------------------------------------------------------------ */

char *cpy_bfill_str(dst, srs, n)
  CHAR_PTR srs, dst;
  int n;
{
    int i, j;

    if((j=strlen(srs)) >= n) {
	strncpy(dst,srs,n);
	return(dst);
    }
    strcpy(dst,srs);

    for(; j < n; j++) {
	*(dst+j) = ' ';		/* blank fill */
    }
    return(dst);
}
/*c----------------------------------------------------------------------*/

int dcdatime( str, n, yy, mon, dd, hh, mm, ss, ms )
  char *str;
  int n;
  short *yy, *mon, *dd, *hh, *mm, *ss, *ms;
{
    /*
     * this routine decodes a time string hh:mm:ss.ms
     */
    char ls[88], *s, *t;
    
    *yy = *mon = *dd = *hh = *mm = *ss = *ms = 0;
    strncpy( ls, str, n );
    ls[n] = '\0';
    s = ls;
    
    if( t = strchr( s, '/' )) {
	/*
	 * assume this string contains date and time
	 * of the form mm/dd/yy:hh:mm:ss.ms
	 */
	*t = '\0';
	*mon = atoi(s);
	s = ++t;
	if( !( t = strchr( s, '/' )))
	      return(NO);
	*t = '\0';
	*dd = atoi(s);
	s = ++t;
	if(( t = strchr( s, ':' ))) {
	    *t = '\0';
	    *yy = atoi(s);
	    s = ++t;
	}
	else if( strlen(s)) {
	    *yy = atoi(s);
	    return(YES);
	}
	else
	      return(NO);
    }
    /* now get hh, mm, ss, ms */
    if(( t = strchr( s, ':' ))) {
	*t = '\0';
	*hh = atoi(s);
    }
    else {
	if(strlen(s))
	      *hh = atoi(s);
	return(YES);
    }    
    s = ++t;
    if(( t = strchr( s, ':' ))) {
	*t = '\0';
	*mm = atoi(s);
    }
    else {
	if(strlen(s))
	      *mm = atoi(s);
	return(YES);
    }    
    s = ++t;
    if(( t = strchr( s, '.' ))) {
	*t = '\0';
	*ss = atoi(s);
    }
    else {
	if(strlen(s))
	      *ss = atoi(s);
	return(YES);
    }
    s = ++t;
    if(strlen(s))
	  *ms = atoi(s);
    return(YES);
}
/* c------------------------------------------------------------------------ */

char *dd_blank_fill(srs,n,dst)
  CHAR_PTR srs, dst;
  int n;
{
    int i;

    for(i=0; i < n && *srs; i++)
	  *(dst+i) = *srs++;

    for(; i < n; i++ )
	  *(dst+i) = ' ';

    return(dst);
}
/* c------------------------------------------------------------------------ */

char *
dd_chop(aa)
  char *aa;
{
    /* remove the line feed from the end of the string
     * if there is one
     */
    int nn;

    if(!aa || !(nn=strlen(aa)))
	  return(NULL);

    if(*(aa + (--nn)) == '\n')
	  *(aa +nn) = '\0';
    return(aa);
}
# ifdef obsolete
/* c------------------------------------------------------------------------ */

int dd_crack_file_name( type, time_stamp, radar, version, fn )
  char *type, *radar, *fn;
  long *time_stamp;
  int *version;
{
    char *delim=DD_NAME_DELIMITER;
    int i, t[6];
    char *a=fn, *b, str[64];
    long clock;
    struct tm tm;
    double d, d_time_stamp();
    DD_TIME dts;

    *version = *time_stamp = 0;
    *type = *radar = '\0';
    if(!a || !strlen(a))
	  return(NO);

    for(; *a != *delim;)
	  *type++ = *a++;
    *type = '\0';
    a++;
    if(!strlen(a))
	  return(NO);

    for(str[2]='\0',i=0; *a != *delim; i++,a+=2 ) {
	/* crack the date-time string */
	strncpy(str, a, 2);
	t[i] = atoi(str);
    }
    dts.year = t[0] > 1900 ? t[0] : t[0]+1900;
    dts.month = t[1];
    dts.day = t[2];
    dts.day_seconds = D_SECS(t[3], t[4], t[5], 0);
    *time_stamp = (long)d_time_stamp(&dts);
    a++;
    if(!strlen(a))
	  return(NO);

    /*
     * instrument name
     */
    for(;*a != *delim;)
	  *radar++ = *a++;
    *radar = '\0';
    a++;
    if(!strlen(a))
	  return(NO);

    /*
     * version number
     */
    for(b=str; *a && *a != *delim;)
	  *b++ = *a++;

    *b = '\0';
    *version = atoi(str);
    return(YES);
}
# else
/* c------------------------------------------------------------------------ */

int dd_crack_file_name( type, time_stamp, radar, version, fn )
  char *type, *radar, *fn;
  long *time_stamp;
  int *version;
{
    char *delim=DD_NAME_DELIMITER;
    double d, d_time_stamp();
    DD_TIME dts;
    char string_space[256], *str_ptrs[32];
    int nt, mon, day, hrs, min, secs, ival;
    char *aa, *bb, *cc;

    *version = *time_stamp = 0;
    *type = *radar = '\0';

    if(!fn || !strlen(fn))
	  return(NO);

    strcpy( string_space, fn );
    nt = dd_tokenz( string_space, str_ptrs, DD_NAME_DELIMITER );

    strcpy( type, str_ptrs[0] );
    strcpy( radar, str_ptrs[2] );

    /* time */
    dd_clear_dts( &dts );
    aa = str_ptrs[1];
    bb = aa + strlen( aa ) - 10; /*  ready to suck off all but year  */
    sscanf( bb, "%2d%2d%2d%2d%2d", &mon, &day, &hrs, &min, &secs );
    *bb = '\0';

    ival = atoi( aa );
    dts.year = ival > 1900 ? ival : ival +1900;
    dts.month = mon;
    dts.day = day;
    dts.day_seconds = D_SECS(hrs, min, secs, 0);
    *time_stamp = d_time_stamp(&dts);

    *version = atoi( str_ptrs[3] ); /* milliseconds and version num */

    return(YES);
}
# endif
# ifdef obsolete
/* c------------------------------------------------------------------------ */

int dd_crack_file_name_ms( type, time_stamp, radar, version, fn, ms )
  char *type, *radar, *fn;
  double *time_stamp;
  int *version, *ms;
{
    char *delim=DD_NAME_DELIMITER;
    int i, t[8];
    char *a=fn, *b, str[4];
    double d, d_time_stamp();
    DD_TIME dts;

    *version = *time_stamp = 0;
    *type = *radar = '\0';
    if(!a || !strlen(a))
	  return(NO);

    for(; *a != *delim;)
	  *type++ = *a++;
    *type = '\0';
    a++;
    if(!strlen(a))
	  return(NO);

    sscanf(a,"%2d%2d%2d%2d%2d%2d"
	   , &t[0], &t[1], &t[2], &t[3], &t[4], &t[5]);
    
    for(; *a && *a != *delim; a++);	/* move to after DateTime */
    a++;
    if(!strlen(a))
	  return(NO);

    /*
     * radar name
     */
    for(; *a != *delim;)
	  *radar++ = *a++;
    *radar = '\0';
    a++;
    if(!strlen(a))
	  return(NO);

    /*
     * version number/ms
     */
    for(b=str; *a && *a != *delim;)
	  *b++ = *a++;
    *b = '\0';
    *version = atoi(str);
    *ms = *version % 1000;
    *version /= 1000;

    dts.year = t[0] > 1900 ? t[0] : t[0]+1900;
    dts.month = t[1];
    dts.day = t[2];
    
    dts.day_seconds = D_SECS(t[3], t[4], t[5], *ms);
    *time_stamp = d_time_stamp(&dts);

    return(YES);
}
# else
/* c------------------------------------------------------------------------ */

int dd_crack_file_name_ms( type, time_stamp, radar, version, fn, ms )
  char *type, *radar, *fn;
  double *time_stamp;
  int *version, *ms;
{
    char *delim=DD_NAME_DELIMITER;
    double d, d_time_stamp();
    DD_TIME dts;
    char string_space[256], *str_ptrs[32];
    int nt, mon, day, hrs, min, secs, ival;
    char *aa, *bb, *cc;

    *version = *time_stamp = 0;
    *type = *radar = '\0';

    if(!fn || !strlen(fn))
	  return(NO);

    strcpy( string_space, fn );
    nt = dd_tokenz( string_space, str_ptrs, DD_NAME_DELIMITER );

    strcpy( type, str_ptrs[0] );
    strcpy( radar, str_ptrs[2] );

    /* time */
    dd_clear_dts( &dts );
    aa = str_ptrs[1];
    bb = aa + strlen( aa ) - 10; /*  ready to suck off all but year  */
    sscanf( bb, "%2d%2d%2d%2d%2d", &mon, &day, &hrs, &min, &secs );
    *bb = '\0';

    ival = atoi( aa );
    dts.year = ival > 1900 ? ival : ival + 1900;
    dts.month = mon;
    dts.day = day;

    *version = atoi( str_ptrs[3] ); /* milliseconds and version num */

    *ms = *version % 1000;
    *version /= 1000;
    dts.day_seconds = D_SECS(hrs, min, secs, *ms);

    *time_stamp = d_time_stamp(&dts);

    return(YES);
}
# endif
/* c------------------------------------------------------------------------ */

void dd_file_name( type, time_stamp, radar, version, fn )
  char *type, *radar, *fn;
  long time_stamp;
  int version;
{
    DD_TIME dts, *d_unstamp_time();

    dts.time_stamp = time_stamp;
    d_unstamp_time(&dts);

    sprintf( fn
	    , "%s%s%02d%02d%02d%02d%02d%02d%s%s%s%d"
	    , type
	    , DD_NAME_DELIMITER
	    , dts.year -1900
	    , dts.month
	    , dts.day
	    , dts.hour
	    , dts.minute
	    , dts.second
	    , DD_NAME_DELIMITER, radar
	    , DD_NAME_DELIMITER, version );
}
/* c------------------------------------------------------------------------ */

void dd_file_name_ms( type, time_stamp, radar, version, fn, ms )
  char *type, *radar, *fn;
  long time_stamp;
  int version, ms;
{
    int vzn=ms+version*1000;
    DD_TIME dts, *d_unstamp_time();

    dts.time_stamp = time_stamp;
    d_unstamp_time(&dts);

    sprintf( fn
	    , "%s%s%02d%02d%02d%02d%02d%02d%s%s%s%d"
	    , type
	    , DD_NAME_DELIMITER
	    , dts.year -1900
	    , dts.month
	    , dts.day
	    , dts.hour
	    , dts.minute
	    , dts.second
	    , DD_NAME_DELIMITER, radar
	    , DD_NAME_DELIMITER, vzn );

}
/* c------------------------------------------------------------------------ */

int dd_hrd16( buf, dd, flag, empty_run )
  char *buf;
  short *dd;
  int flag, *empty_run;
{
    /*
     * routine to unpacks actual data assuming MIT/HRD compression
     */
    short *ss=(short *)buf;
    int n, mark, wcount=0;

    while(*ss != 1) {
	n = *ss & MASK15;
	wcount += n;
	if( *ss & SIGN16 ) {	/* data! */
	    *empty_run = 0;
	    ss++;
	    for(; n--;) {
		*dd++ = *ss++;
	    }
	}	
	else {			/* fill with flags */
	    *empty_run = n;
	    ss++;
	    for(; n--;) {
		*dd++ = flag;
	    }
	}
    }
    return(wcount);
}
/* c------------------------------------------------------------------------ */

int dd_hrd16_uncompressx( ss, dd, flag, empty_run, wmax )
  short *ss, *dd;
  int flag, *empty_run, wmax;
{
    /*
     * routine to unpacks actual data assuming MIT/HRD compression where:
     * ss points to the first 16-bit run-length code for the compressed data
     * dd points to the destination for the unpacked data
     * flag is the missing data flag for this dataset that is inserted
     *     to fill runs of missing data.
     * empty_run pointer into which the number of missing 16-bit words
     *    can be stored. This will be 0 if the last run contained data.
     # wmax indicate the maximum number of 16-bit words the routine can
     *    unpack. This should stop runaways.
     */
    int i, j, k, n, mark, wcount=0;

    while(*ss != 1) {		/* 1 is an end of compression flag */
	n = *ss & 0x7fff;	/* nab the 16-bit word count */
	if(wcount+n > wmax) {
	    printf("Uncompress failure %d %d %d\n"
		   , wcount, n, wmax);
	    mark = 0;
	    break;
	}
	else {
	    wcount += n;		/* keep a running tally */
	}
	if( *ss & 0x8000 ) {	/* high order bit set implies data! */
	    *empty_run = 0;
	    ss++;
	    for(; n--;) {
		*dd++ = *ss++;
	    }
	}	
	else {			/* otherwise fill with flags */
	    *empty_run = n;
	    ss++;
	    for(; n--;) {
		*dd++ = flag;
	    }
	}
    }
    return(wcount);
}
/* c------------------------------------------------------------------------ */

int dd_hrd16LE_uncompressx( ss, dd, flag, empty_run, wmax )
  unsigned short *ss, *dd;
  int flag, *empty_run, wmax;
{
    /*
     * routine to unpacks actual data assuming MIT/HRD compression where:
     * ss points to the first 16-bit run-length code for the compressed data
     * dd points to the destination for the unpacked data
     * flag is the missing data flag for this dataset that is inserted
     *     to fill runs of missing data.
     * empty_run pointer into which the number of missing 16-bit words
     *    can be stored. This will be 0 if the last run contained data.
     # wmax indicate the maximum number of 16-bit words the routine can
     *    unpack. This should stop runaways.
     */
    int i, j, k, n, mark, wcount=0;
    unsigned short rlcw;
    unsigned char *aa, *bb;
    
    aa = (unsigned char *)&rlcw;

    for(;;) {	
       bb = (unsigned char *)ss;
       *aa = *(bb+1);
       *(aa+1) = *bb;		/* set run length code word "rlcw" */
       if(rlcw == 1) { break; }	/* 1 is the end of compression flag */

       n = rlcw & 0x7fff;	/* nab the 16-bit word count */
       if(wcount+n > wmax) {
	  printf("Uncompress failure %d %d %d\n"
		 , wcount, n, wmax);
	  mark = 0;
	  break;
       }
       else {
	  wcount += n;		/* keep a running tally */
       }
       if( rlcw & 0x8000 ) {	/* high order bit set implies data! */
	  *empty_run = 0;
	  ss++;
	  swack_short(ss, dd, n);
	  ss += n;
	  dd += n;
	  
       }	
       else {			/* otherwise fill with flags */
	  *empty_run = n;
	  ss++;
	  for(; n--;) {
	     *dd++ = flag;
	  }
       }
    }
    return(wcount);
}
/* c------------------------------------------------------------------------ */

int dd_nab_floats(str, vals)
  char *str;
  float *vals;
{
    int ii, jj, nt, dd_tokens();
    char string_space[256], *str_ptrs[32];

    strcpy(string_space, str);
    nt = dd_tokens(string_space, str_ptrs);

    for(ii=0; ii < nt; ii++) {
	jj = sscanf(str_ptrs[ii], "%f", vals+ii);
    }
    return(0);
}
/* c------------------------------------------------------------------------ */

int dd_nab_ints(str, vals)
  char *str;
  int *vals;
{
    int ii, jj, nt;
    char string_space[256], *str_ptrs[32];

    strcpy(string_space, str);
    nt = dd_tokens(string_space, str_ptrs);

    for(ii=0; ii < nt; ii++) {
	jj = sscanf(str_ptrs[ii], "%d", vals+ii);
    }
    return(0);
}
/* c------------------------------------------------------------------------ */

int dd_rdp_uncompress8( buf, dd, bad_val)
  unsigned char *buf, *dd, bad_val;
{
    /*
     * routine to unpacks actual data assuming MIT/HRD compression
     */
    unsigned char *uc=buf;
    int count=0, n;

    while (*uc != 0 || *uc != 1 ) {
	n = *uc & MASK7;
	if( *uc & SIGN8 ) {	/* data! */
	    for(; n--;) {
		count++;
		*dd++ = *uc++;
	    }
	}	
	else {			/* fill with nulls */
	    uc++;
	    for(; n--;) {
		count++;
		*dd++ = bad_val;
	    }
	}
    }
    return(count);
}
/* c------------------------------------------------------------------------ */

int
dd_scan_mode(str)
  char *str;
{
    char uc_scan_mode[16];
    int max_scan_mode = 10, ii, nn;
    char *aa;

    if( !str )
      { return(-1); }
    if( !(nn = strlen(str)))
      { return(-1); }
    
    aa = uc_scan_mode;
    strcpy( aa, str );
    for(; !*aa ; aa++ )
      {	*aa = (char)toupper((int)(*aa)); }

    for( ii = 0; ii < max_scan_mode; ii++ )
      {
	if( !strcmp( uc_scan_mode, Stypes[ii] ))
	  { return( ii ); }
      }
    return( -1 );
}
/* c------------------------------------------------------------------------ */

char *
dd_scan_mode_mne(scan_mode, str)
  int scan_mode;
  char *str;
{
    int max_scan_mode = 10;

    scan_mode = scan_mode < 0 || scan_mode > max_scan_mode
	  ? max_scan_mode : scan_mode;

    strcpy(str, Stypes[scan_mode]);
    return(str);
}
/* c------------------------------------------------------------------------ */

char *
dd_radar_type_ascii(radar_type, str)
  int radar_type;
  char *str;
{
    int max_radar_types = 10;
    static char *Rtypes[] = { "GROUND", "AIR_FORE", "AIR_AFT", "AIR_TAIL"
			      , "AIR_LF", "SHIP", "AIR_NOSE", "SATELLITE"
			      , "LIDAR_MOVING", "LIDAR_FIXED", "UNKNOWN"
			      };

    radar_type = radar_type < 0 || radar_type > max_radar_types
	  ? max_radar_types : radar_type;

    strcpy(str, Rtypes[radar_type]);
    return(str);
}
/* c------------------------------------------------------------------------ */

char *
dd_unquote_string(uqs, qs)
char *qs, *uqs;
{
    char *a=uqs;
    /*
     * remove the double quotes from either end of the string
     */
    for(; *qs; qs++)
	  if(*qs != '"') *a++ = *qs;
    *a = '\0';
    return(uqs);
}
/* c------------------------------------------------------------------------ */

double dorade_time_stamp( vold, ryib, dts )
  struct volume_d *vold;
  struct ray_i *ryib;
  DD_TIME *dts;
{

    double d, d_time_stamp();

    dts->year = vold->year > 1900 ? vold->year : vold->year +1900;
    dts->month = dts->day = 0;
    dts->day_seconds = (ryib->julian_day-1)*SECONDS_PER_DAY
	  +D_SECS(ryib->hour, ryib->minute, ryib->second
		  , ryib->millisecond);
    d = d_time_stamp(dts);
	  
    return(d);
 }
/* c----------------------------------------------------------------------- */

char *dts_print(dts)
  DD_TIME *dts;
{
    static char str[32];

    sprintf(str, "%02d/%02d/%02d %02d:%02d:%02d.%03d"
	   , dts->month
	   , dts->day
	   , dts->year
	   , dts->hour
	   , dts->minute
	   , dts->second
	   , dts->millisecond
	   );
    return(str);
}
/* c------------------------------------------------------------------------ */

double eldora_time_stamp( vold, ryib )
  struct volume_d *vold;
  struct ray_i *ryib;
{
    double d, d_time_stamp(), dorade_time_stamp();
    DD_TIME dts;

    d = dorade_time_stamp(vold, ryib, &dts);
	  
    return(d);
}
/* c------------------------------------------------------------------------ */

int fb_read( fin, buf, count )
  int fin, count;
  char *buf;
{
    /* fortran-binary read routine
     */
    long int size_rec=0, rlen1, rlen2=0, nab;

    /* read the record size */
    rlen1 = read (fin, &nab, sizeof(nab));

    if( rlen1 < sizeof(nab))
	  return(rlen1);

    if(LittleEndian) {
       swack4(&nab, &size_rec);
    }
    else {
       size_rec = nab;
    }

    if( size_rec > 0 ) {
	/* read the record
	 * (the read may be less than the size of the record)
	 */
	rlen2 = size_rec <= count ? size_rec : count;

	rlen1 = read (fin, buf, rlen2);	/* read it! */
	if( rlen1 < 1 )
	      return(rlen1);
	rlen2 = rlen1 < size_rec ?
	      size_rec-rlen1 : 0; /* set up skip to end of record */
    }
    else
	  rlen1 = 0;
    
    rlen2 += sizeof(size_rec);

    /* skip thru the end of record */
    rlen2 = lseek( fin, rlen2, 1 );
    return(rlen1);
}
/* c------------------------------------------------------------------------ */

int fb_write( fout, buf, count )
  int fout, count;
  char *buf;
{
    /* fortran-binary write routine
     */
    long int size_rec=count, rlen1, rlen2=0, blip;

    if(LittleEndian) {
       swack4(&size_rec, &blip);
    }
    else {
       blip = size_rec;
    }
    /* write the record length */
    rlen1 = write(fout, &blip, sizeof(blip));
    if( rlen1 < sizeof(size_rec))
	  return(rlen1);

    if( size_rec > 0 ) {
	/* write the record */
	rlen1 = write (fout, buf, size_rec);
	if( rlen1 < 1 )
	      return(rlen1);
    }
    /* write the record length */
    rlen2 = write (fout, &blip, sizeof(blip));
    if( rlen2 < sizeof(blip))
	  return(rlen2);

    else if(size_rec == 0)
	  return(0);
    else
	  return(rlen1);
}
/* c------------------------------------------------------------------------ */

char *
fgetsx(aa, nn, stream)
  unsigned char *aa;
  int nn;
  FILE *stream;
{
    /* similar to fgets()
     */
    int ii, jj=0;
    unsigned char *c=aa;

    for(; nn--; c++,jj++) {
	ii = fgetc(stream);
	if(ii == EOF) {
	    if(jj) {
		*c = '\0';
		return( (char *) aa);
	    }
	    return(NULL);
	}
	*c = ii;
	if(ii == '\n') {
	    *(++c) = '\0';
	    return( (char *) aa);
	}
    }
    return(NULL);
}
/* c------------------------------------------------------------------------ */

long
file_time_stamp(fn)
  char *fn;
{
    long time_stamp;
    int version;
    char type[8], radar[12];

    dd_crack_file_name( type, &time_stamp, radar, &version, fn );

    return(time_stamp);
}
/* c------------------------------------------------------------------------ */

char *
fstring(line, nn, stream)
  char *line;
  int nn;
  FILE *stream;
{
    /* read in lines from a file and remove leading blanks
     * trailing blanks and line feeds
     */
    int ii, jj;
    unsigned char *aa, *cc;

    for(aa=cc=(unsigned char*)line ;; aa=cc=(unsigned char*)line) {
	for(jj=1 ;;) {
	    ii = fgetc(stream);
	    if(ii == EOF || ii == '\n') {
		*cc = '\0';
		break;
	    }
	    if(jj < nn) {
		*cc++ = ii;
		jj++;
	    }
	}
	for(; aa < cc && *aa == ' '; aa++); /* leading blanks */
	for(; aa < cc && *(cc-1) == ' '; *(--cc) = '\0'); /* trailing blanks */
	if(aa != cc)
	      return( (char *) aa);
	if(ii == EOF)
	      return(NULL);
    }
}
/* c------------------------------------------------------------------------ */

char *get_tagged_string( tag )
  char *tag;
{
    int i;

    for(i=0; i < num_tags; i++ ) {
	if(!strcmp(tag,tags[i])) {
	    return(tagged_strings[i]);
	}
    }
    return(0);
}
/* c------------------------------------------------------------------------ */

int gp_read(fid, buf, size, io_type)
  int fid, size, io_type;
  char *buf;
{
    /* general purpose write
     */
    int n=size;

    if(io_type == FB_IO ) { /* fortran binary */
	n = fb_read( fid, buf, size );
    }
    else if(size > 0 ) {
	n = read(fid, buf, size);
    }
    return(n);
}
/* c------------------------------------------------------------------------ */

int gp_write(fid, buf, size, io_type)
  int fid, size, io_type;
  char *buf;
{
    /* general purpose write
     */
    int n;
    struct mtop op;

    if(io_type == FB_IO ) { /* fortran binary */
	n = fb_write( fid, buf, size );
    }
    else if(size > 0 ) {
	n = write(fid, buf, size);
    }
    else if(io_type == BINARY_IO) { /* pure binary output */
	/* since size == 0 this is a noop */
	return(size);
    }    
    else {			/* assume tape io */
	if(size != 0)
	      return(size);
	/* need to write a physical eof */
	op.mt_op = MTWEOF;
	op.mt_count = 1;
	if (ioctl(fid, MTIOCTOP, &op) < 0) {
	    perror ("Mag tape WEOF");
	}
	n = 0;
    }
    return(n);
}
/* c------------------------------------------------------------------------ */

int in_sector(ang, ang1, ang2)
  float ang, ang1, ang2;
{
    if(ang1 > ang2) return(ang >= ang1 || ang < ang2);

    return(ang >= ang1 && ang < ang2);
}
/* c------------------------------------------------------------------------ */

char *put_tagged_string( tag, string )
  char *tag, *string;
{
    int i, j, m=strlen(tag), n=strlen(string);

    if(!m)
	  return(0);
    
    /* see if this tag is there already */
    for(i=0,j= -1; i < num_tags; i++ ) {
	if(!strcmp(tag,tags[i])) {
	    free(tagged_strings[i]);
	    break;
	}
    }    
    if(i >= num_tags) {
	num_tags++;		/* new entry */
	tags[i] = (char *)malloc(m+1);
    }
    tagged_strings[i] = (char *)malloc(n+1);
    strcpy(tags[i], tag);
    strcpy(tagged_strings[i], string);
    return(tagged_strings[i]);
}
/* c------------------------------------------------------------------------ */

int se_free_raqs()
{
    int ii;
    struct running_avg_que *raq=top_raq;

    if(!top_raq)
	  return(0);
    for(ii=0; raq; ii++,raq->in_use=NO,raq=raq->next);
    return(ii);
}
/* c------------------------------------------------------------------------ */

struct running_avg_que *
se_return_ravgs(nvals)
  int nvals;
{
    /* routine to return a pointer to the next free
     * running average struct/queue set up to average "nvals"
     */
    struct que_val *qv, *qv_next, *qv_last;
    struct running_avg_que *raq, *last_raq;
    int ii, jj, kk, mark;

    /* look for a free raq
     */
    for(last_raq=raq=top_raq; raq; raq=raq->next) {
	last_raq = raq;
	if(!raq->in_use)
	      break;
    }
    if(!raq) {			/* did not find a free que so
				 * make one! */
	raq = (struct running_avg_que *)
	      malloc(sizeof(struct running_avg_que));
	memset(raq, 0, sizeof(struct running_avg_que));
	if(last_raq) {
	    last_raq->next = raq;
	    raq->last = last_raq;
	}
	else {
	    top_raq = raq;
	}
	/* raq->next is deliberately left at 0 or NULL */
	top_raq->last = raq;
    }
    raq->sum = 0;
    raq->in_use = YES;

    if(raq->num_vals == nvals) {
	for(qv=raq->at,jj=0; jj < raq->num_vals; jj++,qv=qv->next) {
	    qv->d_val = qv->f_val = qv->val = 0;
	}
	return(raq);
    }
    if(raq->num_vals) {	/* free existing values */
	for(qv=raq->at,jj=0; jj < raq->num_vals; jj++) {
	    qv_next = qv->next;
	    free(qv);
	    qv = qv_next;
	}
    }
    /* now create a new que of the correct size
     */
    raq->num_vals = nvals;
    raq->rcp_num_vals = 1./nvals;
    for(jj=0; jj < raq->num_vals; jj++) {
	qv = (struct que_val *)
	      malloc(sizeof(struct que_val));
	memset(qv, 0, sizeof(struct que_val));
	if(!jj)
	      raq->at = qv;
	else {
	    qv->last = qv_last;
	    qv_last->next = qv;
	}
	qv->next = raq->at;
	raq->at->last = qv;
	qv_last = qv;
    }
    return(raq);
}
/* c------------------------------------------------------------------------ */

char *slash_path( dst, srs )
  char *dst, *srs;
{
    int n;

    if(srs && (n=strlen(srs))) {
	strcpy(dst,srs);
	if(dst[n-1] != '/' ) {
	    dst[n] = '/';
	    dst[n+1] = '\0';
	}
    }
    else
	  *dst = '\0';
    return(dst);
}
/* c------------------------------------------------------------------------ */

void slm_dump_list(slm)
  struct solo_list_mgmt *slm;
{
    char **ptr;
    int nl;

    if(!slm)
	  return;

    if(!(ptr = slm->list))
	  return;

    nl = slm->num_entries;

    for(; nl-- > 0;) {
       solo_message(*ptr++);
       solo_message("\n");
    }
}
/* c------------------------------------------------------------------------ */

void slm_print_list(slm)
  struct solo_list_mgmt *slm;
{
    char **pt, **ptrs=slm->list;
    int ii, jj, kk, ll=0, mm, nl, max=gri_max_lines(), last_mm;
    int nn, ival;
    float val;
    char str[128];

    if(!slm)
	  return;
    nl = slm->num_entries;

    if(nl < max) max = nl;

    last_mm = mm = max;

    for(;;) {
	for(pt=ptrs+ll; mm--; pt++) {
	    printf("%s\n", *pt);
	}
	printf("Hit <return> to continue ");
	nn = getreply(str, sizeof(str));
	if(cdcode(str, nn, &ival, &val) != 1 || ival == 1) {
	    return;
	}
	mm = max;
	if(ival > 0) {
	    if(ll +ival +mm > nl) {
		ll = nl -mm;
	    }
	    else 
		  ll += ival;
	}
	else if( ival < 0) {
	    ll = ll +ival < 0 ? 0 : ll + ival;
	}
	else {
	    if(ll+last_mm == nl)
		  return;
	    ll += last_mm;
	    if(ll +mm > nl) {
		mm = nl -ll;
	    }
	}
	last_mm = mm;
    }
}
/* c------------------------------------------------------------------------ */

void solo_add_list_entry(which, entry, len)
  struct solo_list_mgmt *which;
  char *entry;
  int len;
{
    int ii;
    char *a, *c, *str_terminate();

    if(!which)
	  return;

    if(!len) len = strlen(entry);

    if(which->num_entries >= which->max_entries) {
	which->max_entries += 30;
	if(which->list) {
	    which->list = (char **)realloc
		  (which->list, which->max_entries*sizeof(char *));
	}
	else {
	    which->list = (char **)malloc(which->max_entries*sizeof(char *));
	}
	for(ii=which->num_entries; ii < which->max_entries; ii++) {
	    *(which->list+ii) = a = (char *)malloc(which->sizeof_entries+1);
	    *a = '\0';
	}
    }
    len = len < which->sizeof_entries ? len : which->sizeof_entries;
    c = *(which->list+which->num_entries++);

    if(a=entry) {
	for(; *a && len--; *c++ = *a++);
    }
    *c = '\0';
}
/* c------------------------------------------------------------------------ */

char *
solo_list_entry(which, entry_num)
  struct solo_list_mgmt *which;
  int entry_num;
{
    char *c;

    if(!which || entry_num >= which->num_entries || entry_num < 0)
	  return(NULL);

    c = *(which->list+entry_num);
    return(c);
}
/* c------------------------------------------------------------------------ */

void solo_list_remove_dups(slm)
  struct solo_list_mgmt *slm;
{
    /* remove duplicate strings from the list
     */
    int ii, jj, nd;
    void solo_list_remove_entry();

    if(!slm || slm->num_entries <= 0)
	  return;

    nd = slm->num_entries-1;

    for(ii=0; ii < slm->num_entries-1; ii++) {
	for(; ii < slm->num_entries-1 &&
	    !strcmp(*(slm->list +ii), *(slm->list +ii +1));) {
	    solo_list_remove_entry(slm, ii+1, ii+1);
	}
    }
    return;
}
/* c------------------------------------------------------------------------ */

void solo_list_remove_entry(slm, ii, jj)
  struct solo_list_mgmt *slm;
  int ii, jj;
{
    /* remove a span of entries where ii and jj are entry numbers
     */
    int kk, nn, nd;
    char *keep;
    long file_time_stamp();

    if(!slm || slm->num_entries <= 0 || ii < 0 || ii >= slm->num_entries)
	  return;
    nd = slm->num_entries-1;

    if(jj >= slm->num_entries)
	  jj = nd;
    else if(jj < ii)
	  jj = ii;

    nn = jj - ii + 1;

    for(; nn--; ) {		/* keep shifting the entries up */
	keep = *(slm->list +ii);
	for(kk=ii; kk < nd; kk++) {
	    *(slm->list +kk) = *(slm->list +kk +1);
	}
	strcpy(keep, " ");
	*(slm->list +(nd--)) = keep;
    }
    slm->num_entries = nd +1;
    return;
}
/* c------------------------------------------------------------------------ */

void solo_list_sort_file_names(slm)
  struct solo_list_mgmt *slm;
{
    int ii, jj;
    char *keep;
    long file_time_stamp();

    if(!slm || slm->num_entries <= 0)
	  return;

    for(ii=0; ii < slm->num_entries-1; ii++) {
	for(jj=ii+1; jj < slm->num_entries; jj++) {
	    if(file_time_stamp(*(slm->list+jj))
	       < file_time_stamp(*(slm->list+ii))) {
		keep = *(slm->list+ii);
		*(slm->list+ii) = *(slm->list+jj);
		*(slm->list+jj) = keep;
	    }
	}
    }
    return;
}
/* c------------------------------------------------------------------------ */

struct solo_list_mgmt *
solo_malloc_list_mgmt(sizeof_entries)
  int sizeof_entries;
{
    struct solo_list_mgmt *slm;

    slm = (struct solo_list_mgmt *)malloc(sizeof(struct solo_list_mgmt));
    memset(slm, 0, sizeof(struct solo_list_mgmt));
    slm->sizeof_entries = sizeof_entries;
    return(slm);
}
/* c------------------------------------------------------------------------ */

struct point_in_space *
solo_malloc_pisp()
{
    struct point_in_space *next;

    next = (struct point_in_space *)
	  malloc(sizeof(struct point_in_space));
    memset(next, 0, sizeof(struct point_in_space));

    strcpy(next->name_struct, "PISP");
    next->sizeof_struct = sizeof(struct point_in_space);
    return(next);
}
/* c------------------------------------------------------------------------ */

char *solo_modify_list_entry(which, entry, len, entry_num)
  struct solo_list_mgmt *which;
  char *entry;
  int len;
{
    int ii;
    char *a, *c, *str_terminate();

    if(!which || entry_num > which->num_entries || entry_num < 0)
	  return(NULL);

    if(!len) len = strlen(entry);

    if(entry_num == which->num_entries) { /* this entry doesn't exist
					   * but it can be added */
	solo_add_list_entry(which, entry, len);
	a = *(which->list+entry_num);
	return(a);
    }
    len = len < which->sizeof_entries ? len : which->sizeof_entries;
    a = c = *(which->list+entry_num);

    for(a=entry; *a && len--; *c++ = *a++);
    *c = '\0';
    return(a);
}
/* c------------------------------------------------------------------------ */

void solo_reset_list_entries(which)
  struct solo_list_mgmt *which;
{
    int ii;

    if(!which || !which->num_entries)
	  return;

    for(ii=0; ii < which->num_entries; ii++) {
	strcpy(*(which->list +ii), " ");
    }
    which->num_entries = 0;
}
/* c------------------------------------------------------------------------ */

void solo_unmalloc_list_mgmt(slm)
  struct solo_list_mgmt *slm;
{
    int ii;

    if(!slm)
	  return;

    for(ii=0; ii < slm->max_entries; ii++) {
	free(*(slm->list +ii));
    }
    free(slm);
}
/* c------------------------------------------------------------------------ */

void solo_sort_slm_entries (slm)
  struct solo_list_mgmt *slm;
{
   char *aa, *bb;
   int ii, jj, nn = slm->num_entries;
   char str[256];
   
   for(ii=0; ii < nn-1; ii++) {
      aa = solo_list_entry(slm, ii);
      for(jj=ii+1; jj < nn; jj++) {
	 bb = solo_list_entry(slm, jj);
	 if(strcmp(bb, aa) < 0) {
	    strcpy (str, aa);
	    solo_modify_list_entry(slm, bb, strlen (bb), ii);
	    aa = solo_list_entry(slm, ii);
	    solo_modify_list_entry(slm, str, strlen (str), jj);
	 }
      }
   }
}
  
/* c------------------------------------------------------------------------ */

char *str_terminate(dst, srs, n)
  CHAR_PTR srs, dst;
  int n;
{
    int m=n;
    char *a=srs, *c=dst;

    *dst = '\0';

    if(n < 1)
	  return(dst);
    /*
     * remove leading blanks
     */
    for(; m && *a == ' '; m--,a++);
    /*
     * copy m char or to the first null
     */
    for(; m-- && *a; *c++ = *a++);
    /*
     * remove trailing blanks
     */
    for(*c='\0'; dst < c && *(c-1) == ' '; *(--c)='\0');

    return(dst);
}
/* c------------------------------------------------------------------------ */

long
time_now()
{
    int i;
    struct timeval tp;
    struct timezone tzp;

    i = gettimeofday(&tp,&tzp);
    return((long)tp.tv_sec);
}
/* c------------------------------------------------------------------------ */

time_t
todays_date(date_time)
  short date_time[];
{
    struct tm *tm, *localtime();
    int i;
    struct timeval tp;
    struct timezone tzp;

    i = gettimeofday(&tp,&tzp);
    tm = localtime((time_t *) &tp.tv_sec);
    date_time[0] = tm->tm_year;
    date_time[1] = tm->tm_mon+1;;
    date_time[2] = tm->tm_mday;
    date_time[3] = tm->tm_hour;
    date_time[4] = tm->tm_min;
    date_time[5] = tm->tm_sec;

    return((long)tp.tv_sec);
}
/* c------------------------------------------------------------------------ */

struct run_sum *init_run_sum( length, short_len )
    int length;
{
    struct run_sum * rs;
    rs = (struct run_sum *)malloc( sizeof( struct run_sum ));
    memset( rs, 0, sizeof( struct run_sum ));
    rs->vals = (double *)malloc( length * sizeof(double));
    memset( rs->vals, 0, length * sizeof(double));
    rs->run_size = length;
    rs->index_lim = length -1;
    if( short_len > 0 ) {
        rs->short_size = short_len < length ? short_len : length;
    }
    return( rs );
}
/* c------------------------------------------------------------------------ */

void reset_running_sum( rs )
    struct run_sum *rs;
{
    rs->count = 0;
    memset( rs->vals, 0, rs->run_size * sizeof(double));
    rs->sum = 0;
    rs->short_sum = 0;
}
/* c------------------------------------------------------------------------ */

double short_running_sum( rs )
    struct run_sum *rs;
{
    return( rs->short_sum );
}
/* c------------------------------------------------------------------------ */

double running_sum( rs, val )
    struct run_sum *rs;
    double val;
{
    int ii, nn = ++rs->count, length = rs->run_size;
    int ndx = rs->index_next;

    rs->sum -= *( rs->vals + ndx );
    rs->sum += val;
    *(rs->vals + ndx ) = val;
    if( rs->short_size ) {
	if( nn > rs->short_size ) {
	    ii = (ndx - rs->short_size +length) % length;
	    rs->short_sum -= *( rs->vals + ii );
	}
	rs->short_sum += val;
    }
    rs->index_next = ++ndx % length;
    return( rs->sum );
}

/* c------------------------------------------------------------------------ */

double avg_running_sum( rs )
    struct run_sum *rs;
{
    double d;

    if( rs->count > 0 ) {
	d = rs->count > rs->run_size ? rs->run_size : rs->count;
	return( rs->sum/d );
    }
    else {
	return(0);
    }
}

/* c------------------------------------------------------------------------ */

double short_avg_running_sum( rs )
    struct run_sum *rs;
{
    double d;
    int nn = rs->count;

    if( nn > 0 ) {
	d = nn > rs->short_size ? rs->short_size : nn;
	return( rs->short_sum/d );
    }
    else {
	return(0);
    }
}

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

