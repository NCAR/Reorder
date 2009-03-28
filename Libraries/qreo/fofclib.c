
/* 	$Id: fofclib.c,v 1.1.1.1 2002/08/07 16:03:06 oye Exp $	 */

/* This file contains the following C routines
 * 
 * andmsk_
 * bit_
 * copy8_
 * copy16_
 * iget4_
 * iget8_
 * iget16_
 * ischar_
 * jget16_
 * orit_
 * pak16_
 * put8_
 * put16_
 * sbit_
 * titan_string
 * upk16_
 * upk16s_
 * xmtweof_
 * xtrkt8_
 */

# ifdef LITTLENDIAN
# else
# endif

# include <sys/types.h>
# include <sys/file.h>
# include <stdio.h>
# include <sys/stat.h>
# include <fcntl.h>
# include <time.h>
# include <stdlib.h>
# include <string.h>

# ifdef titan

/*
 * The titan uses string descriptors, which drastically confuses the way
 * that we handle characters here.  This routine tries to figure out whether
 * we are seeing a descriptor or a straight pointer, and does the right thing.
 */

struct titan_descr
{
	char *string;
	int len;
};

# endif


/* c----------------------------------------------------------------------- */
/* c----------------------------------------------------------------------- */

int fofreodebug_( name, len )
     void * name;
     int * len;
{
  char * aa = getenv( "REO_DEBUG" );
  *len = 0;
  if( !aa || !strlen( aa ))
    { return( 0 ); }
  strcpy( name, aa );
  *len = strlen( aa );
  return( strlen( aa ));
}

/* c----------------------------------------------------------------------- */

void fofdie_()
{
  exit(-1);
}

/* c----------------------------------------------------------------------- */

void foflocaltime_( dtm )
     int * dtm;
{
  struct tm *tm;
  time_t tt = time( &tt );
  tm = localtime( &tt );
  *(dtm +0) = tm->tm_year;
  *(dtm +1) = tm->tm_mon+1;
  *(dtm +2) = tm->tm_mday;
  *(dtm +3) = tm->tm_hour;
  *(dtm +4) = tm->tm_min;
  *(dtm +5) = tm->tm_sec;
}

/* c----------------------------------------------------------------------- */
# ifdef titan
# define andmsk_  	ANDMSK
# endif

void andmsk_( vec, mask, n, compl )
  int vec[], *mask, *n, *compl;
{
    int i=0;
    
    if( *compl )
	 for(; i < *n; i++ )
	      vec[i] &= ~(*mask);
    else
	 for(; i < *n; i++ )
	      vec[i] &= *mask;
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define bit_     	BIT
# endif

int bit_( no, word )
  int *no, *word;
{
    return((*word >>(*no)) & 1 );
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define copy8_ COPY8
# endif

void copy8_( s, i, n, d, j ) /* copy n 16 bit words */
  char *s, *d;
  int *i, *n, *j;
{
    int k=*n;
    
    for( s+=*i-1, d+=*j-1; k-- > 0;)
	 *d++ = *s++;
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define copy16_  	COPY16
# endif

void copy16_( s, i, n, d, j ) /* copy n 16 bit words */
  unsigned short *s, *d;
  int *i, *n, *j;
{
    int k=*n;

    for( s+=*i-1, d+=*j-1; k-- > 0;)
	 *d++ = *s++;
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define iget4_   	IGET4
# endif

int iget4_( srs, ith )  /* gimme the ith nibble of srs */
  unsigned char srs[];
  int *ith;
{
    int cn = *ith-1, wd;
    
    wd = srs[cn/2];
    if( cn & 1 )		/* odd implies lower nibble */
	 return( wd & 15 );
    else
	 return((wd>>4) & 15);
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define iget8_   	IGET8
# endif

int iget8_( srs, ith )  /* return the ith byte of srs */
  unsigned char srs[];
  int *ith;
{
    return( srs[*ith-1]);
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define iget16_  	IGET16
# endif

int iget16_( srs, ith )  /* return the ith 16 bit word of srs */
  short *srs;
  int *ith;

# ifdef LITTLENDIAN
{
  short s;
  int cn = (*ith -1) * sizeof(short);
  char *aa= (char *)srs, *bb = (char *)&s;
  aa += cn;

  *bb = *(aa+1);
  *(bb+1) = *aa;
  return((int)s);
}
# else
{
    return( srs[*ith-1]);
}
# endif
/* c----------------------------------------------------------------------- */
# ifdef titan
# define iget32_  	IGET32
# endif

int iget32_( srs, ith )  /* return the ith 32 bit word of srs */
  int srs[];
  int *ith;

# ifdef LITTLENDIAN
{
  short s;
  char *aa, *bb;
  bb = (char *)&s;
  aa = (char *)srs;

  *bb++ = *(aa+3);
  *bb++ = *(aa+2);
  *bb++ = *(aa+1);
  *bb = *aa;
  return((int)s);
}
# else
{
    return( srs[*ith-1]);
}
# endif
/* c----------------------------------------------------------------------- */
# ifdef titan
# define swap32_  	SWAP32
# endif

int swap32_( val )
  int val;
{
   int s;
   char *aa, *bb;
   bb = (char *)&s;
   aa = (char *)&val;
   
   *bb++ = *(aa+3);
   *bb++ = *(aa+2);
   *bb++ = *(aa+1);
   *bb = *aa;
   return((int)s);
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define ischar_ ISCHAR
# endif

ischar_( s ) /* is this character descriptor */
  char *s;
{
# ifdef titan
    if (s[0] == 0x10)
	 return(1);
    else
	 return(0);
# endif    
    return(0);
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define jget16_  	JGET16
# endif

int jget16_( srs, ith )  /* return the ith 16 bit word of srs */
  unsigned short srs[];
  int *ith;
{
    return( srs[*ith-1]);
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define orit_    	ORIT       
# endif

void orit_( vec, mask, n )
  int *vec, *mask, *n;
{
    int i=*n;
    
    for(; i-- > 0;)
	 *vec++ |= *mask++;
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define pak16_   	PAK16      
# endif

void pak16_( kbuf, vec, offs, n )
  short *kbuf;
  int *vec;
  int *offs, *n;
# ifdef LITTLENDIAN
{
   int nn = *n;
   unsigned char *aa, *bb;
   bb = (unsigned char *)kbuf;
   aa = (unsigned char *)vec;
   bb += (*offs) * sizeof(short);

   for( ; nn--; aa += sizeof(int)) {
      *bb++ = *(aa +1);
      *bb++ = *aa;
   }
}
# else
{
    int i=*n;
    
    for( kbuf+=*offs; i-- > 0;)
	 *kbuf++ = (short)*vec++;
}
# endif
/* c----------------------------------------------------------------------- */
# ifdef titan
# define put8_    	PUT8       
# endif

void put8_( dst, ith, val )
  char dst[];
  int *ith, *val;
{
    dst[*ith-1] = *val;
}
/* c----------------------------------------------------------------------- */
# ifdef titan
# define put16_   	PUT16      
# endif

void put16_( dst, ith, val )
  unsigned short dst[];
  int *ith, *val;

# ifdef LITTLENDIAN
{
   int cn = (*ith -1) * sizeof(short);
   char *aa = (char *)val, *bb = (char *)( dst +(*ith) -1 );

   *bb++ = *(aa+1);
   *bb = *aa;
}
# else
{
    dst[*ith-1] = (*val & 0xffff);
}
# endif
/* c----------------------------------------------------------------------- */

# ifdef titan

unsigned char *
titan_string (s)
unsigned char *s;
{
	int i;
	static unsigned char copy[1024];
	struct titan_descr *tdp = (struct titan_descr *) s;
/*
 * Look for non-ascii stuff as a hint that we are looking at a pointer to
 * the string, not the string itself.
 */
# ifdef notdef
	for (i = 0; i < 8; i++)
		if (s[i] < '\t' || s[i] & 0x80)
# endif
		if (s[0] == 0x10)
		{
			strncpy (copy, tdp->string, tdp->len);
			copy[tdp->len] = '\0';
			return (copy);
		}
	return (s);
}



# endif
/* c----------------------------------------------------------------------- */
# ifdef titan
# define upk16_   	UPK16      
# endif

void upk16_( kbuf, vec, offs, n ) /* does no sign extension */
  unsigned short *kbuf;
  int *vec;
  int *offs, *n;
# ifdef LITTLENDIAN
{
   int nn = *n;
   unsigned char *aa = (unsigned char *)vec, *bb = (unsigned char *)kbuf;
   memset( vec, 0, nn*sizeof(int));
   bb += (*offs) * sizeof(short);

   for( ; nn--; aa += sizeof(long)) {
      *(aa +1) = *bb++;
      *aa = *bb++;
   }
}
# else
{
    int i=*n;
    
    for( kbuf+=*offs; i-- > 0;)
	 *vec++ = (int)*kbuf++;
}
# endif
/* c----------------------------------------------------------------------- */
# ifdef titan
# define upk16s_  	UPK16S     
# endif

void upk16s_( kbuf, vec, offs, n ) /* with sign extension */
  short *kbuf;
  int *vec;
  int *offs, *n;
# ifdef LITTLENDIAN
{
   int nn = *n;
   unsigned char *aa = (unsigned char *)vec, *bb = (unsigned char *)kbuf;
   memset( vec, 0, nn*sizeof(int));
   bb += (*offs) * sizeof(short);

   for( ; nn--; aa += sizeof(int)) {
      *(aa +1) = *bb++;
      *aa = *bb++;
   }
   for( nn = *n; nn-- ; vec++ ) {
      if( *vec > 32767 )
        { *vec -= 65536; }
   }
}
# else
{
    int i=*n;
    
    for( kbuf+=*offs; i-- > 0;)
	 *vec++ = (int)*kbuf++;
}
# endif

/* c----------------------------------------------------------------------- */
# ifdef titan
# define xtrkt8_  	XTRKT8
# endif

xtrkt8_( sb, db, b1, bo, nb )
  u_char sb[];
  int db[], *b1, *bo, *nb;
{
    int i, j;

    for( i=*b1-1,j=0; j < *nb; i+=*bo, j++ )
	 db[j] = sb[i];
}
/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c------------------------------------------------------------------------ */

/* c----------------------------------------------------------------------- */
/* c----------------------------------------------------------------------- */
/* c----------------------------------------------------------------------- */

