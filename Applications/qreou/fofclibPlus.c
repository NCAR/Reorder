/* 	$Id: fofclibPlus.c,v 1.1.1.1 2002/08/07 16:03:06 oye Exp $	 */

# include <sys/types.h>
# include <sys/file.h>
# include <stdio.h>
# include <sys/stat.h>
# include <fcntl.h>

# define NDRIVE 20	/* Must be bigger than 8 (to use device /dev/rmt8) */
static int Fds[NDRIVE] = { 0 };
static int Status[NDRIVE] = { 0 };
static int Nwords[NDRIVE] = { 0 };
static char last_name[256];

/*
 * The old status codes from way back in the dark ages.
 */
# define TS_OK	0
# define TS_EOF	1
# define TS_ERROR 2
# define TS_EOT 3

/* c----------------------------------------------------------------------- */

int mtfmnt_ (unit, file)
  int *unit;
  char *file;
  /*
   * Mount a file "tape"
   */
{
   char *trans, *getenv (), *strchr (), *strrchr (), *slash;
   char *dest = last_name, *type = ".tape";
# ifdef titan
    char *titan_string();

    file = titan_string (file);
# endif
    /*
     * If something is already open, close it.
     */
    if (Fds[*unit])
	 close (Fds[*unit]);
    /*
     * Put together the file name.
     */
/*
 * First of all, look at the file name.  If it contains a slash,
 * we simply take it as it is.
 */
	if (strchr(file,'/'))
		strcpy (dest, file);
/*
 * If the environment variable translates, use it.
 */
 	else if (trans = getenv ("SCRATCH"))
	{
		strcpy (dest, trans);
		strcat (dest, "/");
		strcat (dest, file);
	}
/*
 * Look for an extension, in a rather simple sort of way.
 * Look for the last slash (strrchr) then
 * Look for the absence of a dot (strchr) after the last slash otherwise
 * append the type string.
 */
 	if ((slash = strrchr (dest, '/')) == 0)
	     slash = dest;
	if (! strchr (slash, '.'))
	     if( strlen(type))
		  strcat (dest, type);
    
    printf (" Real file is '%s' unit %d\n", last_name, *unit);
    /*
     * Attempt to open it.
     */
    if ((Fds[*unit] = open (last_name, O_RDONLY)) < 0)
	 {
	    Status[*unit] = TS_ERROR;
	    printf (" Unable to open file '%s'\n", last_name);
	    perror ("Open error");
	    return (0);
	 }
    return (1);
}
/* c----------------------------------------------------------------------- */

mtread_ (unit, buffer, len)
  int *unit, *len;		/* len => # 16-bit words */
  char *buffer;
  /*
   * Perform a read from a tape image on disk
   */
{
   int rlen = *len * 2;

   int status = fb_read( Fds[*unit], buffer, rlen ); /* returns byte count */

   if( status == 0 ) {
      Status[*unit] = TS_EOF;
      Nwords[*unit] = 0;
      return( 0 );
   }

   if( status < 0 ) {
      Status[*unit] = TS_EOT;
      Nwords[*unit] = 0;
      return( -1 );
   }
   Status[*unit] = TS_OK;
   Nwords[*unit] = status/sizeof(short);
   return( Nwords[*unit] );
}
/* c----------------------------------------------------------------------- */

mtwrit_ (unit, buffer, len)
  int *unit, *len;
  char *buffer;
  /*
   * Perform a tape write to a disk.
   */
{
   int rlen = *len * 2;

   int status = fb_write( Fds[*unit], buffer, rlen );

   if( status == 0  && rlen == 0 ) {
      Status[*unit] = TS_EOF;
      Nwords[*unit] = 0;
      return( 0 );
   }

   if( status < rlen || status < 0 ) {
      Status[*unit] = TS_ERROR;
      Nwords[*unit] = 0;
      return( -1 );
   }
   Status[*unit] = TS_OK;
   Nwords[*unit] = *len;
   return( Nwords[*unit] );
}
/* c----------------------------------------------------------------------- */

mtwait_ (unit, status, nword)
  int *unit, *status, *nword;
  /*
   * Perform a "wait" for the last operation.
   */
{
    *status = Status[*unit];
    *nword = Nwords[*unit];
    /*
    Status[*unit] = Nwords[*unit] = 0;
     */
}
/* c----------------------------------------------------------------------- */

mtskpf_ (unit, nskip)
  int *unit, *nskip;
  /*
   * Perform a file skip.
   */
{
   char buffer[16];
   int status, rlen=sizeof(buffer), nn = *nskip;

    if( *nskip == 0 ) {
	Nwords[*unit] = *nskip;
	Status[*unit] = TS_OK;
	return( *nskip );
    }
    if( *nskip < 0 ) {
	Nwords[*unit] = 0;
	Status[*unit] = TS_ERROR;
	return( -1 );
    }
    for(;;) {
       status = fb_read( Fds[*unit], buffer, rlen );

       if( status == 0 ) {	/* read an EOF */
	  if( --nn == 0 ) {
	     Nwords[*unit] = *nskip;
	     Status[*unit] = TS_OK;
	     return( *nskip );
	  }
       }       
       if( status < 0 ) {
	  Nwords[*unit] = 0;
	  Status[*unit] = TS_ERROR;
	  return( -1 );
       }
    }
}
/* c------------------------------------------------------------------------ */

void swack_short(ss, tt, nn)
  char *ss, *tt;
  int nn;
{
   for(; nn--;) {
      *tt++ = *(ss+1);
      *tt++ = *ss;
      ss += 2;
   }
}
/* c------------------------------------------------------------------------ */

void swack4(ss, tt)
  char *ss, *tt;
{
   *tt++ = *(ss+3);
   *tt++ = *(ss+2);
   *tt++ = *(ss+1);
   *tt = *ss;
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

# ifdef LITTLENDIAN
    swack4(&nab, &size_rec);
# else
    size_rec = nab;
# endif

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

# ifdef LITTLENDIAN
    swack4(&size_rec, &blip);
# else
    blip = size_rec;
# endif

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
/* c----------------------------------------------------------------------- */
