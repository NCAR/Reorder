c Author:  Oye  Date:  11/12/90
c $Id: unixqreo.F,v 1.1.1.1 2002/08/07 16:03:06 oye Exp $
c
c----------------------------------------------------------------------
c   this file contains the following VAX dependent routine
c
c       ranf
c       opnics
c       iniscr
c       opnbrs
c   getseq
c   getnam
c   getproj
c
c----------------------------------------------------------------------
c        FUNCTION RANF()
c        DATA ISEED / -32767 /
c        RANF = RAN(ISEED)
c        RANF = 0
c        RETURN
c        END
c----------------------------------------------------------------------
      FUNCTION RANF()
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     +   NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2E-7,RNMX=1.-EPS)

C  RANDOM NUMBER GENERATOR OF PARK AND MILLER WITH BAYS-DURHAM SHUFFLE
C  AND ADDED SAFEGUARDS.  TAKEN FROM "NUMERICAL RECIPES IN FORTRAN".
      DIMENSION IV(NTAB)
      DATA ISEED/-5/
      SAVE IV,IY
      DATA IY,IV /0,NTAB*0/

      IF(ISEED.LE.0.OR.IY.EQ.0)THEN
         ISEED=MAX(-ISEED,1)
         DO 20 J=NTAB+8,1,-1
            K=ISEED/IQ
            ISEED=IA*(ISEED-K*IQ)-IR*K
            IF(ISEED.LT.0)ISEED=ISEED+IM
            IF(J.LE.NTAB)IV(J)=ISEED
 20      CONTINUE
         IY=IV(1)
      END IF
      K=ISEED/IQ
      ISEED=IA*(ISEED-K*IQ)-IR*K
      IF(ISEED.LT.0)ISEED=ISEED+IM
      J=1+IY/NDIV
      IY=IV(J)
      IV(J)=ISEED
      RANF=MIN(AM*IY,RNMX)
      RETURN
      END
c----------------------------------------------------------------------
      subroutine opnics( par )
c  this routine assigns the l.u. no. and opens the input control file
    	character*222 name
        integer par
        integer dummy(111)
        integer len
c
        ic = fofreodebug( dummy, len )
        ic = len
        name(1:) = 'stdin'

    	if( ic .gt. 0 ) then
            name(1:ic) = ' '
            call copync( dummy, 1, ic, name, 1 )
            par = LUNQUE( 'IDLUN' )
            OPEN(UNIT=PAR, FILE=name, STATUS='OLD', FORM='FORMATTED')
            write( lunout(), 987 ) name(1:ic)
            write( lunout(), 987 ) name
    	else
    	    par = 5
    	    name(1:) = 'stdin'
            write( lunout(), 987 ) name
    	endif
 987	format( ' REORDER INPUT FILE=', a )
c
  90	continue
        ix = namefx( 'IDSET', i )
        call putfx( i, par )
        RETURN
        END
c----------------------------------------------------------------------
        subroutine iniscr( lun, space, len, n )
c  this routine initializes the scratch file
        do 12 i = 1, n
 12     call branwt( i, space, len )
        return
        end
c----------------------------------------------------------------------
        subroutine opnbrs( lu )
c  this routine assigns the l.u. no. and opens the output file for
c  the histiogram display
c
        lu = lunque( 'BARS' )
        open( UNIT=LU, FILE='BARS.LIS', STATUS='NEW', 
     A    FORM='FORMATTED' )
        return
        end
c----------------------------------------------------------------------
        subroutine getseq(iseq, n)
c   return the sequence number
    	n = 0
        return
        end
c----------------------------------------------------------------------
        subroutine getnam(name, n)
c   return scientist name
    	n = 0
        return
        end
c----------------------------------------------------------------------
        subroutine getproj(proj,n)
c   return the project number
    	n = 0
        return
        end
c-----------------------------------------------------------------------
