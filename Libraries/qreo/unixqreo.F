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
        FUNCTION RANF()
        DATA ISEED / -32767 /
c        RANF = RAN(ISEED)
        RANF = 0
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
