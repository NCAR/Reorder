C  FILE:unixqufap.f
C----------------------------------------------------------------------
C           Unix DEPENDENT CODE
c-----------------------------------------------------------------------
    	subroutine accver()
c   dump out the current access version date
        call dmplin( 'qUFAP--LINUX VERSION 2.10 June 2010 ' ,0)
    	return
    	end
c-----------------------------------------------------------------------
    	subroutine mountu ( ierr ) 
c   make sure that the tape with name as identifier is mounted 
c   on unit lun 
c
        implicit integer( a-z )
        common  / ufapu /       ncs
     a,         taptim( 6 ),    ccstm1( 6 ),    ccstm2( 6 )   
     a,         lenbuf,         lundat,         nfiles
     a,         intpt,          intno,          ivol,           iscan
     a,         irec,           krec,           ueof
c
        common / ufapc /        ufcs
        character*2222 ufcs
c
        character*88 text
        logical ueof
c
c
c
c  get the tape label
        call attarg( ufcs, intpt, ncs, intno, arg, na )
        if( na .le. 0 ) then
c---------call typstg( 6, 'no more tapes      ^', 1, -1 )
          ierr = 999
          go to 90
        endif
c
      	intno = intno+1
c
        c = 1
        call copyc( ufcs, arg, na, text, inca(c,na))
c    	call lc( text, c-1 )
        call zeroc( text, c, 8 )
c
c   mount the file
    	if( mtfmnt( lundat, text ) .ne. 1 ) then
    	    ierr = 999
    	    go to 90
    	endif
c
      if( nfiles .gt. 0 ) then
        call mtskpf( lundat, nfiles )
        call mtwait( lundat, state, nwds )
      endif
c 
      nfiles = 0
      krec = 0
c
 90   continue
      return
      end
c----------------------------------------------------------------------
        subroutine rewusc( ierr )
c   this routine rewinds the ufap scratch file record
c   this should not be active in the unix version
c
        implicit integer( a-z )
c
        return
        end
c----------------------------------------------------------------------

