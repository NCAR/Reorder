c----------------------------------------------------------------------------
c   $Revision: 1.1.1.1 $ $Date: 2002/08/07 16:03:06 $
c
c   ihostw
c   libver
c   nwds16
c   nwords
c   panic
c   readcd
c---------------------------------------------------------------------
	function ihostw( nwds )
c  this routine converts the number of 16 bit words into
c  an equivalent no. of host machine  words.
c
        ihostw = ( nwds - 1 )*16/32 + 1
        return
        end
c----------------------------------------------------------------------
      subroutine libver
c  this routine prints the current version of the library
      call dmplin( 'unixlib--version 27-JAN-94 15:00      ', 33 )
      return
      end
c----------------------------------------------------------------------
	function nwds16( nwds )
c  this routine converts the number of host machine words into
c  an equivalent no. of 16 bit words.
c
        nwds16 = nwds*32/16
        return
        end
c---------------------------------------------------------------------
	function nwords( nchar )
c  this routine returns the number of host computer words
c  required to hold an 'nchar' string
c
	nwords = ( nchar-1 )/4 + 1
	return
	end
c----------------------------------------------------------------------
      subroutine panic
c
      data x, y / 1., 0 /
c     abort the execution
c
      write(6,7)
    7 format(' *** execution being aborted from program *** ')
      call dmplnb
      z=x/y
      z = z+1.
      call fofdie()
      return
      end
c----------------------------------------------------------------------
      subroutine readcd( lun, card, n, zend, zerr )
c
c  this routine is called for every single card/line from an input file.
c     Appears to never be called, instead foflib.F:subroutine readln
c     is called.  Card is dimensioned char*132 in gnxatt and char*256 in
c     subroutine hgnxatt.
c  
        character card*(*)
      logical zend, zerr
c
      print *,'unixflib.F:subroutine readcd has been called'
c
      nc = 0
      zend = .false.
      zerr = .false.
      read( lun, 987, err=77, end=66 ) card(1:80)
  987 format( a )
      n = 80
        do 22 i = 80, 1, -1
            if( card(i:i) .ne. ' ' ) then
                go to 23
            endif
            n = n-1
  22    continue
  23    continue
      return
c
 66   continue
      zend = .true.
      return
c
 77   continue
    	write( lunout(), 789 )
 789	format( ' Error in readcd ' )
      zerr = .true.
      return
      end
c----------------------------------------------------------------------------
c----------------------------------------------------------------------------
c----------------------------------------------------------------------------
