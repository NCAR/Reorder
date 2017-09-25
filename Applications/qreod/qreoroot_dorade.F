c  Author:  Oye  Date:  11/12/90
c  $Id: qreoroot.F,v 1.1.1.1 2002/08/07 16:03:06 oye Exp $
c
c----------------------------------------------------------------------
        program root
    	parameter( lenbuf=17000 )
    	integer tbuf( lenbuf )
        data tbuf /lenbuf*0/

    	write(*,*) ' '
    	write(*,*)
     +' qreod - REORDER executable for reading Dorade sweep files'
    	write(*,*) ' '
        call reo( tbuf, lenbuf )
        stop00
        end
c----------------------------------------------------------------------
