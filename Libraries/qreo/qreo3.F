c--------------------------------------------------------------------------
c--------------------------------------------------------------------------
c..file:reo3.for
c
c  this file contains
c       mximyz
c	locarx
c       unfold
c       octant
c       radinf
c	wrimud
c       mudfil
c       bsmud
c       codeit
c       bsym
c       bsit
c       inisym
c	histit
c	bars
c	hrng
c	ze9
c       
c
c--------------------------------------------------------------------
        subroutine mximyz( n, a, b )
c   this routine determines "a" and "b" such that the product is
c   closest to "n"
c
        implicit integer( a-z )
c
        s = sqrt( float( n ))
        err = n
c
        do 11 i = s, s+s/2+1
            if( mod( n, i ) .lt. err ) then
                a = i
                b = n/a
                err = n - a*b
            endif
  11    continue
c
        return
        end
c----------------------------------------------------------------------
        function locarx( carton )
c   this function returns an offset to the desired carton in storage 
c   shelves
c
        implicit        integer( a-z )
c
c===========================================================================
c___qcommon.for
c
    	parameter( maxcart=4001, sizcart=1024 )
        parameter( inull=-32768 )
        parameter( znull=-32768., wmin=-1000000., wmax=1000000. )
        parameter( maxval=500 )
        parameter( maxspc=maxcart*sizcart )
        parameter( maxgt=2048)
        parameter( mxg3=3*maxgt )
        parameter( mxfld=16, mxsfld=20  )
c           
        integer nflds, fldtyp, fldoff
        integer wtfun, wtfptr, unfptr
        integer natfld, attyp, atptr, thrptr, hthptr, thrpt2
        real    thrval, zlard, atten
        logical dupfld, unfflg
c
        real space
        common  / store /   space(maxspc)
c
        common  / boxez /       nbxcar,         lenbox
     +,         ixbox(maxval),  iybox(maxval),  izbox(maxval)
c
        common / fldcom /   nflds
     +,     fldtyp(mxfld)
     +,     fldoff(mxfld)
     +,     dupfld(mxfld)
     +,     nyqptr(mxfld)
     +,     wtfun, wtfptr, atten
     +,     unfflg(mxfld)
     +,     unfptr(mxfld)
     +,     mnrptr
     +,     thrval(mxfld)
     +,     natfld(mxfld)
     +,     attyp(mxsfld,mxfld)
     +,     atptr(mxsfld,mxfld)
     +,     nlards
     +,     zlard(50)
c
c
c   nflds   =   the number of source data fields 
c   fldtyp  =   type of field   
c                   1 = normal data
c                   2 = dbz field
c                   3 = time
c                   4 = azimuth
c                   5 = elevation
c                   6 = range
c                   7 = 
c                   8 = 
c                   9 = k or the weighted number of folds
c                  10 = field created by function FSPECL 
c
c   fldoff  =   offset to first gate in data buffer
c   dupfld  =   duplicate field flag 
c   nyqptr  =   pointer to the Nyquist velocity associated with this field
c   wtfun   =   weighting function
c                   1 = Cressman
c                   2 = Exponential
c                   3 = closest point
c                   4 = Uniform (average)
c   wtfptr  =   pointer to additional weighting factor
c                   Assumed to be an input data field
c   atten   =   attenuation factor used in exponential weighting
c   unfflg  =   flag to indicate unfolding for this field
c   unfptr  =   pointer to reference velocity for unfolding
c   mnrptr  =   pointer to the minimum radius squared
c   thrval  =   high or low threshold value 
c
c   natfld  =   the number of fields attached to this field
c   attyp   =   type of attached field
c                    1 = weighted sum
c                    2 = sum of data points    
c                    3 = sum of the squares of the data points
c                    4 = nearest point
c                    5 = minimum radius
c                   11 = sum of the weights
c                   12 = count of data points used
c                   13 = octant flag word
c                   14 = sum of the squares of the weights
c                   15 = Min. big R
c                   16 = Max. big R
c
c   atptr   =   pointers to attached field in box or other fields required
c                   to compute the attached field (requires a pointer pointer
c                   because an attached field may require other components.
c                   The pointer pointer is bumped to point to other 
c                   components as they are needed). i.e. the number of 
c                   special field pointers may be greater than the number 
c                   of special fields.
c
c   nlards  =   number of spaces used up in "zlard"
c   zlard   =   extra space for special parameters such as the nyquist vel.
c
        character*256 mnames
        character*32 fldmne
        common / fldcomc /  fldmne,     mnames
c   fldmne  =   the original 2 character source field mnemonic
c   mnames  =   character data--8 character names of the output fields
c
c
        parameter(  mxcomp=8, mxtot=mxfld+mxsfld  )
c
        integer noutf, ncomp, comptr, oftyp, namptr, bsflag
        integer octthr
        real    bcntr, binc
c                       
        common / ofldcom /  noutf
     +,     oftyp(mxtot)
     +,     comptr( mxcomp, mxtot )
c
c   noutf   =   the number of output fields
c   oftyp   =   type of output field (see attached fields) plus 
c                  21 = Weighted sum over sum of the weights
c                  22 = Quality field
c                  23 = Interpolated point (dBz field)
c                  24 = Sum of weights squared over sum of weights
c                  25 = Closest point
c                  26 = Average
c                  27 = Count
c                  28 = Standard deviation
c                  29 = Octant
c                  30 = Radius
c   comptr  =   pointers to each of the components for the output field
c
c
c
c___qcommon.reo;10
c===========================================================================
c
      common / woopee / kount
c
        parameter( treelm=2*maxcart )
c
        integer carnum( maxcart ), shelf( maxcart ), usage( maxcart )
c
        common / storef /
     a          drecln, lencar, mxshlf, carnum, shelf,  usage
c
        integer carno( treelm ), caref( treelm )
        integer carlo( treelm ), carhi( treelm )
c
        common / treez /
     a          branch, carno, caref, carlo, carhi
c
c
c
c        call i5( '**locarx** c     ', 12, carton )
c        call z8( 'lcx', 3, carton )
c
c  determine if this carton is in the cache
        i = mod( carton-1, maxcart ) + 1
        j = shelf( i )
        locarx = j
c        call z8( 'i', 1, i )
c        call z8( 'j', 1, j )
c        call z8( 'ci', 2, carnum(i) )
        if( carnum( i ) .eq. carton ) then
c  the carton is in the cache
          continue
        else
c           call i8( 'w', 1, carnum(i))
c           call i8( 'r', 1, carton )
c           call i8( 'j', 1, j )
          call branwt( carnum(i), space(j), drecln )
          call branrd( carton, space(j), drecln )
          carnum( i ) = carton
          usage(i) = usage(i)+1
        endif

        return
        end


c-------------------------------------------------------------------------
        subroutine unfold( vsubn, vsubr, v, k )
c   this routine unfolds velocity "v" if necessary where "vsubn" is the
c   nyquist velocity and "vsubr" is the reference velocity
c
c
        if( vsubr - v .gt. vsubn ) then
            v = v + 2*vsubn
            k = 1
        elseif( v - vsubr .gt. vsubn ) then
            v = v - 2*vsubn
            k = -1
        else
            k = 0
        endif
c
        return
        end
c-------------------------------------------------------------------------
        subroutine octant( x, y, z, x0, y0, z0, oct )
c   this routine determines which octant (x,y,z) is in relative to
c   (x0,y0,z0)
c
        integer oct, octw
c
c        call z4( 'oct', 3, oct )
c        call f7( 'x', 1, x )
c        call f7( 'x0', 2, x0 )
c        call f7( 'y', 1, y )
c        call f7( 'y0', 2, y0 )
c        call f7( 'z', 1, z )
c        call f7( 'z0', 2, z0 )
c
        if( x .lt. x0 ) then
            octw = 1
        else
            octw = 2
        endif
c
        if( y .lt. y0 ) then
            octw = octw*1
        else
            octw = octw*4
        endif
c
        if( z .lt. z0 ) then
            octw = octw*1
        else          
            octw = octw*16
        endif
c
        if( mod( oct/octw, 2 ) .eq. 0 ) then
            oct = oct+octw
        endif
c  
c        call z4( 'oct', 3, oct )
c        call z4( 'octw', 4, octw )
        return     
        end                                               
c----------------------------------------------------------------------
        subroutine radinf( n, az, el, r, da, de, dr,
     +      dx, dy, dz )
c   radius of influence for interpolated grid points
c   this routine converts da, de, and dr (in radar space)
c   into dx, dy, dz (in cartesian space).
c
c_______________________________________________________________________
c  06 February 1990: Modification by LJM - NCAR/MMM
c  Replaced sum of absolute values with square root of sum of squares
c  in the calculation of the sphere of influence for radar space.
c_______________________________________________________________________
c
        real r(*), dx(*), dy(*), dz(*)
        common / scr / dd( 2048 )
c
c
c
        sina = sin( az )
        cosa = cos( az )
        sine = sin( el )
        cose = cos( el )
        sace = sina*cose
        cace = cosa*cose
        sase = sina*sine
        cace = cosa*cose
        case = cosa*sine
c
c
        if( dr .le. 0 ) then
c   dr is a function of r and da
            do 11 i = 1, n
                dd(i) = r(i)*da
  11        continue
c
            do 22 i = 1, n
c_______________________________________________________________________
c  Modification by LJM - NCAR/MMM
c  replaced sum of absolute values with square root of sum of squares
c
c                dx(i) = abs(sace*dd(i)) + abs(r(i)*cace*da) + 
c     +                                              abs(r(i)*sase*de)
c                dy(i) = abs(cace*dd(i)) + abs(r(i)*sace*da) + 
c     +                                              abs(r(i)*case*de)
c                dz(i) = abs(dd(i)*sine) + abs(r(i)*cose*de)
c
c_______________________________________________________________________
                   x1 = sace*dd(i)
                   x2 = r(i)*cace*da
                   x3 = r(i)*sase*de
                dx(i) = sqrt( x1*x1 + x2*x2 + x3*x3 )
                   y1 = cace*dd(i)
                   y2 = r(i)*sace*da
                   y3 = r(i)*case*de
                dy(i) = sqrt( y1*y1 + y2*y2 + y3*y3 )
                   z1 = sine*dd(i)
                   z2 = r(i)*cose*de
                dz(i) = sqrt( z1*z1 + z2*z2 )
  22        continue
            go to 44
        endif
c
c   dr is a constant
        do 33 i = 1, n
c_______________________________________________________________________
c  Modification by LJM - NCAR/MMM
c  replaced sum of absolute values with square root of sum of squares
c
c                dx(i) = abs(sace*dr) + abs(r(i)*cace*da) + 
c     +                                          abs(r(i)*sase*de)
c                dy(i) = abs(cace*dr) + abs(r(i)*sace*da) + 
c     +                                          abs(r(i)*case*de)
c                dz(i) = abs(dr*sine) + abs(r(i)*cose*de)
c
c_______________________________________________________________________
                   x1 = sace*dr
                   x2 = r(i)*cace*da
                   x3 = r(i)*sase*de
                dx(i) = sqrt( x1*x1 + x2*x2 + x3*x3 )
                   y1 = cace*dr
                   y2 = r(i)*sace*da
                   y3 = r(i)*case*de
                dy(i) = sqrt( y1*y1 + y2*y2 + y3*y3 )
                   z1 = sine*dr
                   z2 = r(i)*cose*de
                dz(i) = sqrt( z1*z1 + z2*z2 )
  33    continue
c
  44    continue
c
  90    continue
        return
        end
c---------------------------------------------------------------------------
      subroutine wrimud( outrec, scrx, iscrx, hedrec, syms, nsym,
     +      acs, nacs )
c
        character   syms*(*), acs*(*)
c===========================================================================
c___qcommon.for
c
    	parameter( maxcart=4001, sizcart=1024 )
        parameter( inull=-32768 )
        parameter( znull=-32768., wmin=-1000000., wmax=1000000. )
        parameter( maxval=500 )
        parameter( maxspc=maxcart*sizcart )
        parameter( maxgt=2048)
        parameter( mxg3=3*maxgt )
        parameter( mxfld=16, mxsfld=20  )
c           
        integer nflds, fldtyp, fldoff
        integer wtfun, wtfptr, unfptr
        integer natfld, attyp, atptr, thrptr, hthptr, thrpt2
        real    thrval, zlard, atten
        logical dupfld, unfflg
c
        real space
        common  / store /   space(maxspc)
c
        common  / boxez /       nbxcar,         lenbox
     +,         ixbox(maxval),  iybox(maxval),  izbox(maxval)
c
        common / fldcom /   nflds
     +,     fldtyp(mxfld)
     +,     fldoff(mxfld)
     +,     dupfld(mxfld)
     +,     nyqptr(mxfld)
     +,     wtfun, wtfptr, atten
     +,     unfflg(mxfld)
     +,     unfptr(mxfld)
     +,     mnrptr
     +,     thrval(mxfld)
     +,     natfld(mxfld)
     +,     attyp(mxsfld,mxfld)
     +,     atptr(mxsfld,mxfld)
     +,     nlards
     +,     zlard(50)
c
c
c   nflds   =   the number of source data fields 
c   fldtyp  =   type of field   
c                   1 = normal data
c                   2 = dbz field
c                   3 = time
c                   4 = azimuth
c                   5 = elevation
c                   6 = range
c                   7 = 
c                   8 = 
c                   9 = k or the weighted number of folds
c                  10 = field created by function FSPECL 
c
c   fldoff  =   offset to first gate in data buffer
c   dupfld  =   duplicate field flag 
c   nyqptr  =   pointer to the Nyquist velocity associated with this field
c   wtfun   =   weighting function
c                   1 = Cressman
c                   2 = Exponential
c                   3 = closest point
c                   4 = Uniform (average)
c   wtfptr  =   pointer to additional weighting factor
c                   Assumed to be an input data field
c   atten   =   attenuation factor used in exponential weighting
c   unfflg  =   flag to indicate unfolding for this field
c   unfptr  =   pointer to reference velocity for unfolding
c   mnrptr  =   pointer to the minimum radius squared
c   thrval  =   high or low threshold value 
c
c   natfld  =   the number of fields attached to this field
c   attyp   =   type of attached field
c                    1 = weighted sum
c                    2 = sum of data points    
c                    3 = sum of the squares of the data points
c                    4 = nearest point
c                    5 = minimum radius
c                   11 = sum of the weights
c                   12 = count of data points used
c                   13 = octant flag word
c                   14 = sum of the squares of the weights
c                   15 = Min. big R
c                   16 = Max. big R
c
c   atptr   =   pointers to attached field in box or other fields required
c                   to compute the attached field (requires a pointer pointer
c                   because an attached field may require other components.
c                   The pointer pointer is bumped to point to other 
c                   components as they are needed). i.e. the number of 
c                   special field pointers may be greater than the number 
c                   of special fields.
c
c   nlards  =   number of spaces used up in "zlard"
c   zlard   =   extra space for special parameters such as the nyquist vel.
c
        character*256 mnames
        character*32 fldmne
        common / fldcomc /  fldmne,     mnames
c   fldmne  =   the original 2 character source field mnemonic
c   mnames  =   character data--8 character names of the output fields
c
c
        parameter(  mxcomp=8, mxtot=mxfld+mxsfld  )
c
        integer noutf, ncomp, comptr, oftyp, namptr, bsflag
        integer octthr
        real    bcntr, binc
c                       
        common / ofldcom /  noutf
     +,     oftyp(mxtot)
     +,     comptr( mxcomp, mxtot )
c
c   noutf   =   the number of output fields
c   oftyp   =   type of output field (see attached fields) plus 
c                  21 = Weighted sum over sum of the weights
c                  22 = Quality field
c                  23 = Interpolated point (dBz field)
c                  24 = Sum of weights squared over sum of weights
c                  25 = Closest point
c                  26 = Average
c                  27 = Count
c                  28 = Standard deviation
c                  29 = Octant
c                  30 = Radius
c   comptr  =   pointers to each of the components for the output field
c
c
c
c___qcommon.reo;10
c===========================================================================
c
c
c
c
c
      integer HEADER_LEN, LEVEL_LEN
      parameter(HEADER_LEN=510)
      parameter(LEVEL_LEN=10)
c
      integer c, ff, ofn, octfch, ckeep, cedwrt
      integer box, carton, offset
      logical pow
c
c  workspace and workspace management
c
      real scrx(1)
      integer iscrx(1)
      parameter( radian=57.29578 )
c
c
      integer delete
      parameter(delete = -32768)
c
      real zdelet
      parameter(zdelet=-32768.)
c 
      real cf
      parameter(cf = 64.0)
c 
      real sf
      parameter(sf = 100.0)
c 
      real scale
c      parameter(scale = 64.0)
      integer iscale
      parameter(iscale = 64)
c 
      real slong, slati, salti
      real sbase
      real dlong, dlati, dalti
      real dbase
c
        common  / datset /      slong,          slati,          salti
     +,         dlong,          dlati,          dalti,          dbase
     +,         axmin(3),       axmax(3),       radius(3),    delta(3)
     +,         deltai(3),      numval(3),      spacng(3)
     +,         tref,           values(maxval,3)
c 
      integer tref
c 
      integer deg, min
      real sec
      integer secnds
c 
      logical flash, dbz
c
    	integer iname(66)
    	integer name_netcdf(66)
        character*222 tname
        character*128 ll
        character tmp*8
c
      integer outrec(1)
c
        logical copln
c
        data lunwri / 14 /
        data pow / .false. /
c
        mnptr( i ) = 1 + (i-1)*8
code
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c
c
        call dmplin( 'c...mark   ', 8 )
c        call dmplin( '**wrimud**', 10 )
        copln = namefx( 'COPLANE', i ) .ne. 0

c
c   get the output file directory name
    	call getstr('NETCDF', tname, 1, nncdf)
    	if(nncdf .gt. 0) then
    	    call copyc(tname, 1, nncdf, name_netcdf, 1)
    	endif

        call getstr( 'OUTPUT', tname, 1, no )
        if( no .lt. 1 ) then
            call getstr( 'CEDRIC', tname, 1, no )
        endif
c   strip off the last 8 characters
        if( no .ge. 8 ) then
            call copyc( tname, no-7, 8, tmp, 1 )
        else
            call copyc( tname, 1, no, tmp, 1 )
            tmp(no+1:8) = ' '
        endif
c
    	call copycn( tname, 1, no, iname, 1 )
    	i = HEADER_LEN*2 + numval(3)*LEVEL_LEN*2 + 
     a	    numval(1)*numval(2)*numval(3)*noutf*2
c
        i = cedopn( iname, no, i)
c 
c  finish volume header 
      call putasc( hedrec, 1, tmp, 8 )
c
    	i = cedwrt( hedrec, HEADER_LEN*2 )
c
c 
c 
      do 600 l3=1,numval(3)
c   for every level... 
c
c   build and write the level header
c
      call putasc(outrec,1,'LEVEL ',6)
      if( copln ) then
        zlevel = values(l3,3)
      else
        zlevel = values(l3,3)
      endif
      call put16(outrec,4,nint(sf*zlevel))
      call typstg( lunout(), ' ', 1, 1 )
      c = 1
      call bfill( ll, 1, 80 )
      call zipstr( '  Level ^', ll, c )
      call zncode( ll, inca(c,4), 3, 1, l3, x, 0 )
      call zncode( ll, c, 5, 2, l3, zlevel, 2 )
      if( copln ) then
        call zipstr( ' DEG.^', ll, c )
      else
        call zipstr( ' KM. ^', ll, c )
      endif
        call zncode( ll, c, 5, 1, numval(1)*numval(2), x, 0 )
    	call zipstr( ' Grid Points^', ll, c )
      call putc( ll, c, '^' )
      call typstg( lunout(), ll, 1, 0 )
c
      call put16(outrec,5,l3)
      call put16(outrec,6,noutf)
      call put16(outrec,7, numval(1)*numval(2))
      call put16(outrec,8, (numval(1)*numval(2)-1) /3200 + 1)
      call put16(outrec,9, ((numval(1)*numval(2)-1) /3200+1)
     1   *noutf)
      call put16(outrec,10,0)
c
      i = cedwrt( outrec, LEVEL_LEN*2 )
c 
c  assemble the data for each output field
c
      do 530 ofn = 1, noutf
c
c     get the scale for each field
         i16w = 176 + (ofn -1) * 5
         scale = iget16( hedrec, i16w+4)
c
        icase = oftyp(ofn)-20
        zmin = wmax
        zmax = wmin
        zmean = 0
        zvar = 0
        nmean = 0
        nzap = 0
c 
      do 520 l2=1,numval(2)
c
      do 510 l1=1,numval(1)
c
c...mark                                           
c
c
c  determine the box no.
        box = ixbox( l1 ) + iybox( l2 ) + izbox( l3 )

c  determine the carton no.
        carton = ( box-1 )/nbxcar + 1

c  get the offset to this carton
        offset = locarx( carton ) -1

c  determine the offset to the box in the carton
        offset = offset + mod( box-1, nbxcar )*lenbox
c
c
        go to( 421, 422, 423, 424, 425, 426, 427, 428, 429, 430 )
     +,      icase
c
 421    continue
c   weighted sum!  first component is the sum of the weights
        sumw = space( offset+comptr(1,ofn) )
        if( sumw .le. 0 ) then
            zval = znull
        else
c   second component is the weighted sum
            wsum = space( offset+comptr(2,ofn) )
            zval = wsum/sumw
            iscrx(l1)=nint(scale*zval)
            if( fldtyp(ofn) .eq. 4 .or. fldtyp(ofn) .eq. 5 ) then
               zval = amod( zval+720., 360. )
            endif
        endif
        go to 508
c
 422    continue
c   quality field!  first component is the count
        nq = space( offset+comptr(1,ofn) )
c   second component is the sum
        sum = space( offset+comptr(2,ofn) )
c   third component is sum squared
        sumsq = space( offset+comptr(3,ofn) )
c   fourth component is the nyquist velocity which is in the larder
        znyqve = zlard( comptr(4,ofn) )
c
        if( nq .gt. 1 ) then        
c_______________________________________________________________________
c  Modification by LJM - NCAR/MMM
c  replaced computation of qual field to be consistent with sprint
c
c            zval = 1. - 
c     +  (( nq*sumsq - sum*sum )/(nq*(nq-1)))/(znyqve**2/3)
c
c_______________________________________________________________________
            vstd = 0.0
            vsqr = ( nq*sumsq - sum*sum ) / (nq*(nq-1))
            if( vsqr .gt. 0.0 ) vstd =  sqrt( vsqr )
            zval = 1. - vstd / (znyqve/1.73205808)
            iscrx(l1)=nint(scale*zval)
        else
            zval = znull
        endif
        go to 508
c
c
 423    continue
c   dbz field!  first component is the sum of the weights
        sumw = space( offset+comptr(1,ofn) )
        if( sumw .le. 0 ) then
            zval = znull
        else
c   second component is the weighted sum
            zval = space( offset+comptr(2,ofn) )/sumw
            if( zval .gt. 0 ) then
                zval = 10.*alog10(zval)
                iscrx(l1)=nint(scale*zval)
            else
c    	    	call inslc( 'dbzap' )
c    	    	call i8( 'l1', 2, l1 )
c    	    	call i8( 'l2', 2, l2 )
c    	    	call i8( 'l3', 2, l3 )
                zval = znull
            endif
        endif
        go to 508
c
c
 424    continue
c   sum of the weights squared over the sum of the weights
        sumw = space( offset+comptr(1,ofn) )
        if( sumw .le. 0 ) then
            zval = znull
        else
c   second component is the sum of the weights squared
            zval = space( offset+comptr(2,ofn) )/(sumw*sumw)
            iscrx(l1)=nint(scale*zval)
        endif
        go to 508
c
c       
 425    continue
c   closest point
        zval = space( offset+comptr(1,ofn) )
        iscrx(l1)=nint(scale*zval)
        go to 508
c
c
 426    continue
c   Average...get the count
        nq = space( offset+comptr(1,ofn) )
c   second component is the sum
        if( nq .ge. 1 ) then        
            zval = space( offset+comptr(2,ofn) )/ float( nq )
            iscrx(l1)=nint(scale*zval)
        else
            zval = znull
        endif
        go to 508
c
c
 427    continue
c   the number of data points used in interpolating this grid point
        zval = space( offset+comptr(1,ofn) )
        if( zval .le. 0 ) then
            zval = znull
        else
            iscrx(l1)=nint(scale*zval)
        endif
        go to 509
c
c
 428    continue
c   standard deviation field!  first component is the count
        nq = space( offset+comptr(1,ofn) )
        if( nq .gt. 1 ) then        
c   second component is the sum
            sum = space(offset+comptr(2,ofn))
    	    sqsum = sum*sum
c   third component is sum squared
    	    sumsq = space(offset+comptr(3,ofn))
    	    zn = nq
    	    if((zn*sumsq) - sqsum .lt. 0 ) then
c    	    	write(*,*) l1, l2, l3, ofn
c    	    	write(*,*) 'wrimud '
c     +	    	    , zn, sqsum, sumsq, zn*sumsq, (zn*sumsq)-sqsum
    	    	zval = 0
    	    else
            	zval = sqrt
     + 	    	    ((zn*sumsq)-sqsum)/float(nq*(nq-1))
c
c     + (nq*space(offset+comptr(3,ofn)) - sum*sum)/(nq*(nq-1))
    	    endif
c
            iscrx(l1)=nint(scale*zval)
        else
            zval = znull
        endif
        go to 508
c
c
 429    continue
c   the octant flag
        noctz = nocts( space( offset+comptr(1,ofn)))
    	if( noctz .lt. 1 ) then
    	    zval = znull
    	else
    	    zval = noctz
            iscrx(l1)=iscale*noctz
    	endif
        go to 509
c
c
 430    continue
c   save the radius
        zval = space( offset+comptr(1,ofn) )
        if( zval .eq. wmax .or. zval .lt. 0 ) then
            zval = znull
        else
            iscrx(l1)=nint(scale*sqrt(zval))
        endif
        go to 509
c
c
 508    continue
c   octant criteria?
c        if( octptr .ne. inull ) then
c            call insl( 'octants  ', 7 )
c            if( nocts( space( offset+octptr )) .lt. octthr ) then
c                zval = znull
c            endif
c        endif
c
 509    continue
        if( zval .ne. znull ) then
            nmean = nmean + 1
            zmax = amax1( zmax, zval )
            zmin = amin1( zmin, zval )
            zmean = zmean + zval
            zvar = zvar + zval**2
         else
            iscrx(l1)=delete
            nzap = nzap + 1
         endif
c
 510  continue
c
      call pak16( outrec, iscrx, 0, numval( 1 ))
      i = cedwrt(outrec,numval(1)*2)
c
 520  continue
c
        call bfill( ll, 1, 80 )
        c = 1
        call copyc( mnames, mnptr(ofn), 8, ll, inca(c,8))
c
        if( nmean .gt. 1 ) then
          zmean = zmean/float( nmean )
          zvar = sqrt((nmean*zvar - zmean**2)/float(nmean*(nmean-1)))
          call zipstr( ' Min ^', ll, c )
          call ze9( zmin, ll, inca(c,9))
          call zipstr( ' Max ^', ll, c )
          call ze9( zmax, ll, inca(c,9))
          call zipstr( ' Av ^', ll, c )
          call ze9( zmean, ll, inca(c,9))
          call zipstr( ' Sd ^', ll, c )
          call ze9( zvar, ll, inca(c,9))
        endif          
    	c = c+1
        call zncode( ll, c, 5, 1, nzap, x, 0 )
        call zipstr( ' Empty^', ll, c )
        call typstg( lunout(), ll, 1, c-1 )
 
 530  continue
 600  continue
 
        call dmplin( 'c...mark   ', 8 )
        call bsmud( lunwri, outrec, space, acs, 1, nacs,
     +      syms, nsym )
        if(nncdf .gt. 0) then
    	call reonetcdf(name_netcdf, nncdf)
	endif
    	call cedcls()
        return 
c
c
c
c
c        pow = ( l1 .eq. 25 .and. l2 .eq. 30 .and. l3 .eq. 1 ) 
c     +   .or. ( l1 .eq. 25 .and. l2 .eq. 31 .and. l3 .eq. 1 ) 
c        if( pow ) then
c            call i8( 'l1', 2, l1 )
c            call i8( 'l2', 2, l2 )
c            call i8( 'l3', 2, l3 )
c            call i8( 'box', 3, box )
c            call i8( 'car', 3, carton )
c            call i8( 'off', 3, offset )
c            call i8( '1', 1, comptr(1,ofn))
c            call i8( '2', 1, comptr(2,ofn))
c            call e12( 'sw', 2, sumw )
c            call e12( 'ws', 2, wsum )
c            call e12( 'zv', 2, zval )
c            call dmplnb
c            call typefp( space, offset+1, offset+lenbox )
c        endif
c
       end
c-------------------------------------------------------------------------
        integer function nocts( oct )
c   this routine counts the number of octants turned on
c
        implicit integer( a-z )
c
c
        octw = oct
        nocts = 0
        do 11 i = 1, 8
            nocts = nocts + mod( octw, 2 )
            octw = octw/2
  11    continue
c
        return
        end
c-------------------------------------------------------------------------
        integer function octfch( oct )
c   this routine guarentees the return of the octant pattern as an integer
c
        integer oct
c
        octfch = oct
        return
        end
c----------------------------------------------------------------------
        subroutine bsmud( lun, kbuf, scr, acs, a1, a2,
     +      syms, nsym )
c  this routine does a display of the mudras data volume on "lun".
c
        implicit integer( a-z )
    	parameter(HEADER_LEN=510)
      	parameter(LEVEL_LEN=10)
        character   acs*(*), syms*(*)
        parameter( maxval=500 )
        parameter( maxcf=25*8 )
        character*444 flist
        character*8 ll
        dimension sff( 25 ), xval( maxval )
        character*444 ldt, line
        real xmin, xinc, ymin, yinc, zmin, zinc, sfh, sff
        real zval( maxval ), pt1(25), pinc(25)
        logical xlev( maxval ), xref(25), flee
c
c   calculate the pointer to the corresponding 8 character output field name
        mnptr( i ) = 1 + (i-1)*8
        cn( i ) = i*2-1
c
c
c
c        call insl( '**bsmud**', 9 )
c
        if( indexa( acs, a1, a2, 4, 'DISPLAY' ) .le. 0 ) then
    	    call dmplin( 'exit', 4 )
            go to 90
        endif
c
        call cedrwd()
c  get the header
        i = cedrd( kbuf, HEADER_LEN*2 )
c  missing data flag
        zap = iget16( kbuf, 67 )
c  get the header scale factor
        sfh = 1./float( iget16( kbuf, 68 ))
c
        xmin = float( iget16( kbuf, 160 ))*sfh
        nx = iget16( kbuf, 162 )
        xinc = float( iget16( kbuf, 163 ))*.001
c
        ymin = float( iget16( kbuf, 165 ))*sfh
        ny = iget16( kbuf, 167 )
        yinc = float( iget16( kbuf, 168 ))*.001
c          
        zmin = float( iget16( kbuf, 170 ))*.001
        nz = iget16( kbuf, 172 )
        zinc = float( iget16( kbuf, 173 ))*.001
c
        do 10 i = 1, nz
            zval(i) = zmin + (i-1)*zinc
  10    continue
c
        call bfill( flist, 1, maxcf )
c  number of fields             
        nf = iget16( kbuf, 175 )   
        j = 176
        do 11 ff = 1, nf
c  get the name and scale factor
                call get16c( kbuf, j, 8, ll )
                call copyc( ll, 1, 8, flist, mnptr(ff))
                sff( ff ) = 1./float( iget16( kbuf, j+4))
                j = j+5
  11    continue
c
        call inisym( syms, nsym )
c
        flee = .false.
        call getbsc( acs, a1, a2, nsym, nf, flist, xref, 
     +      nz, xlev, zval, pt1, pinc, flee )
        if( flee ) then
            go to 90
        endif
c
c
c
c
c        call i8( 'nf', 2, nf )      
c        call i8( 'nl', 2, nlev )
c        call dmplin( 'xref', 4 )
c        call typez16( xref, 1, nf )
c        call dmplin( 'xlev', 4 )
c        call typez16( xlev, 1, nz )
c
c  encode the date and time limits into a header line
        c = 1
        call zipstr( 'DATA FROM  ^', ldt, c )
        call ydate( iget16(kbuf,116),iget16(kbuf,117),iget16(kbuf,118),
     +      ldt, c, nd )
        c = c + nd
        call bfill( ldt, inca(c,1), 1 )
        call ytime( iget16(kbuf,119),iget16(kbuf,120),iget16(kbuf,121),
     +      ldt, c, nd )
        c = c + nd
        call zipstr( '  TO  ^', ldt, c )
        call ydate( iget16(kbuf,122),iget16(kbuf,123),iget16(kbuf,124),
     +      ldt, c, nd )
        c = c + nd
        call bfill( ldt, inca(c,1), 1 )
        call ytime( iget16(kbuf,125),iget16(kbuf,126),iget16(kbuf,127),
     +      ldt, c, nd )
        c = c + nd
        call bfill( ldt, inca(c,4), 4 )
        call get16c( kbuf, 8, 4, ll )
        call copyc( ll, 1, 4, ldt, inca(c,4))
        call bfill( ldt, inca(c,4), 4 )
        call get16c( kbuf, 13, 6, ll )
        call copyc( ll, 1, 6, ldt, inca(c,6))
        call bfill( ldt, inca(c,4), 4 )
        call get16c( kbuf, 312, 6, ll )
        call copyc( ll, 1, 6, ldt, inca(c,6))
        call putc( ldt, c, '^' )
c
c
c  for each level
        do 66 nlev = 1, nz
c  read the level header
            i = cedrd( kbuf, LEVEL_LEN*2 )
c  for each field
            do 55 ff = 1, nf
                is = 1
                io = 0
c  for each y value
                do 44 iy = 1, ny
c  get the next line
                    i = cedrd( kbuf, nx*2 )
   	    	    call upk16s( kbuf, xval, 0, nx )
                    if( xlev(nlev) .and. xref(ff) ) then
c  code it into symbols in the scratch space
                        call codeit( sff( ff ), xval, nx, scr, is,
     +                  pt1(ff), pinc(ff), syms, nsym, zap )
                    endif
  44            continue
                if( xlev(nlev) .and. xref(ff) ) then
                    call page(lunout())
c  print the symbol table for this field
                    call bsym( flist, (ff-1)*8+1, pt1(ff), pinc(ff),
     +              syms, nsym, line )   
                    call lf(lunout())
                    call typlong( lunout(), ldt, 1, 0 )
c  print this field at this level
                    call bsit( scr, nx, ny, nlev, zval(nlev), 
     +              xmin, xinc, ymin, yinc, line )
                endif
  55        continue
  66    continue
    	call page(lunout())
c                                           
  90    continue              
        return
        end
c------------------------------------------------------------------
        subroutine getbsc( acs, a1, a2, nsym, nf, flist, xref, nl, xlev,
     +      zval, pt1, pinc, flee )
c   this routine extracts display info from in input control string
c
        implicit integer( a-z )
        character   acs*(*), flist*(*)
        logical same, xref(*), xlev(*), anumbr, ldummy
        real pt1(*), pinc(*), zval(*)
        real zrad, wmax, zlev, zns
        parameter( wmax=32767. )
c
        mnptr(i) = 1+(i-1)*8
c
c             
c        call insl( '**getbsc**', 10 )
c        call i8( 'nf', 2, nf )
c        call i8( 'a1', 2, a1 )
c        call i8( 'a2', 2, a2 )
c        call i8( 'nsym', 4, nsym )
c
c
        do 11 i = 1, nf
            xref(i) = .false.
  11    continue
c   do all levels unless there is a LEVEL attribute
        do 12 i = 1, nl
            xlev(i) = .true.
  12    continue
c
c   set up display parameters by looping thru input control string
  22    continue
        ae = a1-1
  23    continue
        if( ae+1 .ge. a2 ) then
            go to 44
        endif
        call nxtatt( acs, ae+1, a2, f1, nch, ae )
        call attarg( acs, f1, ae, 1, aa, na )
c
        if( same( acs, f1, 3, 'DISPLAY', 1 )) then
c            call inslx( acs, f1, nch )
            if( na .lt. 2 ) then
                go to 23
            endif
c  look for the field in flist
            do 26 i = 1, nf
c                    call i8( 'i', 1, i )
c                    call inslx( flist, mnptr(i), 8 )
                if( same( flist, mnptr(i), min0(8,na), acs, aa )) then
c                    call insl( 'found', 5 )
                    xref(i) = .true.
                    pt1(i) = 0
                    pinc(i) = 5
c   get the center and increment
                    ldummy = anumbr( 2, acs, f1, ae, kint, pt1(i))
                    ldummy = anumbr( 3, acs, f1, ae, kint, pinc(i))
c   adjust to left side
                    pt1(i) = pt1(i)-(float(nsym-1)/2.)*pinc(i)
c                    call f9( 'pt1', 3, pt1(i))
c                    call f9( 'pin', 3, pinc(i))
                    go to 23
                endif
  26        continue
c   no match
            call typstg( lunout(), 'DISPLAY--NO MATCH FOR FIELD ^',1,0)
            call typstg( lunout(), acs, aa, na )
        endif
        go to 23
c
c
c   extract level information if any by looping thru ICS
  44    continue
c
        ae = a1-1
  46    continue
        if( ae+1 .ge. a2 ) then
            go to 66
        endif
        call nxtatt( acs, ae+1, a2, f1, nch, ae )
c
        if( same( acs, f1, 5, 'LEVEL', 1 )) then
            do 51 i = 1, nl
    	        xlev(i) = .false.
  51        continue
c            call inslx( acs, f1, nch )
            arg = 0
  55        continue
            arg = arg+1
c            call i8( 'arg', 3, arg )
            call attarg( acs, f1, ae, arg, aa, na )
            if( na .gt. 0 ) then
                call dcode( acs, aa, na, ityp, kint, zlev )
            else
                go to 46
            endif
c
            if( ityp .gt. 0 ) then
c   find this level
                call lvlsrch( zval, nl, zlev, ith )
c                call i8( 'ith', 3, ith )   
                xlev( ith ) = .true.        
            elseif( indexc( acs, aa, aa+na-1, 1, '-' ) .gt. 0 ) then
c   a run
                ab = indexc( acs, aa, aa+na-1, 1, '-' )
                call dcode( acs, aa, ab-aa, ityp, kint, zlev )
                call lvlsrch( zval, nl, zlev, ith )    
                call dcode( acs, ab+1, aa+na-(ab+1),ityp,kint,zlev)
                call lvlsrch( zval, nl, zlev, jth )
                do 59 i = ith, jth
                    xlev(i) = .true.
  59            continue
            endif
            go to 55
        endif
        go to 46
c
  66    continue
c
  88    continue
  90    continue
        return
        end
c----------------------------------------------------------------------
        subroutine lvlsrch( x, n, q, i )
c   this routine does a binary search of the "n" element floating point
c   array "x" for value "q" and returns the index "i".
c
        implicit integer( a-z )
        real x(*), q, zdelta
c
c
        r = n
c
        zdelta = 1.e19
        do 99 k = 1, n
            if( abs( x(k)-q ) .lt. zdelta ) then
                zdelta = abs( x(k)-q )
                i = k
            endif
  99    continue
c
        return
        end
c----------------------------------------------------------------------
        subroutine bsrch( x, n, q, i )
c   this routine does a binary search of the "n" element floating point
c   array "x" for value "q" and returns the index "i".
c
        implicit integer( a-z )
        real x(*), q, zdelta
c
c
c        call insl( '**bsrch**   ', 10 )
c        call f9( 'q', 1, q )
c                  
        l = 1
        r = n
c        call i8( 'n', 1, n )
c
  11    continue
        i = (l+r)/2
        if( q .lt. x(i)) then
            r = i-1
        elseif( q .gt. x(i)) then
            l = i+1
        else
            go to 90
        endif
c
        if( l .le. r ) then
            go to 11
        endif
c
  90    continue
c        call i8( 'i', 1, i )
c        call f9( 'z', 1, x(i))
c        zdelta = 1.e19
c        do 99 k = 1, n
c            if( abs( x(k)-q ) .lt. zdelta ) then
c                zdelta = abs( x(k)-q )
c                i = k
c            endif
c  99    continue
c        call i8( 'i', 1, i )
c        call f9( 'z', 1, x(i))
c
        return
        end
C----------------------------------------------------------------------
        subroutine codeit( sf, xval, nx, scr, is, pt1, pinc, syms, ns,
     +      zap )
c  this routine converts the values to symbols
c
        implicit integer( a-z )
        character syms*(*)
        dimension scr(*), xval(*)
        real sf, pt1, pinc, rcpinc                  
c
c                  
        rcpinc = 1./pinc
        do 22 i = 1, nx
            if( xval(i) .eq. zap ) then
                call put8( scr, is, ichar( ' ' ))
            else
                n = (float(xval(i))*sf - pt1) * rcpinc + 1.
                if( n .lt. 1 .or. n .gt. ns ) then
                    call put8( scr, is, ichar( ' ' ))
                else
                    call put8( scr, is, ichar( syms(n:n)))
                endif
            endif
            is = is+1
  22    continue
c
  90    continue
        return
        end
c----------------------------------------------------------------------
        subroutine bsym( mne, ith, pt1, pinc, syms, ns, l )
c  this routine outputs a table of the alphanumeric symbols versus
c  the values they symbolize.
c
c  function inca(i,n) returns i as its value but also increments i by n
c  so the next time i is used, it is n greater 
c
c
        character   mne*(*), syms*(*), l*(*)
        integer c
c
        lout = lunout()
c
          call bfill( l, 1, 128 )
          c = 1
          call zipstr( ' FIELD: ^', l, c )
          call copyc( mne, ith, 8, l, inca(c,8)) 
          call zipstr( '    INTERVAL BETWEEN SYMBOLS = ^', l, c )
          call ncode( l, inca(c,6), 6, 2, ix, pinc, 2 )
          call typlin( lout, l, c-1 )
c
          call bfill( l, 1, 128 )
          pt = pt1
          c = 2
          do 11 i = 1, ns, 5
            if( ns*2 - c .gt. 7 ) then
                call ncode( l, inca(c,10), 7, 2, x, pt, 2 )
            endif
            pt = pt+5.*pinc
 11       continue
          call typlin( lout, l, 128 )
c
          call bfill( l, 1, 128 )
          c = 6                    
          do 22 i = 1, ns, 5
            if( ns*2 - c .gt. 3 ) then
                call putc( l, c, '|' )
            endif
            c = c + 10
 22       continue
          call typlin( lout, l, 128 )
c
          call bfill( l, 1, 128 )
          c = 6
          do 33 i = 1, ns
            call putc( l, inca(c,2), syms(i:i))
 33       continue
          call typlin( lout, l, 128 )
c
 90     continue
        return
        end
C----------------------------------------------------------------------
        subroutine bsit( scr, nx, ny, lev, zval, 
     +      xmin, xinc, ymin, yinc, ll )
c  this routine actually prints the display for this field 
c
        implicit integer( a-z )
        character   ll*(*)
        dimension   scr(*)
        real xmin, xinc, ymin, yinc, x, y, zval
c
c
c
        lout = lunout()
c  print the level and zvalue
        c = 1
        call zipstr( 'LEVEL:^', ll, c )
        call zncode( ll, c, 3, 1, lev, x,x)
        call bfill( ll, inca(c,4), 4 )
        call zncode( ll, c, 5, 2, x, zval, 2 )
        call zipstr( ' KM.^', ll, c )
        call typlong( lout, ll, 1, c-1 )
c
c  print distances along the top
        call bfill( ll, 1, 444 )
        c = 4
        x = xmin
        do 11 i = 1, nx, 10
            call ncode( ll, inca(c,10), 6, 2, ix, x, 1 )
            x = x + 10*xinc
  11    continue
        call typlong( lout, ll, 1, c-4 )
c
        call bfill( ll, 1, 444 )
        c = 8
        do 14 i = 1, nx, 10
            call putc( ll, inca(c,10), '|' )
  14    continue
        call typlong( lout, ll, 1, c-9 )
c
c  determine the line where the first y distance will be shown
        lytrip = ny - mod( ny-1, 5 )
c            
c  now print the lines
        j = (ny-1)*nx + 1
        do 22 i = ny, 1, -1
            call bfill( ll, 1, 7 )
            if( i .le. lytrip ) then
                call ncode( ll, 1, 6, 2, ix, ymin+(i-1)*yinc, 1 )
                lytrip = lytrip - 5
            endif
            call copync( scr, inca(j,-nx), nx, ll, 8 )
            call typlong( lout, ll, 1, nx+7 )
  22    continue
c
c  do the distances along the bottom
        call bfill( ll, 1, 444 )
        c = 8
        do 33 i = 1, nx, 10
            call putc( ll, inca(c,10), '|' )
  33    continue
        call typlong( lout, ll, 1, c-9 )
c
        call bfill( ll, 1, 444 )
        c = 4
        x = xmin
        do 36 i = 1, nx, 10
            call ncode( ll, inca(c,10), 6, 2, ix, x, 1 )
            x = x + 10*xinc
  36    continue
        call typlong( lout, ll, 1, c-4 )
c
  90    continue
        return
        end
c----------------------------------------------------------------------
        subroutine inisym( syms, nsym )
c   this routine initializes the display symbol table unless there is 
c   already something there
c
        implicit integer( a-z )
        character   syms*(*)
        character*1 char
c
        if( nsym .gt. 0 ) then
            go to 90
        endif
c
c  set up default symbol table
        ich = ichar( 'a' )
        do 11 i = 25, 1, -1
            syms(i:i) = char( inca(ich,1))
  11    continue 
        syms(26:26) = '0'
        ich = ichar( 'A' )         
        do 22 i = 27, 51
            syms(i:i) = char( inca(ich,1))
  22    continue 
        nsym = 51
c
  90    continue
        return
        end
c----------------------------------------------------------------------
        subroutine histit
c----------------------------------------------------------------------
c
c  maximun number of cartons
        parameter( maxcart=4001 )
        integer carnum( maxcart ), shelf( maxcart ), usage( maxcart )
c
      integer drecln

        common / storef /
     a          drecln, lencar, mxshlf, carnum, shelf,  usage
c
        call bars( usage, maxcart )
        return
        end
c----------------------------------------------------------------------
        subroutine bars( a, n )
c----------------------------------------------------------------------
c  this routine contructs a printer bar chart based on the n values
c  of array a
c
        dimension a(1)
        character*132 l
        integer a, c, r1, r2, tot
        data lu / 9 /
c
        if( n .le. 0 ) then
          call typstg( 6,
     a'the number of points in the bar chart array is .le. 0^',
     a          1, 0 )
          go to 90
        endif
c
c  open histogram output file
        call opnbrs( lu )
c
        call hrng( a, n, r1, r2 )
        if( r1 .eq. r2 ) then
          call typstg( 6,
     a'there is no range to the data for the bar chart^',
     a          1, 0 )
          go to 90
        endif
c
c  do the annotation across the top (plot starts in col. 11)
        call page( lu )
        call bfill( l, 1, 132 )
        r = r1
        scale = 100./float( r2 - r1 )
        c = 7
        do 11 i = 1, 10
          call ze9( r, l, c )
          c = c+10
          r = r +.1*float( r2 - r1 )
 11     continue
        call typlin( lu, l, 128 )
c
c  put in the tic marks
        call bfill( l, 1, 132 )
        c = 12
        do 13 i = 1, 10
          call putc( l, c, 'V' )
          c = c + 10
 13     continue
        call typlin( lu, l, 128 )
c
        tot = 0
c  do the bars
        do 44 i = 1, n
          call bfill( l, 1, 132 )
          if( mod( i, 5 ) .eq. 0 ) then
c  put in some annotation
            call ncode( l, 2, 9, 1, i, x, x )
          endif
c
          k = scale*( a(i)-r1 ) + .4999999999
c               type *, 'k,i,a,r', k, i, a(i), r1
          call cfill( l, 12, k, 'H' )
          call typlin( lu, l, 128 )
          tot = tot+a(i)
 44     continue
        call lf( lu )
        call bfill( l, 1, 132 )
        c = 1
        call zipstr( ' TOTAL HITS=^', l, c )
        call zncode( l, c, 7, 1, tot, x, y )
        call typlin( lu, l, 128 )
c
 90     continue
        return
        end
c----------------------------------------------------------------------
        subroutine hrng( a, n, r1, r2 )
c----------------------------------------------------------------------
c  the purpose of this routine is to determine the range of the n
c  values in the array 'a'.
c
        implicit        integer( a-z )
        dimension a(1)
c
        if( n .le. 0 ) then
          r1 = 0
          r2 = 0
        endif
c
        r1 =  32000
        r2 = -32000
c
        do 11 i = 1, n
c
          if( a(i) .gt. r2 ) then
            r2 = a(i)
          endif
c
          if( a(i) .lt. r1 ) then
            r1 = a(i)
          endif
c
 11     continue
c
        return
        end
c----------------------------------------------------------------------
        subroutine ze9( z, l, c )
c----------------------------------------------------------------------
c  this routine encodes a real no. in e9.3 format into string l at
c  location c.
c
        implicit        integer( a-y )
        character       l*(*)
        character*10    n
c
    	write( n, 123 ) z
c        encode( 10, 123, n ) z
 123    format( e10.3 )
c  get the sign
        l(c:c) = n(1:1)
c  forget the zero                          
        call copyc( n, 3, 8, l, c+1 )
        return
        end
C----------------------------------------------------------------------
      subroutine typlong( lun, is, ith, nchr )
c  this routine prints an arbitrary string of one line or less to
c  logical unit 'lun'.
c
        implicit integer( a-z )
    	parameter( LLIM=444 )
        character is*(*)
c
c
        i = ith
        n = nchr
	k = LLIM
c
        if( n .lt. 1 ) then
c  search for a "^"
            h = index( is(i:i+444), '^' )
            if( h .gt. 0 ) then
                n = h -1
            else
                n = len( is )
            endif
        endif
c
        if( n .lt. 1 ) then
          go to 90
        endif
c
 11     continue
        if( n .gt. LLIM ) then
          j = i+LLIM-1
          n = n-LLIM
        else
          j = i + n -1
          n = 0
        endif
c
        write( lun, 988 ) is(i:j)
 988    format( ' ', a )
c	write(lun,*) nchr, n, i, j, k
c
        if( n .gt. 0 ) then
          i = j+1
          go to 11
        endif
c
 90     continue
        return
        end
C----------------------------------------------------------------------

