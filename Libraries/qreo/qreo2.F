c--------------------------------------------------------------------------
        block data qreo2
        parameter( maxcart=4001, sizcart=1024 )
        common  / storef /      drecln,         lencar,         mxshlf
     +,         carnum(maxcart), shelf(maxcart),  usage(maxcart)
        integer drecln, lencar, mxshlf, carnum, shelf, usage
        data drecln / sizcart /
        end
c--------------------------------------------------------------------------
c..file:reo2.for
c
c  this file contains the following routines
c	inireo
c	genhed
c	transc
c	toeart
c	tosurf
c	putasc
c	advxyz
c	adhxyz
c	xyz
c	esurf
c	dxyz
c	dcopl
c	xyswap
c	insoff
c       filter
c
c--------------------------------------------------------------------------
        subroutine inireo( buf, advect, aaz, advec
     +,         advhgt, advx, advy, azrad, elrad, rgrad, rg )
c  this routine defaults all parameters that have not been input
c
        parameter(      maxval=500,     maxfld=7,       maxf32=maxfld*8
     +,                 par=22,         list=4,         infil=11
     +,                 outfil=12,      maxgt=2048,     mxtfld=maxfld+3
     +,                 range1=0.0,     range2=10000.0, zap=-32768.
     +,                 mmm=mxtfld*maxgt
     +,                 torad=0.0174532925199433
     +)
c
        integer advect
    	real rg(*)
c
        real    aaz(maxval),    advec(maxval),  advhgt(maxval)
        real    advx(maxval),   advy(maxval)
c
        common  / datset /      slong,          slati,          salti
     +,         dlong,          dlati,          dalti,          dbase
     +,         axmin(3),       axmax(3),       radius(3),    delta(3)
     +,         deltai(3),      numval(3),      spacng(3)
     +,         tref,           values(maxval,3)
c 
	real zvx(maxval),   zvy(maxval),  zht(maxval)
      integer tref
c
c
c
c        call insl( '*inireo*', 8 )
c
c
c       call f9( 'db', 2, dbase )
        if( slong .lt. -32000. ) slong = ulongt( buf )
        if( slati .lt. -32000. ) slati = ulatit( buf )
        if( salti .lt. -32000. ) salti = uhight( buf )
        if( dlong .lt. -32000. ) dlong = slong
        if( dlati .lt. -32000. ) dlati = slati
        if( dalti .lt. -32000. ) dalti = salti
        if( dbase .lt. -32000. ) dbase = 90.
c       dbase = dbase*torad
c
        if( axmin(1) .lt. -32000. ) axmin(1) = 10.
        if( axmax(1) .lt. -32000. ) axmax(1) = 30.
        if( spacng(1) .le. 0 ) spacng(1) = .5
        if( radius(1) .le. 0 ) radius(1) = 1.0
        n = ( axmax(1)-axmin(1))/spacng(1) + 1.000001
        if( n .gt. maxval ) n = maxval
        numval(1) = n
        do 111 i = 1, maxval
 111    values(i,1) = axmin(1) + (i-1)*spacng(1)
        delta(1) = spacng(1)
        deltai(1) = 1./spacng(1)
        i = 1
c        call insl( 'xaxis', 5 )
c        call f9( 'min', 3, axmin(i))
c        call f9( 'max', 3, axmax(i))
c        call f9( 'spc', 3, spacng(i))
c        call f9( 'ide', 3, deltai(i))
c        call f9( 'rad', 3, radius(i))
c        call i8( 'n', 1, n )
c
        if( axmin(2) .lt. -32000. ) axmin(2) = 10.
        if( axmax(2) .lt. -32000. ) axmax(2) = 30.
        if( spacng(2) .le. 0 ) spacng(2) = .5
        if( radius(2) .le. 0 ) radius(2) = 1.0
        n = ( axmax(2)-axmin(2))/spacng(2) + 1.000001
        if( n .gt. maxval ) n = maxval
        numval(2) = n
        do 222 i = 1, maxval
 222    values(i,2) = axmin(2) + (i-1)*spacng(2)
        delta(2) = spacng(2)
        deltai(2) = 1./spacng(2)
        i = 2
c        call insl( 'yaxis', 5 )
c        call f9( 'min', 3, axmin(i))
c        call f9( 'max', 3, axmax(i))
c        call f9( 'spc', 3, spacng(i))
c        call f9( 'ide', 3, deltai(i))
c        call f9( 'rad', 3, radius(i))
c        call i8( 'n', 1, n )
c
        if( axmin(3) .lt. -32000. ) axmin(3) = .5
        if( axmax(3) .lt. -32000. ) axmax(3) = 15.
        if( spacng(3) .le. 0 ) spacng(3) = 1.0
        if( radius(3) .le. 0 ) radius(3) = 1.0
        n = ( axmax(3)-axmin(3))/spacng(3) + 1.000001
        if( n .gt. maxval ) n = maxval
        numval(3) = n
        do 333 i = 1, maxval
 333    values(i,3) = axmin(3) + (i-1)*spacng(3)
        delta(3) = spacng(3)
        deltai(3) = 1./spacng(3)
        i = 3
c        call insl( 'zaxis', 5 )
c        call f9( 'min', 3, axmin(i))
c        call f9( 'max', 3, axmax(i))
c        call f9( 'spc', 3, spacng(i))
c        call f9( 'ide', 3, deltai(i))
c        call f9( 'rad', 3, radius(i))
c        call i8( 'n', 1, n )
c
       if( azrad .gt. 0 .and. elrad .le. 0 ) then
         elrad = azrad
       elseif( elrad .gt. 0 .and. azrad .le. 0 ) then
         azrad = elrad
       endif
c
c        call f9( 'azrad', 5, azrad )
c        call f9( 'elrad', 5, elrad )
c        call dmplin( ' ', 1 )
c
c
        nh = 0
c  see if any advection values
        do 40 i = 1, maxval
          if( advhgt(i) .ne. zap ) nh = nh+1
 40     continue
        advect = nh
c	call insl( 'advect', 6 )
c        call i8( 'nh', 2, nh )
c
        if( nh .lt. 1 ) then
          go to 55
        endif
c
c  convert advection velocities to x and y compenents in km/sec
        do 41 i = 1, nh
          zaz = (90.-aaz(i))*torad
          advx(i) = advec(i)*cos(zaz)*.001
          advy(i) = advec(i)*sin(zaz)*.001
c          call i8( 'i', 1, i )
c          call f9( 'x', 1, advx(i))
c          call f9( 'y', 1, advy(i))
 41     continue
c
        if( nh .le. 1 ) then
          go to 55
        endif
c  if more than one sort by height
        do 45 i = 1, nh-1
	    do 45 j = i+1, nh
                if( advhgt(j) .lt. advhgt(i)) then
                  tempx = advx(i)
                  tempy = advy(i)
                  tempht = advhgt(i)
                  advx(i) = advx(j)
                  advy(i) = advy(j)
                  advhgt(i) = advhgt(j)
                  advx(j) = tempx
                  advy(j) = tempy
                  advhgt(j) = tempht
                endif
 45     continue
c          call typstg( 6, 'sort^', 1, 0 )
c          call typefe( advx, 1, 20 )
c          call typefe( advy, 1, 20 )
c          call typefe( advhgt, 1, 20 )
c
c   copy them into scratch arrays
c
	do 46 ii = 1, nh
	    zvx(ii) = advx(ii)
	    zvy(ii) = advy(ii)
	    zht(ii) = advhgt(ii)
	    advx(ii) = zap
	    advy(ii) = zap
	    advhgt(ii) = zap
 46	continue
c
c  now position them closest to the actual grid points
        dzh = delta(3)*.5
	nlev = numval(3)
c
	do 488 ii = 1, nlev
	    zlev = values(ii, 3)
	    dzlast = 1.e22

	    do 477 jj = 1, nh
		if(abs(zlev - zht(jj)) .le. dzlast) then
		    dzlast = abs(zlev - zht(jj))
		    kk = jj
		endif
 477	    continue
c
	    if(dzlast .le. dzh) then
c   the delta is .le. half the resolution
		advx(ii) = zvx(kk)
		advy(ii) = zvy(kk)
		advhgt(ii) = zht(kk)
	    endif
 488	continue
c
c          call typstg( 6, 'position^', 1, 0 )
c          call typefe( advx, 1, 20 )
c          call typefe( advy, 1, 20 )
c          call typefe( advhgt, 1, 20 )
c  now do a linear fill of the arrays
        call linfit( advx, 1, maxval, zap )
c          call typstg( 6, 'advx^', 1, 0 )
c          call typefe( advx, 1, 20 )
        call linfit( advy, 1, maxval, zap )
c          call typstg( 6, 'advy^', 1, 0 )
c          call typefe( advy, 1, 20 )
        call linfit( advhgt, 1, maxval, zap )
c          call typstg( 6, 'advhgt^', 1, 0 )
c          call typefe( advhgt, 1, 20 )
c
 55     continue
c
c   initialize the range array
    	do 57 i = 1, maxgt
    	    rg(i) = 0
  57	continue

c       call f9( 'sl', 2, slong )
c       call f9( 'sl', 2, dlong )
c       call f9( 'db', 2, dbase )
        return
        end
c----------------------------------------------------------------------
      subroutine genhed( kbuf, hed, fldnam, gnyqv, grc, acs, nacs
     +       , sprint_xlt, iadjust_z )
c  this routine builds the mudras file header record
c
c  access string
        character   acs*(*)
        logical sprint_xlt, iadjust_z
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
c
      parameter( radian=57.29578 )
      parameter( torad=0.0174532925199433 )
c 
      integer delete
      parameter(delete = -32768)
      parameter(zdelet=-32768.)
      parameter(cf = 64.0)
      parameter(sf = 100.0)
      parameter(scale = 64.0)
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
      integer dtm(6)
c
        logical copln
c
        character*88 str
        integer cn
c
c   convert a 16 bit word number into a character number
        cn(n) = n*2-1
c   mnemonic pointer
        mnptr( i ) = 1 + (i-1)*8
c
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c
c
      rota=cos(3.1415926535/180.0*(dbase))
      rotb=sin(3.1415926535/180.0*(dbase))
        copln = namefx( 'COPLANE', i ) .ne. 0
c 
c  set up 510 16 bit word volume header 
c
      call putasc(hed,5,'REOR',4)
      call putasc(hed,7,'03',2)
        str(1:4) = ' '
        call getstr( 'PROJECT', str, 1, n )
        if( n .gt. 0 ) then
            call putasc(hed,8,str,4)
        else
    	    call getproj( str, n )
    	    if( n .gt. 0 ) then
            	call putasc(hed,8,str,4)
    	    else
            	call kproj( kbuf, str )
            	call putasc(hed,8,str,4)
    	    endif
        endif
c
c  search the string for a scientist name
        str(1:6) = ' '
        call getstr( 'SCIENTIST', str, 1, n )
        if( n .gt. 0 ) then
            call putasc(hed,10,str,6)
            call putasc(hed,48,str,6)
        else
    	    call getnam(str,n)
    	    if( n .gt. 0 ) then
            	call putasc(hed,10,str,6)
            	call putasc(hed,48,str,6)
    	    else
            	call putasc(hed,10,'ANONYMOUS',6)
            	call putasc(hed,48,'ANONYMOUS',6)
    	    endif
        endif
      call kradar( kbuf, str )
      call putasc(hed,13,str,6)
c
      call bfill( str, 1, 4 )
      call scntyp( kswepm(kbuf), str, 1, nc )
      call putasc(hed,16,str,4)
c     hardwired to "CRT" per J. Miller
      call putasc(hed,16,'CRT ',4)
c
      call putasc(hed,18,'000000',6)

      call put16(hed,21,kyear(kbuf))
      call put16(hed,22,kmonth(kbuf))
      call put16(hed,23,kday(kbuf))
      call put16(hed,24,khour(kbuf))
      call put16(hed,25,kminut(kbuf))
      call put16(hed,26,ksecnd(kbuf))
c 
      deg=int(dlati)
      frac = dlati-deg
      min=int(frac*60.0)
      sec = (frac-(float(min)/60.))*3600.
      call put16(hed,33,deg)
      call put16(hed,34,min)
      call put16(hed,35,nint(sec*sf))
c 
      deg=int(dlong)
      frac = dlong-deg
      min=int(frac*60.0)
      sec = (frac-(float(min)/60.))*3600.
      call put16(hed,36,deg)
      call put16(hed,37,min)
      call put16(hed,38,nint(sec*sf))
c 
      call put16(hed,39,nint(dalti*1000.0))
      call put16(hed,40,nint(cf*dbase))
      call put16(hed,41,0)
      call put16(hed,42,0)
      str(1:4) = ' '
      call ktzone( kbuf, str )
      call putasc(hed,43,str,4)
c  search the string for a sequence number
        str(1:6) = ' '
        call getstr( 'SEQUENCE', str, 1, n )
        if( n .gt. 0 ) then
            call putasc(hed,45,str,6)
        else
    	    call getseq(str,n)
    	    if( n .gt. 0 ) then
            	call putasc(hed,45,str,6)
    	    else
            	call putasc(hed,45,'ANONYMOUS',6)
    	    endif
        endif
 122  format( i2, '/', i2, '/', i2 )
 123  format( i2, ':', i2, ':', i2 )

      call foflocaltime( dtm )
      write( str, 122 ) dtm(2), dtm(3), mod(dtm(1),100)
      call putasc(hed,51,str,8)
      write( str, 122 ) dtm(4), dtm(5), dtm(6)
      call putasc(hed,55,str,8)
      call put16(hed,59,0)
      call put16(hed,60,1)
      call put16(hed,61,510)
      call putasc(hed,62,'RD',2)
      call put16(hed,63,16)
      call put16(hed,64,2)
      call put16(hed,65,3200)
      call putasc(hed,66,'SU',2)
      call put16(hed,67,delete)
      call put16(hed,68,100)
      call put16(hed,69,64)
      call put16(hed,70,0)
c
c  input tape name
        str(1:6) = ' '
        call ktapen(kbuf,str)
        call put16c( hed, 71, 6, str )
c
        call bfill( str, 1, 7*6 )
        call put16c( hed, 74, 7*6, str )
        call put16(hed,95,0)
c 
      n1=(numval(1)*numval(2)-1) /3200 +1
      call put16(hed,96,n1)
c 
      n1=n1*noutf
      call put16(hed,97,n1)
c 
      n2=n1*numval(3)
      call put16(hed,98,n2)
c 
      n2=1+numval(3)*(n1+1)
      call put16(hed,99,n2)
c 
      call put16(hed,100,0)
      	ix = ((kmonth(kbuf)*100+kday(kbuf))*100+khour(kbuf))*100+
     a	    	kminut(kbuf)
    	call ncode( str, 1, 8, 1, ix,x,x)
    	call zrofil( str, 1, 8 )
      	call putasc(hed,101,str,8)
c 
      call put16(hed,105,0)
      call put16(hed,106,numval(3))
      call put16(hed,107,delete)
      call put16(hed,108,delete)
      call put16(hed,109,delete)
      call put16(hed,110,0)
      call put16(hed,111,1)
      call put16(hed,112,0)
      call put16(hed,113,0)
      call put16(hed,114,0)
      call put16(hed,115,0)
c
      call put16(hed,116,kyear(kbuf))
      call put16(hed,117,kmonth(kbuf))
      call put16(hed,118,kday(kbuf))
      call put16(hed,119,khour(kbuf))
      call put16(hed,120,kminut(kbuf))
      call put16(hed,121,ksecnd(kbuf))
c 
      call put16(hed,128,delete)
c 
      call put16(hed,129,4)
      call put16(hed,130,0)
      call put16(hed,131,0)
c 
      call put16(hed,132,delete)
      call put16(hed,133,delete)
      call put16(hed,134,delete)
      call put16(hed,135,delete)
      call put16(hed,136,delete)
      call put16(hed,137,delete)
      call put16(hed,138,0)
      call put16(hed,139,1)
      call put16(hed,140,0)
      call put16(hed,141,0)
      call put16(hed,142,delete)
      call put16(hed,143,delete)
      call put16(hed,144,delete)
      call put16(hed,145,delete)
      call put16(hed,146,delete)
      call put16(hed,147,delete)
      call put16(hed,148,delete)
      call put16(hed,149,2)
      call put16(hed,150,0)
      if( copln ) then
        call putasc(hed,151,'CO',2)
      else
        call putasc(hed,151,'PP',2)
      endif
      call put16(hed,152,values(1,3))
      n1=numval(3)
      call put16(hed,153,values(n1,3))
      call put16(hed,154,n1)
      call put16(hed,155,(values(n1,3)-values(1,3))/n1)
      call put16(hed,156,delete)
      call put16(hed,157,1)
      call put16(hed,158,nint(cf*dbase))
      call put16(hed,159,3)
c 
      n1 = numval(1)
      call put16(hed,160,nint(sf*values(1,1)))
      call put16(hed,161,nint(sf*values(n1,1)))
      call put16(hed,162,n1)
      call put16(hed,163,nint(1000.0*(values(2,1)-values(1,1))))
      call put16(hed,164,1)
c                                         
      n1 = numval(2)
      call put16(hed,165,nint(sf*values(1,2)))
      call put16(hed,166,nint(sf*values(n1,2)))
      call put16(hed,167,n1)
      call put16(hed,168,nint(1000.0*(values(2,2)-values(1,2))))
      call put16(hed,169,2)
c 
      n1 = numval(3)
      if( copln ) then
        call put16(hed,170,nint(1000.*values(1,3)))
        call put16(hed,171,nint(1000.*values(n1,3)))
        call put16(hed,172,n1)
        call put16(hed,173,
     a    nint(1000.0*(values(2,3)-values(1,3))))
        call put16(hed,174,3)
      else
        call put16(hed,170,nint(1000.*values(1,3)))
        call put16(hed,171,nint(1000.*values(n1,3)))
        call put16(hed,172,n1)
        call put16(hed,173,nint(1000.0*(values(2,3)-values(1,3))))
        call put16(hed,174,3)
      endif
c 
      call put16(hed,175,noutf)
c   for each possible output field
      do 70 i=1,25
        i16w = 176+(i-1)*5
        if(i.le.noutf)then
c   copy the output field name
            mp = mnptr(i)
            call put16c( hed, i16w, 8, mnames(mp:mp+7))
c   and the scale value
            call put16(hed,i16w+4,nint(scale))
            if( mnames(mp:mp+3) .eq. 'TIME' ) then
               call put16( hed, i16w+4, 10 )
            endif
        else
            call put16c( hed, i16w, 8, '        ' )
            call put16(hed,i16w+4,0)
        endif
 70   continue
c 
      call put16(hed,301,numval(1)*numval(2))
      call put16(hed,302,2)
      call put16(hed,303,1)
      call put16(hed,304,nint(gnyqv*sf))
      call put16(hed,305,nint(grc*sf))
c 
      call putasc(hed,306,'ORIGIN',6)
      call put16(hed,309,0)
      call put16(hed,310,0)
      call put16(hed,311,0)
c 
      if( namefx( 'AIRCRAFT', i ) .ne. 0 ) then
      	call putasc(hed,312,'AIRBRN',6)
      else
      	call ksite( kbuf, str )
      	call putasc(hed,312,str,6)
      endif


      if( sprint_xlt ) then
         call ll2xydrv( slati, -slong, x, y, dlati, -dlong, dbase )
         z = salti - dalti
      else
         call transc(dbase, 0.0, 0.0, 0.0,
     1        salti,
     1        slati,
     1        slong,
     1        dalti,
     1        dlati,
     1        dlong,
     1        x,y,z)
         if( iadjust_z ) then
            z = salti - dalti
         endif
      endif
      call put16(hed,315,nint(x*sf))
      call put16(hed,316,nint(y*sf))
      call put16(hed,317,nint(z*1000.))
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      call put16(hed,400,((numval(1)*numval(2)-1)/4+1)*numval(3))
      call put16(hed,451,(numval(1)*numval(2)-1)/4+1)
      call put16(hed,452,((numval(1)*numval(2)-1)/4+1)*numval(3))
      call put16(hed,453,0)
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        call e12( 'x0', 2, x )
        call e12( 'y0', 2, y )
        call e12( 'z0', 2, z )
c      call tswp16( hed, 1, 330 )
c
      return
      end
c----------------------------------------------------------------------X
c
      SUBROUTINE LL2XYDRV(PLAT, PLON, X, Y, ORLAT, ORLON, ANGXAX)
C
C     DRIVER ROUTINE FOR CONVERTING THE SEPARATION OF TWO POINTS
C     SPECIFIED IN LAT, LON TO SEPARATION IN X, Y IN KM. 
C     THIS ROUTINE (AND LL2XY) WILL WORK FOR POINTS IN ANY PART OF
C     THE GLOBE WITH SOME RESTRICTIONS (SEE BELOW). THE ANGLE CONVENTIONS ARE:
C     0   < LAT < 90   ==>  NORTHERN HEMISPHERE
C     -90 < LAT < 0    ==>  SOUTHERN HEMISPHERE
C     
C     0    < LON < 180 ==>  WESTERN HEMISPHERE
C     -180 < LON < 0   ==>  EASTERN HEMISPHERE
C     
C     PLAT  - LAT. OF POINT FOR WHICH X,Y IS DESIRED
C     PLON  - LON. OF POINT FOR WHICH X,Y IS DESIRED
C     X     - OUTPUT X VALUE RELATIVE TO ORLAT, ORLON IN KM
C     Y     - OUTPUT Y VALUE RELATIVE TO ORLAT, ORLON IN KM
C     ORLAT - LAT. OF ORIGIN
C     ORLON - LON. OR ORIGIN
C     ANGXAX- ANGLE OF X-AXIS REL. TO TRUE NORTH (USUALLY 90.0)
C
C     KNOWN RESTRICTIONS AND LIMITATIONS:
C
C     1) ||PLAT| - |ORLAT|| <= 90.0
C     2) ||PLON| - |ORLON|| <= 90.0
C     3) NO INPUT LAT OR LON VALUE SHOULD BE EQUAL TO EXACTLY ZERO
C     4) THE CODE IS NOT SETUP TO HANDLE CROSSING OVER BOTH HEMISPHERE 
C     BOUNDARIES AT ONCE; IT CAN HANDLE CROSSING EITHER THE NORTH/SOUTH 
C     HEMISPHERE BOUNDARY OR THE WEST/EAST BOUNDARY, BUT NOT BOTH AT ONCE. 
C     FOR EXAMPLE, YOU CAN'T HAVE AN ORIGIN AT (1.0 deg, 1.0 deg) AND TRY 
C     TO FIND THE X,Y OF A POINT AT (-1.0 deg, -1.0 deg). YOU COULD FIND 
C     THE X,Y OF A POINT AT (-1.0 deg, 1.0 deg), HOWEVER.
C     5) CODE WON'T WORK IF YOU TRY TO CROSS A POLE
C
      
      PARAMETER (EPS=0.0001, DEGRAD=0.01745329)
      
      ICROSS=0
C
C     DETERMINE IF A HEMISPHERE BOUNDARY HAS BEEN CROSSED
C
      IF (SIGN(1.0,ORLAT).GT.SIGN(1.0,PLAT)) ICROSS=1
      IF (SIGN(1.0,ORLAT).LT.SIGN(1.0,PLAT)) ICROSS=2
      IF (SIGN(1.0,ORLON).GT.SIGN(1.0,PLON)) THEN
         IF (ICROSS.NE.0) THEN
            WRITE(*,10)
 10         FORMAT(/,5X,'+++ CANNOT HANDLE DUAL HEMISPHERE CROSSOVER ',
     X           '+++')
            STOP
         ELSE
            ICROSS=3
         END IF
      ELSE IF (SIGN(1.0,ORLON).LT.SIGN(1.0,PLON)) THEN
         IF (ICROSS.NE.0) THEN
            WRITE(*,10)
            STOP
         ELSE
            ICROSS=4
         END IF
      END IF
         
      IF (ORLAT.GT.0.0) THEN
         INHEM=1
      ELSE
         INHEM=0
      END IF
      IF (ORLON.GT.0.0) THEN
         IWHEM=1
      ELSE
         IWHEM=0
      END IF
      IF (ICROSS.EQ.0) THEN
C
C     NO HEMISPHERE CROSSOVER; JUST CALL LL2XY
C
      
C
C     MAKE SIGNED VALUES POSITIVE 
C
         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)         
         
         CALL LL2XY(DEGLAT,DEGLON,X,Y,SWLAT,SWLON)

C
C     SWITCH SIGNS IF NOT IN NORTHERN OR WESTERN HEMISPHERES
C
         IF (INHEM.EQ.0) Y=-Y
         IF (IWHEM.EQ.0) X=-X

      ELSE IF (ICROSS.EQ.1) THEN
C
C     +LAT -> -LAT
C

         DEGLAT=EPS
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)
         
         IF (IWHEM.EQ.0) X1=-X1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =EPS
         SWLON =ABS(PLON)
         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=-(ABS(Y1)+ABS(Y2))
         X=X1
         
      ELSE IF (ICROSS.EQ.2) THEN
C
C     -LAT -> +LAT
C
         DEGLAT=EPS
         DEGLON=ABS(PLON)
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)
         
         IF (IWHEM.EQ.0) X1=-X1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =EPS
         SWLON =ABS(PLON)
         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=(ABS(Y1)+ABS(Y2))
         X=X1
         
      ELSE IF (ICROSS.EQ.3) THEN
C
C     +LON -> -LON
C
         DEGLAT=ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            DEGLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            DEGLON=180.0-EPS
         END IF
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)

         IF (INHEM.EQ.0) Y1=-Y1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            SWLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            SWLON=180.0-EPS
         END IF

         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=Y1
         X=(ABS(X1)+ABS(X2))

      ELSE IF (ICROSS.EQ.4) THEN
C
C     -LON -> +LON
C
         DEGLAT=ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            DEGLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            DEGLON=180.0-EPS
         END IF
         SWLAT =ABS(ORLAT)
         SWLON =ABS(ORLON)

         CALL LL2XY(DEGLAT,DEGLON,X1,Y1,SWLAT,SWLON)

         IF (INHEM.EQ.0) Y1=-Y1

         DEGLAT=ABS(PLAT)
         DEGLON=ABS(PLON)
         SWLAT =ABS(PLAT)
         IF (ABS(PLON).LE.45.0) THEN
            SWLON=EPS
         ELSE IF (ABS(PLON).GE.135.0) THEN
            SWLON=180.0-EPS
         END IF

         CALL LL2XY(DEGLAT,DEGLON,X2,Y2,SWLAT,SWLON)
         
         Y=Y1
         X=-(ABS(X1)+ABS(X2))
         
      END IF

C
C     ROTATE, IF NECESSARY
C
      IF (ANGXAX.NE.90.0) THEN
         THETA=(ANGXAX-90.0)*DEGRAD
         XT=X
         YT=Y
         X=XT*COS(THETA) - YT*SIN(THETA)
         Y=XT*SIN(THETA) + YT*COS(THETA)
      END IF
      

      RETURN

      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE  LL2XY (DEGLAT, DEGLON, X, Y, SWLAT, SWLON)
C
C  TO CONVERT LAT.,LONG. TO X,Y IN KM WITH RESPECT TO SWLAT,SWLON
C  PASCAL BY P. JOHNSON, 17-FEB-81.  FORTRAN TRANS R. VAUGHAN 9/81.
C  FINAL REPAIR, M. BRADFORD, 4/88
C  WARNING!  WORKS ONLY IN NORTHERN/WESTERN HEMISPHERES!
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (PI=3.141592654)
      PARAMETER (R=6380.12)
      PARAMETER (DEGRAD=0.01745329)
C
      REAL DEGLAT,DEGLON,X,Y,SWLON,SWLAT

      X=0.0
      Y=0.0
      ALAT = SWLAT * DEGRAD
      CALAT = COS(ALAT)
      SALAT = SIN(ALAT)
      ALONG = ABS(SWLON * DEGRAD)
      BLAT = DEGLAT*DEGRAD
      BLONG = ABS(DEGLON)*DEGRAD
      CBLAT = COS(BLAT)
      SBLAT = SIN(BLAT)
      DLON = ALONG-BLONG
      DLAT=ABS(DEGLAT-SWLAT)*DEGRAD
      IF(DLAT.LT.0.0001.AND.ABS(DLON).LT.0.0001) GO TO 90
      CDLON = COS(DLON)
      AZA = ATAN(SIN( DLON)/(CALAT*SBLAT/CBLAT-SALAT*CDLON))
      AZB = ATAN(SIN(-DLON)/(CBLAT*SALAT/CALAT-SBLAT*CDLON))
C
C  GET BEARING
C
      IF(BLAT .LT. ALAT) AZA = AZA+PI
      IF(ALAT .LT. BLAT) AZB = AZB+PI
      IF(AZA .LT. 0) AZA = AZA + 2.*PI
      IF(AZB .LT. 0) AZB = AZB + 2.*PI
C
      SINN =  DLAT
      IF(DLON.NE.0.0)
     .       SINN = SIN(DLON) * SIN(PI/2.0-BLAT) / SIN(AZA)
C
      COSN = SALAT*SBLAT + CALAT*CBLAT*CDLON
      S = R * ATAN(SINN/COSN)
      X = S * SIN(AZA)
      Y = S * COS(AZA)
C
   90 CONTINUE
      RETURN
C
      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE XY2LLDRV(PLAT,PLON,XP,YP,ORLAT,ORLON,ANGXAX)
C
C     DRIVER ROUTINE FOR COMPUTING LAT AND LONGITUDE FROM 
C     X,Y  COORDINATES RELATIVE TO LOCATION SPECIFIED BY ORLAT, ORLON.
C     THIS ROUTINE (AND XY2LL) WILL WORK FOR POINTS IN ANY PART OF
C     THE GLOBE WITH SOME RESTRICTIONS (SEE BELOW). THE ANGLE CONVENTIONS ARE:
C     0   < LAT < 90   ==>  NORTHERN HEMISPHERE
C     -90 < LAT < 0    ==>  SOUTHERN HEMISPHERE
C     
C     0    < LON < 180 ==>  WESTERN HEMISPHERE
C     -180 < LON < 0   ==>  EASTERN HEMISPHERE
C
C     PLAT  - OUTPUT LATITUDE OF POINT AT POSITION (X,Y)
C     PLON  - OUTPUT LONGITUDE OF POINT AT POSITION (X,Y)
C     XP,YP - INPUT COORDINATES IN KM OF LOCATION TO BE CONVERTED
C     ORLAT - INPUT LATIT. OF REFERENCE POINT (ORIGIN)
C     ORLON - INPUT LONG.  OF REFERENCE POINT (ORIGIN)
C     ANGXAX- ANGLE OF X-AXIS RELATIVE TO TRUE NORTH (USUALLY 90.0)
C
C     KNOWN RESTRICTIONS AND LIMITATIONS:
C
C     1) ||PLAT| - |ORLAT|| <= 90.0
C     2) ||PLON| - |ORLON|| <= 90.0
C     3) NO INPUT LAT OR LON VALUE SHOULD BE EQUAL TO EXACTLY ZERO
C     4) THE CODE IS NOT SETUP TO HANDLE CROSSING OVER BOTH HEMISPHERE 
C     BOUNDARIES AT ONCE; IT CAN HANDLE CROSSING EITHER THE NORTH/SOUTH 
C     HEMISPHERE BOUNDARY OR THE WEST/EAST BOUNDARY, BUT NOT BOTH AT ONCE. 
C     FOR EXAMPLE, YOU CAN'T HAVE AN ORIGIN AT (1.0 deg, 1.0 deg) AND TRY 
C     TO FIND THE LAT, LON OF A POINT AT (-1000. km, -1000. km). 
C     YOU COULD FIND THE LAT, LON OF A POINT AT (-1000. km, 1000. km), HOWEVER.
C     5) CODE WON'T WORK IF YOU TRY TO CROSS A POLE
      
      PARAMETER (EPS=0.0001, DEGRAD=0.01745329)

      X=XP
      Y=YP
C
C     ROTATE, IF NECESSARY
C
      IF (ANGXAX.NE.90.0) THEN
         THETA=(90.0-ANGXAX)*DEGRAD
         XT=X
         YT=Y
         X=XT*COS(THETA) - YT*SIN(THETA)
         Y=XT*SIN(THETA) + YT*COS(THETA)
      END IF


C
C     DETERMINE IF A HEMISPHERE BOUNDARY IS CROSSED
C
      ICROSS=0
      IF (ORLON.GT.0.0) THEN
         IF (X.GT.0.0 .AND. ORLON.LT.90.0) THEN
            CALL LL2XYDRV(ORLAT,EPS,X2,Y2,ORLAT,ORLON,90.0)
            IF (X2.LT.X) ICROSS=1
         ELSE IF (X.LT.0.0 .AND. ORLON.GT.90.0) THEN
            CALL LL2XYDRV(ORLAT,(180.-EPS),X2,Y2,ORLAT,ORLON,90.0)
            IF (X2.GT.X) ICROSS=2
         END IF
      ELSE IF (ORLON.LT.0.0) THEN
         IF (X.GT.0.0 .AND. ORLON.LT.-90.0) THEN
            CALL LL2XYDRV(ORLAT,(EPS-180.0),X2,Y2,ORLAT,ORLON,90.0)
            IF (X2.LT.X) ICROSS=3
         ELSE IF (X.LT.0.0 .AND. ORLON.GT.-90.0) THEN
            CALL LL2XYDRV(ORLAT,-EPS,X2,Y2,ORLAT,ORLON,90.0)
            IF (X2.GT.X) ICROSS=4
         END IF
      END IF

      IF (ORLAT.GT.0.0) THEN
         IF (Y.GT.0.0) THEN
            CALL LL2XYDRV((90.0-EPS),ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.LT.Y) THEN
               WRITE(*,10)
 10            FORMAT(/,5X,'+++XY2LL ROUTINE CANNOT HANDLE CASE',
     X              ' WHERE POINTS CROSS A POLE+++')
               STOP
            END IF
         ELSE
            CALL LL2XYDRV(EPS,ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.GT.Y) THEN
               IF (ICROSS.EQ.0) THEN
                  ICROSS=5
               ELSE
                  WRITE(*,20)
 20               FORMAT(/,5X,'+++ XY2LL CANNOT HANDLE DUAL HEMISPHERE',
     X                 ' CROSSOVER ','+++')
                  STOP
               END IF
            END IF
         END IF
      ELSE IF (ORLAT.LT.0.0) THEN
         IF (Y.GT.0.0) THEN
            CALL LL2XYDRV(-EPS,ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.LT.Y) THEN
               IF (ICROSS.EQ.0) THEN
                  ICROSS=6
               ELSE
                  WRITE(*,20)
                  STOP
               END IF
            END IF
         ELSE IF (Y.LT.0.0) THEN
            CALL LL2XYDRV((EPS-90.0),ORLON,X3,Y3,ORLAT,ORLON,90.0)
            IF (Y3.GT.Y) THEN
               WRITE(*,10)
               STOP
            END IF
         END IF
      END IF

C
C     NOW PERFORM THE CALCULATIONS
C
      IF (ICROSS.EQ.0) THEN
C
C     NO CROSSOVER
C
         IF (ORLON.LT.0.0) THEN
            ORLON2=-ORLON
            X2=-X
         ELSE
            ORLON2=ORLON
            X2=X
         END IF
         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-Y
         ELSE
            ORLAT2=ORLAT
            Y2=Y
         END IF
         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         IF (ORLON.LT.0.0) PLON=-PLON
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.1) THEN
C
C     +LON (EPS) -> -LON (-EPS)
C
         XNEW=X-X2
         YNEW=Y-Y2

         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-YNEW
         ELSE
            ORLAT2=ORLAT
            Y2=YNEW
         END IF
         ORLON2=EPS
         X2=-XNEW

         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         PLON=-PLON
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.2) THEN
C
C     +LON (180-EPS) -> -LON (EPS-180)
C
         XNEW=X-X2
         YNEW=Y-Y2

         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-YNEW
         ELSE
            ORLAT2=ORLAT
            Y2=YNEW
         END IF
         ORLON2=180.0-EPS
         X2=-XNEW
         
         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         PLON=-PLON
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.3) THEN
C
C     -LON (EPS-180) -> +LON (180-EPS)
C
         XNEW=X-X2
         YNEW=Y-Y2

         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-YNEW
         ELSE
            ORLAT2=ORLAT
            Y2=YNEW
         END IF
         ORLON2=180.0-EPS
         X2=XNEW

         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.4) THEN
C
C     -LON (-EPS) -> +LON (EPS)
C
         XNEW=X-X2
         YNEW=Y-Y2

         IF (ORLAT.LT.0.0) THEN
            ORLAT2=-ORLAT
            Y2=-YNEW
         ELSE
            ORLAT2=ORLAT
            Y2=YNEW
         END IF
         ORLON2=EPS
         X2=XNEW

         CALL XY2LL(PLAT,PLON,X2,Y2,ORLAT2,ORLON2)
         IF (ORLAT.LT.0.0) PLAT=-PLAT
      ELSE IF (ICROSS.EQ.5) THEN
C
C     +LAT -> -LAT
C
         XNEW=X-X3
         YNEW=Y-Y3

         IF (ORLON.LT.0.0) THEN
            ORLON2=-ORLON
            X3=-XNEW
         ELSE
            ORLON2=ORLON
            X3=XNEW
         END IF
         ORLAT2=EPS
         Y3=-YNEW

         CALL XY2LL(PLAT,PLON,X3,Y3,ORLAT2,ORLON2)
         PLAT=-PLAT
         IF (ORLON.LT.0.0) PLON=-PLON
      ELSE IF (ICROSS.EQ.6) THEN
C
C     -LAT -> +LAT
C
         XNEW=X-X3
         YNEW=Y-Y3

         IF (ORLON.LT.0.0) THEN
            ORLON2=-ORLON
            X3=-XNEW
         ELSE
            ORLON2=ORLON
            X3=XNEW
         END IF
         ORLAT2=EPS
         Y3=YNEW

         CALL XY2LL(PLAT,PLON,X3,Y3,ORLAT2,ORLON2)
         IF (ORLON.LT.0.0) PLON=-PLON
      END IF


      RETURN

      END
c
c----------------------------------------------------------------------X
c
      SUBROUTINE XY2LL (DEGLAT,DEGLON,X,Y,SWLAT,SWLON)
C
C-----COMPUTES LATITUDE AND LONGITUDE FROM X,Y COORDINATES RELATIVE TO
C     LOCATION SPECIFIED BY SWLAT,SWLON.
C
C     DEGLAT- OUTPUT PARAMETER IN DEGREES OF LATITUDE
C     DEGLON- OUTPUT PARAMETER IN DEGREES OF LONGITUDE
C     X,Y-    INPUT COORDINATES OF LOCATION TO BE CONVERTED
C     SWLAT-  INPUT PARAMETER IN DEG OF LATITUDE OF REFERENCE LOCATION
C     SWLON-  INPUT PARAMETER IN DEG OF LONG. OF REFERERENCE LOCATION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL DEGLAT,DEGLON,X,Y,SWLAT,SWLON

C      REAL  ACZ
C      REAL  CANG
C      REAL  DTR
      DOUBLE PRECISION  LAMDA1,LAMDA2
C      REAL  PHI1,PHI2
C      REAL  R
C      REAL  RTD
C      REAL  SANG
C      REAL  THETA
C
      DATA DEGARC/111.354/
      DATA IENTRY /0/
C
      IF(IENTRY .EQ. 1) GO TO 100
      IENTRY = 1
      DTR = ATAN(1.)/45.
      RTD = 1./DTR
100   CONTINUE
C
      DEGLAT = SWLAT
      DEGLON = SWLON
C
      R = SQRT(X*X + Y*Y)
      IF (R .LT. 0.01) GO TO 10
C
      THETA = ATAN2(X,Y)
      R = (R/DEGARC) * DTR
C
      PHI1 = DTR * SWLAT
      LAMDA1 = DTR * SWLON
C
      SANG = COS(THETA)*COS(PHI1)*SIN(R)+SIN(PHI1)*COS(R)
      IF(ABS(SANG).GT.1.0) THEN
         IF(SANG .LT. 0.0) SANG = -1.0
         IF(SANG .GT. 0.0) SANG =  1.0
      ENDIF
CSANG=SIGN(1.0,SANG)
      PHI2=ASIN(SANG)
C
      CANG = (COS(R)-SIN(PHI1)*SIN(PHI2))/(COS(PHI1)*COS(PHI2))
      IF(ABS(CANG).GT.1.0) THEN
         IF(CANG .LT. 0.0) CANG = -1.0
         IF(CANG .GT. 0.0) CANG =  1.0
      ENDIF
CCANG=SIGN(1.0,CANG)
      ACZ=ACOS(CANG)
C
      IF (X .LT. 0.0) THEN
          LAMDA2 = LAMDA1 + ACZ
      ELSE
          LAMDA2 = LAMDA1 - ACZ
      END IF
C
      DEGLAT = RTD * PHI2
      DEGLON = RTD * LAMDA2
C
 10   CONTINUE
C
      RETURN
      END

c---------------------------------------------------------------------
      subroutine transc(dbase, xa, ya, za,
     +               alta, lata, lona,
     +               altb, latb, lonb, xb, yb, zb)
c----------------------------------------------------------------------
      real rangea,eleva,azima,alta,lata,lona,
     + rangeb,elevb,azimb,altb,latb,lonb, namefp

      double precision arange,aelev, aazim, aalt, alat, alon,
     + brange, belev, bazim, balt, blat, blon,
     + surfxa, surfya, surfza, surfxb, surfyb, surfzb, x2, y2,
     + alpha2, earx, eary, earz, alpha, beta, piovr2, dbaser
 
      parameter( piovr2= 1.5707963 )
c
c
      dbaser = dbase*0.0174532925199433
      beta = piovr2 - dbaser
      earthr = namefp( 'EARTHR', i )
c
      surfxa = xa
      surfya = ya
      surfza = za
      aalt   = alta
      alat   = lata
      alon   = lona
      balt   = altb
      blat   = latb
      blon   = lonb
c 
 123    format( ' ', 10e12.4 )
      call toeart(alat, alon, earthr+aalt, surfxa, surfya,
     + surfza, earx, eary, earz)
c        call dmplin( 'toeart   ', 6 )
c        print 123, alat, alon, earthr+aalt, surfxa, surfya, surfza
c        print 123, earx, eary, earz
      call tosurf(blat, blon, earthr+balt, surfxb, surfyb,
     + surfzb, earx, eary, earz)
c        call dmplin( 'tosurf   ', 6 )
c        print 123, blat, blon, earthr+balt, surfxb, surfyb, surfzb
c        print 123, earx, eary, earz
 
      brange = sqrt( surfxb**2 + surfyb**2 )
        if( abs( surfyb ) .le. 1.e-9 ) then
            alpha = -beta
        else
            alpha = atan2( surfxb, -surfyb ) - beta             
        endif
      xb = brange*cos( alpha )
      yb = brange*sin( alpha )
      zb = surfzb
                                     
      end
 
 
 
c----------------------------------------------------------------------


c subroutine toeart

c this subroutine converts a surface cartesian coordinate vector 
c to an earth cartesian coordinate vector.  the surface coordinate 
c system is centered at the radar with positive x-axis to the north, 
c positive y-axis to the west, and positive z-axis upwards.  the 
c earth coordinate system has its origin at the center of the earth 
c with z-axis to the north, x-axis through 0 degrees longitude, and 
c y-axis through 90 degrees longitude.
c----------------------------------------------------------------------
      subroutine toeart(lat,lon,radius,surfx,surfy,
     + surfz,earx,eary,earz)
c----------------------------------------------------------------------
      double precision lat,lon,radius,surfx,surfy,surfz,
     + earx,eary,earz,radlat,radlon
c variables:
c     radlat.....latitude of surface coordinate system (radians)
c     radlon.....longitude of surface coordinate system (radians)
c     radius.....distance from earth origin to surface origin (km)
c     surfx......surface x-component (km)
c     surfy......surface y-component (km)
c     surfz......surface z-component (km)
c   output:
c     earx.......earth x-component (km)
c     eary.......earth y-component (km)
c     earz.......earth z-component (km)

c     ..convert to radians
      radlat = lat*0.0174532925199433
      radlon = lon*0.0174532925199433

c     ..calculate x-component
      earx = -surfx*dsin(radlat)*dcos(radlon) +
     + surfy*dsin(radlon) + surfz*dcos(radlon)*dcos(radlat) +
     + radius*dcos(radlat)*dcos(radlon)
c     ..calculate y-component
      eary = -surfx*dsin(radlat)*dsin(radlon) -
     + surfy*dcos(radlon) + surfz*dsin(radlon)*dcos(radlat) +
     + radius*dcos(radlat)*dsin(radlon)
c     ..calculate z-component
      earz = surfx*dcos(radlat) + surfz*dsin(radlat) +
     + radius*dsin(radlat)
c      type *, 'lat,lon,rad', lat, lon, radius
c      type *, 'toeart-xyz', earx,eary,earz

      return
      end
c----------------------------------------------------------------------



c subroutine tosurf

c this subroutine converts a earth cartesian coordinate vector to
c a surface cartesian coordinate vector.  the surface coordinate 
c system is centered at the radar with positive x-axis to the north, 
c positive y-axis to the west, and positive z-axis upwards.  the 
c earth coordinate system has its origin at the center of the earth 
c with z-axis to the north, x-axis through 0 degrees longitude, and 
c y-axis through 90 degrees longitude.
c----------------------------------------------------------------------
      subroutine tosurf(lat,lon,radius,surfx,surfy,
     + surfz,earx,eary,earz)
c----------------------------------------------------------------------
      double precision lat,lon,radius,surfx,surfy,
     + surfz,earx,eary,earz,radlat,radlon
c variables:
c     radlat.....latitude of surface coordinate system (radians)
c     radlon.....longitude of surface coordinate system (radians)
c     radius.....distance from earth origin to surface origin (km)
c     surfx......surface x-coordinate (km)
c     surfy......surface y-coordinate (km)
c     surfz......surface z-coordinate (km)
c     earx.......earth x-coordinate (km)
c     eary.......earth y-coordinate (km)
c     earz.......earth z-coordinate (km)

c     ..convert to radians
      radlat = lat*0.0174532925199433
      radlon = lon*0.0174532925199433

c     ..calculate x-component
      surfx = -earx*dsin(radlat)*dcos(radlon) -
     + eary*dsin(radlat)*dsin(radlon) + earz*dcos(radlat)
c     ..calculate y-component
      surfy = earx*dsin(radlon) - eary*dcos(radlon)
c     ..calculate z-component
      surfz = earx*dcos(radlat)*dcos(radlon) +
     + eary*dcos(radlat)*dsin(radlon) + 
     + earz*dsin(radlat) - radius
c      type *, 'lat,lon,radius', lat, lon, radius
c      type *, 'tosurf-xyz', surfx, surfy, surfz

      return
      end
c----------------------------------------------------------------------
      subroutine putasc(outrec, iword, str, len)
c----------------------------------------------------------------------
        character str*(*) 
c   copy ascii code beginning at 16 bit word number "iword"
        call put16c( outrec, iword, len, str )
 
      return
      end
c----------------------------------------------------------------------
        subroutine advxyz( n, x, y, advx, advy, et )
c  this routine applies a constant advection to x and y where advx is
c  the x advection in m/s and advy is the y advection in m/s and
c  et is the elapsed time is seconds.
c
        dimension x(1), y(1)
    	integer et
c
c   remember: y is really -x and x is really y
c
c    	call i8( 'e', 1, et )
c    	call e12( 'y', 1, advy )
c    	call e12( 'x', 1, advx )
        adv = advy*et
c    	call e12( 'a', 1, adv )
        do 11 i = 1, n
          x(i) = x(i) + adv
 11     continue
c
        adv = -advx*et
c    	call e12( 'a', 1, adv )
        do 12 i = 1, n
          y(i) = y(i) + adv
 12     continue
c
        return
        end
C----------------------------------------------------------------------
        subroutine adhxyz( n, x, y, z, advx, advy, advz, et,
     +    zmin, dzinv, maxz )
c  This routine advects as a function of height
c
c   remember: y is really -x and x is really y
c
        dimension x(1), y(1), z(1), advx(1), advy(1), advz(1)
    	integer et
c
        do 11 i = 1, n
          j = ( z(i)-zmin )*dzinv +1.5
c          if( j .lt. 1 ) then
c            j = 1
c          elseif( j .gt. maxz ) then
c            j = maxz
c          endif
          if( j .le. maxz .and. j .ge. 1 ) then
              x(i) = x(i) + advy(j)*et
              y(i) = y(i) - advx(j)*et
          endif
 11     continue
c
        return
        end
c----------------------------------------------------------------------
      subroutine xyz( n, r, x, y, z, c1, c2, sinel, cosel, earthr)
c  convert from radar space to x,y,z space
c_______________________________________________________________________
c  06 February 1990: Modification by LJM - NCAR/MMM
c  Replaced computation of height to include 4/3 earth correction.
c
c     zcor = slantr*slantr / ((4/3)*earth diameter)
c
c_______________________________________________________________________
c

      dimension r(n), x(n), y(n), z(n)

c      call e9( 'c1', 2, c1 )
c      call e9( 'c2', 2, c2 )
c      call e9( 'se', 2, sinel )

      do 11 i = 1, n
        x(i) = r(i)*c1
 11   continue

      do 12 i = 1, n
        y(i) = r(i)*c2
 12   continue

c_______________________________________________________________________
c  Modification by LJM - NCAR/MMM
c  Replaced computation of height to include 4/3 earth correction.
c
c      do 13 i = 1, n
c        z(i) = r(i)*sinel
c 13   continue
c
c_______________________________________________________________________

      cfac = 0.375/earthr
      do 13 i = 1, n
        slantr = r(i)*cosel
        z(i) = r(i)*sinel + cfac*slantr*slantr
 13   continue

      return
      end
c----------------------------------------------------------------------
      subroutine sprint_xyz(  n, x, y, z, slat, slon, salt
     +     , dlat, dlon, dalt, xx, yy, zz, angxax )
c  routine that uses Jay Millers coordinated transformation routines
c  to convert from the source radar coordinates to coordinates relative
c  to the origin of the destination grid.
c        
c  for each gate determine its lat/lon value, then use this lat/lon
c  to calculate (x,y) relative to the origin of the grid
c  
      parameter( maxgt=2048 )
      dimension plat(maxgt), plon(maxgt), palt(maxgt)
      dimension x(n), y(n), z(n), xx(n), yy(n), zz(n)
      data R_earth / 6366.8056 /
c        
c     get the lat/lon/alt of each gate
c
      ii = loop_xy2ll_v4( plat, plon, palt, x, y, z
     +     , slat, slon, salt, R_earth, n )
c
c  get (x,y,z) relative to the grid origin
c
      ii = loop_ll2xy_v4( plat, plon, palt, xx, yy, zz
     +     , dlat, dlon, dalt, R_earth, n )
c
c
c
c
c
c
c
c
c      do 11 ii = 1, n
c         call xy2lldrv( plat, plon, x(ii), y(ii)
c     +        , slat, slon, angxax )
c         call ll2xydrv( plat, plon, xx(ii), yy(ii)
c     +        , dlat, dlon, angxax )              
c         zz(ii) = z(ii) + salt - dalt
c 11   continue
c
      jj = 0
c
      end
c----------------------------------------------------------------------X
 
      function loop_xy2ll_v4( plat, plon, palt, x, y, z, olat, olon, 
     +     oalt, R_earth, num_pts )
      real plat(*), plon(*), palt(*)
      real olat, olon, oalt, R_earth
      real x(*), y(*), z(*)
      integer num_pts

c    /* calculate (plat,plon) of a point at (x,y) relative to (olat,olon) */
c    /* all dimensions in km. */

c    /* transform to earth coordinates and then to lat/lon/alt */
c
c    /* These calculations are from the book
c     * "Aerospace Coordinate Systems and Transformations"
c     * by G. Minkler/J. Minkler
c     * these are the ECEF/ENU point transformations
c     */

      double precision delta_o, lambda_o
      double precision delta_p, lambda_p
      double precision R, rr
      double precision sinLambda, cosLambda, sinDelta, cosDelta
      double precision xe, ye, ze, h
      double precision rad_cnvt

      data rad_cnvt / .017453292 /
      data deg_cnvt / 57.29577951 /


      h = R_earth + oalt
      delta_o = rad_cnvt * ( olat )	
      lambda_o = rad_cnvt * ( olon )
      
      sinLambda = sin( lambda_o )
      cosLambda = cos( lambda_o )
      sinDelta = sin( delta_o )
      cosDelta = cos( delta_o )

    
      do 44 ii = 1, num_pts

c	/* transform to earth coordinates */

         xe = h * sinDelta + cosDelta * y(ii) + sinDelta * z(ii)

         ye = -h * cosDelta * sinLambda   -cosLambda * x(ii)
     +     + sinLambda * sinDelta * y(ii) -sinLambda * cosDelta * z(ii)

         ze = h * cosDelta * cosLambda   -sinLambda * x(ii)
     +     -cosLambda * sinDelta * y(ii) + cosLambda * cosDelta * z(ii)

	lambda_p = datan2( -ye, ze )
	delta_p = datan2( xe, sqrt( ye * ye + ze * ze ))

	plat(ii) = deg_cnvt * ( delta_p )
	plon(ii) = deg_cnvt * ( lambda_p )
	palt(ii) = dsqrt( xe * xe + ye * ye + ze * ze ) - R_earth

c         write(*,*) 'xy2ll ', plat(ii), plon(ii), palt(ii)
c     +         , x(ii), y(ii), z(ii)

 44      continue

         loop_xy2ll_v3 = num_pts
         return

         end

                   
c----------------------------------------------------------------------X

      function loop_ll2xy_v4( plat, plon, palt, x, y, z, olat, olon
     +     , oalt, R_earth, num_pts )
      real plat(*), plon(*), palt(*)
      real olat, olon, oalt, R_earth
      real x(*), y(*), z(*)
      integer num_pts

c    /* calculate (plat,plon) of a point at (x,y) relative to (olat,olon) */
c    /* all dimensions in km. */
c
c    /* transform to earth coordinates and then to lat/lon/alt */
c
c    /* These calculations are from the book
c     * "Aerospace Coordinate Systems and Transformations"
c     * by G. Minkler/J. Minkler
c     * these are the ECEF/ENU point transformations
c     */

      double precision delta_o, lambda_o, R_p, R_p_pr
      double precision delta_p, lambda_p
      double precision sinLambda, cosLambda, sinDelta, cosDelta
      double precision xe, ye, ze, h, a, b, c
      double precision rad_cnvt

      data rad_cnvt / .017453292 /


      h = R_earth + oalt
      delta_o = rad_cnvt * ( olat )	
      lambda_o = rad_cnvt * ( olon )

      sinLambda = dsin( lambda_o )
      cosLambda = dcos( lambda_o )
      sinDelta = dsin( delta_o )
      cosDelta = dcos( delta_o )
      
      

      do 44 ii = 1, num_pts

         R_p = R_earth + (palt(ii))
         delta_p = rad_cnvt * ( plat(ii) )
         lambda_p = rad_cnvt * ( plon(ii) )
         R_p_pr = R_p * dcos( delta_p )
         
         xe = R_p * dsin( delta_p )
         ye = -R_p_pr * dsin( lambda_p )
         ze = R_p_pr * dcos( lambda_p )

c	/* transform to ENU coordinates */

         a = -h * sinDelta + xe
         b =  h * cosDelta * sinLambda + ye
         c = -h * cosDelta * cosLambda + ze
         
         x(ii) = -cosLambda * b  -sinLambda * c
         y(ii) = cosDelta * a  +  sinLambda * sinDelta * b
     +        -cosLambda * sinDelta * c
         z(ii) = sinDelta * a  -sinLambda * cosDelta * b
     +        +cosLambda * cosDelta * c

c         write(*,*) 'll2xy ', plat(ii), plon(ii), palt(ii)
c     +         , x(ii), y(ii), z(ii)

 44      continue

         loop_ll2xy_v3 = num_pts
         return

         end

                   
c----------------------------------------------------------------------
      subroutine esurf( n, x, y, z, r, cosrlo, s1, s2, sinrlo,
     a  s3, s4, cosrla, sinrla, ex, ey, ez )
c----------------------------------------------------------------------
c  convert xyz values to earth surface coordinates.
c  ie. km. relative to 0 deg. lat., km. relative to 0 deg. lon., and
c  km. relative to earth center

      dimension x(n), y(n), z(n), ex(n), ey(n), ez(n)

c        call e9( 'r', 1, r )
c        call e9( 'crlo', 4, cosrlo )
c        call e9( 's1', 2, s1 )
c        call e9( 's2', 2, s2 )
c        call e9( 'srlo', 4, sinrlo )
c        call e9( 's3', 2, s3 )
c        call e9( 's4', 2, s4 )
c        call e9( 'crla', 4, cosrla )
c        call e9( 'srla', 4, sinrla )
c
      con = r*s2
      do 11 i = 1, n
        ex(i) = -s1*x(i) +sinrlo*y(i) +s2*z(i) +con
 11   continue  
c
      con = r*s4
      do 22 i = 1, n
        ey(i) =  -s3*x(i) -cosrlo*y(i) +s4*z(i) +con
 22   continue
c
      con = r*sinrla
      do 33 i = 1, n
        ez(i) =  cosrla*x(i) +sinrla*z(i) + con
 33   continue
c
c
c      con = r*s2
c      do 11 i = 1, n
c        ex(i) = -x(i)*s1+y(i)*sinrlo+z(i)*s2+con
c 11   continue
c
c      con = r*s4
c      do 12 i = 1, n
c        ey(i) = -x(i)*s3-y(i)*cosrlo+z(i)*s4+con
c 12   continue
c
c      con = r*sinrla
c      do 13 i = 1, n
c        ez(i) = x(i)*cosrla+z(i)*sinrla+con
c 13   continue

      return
      end

c----------------------------------------------------------------------
      subroutine dxyz( n, ex, ey, ez, r, d1, d2, sindla, cosdlo,
     a  sindlo, d3, d4, cosdla, x, y, z, iadjust_z, salti, dalti )
c----------------------------------------------------------------------
c  translate to the destination landmark and convert back to xyz

      dimension ex(n), ey(n), ez(n), x(n), y(n), z(n)
      real zdiff
      logical iadjust_z

c        call e9( 'r', 1, r )
c        call e9( 'd1', 2, d1 )
c        call e9( 'd2', 2, d2 )
c        call e9( 'sdla', 4, sindla )
c        call e9( 'cdlo', 4, cosdlo )
c        call e9( 'sdlo', 4, sindlo )
c        call e9( 'd3', 2, d3 )
c        call e9( 'd4', 2, d4 )
c        call e9( 'cdla', 4, cosdla )
c        call dmplin( 'dxyz ', 4 )
c
c
c     
c
c
      do 22 i = 1, n
        x(i) = -d1*ex(i) -d2*ey(i) +cosdla*ez(i)
 22   continue
c
      do 23 i = 1, n
        y(i) = sindlo*ex(i) -cosdlo*ey(i)
 23   continue
c
      if( iadjust_z ) then
c  the final z should be equivalent to the original z plus
c  the difference in altitude between the two origins
         zdiff = salti - dalti

         do 234 i = 1, n
            z(i) =  z(i) + zdiff
c            if( i .gt. 900 ) then
c               j = 0
c            endif
 234     continue
      else
         do 24 i = 1, n
            z(i) =  d3*ex(i) +d4*ey(i) +sindla*ez(i) -r
c            if( i .gt. 900 ) then
c               j = 0
c            endif
 24      continue
      endif

c z(901) = 17.19589 adjusted at 7.3 deg. .15 gate spacing
c z(901) = 17.28711 not adjusted

c
c
c      do 11 i = 1, n
c        x(i) = -ex(i)*d1 - ey(i)*d2 + ez(i)*cosdla
c 11   continue
c
c      do 12 i = 1, n
c        y(i) = ex(i)*sindlo - ey(i)*cosdlo
c 12   continue
c
c      do 13 i = 1, n
c        z(i) = ex(i)*d3 + ey(i)*d4 + ez(i)*sindla - r
c 13   continue
c
c        call typefe( x, 51, 65 )
c        call typefe( y, 51, 65 )
c        call typefe( z, 51, 65 )

      return
      end

c----------------------------------------------------------------------
      subroutine dcopl( n, cx, cy, al, dbaser, x, y, z, zok )
c----------------------------------------------------------------------
c  convert destination xyz to destination coplane coordinates

      parameter( unrad=57.29578 )
      parameter( piovr2= 1.5707963 )
      dimension cx(n), cy(n), al(n), x(n), y(n), z(n)
      logical zok


c  rotate x and y axes
      beta = piovr2 - dbaser
c      theta = atan2( x(n), -y(n))
c      gamma = theta - (piovr2-dbaser)
c       if( zok ) then
c      call i8( 'n', 1, n )
c      call f9( 'x', 1, x(n))
c      call f9( 'y', 1, y(n))
c      call f9( 'th', 2, theta*unrad )
c      call f9( 'ga', 2, gamma*unrad )
c       endif
c      singam = sin( gamma )
c      cosgam = cos( gamma )

      do 10 i = 1, n
        cy(i) = sqrt( x(i)**2 + y(i)**2 )
        x(i) =  atan2( x(i), -y(i)) - beta
 10   continue

      do 11 i = 1, n
        cx(i) = cy(i)*cos( x(i) )
 11   continue

      do 12 i = 1, n
        cy(i) = cy(i)*sin( x(i) )
 12   continue
 
      do 13 i = 1, n
        cy(i) = sqrt( cy(i)**2+z(i)**2 )
 13   continue

c  now convert this to the coplane angle
      do 14 i = 1, n
        al(i) = asin(z(i)/cy(i))
 14   continue


      return
      end


c----------------------------------------------------------------------
      subroutine xyswap_ljm( n, x, y )
c----------------------------------------------------------------------
      dimension x(n), y(n)
c  swap x and y axes

      do 11 ii = 1, n
         tmp = x(ii)
         x(ii) = -y(ii)
         y(ii) = tmp
 11   continue

      return
      end
c----------------------------------------------------------------------
      subroutine xyswap( n, cx, cy, cz, x, y, z, dbaser, zok )
c----------------------------------------------------------------------
c  convert destination xyz to destination coplane coordinates

      parameter( unrad=57.29578 )
      parameter( piovr2= 1.5707963 )
      dimension cx(n), cy(n), cz(n), x(n), y(n), z(n)
      logical zok

      beta = piovr2 - dbaser

      if( abs( beta ) .gt. .009999 ) then
c  rotate axes
        do 10 i = 1, n
          cy(i) = sqrt( x(i)**2 + y(i)**2 )
          x(i) =  atan2( x(i), -y(i)) - beta
 10     continue

        do 11 i = 1, n
          cx(i) = cy(i)*cos( x(i) )
 11     continue

        do 12 i = 1, n
          cy(i) = cy(i)*sin( x(i) )
 12     continue
 
      else
c  swap x and y axes

        do 22 i = 1, n
          cy(i) = x(i)
 22     continue
 
        do 23 i = 1, n
          cx(i) = -y(i)
 23     continue

      endif


      do 24 i = 1, n
        cz(i) = z(i)
 24   continue


      return
      end


c----------------------------------------------------------------------
        subroutine insoff( xo, yo, zo, n, x, y, z )
c
        dimension x(1), y(1), z(1)
c  offset the xyz values by constants
c
c   remember: y is really -x and x is really y
c
        do 11 i = 1, n
          x(i) = x(i) + yo
 11     continue
c
        do 12 i = 1, n
          y(i) = y(i) - xo
 12     continue
c
        do 13 i = 1, n
          z(i) = z(i) + zo
 13     continue
c
        return
        end
c----------------------------------------------------------------------
      subroutine filter( x, y, z, flds, ngts, zok, az, el, itm
     +  , newvol, igskip, azrad, elrad, rgrad )
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
        real    xmin, ymin, zmin
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

        parameter(      maxfld=7,       maxf32=maxfld*8
     +,                 maxrec=40000
     +,                 input=5
     +,                 list=6,         infile=12
     +,                 mxtfld=maxfld+3
     +,                 range1=0.0,     range2=10000.0
     +,                 mmm=mxtfld*maxgt
     +,                 torad=0.0174532925199433
     +)
c

        logical                 aznsyd,         zok,            ufeof
     +,         equal,          eof, 	    	newvol
     +,         endofd,         first,          ok,             pow
     +,         nonzro,         bok,            copln,          itrap


        integer
     +          year1,          day1,           error,          tcount
     +,         hour1,          secnd1,         drecln
     +,         volnum,         spot
     +,         hedrec(512),    offset,         bound
     +,         carnum,         shelf,          usage
     +,         box,            carton
     +,         ixlo(maxgt),    ixhi(maxgt)
     +,         iylo(maxgt),    iyhi(maxgt)
     +,         izlo(maxgt),    izhi(maxgt)


        real    flds(*),        x(maxgt),       y(maxgt),    z(maxgt)
     +,         dx(maxgt),      dy(maxgt),      dz(maxgt)
     +,         xval(maxval),   yval(maxval),   zval(maxval)
c
c
        common  / datset /      slong,          slati,          salti
     +,         dlong,          dlati,          dalti,          dbase
     +,         axmin(3),       axmax(3),       radius(3),    delta(3)
     +,         deltai(3),      numval(3),      spacng(3)
     +,         tref,           values(maxval,3)
c 
      integer tref
c
c
    	dimension box1(256)
c
        common  / fatz /        rg(maxgt)
     +,         fat(mmm)
c
        common  / storef /      drecln,         lencar,         mxshlf
     +,         carnum(maxcart), shelf(maxcart),  usage(maxcart)
c
        common  / scr /         ok(maxgt)
c
    	save ikount, first, tcount
     +, copln, nxval, nyval, nzval, rcpdx, rcpdy, rcpdz
     +, xrad, yrad, zrad, xval, yval, zval, dx, dy, dz
     +, xmin, ymin, zmin
c
      data first / .true. / 
code
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
c 
c       call inslc( 'FILTER' )
c
c
c  first pass initialization
        if( first ) then
            first = .false.
            tcount =  namefx( 'TCOUNT', ikount )
c   try to cut down on subscript calculations
c   a lot of this stuff used to be in equivalence statements
            copln = namefx( 'COPLANE', i ) .ne. 0
            nxval = numval(1)
            nyval = numval(2)
            nzval = numval(3)
            rcpdx = deltai(1)
            rcpdy = deltai(2)
            rcpdz = deltai(3)
            xrad = radius(1)
            yrad = radius(2)
            zrad = radius(3)
c
c  establish box and carton characteristics
c  a box contains all the data associated with a grid point
c
c  establish the no. of boxes in a carton
c  a carton is equivalent to a disk record
            nbxcar = drecln/lenbox
c       call i8( 'lenbox', 6, lenbox )
c       call i8( 'nbxcar', 6, nbxcar )
c
c   determine the dimensions of a carton i.e. number of boxes in x and y
            if( nxval .gt. nyval ) then
                call mximyz( nbxcar, nbx, nby )
            else                   
                call mximyz( nbxcar, nby, nbx )
            endif
c       call i8( 'nbx', 3, nbx )
c       call i8( 'nby', 3, nby )
            nbxcar = nbx*nby
c       call i8( 'nbxcar', 6, nbxcar )
            lencar = nbxcar*lenbox

c  no. of boxes in an ideal x row (integer no. of cartons)
            nbxro = ((nxval-1)/nbx + 1 )*nbx
c       call i8( 'nbxro', 5, nbxro )
            nbyro = ((nyval-1)/nby + 1 )*nby
c       call i8( 'nbyro', 5, nbyro )
c  establish the number of cartons needed to contain data set
            itotal = nbxro*nbyro*nzval
            ncarts = ( itotal-1 )/nbxcar + 1
       call i8( 'tot', 3, itotal )
       call i8( 'mxcart', 6, maxcart )
       call i8( 'ncarts', 6, ncarts )
c
c
            if( ncarts .gt. maxrec ) then
                call i8( 'total grid points   ', 17, itotal )
                call i8( 'max. possible  ', 13, maxrec*nbxcar )
                call dmplin( ' ', 1 )
                call typstg( list,
     a  'the total number of grid points is too large ^', 1, -1 )
                stop66
            endif
c
c
            if( ncarts .lt. maxcart ) then
                mxshlf = ncarts
            else
                mxshlf = maxcart
            endif
       call i8( 'mxshlf', 6, mxshlf )
       call dmplnb
c
c  set up the box index determination arrays
            do 121 i = 1, nxval
                ixbox( i ) = 1 + mod(i-1,nbx) + ((i-1)/nbx)*nbxcar
                xval(i) = values(i,1)
  121       continue
            xmin = xval(1)
c       call dmplin( 'xboxes', 0 )
c       call typefx( ixbox, 1, nxval )
c       
            do 122 i = 1, nyval
                iybox( i ) = mod( i-1, nby )*nbx + ((i-1)/nby)*nby*nbxro
                yval(i) = values(i,2)
  122       continue
            ymin = yval(1)
c       call dmplin( 'yboxes', 0 )
c       call typefx( iybox, 1, nyval )
c       
            do 123 i = 1, nzval
                izbox( i ) = ( i-1 )*nbxro*nbyro
                zval(i) = values(i,3)
  123       continue
            zmin = zval(1)
c       call dmplin( 'zboxes', 0 )
c       call typefx( izbox, 1, nzval )
c
c   save the first box
            call copyfp( space, 1, lenbox, box1, 1 )
c
c  open the big scratch file
            call opnscr( luscr, drecln, ncarts )
c
c  initialize the cache housekeeping info
            j = 1
            do 13 i = 1, maxcart
c       call i8( 'i', 1, i )
c       call i8( 'j', 1, j )
                carnum(i) = i
                shelf(i) = inca(j,drecln)
                usage(i)=0
  13        continue
c            
c  for coplane data convert angles to radians                      
            if( copln ) then
                do 155 i = 1, nzval
                    zval(i) = zval(i)*torad
  155           continue
                zrad = zrad*torad
                rcpdz = rcpdz/torad
            endif
            zmin = zval(1)
c       
c
            do 16 i = 1, maxgt
                if( azrad .gt. 0 ) then
                    dx(i) = rg(i)*azrad*torad
                    dy(i) = rg(i)*azrad*torad
                else
                    dx(i) = xrad
                    dy(i) = yrad
                endif
c       
                if( elrad .gt. 0 .and. copln ) then
                    dz(i) = elrad*torad
                elseif( elrad .gt. 0 ) then
                    dz(i) = rg(i)*elrad*torad
                else
                    dz(i) = zrad
                endif
  16        continue
c
        endif
c
c
    	if( newvol ) then
c   new volume
    	    newvol  = .false.
c   initialize the work space
c
c   first carton
            j = 1
            do 161 i = 1, nbxcar
            	call copyfp( box1, 1, lenbox, space, inca(j,lenbox))
 161        continue
c   remainder of the cartons
            j = 1
            do 162 i = 2, maxcart
            	call copyfp( space, 1, drecln, space, incb(j,drecln))
 162        continue
c
c  initialize all the disk records 
            call iniscr( luscr, space, drecln, ncarts )
    	endif
c
c
c   end of initialization
c
c
c
c
c   begin processing the current beam!
c
c
c   recalculate deltas if using angular radius of influence
        if( azrad .gt. 0 .and..not. copln ) then
            call radinf( ngts, az*torad, el*torad, rg, 
     +          azrad*torad, elrad*torad, rgrad, dx, dy, dz )
        endif
c
c
c  determine the range of the x indices
      do 303 i = 1, ngts
        ixlo(i) = ( x(i)-dx(i)-xmin )*rcpdx +1.
 303  continue
c
c
      do 305 i = 1, ngts
        ixhi(i) = ( x(i)+dx(i)-xmin )*rcpdx +1.5
 305  continue
c
c
      do 304 igt = 1, ngts
        if( ixlo(igt) .lt. 1 ) ixlo(igt) = 1
 304  continue
c
      do 306 igt = 1, ngts
        if( ixhi(igt) .gt. nxval ) then
            ixhi(igt) = nxval
        endif
 306  continue
c
c
c  determine the range of the y indices
      do 313 i = 1, ngts
        iylo(i) = ( y(i)-dy(i)-ymin )*rcpdy +1.
 313  continue
c
      do 315 i = 1, ngts
        iyhi(i) = ( y(i)+dy(i)-ymin )*rcpdy +1.5
 315  continue
c
c
      do 314 igt = 1, ngts
        if( iylo(igt) .lt. 1 ) iylo(igt) = 1
 314  continue
c
      do 316 igt = 1, ngts
        if( iyhi(igt) .gt. nyval ) iyhi(igt) = nyval
 316  continue
c
c
c  determine the range of the z indices
      do 323 i = 1, ngts
        izlo(i) = ( z(i)-dz(i)-zmin )*rcpdz +1.
 323  continue
c
      do 325 i = 1, ngts
        izhi(i) = ( z(i)+dz(i)-zmin )*rcpdz +1.5
 325  continue
c
c
      do 324 igt = 1, ngts
        if( izlo(igt) .lt. 1 ) izlo(igt) = 1
 324  continue
c
      do 326 igt = 1, ngts
        if( izhi(igt) .gt. nzval ) izhi(igt) = nzval
 326  continue
c
c...mark
c
c            
c
c   filter loop!
c
c
      do 555 igt = 1, ngts, igskip
c
c
      do 503 l3=izlo(igt),izhi(igt)
        if( abs( zval(l3)-z(igt) ).gt. dz(igt) ) then
          go to 503
        endif
c
c
      do 502 l2=iylo(igt),iyhi(igt)
        if( abs( yval(l2)-y(igt) ) .gt. dy(igt) ) then
          go to 502
        endif
c
c
      do 501 l1=ixlo(igt),ixhi(igt)
        if( abs( xval(l1)-x(igt) ) .gt. dx(igt) ) then
           go to 501
        endif
c
        bigrsq = dx(igt)**2 + dy(igt)**2 + dz(igt)**2
c
        rsq = (xval(l1)-x(igt))**2 + (yval(l2)-y(igt))**2 +
     a    (zval(l3)-z(igt))**2
c                             
c
c  determine the box no.
        box = ixbox( l1 ) + iybox( l2 ) + izbox( l3 )
c
c  determine the carton no.
        carton = ( box-1 )/nbxcar + 1
c
c  get the offset to this carton
        offset = locarx( carton ) - 1
c                                                     
c  determine the offset to the box in the carton
        offset = offset + mod( box-1, nbxcar )*lenbox 
c
        go to ( 361, 362, 400, 364 ), wtfun
 361    continue
c   cressman weighting
        w = (bigrsq-rsq)/(bigrsq+rsq)
        go to 400
 362    continue
c   exponential weighting
        w = exp(atten*rsq/bigrsq)
        go to 400
 364    continue
c   uniform weighting
        w = 1.0
        go to 400
c
 400    continue
c
c   for each field
c
        do 466 i = 1, nflds
c
            if( unfflg(i)) then
c   unfold
                zdata = flds( igt + fldoff(i))
                zref = space(offset+unfptr(i))
    	    	if( zdata .ne. znull .and. zref .ne. znull ) then
                    call unfold( zlard(nyqptr(i)), zref, zdata, k )
                    if( fldtyp(i) .eq. 9 ) then
    	                zdata = k
                    endif
                else
                    space( offset+unfptr(i) ) = zdata
                endif
                go to 417
            endif
c       
            go to ( 401, 401, 403, 404, 405, 406, 466
     +              , 466, 417, 401 ), fldtyp(i)
c       
            call panic( '** Bad field type **' )
c       
  401       continue
c       just use the data point
            zdata = flds( igt + fldoff(i))
            go to 417
  403       continue
c       time!                               
            zdata = itm
            go to 417
  404       continue
c       azimuth
            zdata = az
    	    zref = space(offset+unfptr(i))
            if( oftyp(i) .eq. 25 ) then
c        closest point
               mark = 0
    	    elseif( zref .eq. znull ) then
    	    	space( offset+unfptr(i) ) = zdata
    	    else
		zdiff = zdata - zref
		if( zdiff .gt. 180. ) then
		    zdata = zdata -360.
		elseif( zdiff .lt. -180. ) then
		    zdata = zdata +360.
		endif
    	    endif
            go to 417
  405       continue
c       elevation   
            zdata = el
    	    zref = space(offset+unfptr(i))
            if( oftyp(i) .eq. 25 ) then
c        closest point
               mark = 0
    	    elseif( zref .eq. znull ) then
    	    	space( offset+unfptr(i) ) = zdata
    	    else
		zdiff = zdata - zref
		if( zdiff .gt. 180. ) then
		    zdata = zdata -360.
		elseif( zdiff .lt. -180. ) then
		    zdata = zdata +360.
		endif
    	    endif
            go to 417
  406       continue
c       range
            zdata = rg(igt)
            go to 417
c       
  417       continue
c
    	    if( zdata .eq. znull ) then
c   	don't use it
    	    	go to 465
    	    endif
c
    	    j = 0
  419       continue
            j = j+1
c       do the attached fields
            if( j .gt. natfld(i)) then
                go to 465
            endif
            n = offset + atptr(j,i)
c       
            go to ( 421, 422, 423, 424, 425 ), attyp(j,i)
            go to ( 431, 432, 433, 434, 435, 436 ), attyp(j,i) -10
c       
            call panic( '** Bad attached field type **' )
c       
  421       continue
c       weighted sum
            space(n) = space(n) + w*zdata
            go to 419
  422       continue
c       sum
            space(n) = space(n) + zdata
            go to 419
  423       continue
c       sum of the squares
            space(n) = space(n) + zdata*zdata
            go to 419
  424       continue
c       nearest data point
            if( space(n+1) .gt. rsq ) then
                space(n) = zdata
            endif
            go to 419
  425       continue
c    	min radius
            if( space(n) .gt. rsq ) then
                space(n) = rsq
            endif
            go to 419
c       
  431       continue
c       sum of the weights
            space(n) = space(n) + w
            go to 419
  432       continue
c       count of data points used
            space(n) = space(n) + 1.0
            go to 419
  433       continue
c       octant flag word
            call octant( x(igt), y(igt), z(igt),
     +              xval(l1), yval(l2), zval(l3), space(n))
            go to 419
  434       continue
c       sum of the squares of the weights
            space(n) = space(n) + w*w
            go to 419
  435       continue
c       minimum big R
            if( bigrsq .lt. space(n) ) then
                space(n) = bigrsq
            endif
            go to 419
  436       continue
c       maximum big R
            if( bigrsq .gt. space(n) ) then
                space(n) = bigrsq
            endif
            go to 419
c       
  465       continue
c       
  466   continue
c       
c
 501    continue  
 502    continue 
 503    continue
 555    continue
c 
c
        call putfx( ikount, tcount )
        return
c
        end
c----------------------------------------------------------------------
