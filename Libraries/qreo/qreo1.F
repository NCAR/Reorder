c-------------------------------------------------------------------------
    	block data qreo1
c
        parameter( znull=-32768., wmin=-1000000., wmax=1000000. )
        parameter( inull=-32768 )
        parameter( mxfld=16, mxsfld=20  )
        parameter(  mxcomp=8, mxtot=mxfld+mxsfld  )
        parameter( maxval=500 )
c
        integer tref
        common  / datset /      slong,          slati,          salti
     +,         dlong,          dlati,          dalti,          dbase
     +,         axmin(3),       axmax(3),       radius(3),    delta(3)
     +,         deltai(3),      numval(3),      spacng(3)
     +,         tref,           values(maxval,3)
c
        integer wtfun, wtfptr, unfptr
        integer natfld, attyp, atptr
        integer nflds, fldtyp, fldoff
        logical dupfld, unfflg
        real    thrval, zlard, atten
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
        common  / boxez /       nbxcar,         lenbox
     +,         ixbox(maxval),  iybox(maxval),  izbox(maxval)
c
        common / ofldcom /  noutf
     +,     oftyp(mxtot)
     +,     comptr( mxcomp, mxtot )
c
c
c   data
c
c   	/ datset /
        data                    slong,          slati,          salti
     +,         dlong,          dlati,          dalti,          dbase
     +,         axmin,          axmax,          radius,         delta
     +,         deltai
     +/         22*znull  /
        data tref / inull /
c
c   	/ fldcom /
        data wtfptr, mnrptr  / 2*inull /
        data nyqptr / mxfld*inull /
        data fldoff / mxfld*inull /
c   duplicate field flag
        data dupfld, unfflg / mxfld*.false., mxfld*.false. /
c   exponential weighting factor
        data atten / -2.302585093 /
c   initialize the larder counter
        data nlards / 0 /
c   count of input fields
        data nflds / 0 /
c   count of attached fields
        data natfld / mxfld*0 /
c
c   	/ boxez /
c   initialize the box length
        data lenbox / 0 /
c
c   	/ ofldcom /
c   count of output fields
        data noutf / 0 /
c
    	end
c-------------------------------------------------------------------------
c..file:qreo1.for                                
c  contains routines
c	reo
c	inidat
c       fields
c       typsrc
c       mnesrc
c       unfsrc
c   	crefld
c       scanfo
c       fspecl
c   	setced
c
c----------------------------------------------------------------------
      subroutine reo( butter, lenbuf )
c===========================================================================
c___qcommon.for
c

c       max cart output set to 4001, that is nx * ny * nz <= maxcart
    	parameter( maxcart=4001, sizcart=1024 )
        parameter( inull=-32768 )
        parameter( znull=-32768., wmin=-1000000., wmax=1000000. )
        parameter( maxval=500 )
        parameter( maxspc=maxcart*sizcart )

c       Max gates is 2048
        parameter( maxgt=2048)
        parameter( mxg3=3*maxgt )

c       Max fields set to 16, max field for scratch is 20  -DFF 3/17
        parameter( mxfld=16, mxsfld=20  )
c           
        integer nflds, fldtyp, fldoff
        integer wtfun, wtfptr, unfptr
        integer natfld, attyp, atptr
        real    thrval, zlard, atten
        logical dupfld, unfflg
c
c     Diagnostic switch for large amounts of diagnostic printout
c     Change diag_ljm to .true. when printout desired.
c
        logical diag_ljm
        data diag_ljm/.false./
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
c   acs     =   user-specified input "attribute:parameter;" stream
c               compressed into a large, continuous, without spaces
c               stream starting with ";"
c   ncas    =   the length of acs (~600-800 characters)
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
        integer noutf, comptr, oftyp
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
        parameter(      maxfld=7,       maxf32=maxfld*8
     +,                 list=4,         infil=11
     +,                 outfil=12,      mxtfld=maxfld+3
     +,                 mmm=mxtfld*maxgt
     +,                 torad=0.0174532925199433,     unrad=57.29578
     +,	    	    	maxxcl=132
     +)

        logical                 aznsyd,         zok
     +,         airpln,         copln,          flee
     +,         specl
     +,		eovset,         present,        firstf,	    	fixflt
     +,		changed,    	grnd_out,       iadjust_z,      sprint_xlt
c
c
        integer                 error
     +,         volnum,         spot,	    	numvol,	    	fcount
     +,         hedrec(512),    advect
     +,         ttm(6),         exct1(maxxcl), 	exct2(maxxcl),	excnt
     +,		swpskp,		swpcnt,		bmskp,		bmcnt
     +,         ltp
c
c
        real    aaz(maxval),    advec(maxval),  advhgt(maxval)
     +,         advx(maxval),   advy(maxval)    
     +,         pt1(maxfld),    pinc(maxfld),	rscr(maxgt)
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
        common  / fatz /        rg(maxgt)
     +,         fat(mmm)
c
        logical span, newvol, multiv, final
        logical prznt
c
        character*2     mne,     mne1
        character*128   syms
        character*2222  acs
c
        data aaz, advec, advhgt, advx, advy
     +          / maxval*znull, maxval*znull, maxval*znull
     +,           maxval*znull, maxval*znull /

c             
        data advect, numfld, numvol, fcount / 4*0 /
        data excnt, nxcl / 2*0 /
        data nsym, pt1, pinc / 0, maxfld*-96., maxfld*3. /
        data earthr / 6366.8056 /
        data az1, az2 / -360., 360. /
    	data prf1, prf2 / -1.1e11, 1.1e11 /
    	data gspc1, gspc2 / -1.1e11, 1.1e11 /
        data gnyqv, grc, gbmwid / 3*znull /
    	data span, multiv, final, fixflt / 4*.false. /
    	data igskip / 1 /
	data swpskp, swpcnt, bmskp, bmcnt, ltp / 5*0 /
    	data azrad, elrad, rgrad / znull, znull, 0 /
    	data grnd_out / .false. /
        data iadjust_z / .false. /
        data sprint_xlt / .false. /
        data range1, range2 / 0.00, 10000.00 /
code
c>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

      call dmplin( 'QREO--LINUX VERSION 2.00 March 2010 ', 0 )
c
    	write(*,*) ' '
    	write(*,*)
     +'    This version of REORDER produces the PURE cedric format'
    	write(*,*) ' '
c
c  open input control string file
      call opnics( ipar )
      twiddle = 1
c
c  get the input control string
      call inidat( ipar, acs, nacs, aaz, advec, advhgt, xoff
     a, yoff, zoff, pt1, pinc, syms, nsym, az1, az2
     a, exct1, exct2, nxcl, multiv, prf1, prf2, fixflt, igskip
     a, flee, azrad, elrad, rgrad, earthr, gnyqv, grc, gbmwid
     a, grnd_out, range1, range2, swpskp, bmskp, iadjust_z
     a, sprint_xlt, twiddle, iradfun, diag_ljm)
      if(diag_ljm)print *,'After inidat iradfun=',iradfun
c
c      sprint_xlt = .true.
c      sprint_xlt = .false.
c
      if( nacs .lt. 11 .or. flee ) then
c  there is no input control string
        go to 909
      endif
c 
c
      if( iadjust_z ) then
         call inslc( "ADJUST_Z" )
      endif
      if( sprint_xlt ) then
         call inslc( "SPRINT_XYZ" )
      endif
      l1 = 1
      l2 = 1+1*maxgt
      l3 = 1+2*maxgt
      l4 = 1+3*maxgt
      l5 = 1+4*maxgt
      l6 = 1+5*maxgt
c   set the end of volume flag
        call seteov( .false. )
c
c
c  initialize the access package and position to first record
      call setcat( butter, lenbuf,
     1   7, 8, ltp, acs, nacs, error)
c 
      if(error.ne.0)then
        call typstg( lunout()
     +	, 'Unable to access requested dataset^',1,0)
         call typstg( lunout()
     +  , '  Check radar name and times^',1,0)
         stop
      endif
      volnum = kvoln(butter)
c
c  set up reorder parameters
      call inireo( butter, advect, aaz, advec
     +,         advhgt, advx, advy, azrad, elrad, rgrad, rg )
c
c   this is all the stuff that used to be in equivalence statements
        zmin = values(1,3)
        rcpdz = deltai(3)
        airpln = namefx( 'AIRCRAFT', i ) .ne. 0
        copln = namefx( 'COPLANE', i ) .ne. 0
        span = namefx( 'SPANVOLS', i ) .ne. 0
        xi = namefp( 'EARTHR', i )
        call putfp( i, earthr )
c
      call f9( 'slat', 4, slati )
      call f9( 'slon', 4, slong )
      call f9( 'salt', 4, salti )
      call f9( 'dlat', 4, dlati )
      call f9( 'dlon', 4, dlong )
      call f9( 'dalt', 4, dalti )
      call f9( 'dbas', 4, dbase )
      call e12( 'earthr', 6, earthr )
c
c  set up source and destination position constants

c  destination baseline
      cosbas = cos( dbase*torad )
      cosbsq = cosbas*cosbas
      sinbas = sin( dbase*torad )
c
c  source radar lat., lon., and altitude
c     slati - latitude of the radar
c     slong - longitude of the radar
c     salti - altitude of the radar
c
      sinrla = sin( slati*torad )
      cosrla = cos( slati*torad )
      sinrlo = sin( slong*torad )
      cosrlo = cos( slong*torad )
      srad = earthr+salti
      call e12( 'srad', 4, srad )
c
      s1 = sinrla*cosrlo
      s2 = cosrla*cosrlo
      s3 = sinrla*sinrlo
      s4 = cosrla*sinrlo
c
c  landmark or destination latitude, latitude, and altitude
c     dlati - latitude of grid origin
c     dlong - longitude of grid origin
c     dalti - altitude of grid origin
c
      sindla = sin( dlati*torad )
      cosdla = cos( dlati*torad )
      sindlo = sin( dlong*torad )
      cosdlo = cos( dlong*torad )
      drad = earthr+dalti
      call e12( 'drad', 4, drad )
c
      d1 = sindla*cosdlo
      d2 = sindla*sindlo
      d3 = cosdla*cosdlo
      d4 = cosdla*sindlo
c 
      if(tref.lt.0)then
         tref=3600*khour(butter)
     1      +60*kminut(butter)+ksecnd(butter)
      endif
c 
c   are all the input fields there?
      flee = .false.
c   set diagnostic print out
      call setdpr( .true. )
      call dmplnb
      firstf = .true.
c
      do 22 i=1,nflds
c   special fields?
          mne(1:2) = fldmne(i*2-1:)
          specl =   fldtyp(i) .eq. 3 .or.
     +              fldtyp(i) .eq. 4 .or.
     +              fldtyp(i) .eq. 5 .or.
     +              fldtyp(i) .eq. 6 .or.
     +              fldtyp(i) .eq. 10
c
         if( .not.specl ) then
c   make sure the source field is present for this field
            if( .not.prznt(butter,mne)) then
		call	inslc('**** Missing Field:')
                flee = .true.
            elseif( mne(1:1) .eq. 'V' .and. gnyqv .eq. znull ) then
c   assume this is the first velocity field
                gnyqv = uvenyq( butter, mne )
            elseif(( mne(2:2) .eq. 'Z'.or. mne(2:2) .eq. 'Y') .and. 
     +              grc .eq. znull ) then
c   assume this is the first reflectivity field
                grc = udmrc( butter, mne )
            endif
c
            if( firstf ) then
                firstf = .false.
                mne1(1:2) = mne(1:2)
            endif
         endif
c
        call inslc( mne(1:2))
        call i8( 'i', 1, i )
        call i8( 't', 1, fldtyp(i))
        call dmplnb
c
         if( nyqptr(i) .ne. inull ) then
c   get the nyquist velocity for this field
            vnyq = uvenyq( butter, mne(1:2))
    	    gnyqv = vnyq
            if( vnyq .gt. 0 ) then
                zlard( nyqptr(i)) = vnyq
            else
c   error!
                flee = .true.
                call typstg( lunout(), 
     +          'nyqusit velocity .le. 0 for^',1,0)
                call typstg( lunout(), mne(1:2), 1, 2 )
            endif
         endif
 22   continue
c
      	if( flee ) then
            go to 909
      	endif
      	call setdpr( .false. )
        call f7( 'nyq', 3, gnyqv )
        call f7( 'rc', 2, grc )
c
    	if(gbmwid .eq. znull) then
    	   gbmwid = uhbwid(butter, mne1)
    	endif
c
    	if(gbmwid .lt. .5) then
    	    gbmwid = 1.1
    	endif
c
c  to know when parallel to the base line, define:
      dbase2 = amod( dbase+180., 360. )
      dbaser = dbase*torad
c 
c
c
c
c   loop through the next volume
c
  24	continue
c
c  generate the mudras tape file header
        if(diag_ljm)then
           print *,' '
           print *,'Before call genhead: sprint_xlt=',sprint_xlt
        endif
        call genhed( butter, hedrec, fldnam, gnyqv, grc, acs, nacs
     +       , sprint_xlt, iadjust_z, diag_ljm )
        if(diag_ljm)then
           print *,"After call genhead"
           print *,' '
        endif
c
c   set the end of volume flag
        call seteov( .false. )
     	newvol = .true.
    	volnum = kvoln(butter)
      	kscan = 0
      	kray = 1
    	numvol = numvol+1
    	oldfix = -1.1e11
c   set up the current output volume name
c    	call setced( numvol, acs, nacs )
  	numbm = 1
      	numpnt=0
c
c
c
c
        print *,' '
        print *,' '
        call dmplin( 'c...top of loop through input data', 34 )
        call flush(6)

c  loop thru the input data 
c
  44  continue

c     For each ray get (azz,ell,fx,zprf) from functions in
c     either
c        UF: qufap.F (compiled and linked for qreou)
c     or
c        DORADE: qdfap.F (compiled and linked for qreod)
c
c     These functions are called here for both UF and DORADE formats.
c     These housekeeping words are inside the kbuf integer array.
c     The extraction of specific words from kbuf is done in functions
c     such as kintgd which are "called" from functions like uelev.
c
 
c     LJM (04/29/09) - diagnostic print
c
      if(diag_ljm)print *,'Before uelev,uazim:',bmcnt,azz,ell,fx
c
      ell=uelev( butter )
c      call f9( 'el', 2, ell )
      el = ell*torad
      cosel=cos( el )
      azz=uazim( butter )
      fx = ufixed( butter )
      zprf = udmprf(butter, mne1 )
      bmcnt = bmcnt +1

c     LJM (04/29/09) - diagnostic print
c
      if(diag_ljm)print *,'After uelev,uazim:',bmcnt,azz,ell,fx
c
      if( kswepn(butter).ne.kscan ) then
c        call f5( 'fixed', 5, ufixed(butter))
        kscan = kswepn(butter)
	swpcnt = swpcnt +1
        zok = .true.
      else   
        zok = mod( numbm, 33 ) .eq. 0
      endif
c
c   don't do azimuths parallel to the base line or outside the azimuth
c       limits
c
      if( copln .and. ( abs( dbase-azz ) .lt. .001 .or.
     a    abs( dbase2-azz ) .lt. .001 )) then
    	excnt = excnt+1
        go to 77
c
c   apply other filters
      elseif( .not.aznsyd( azz, az1, az2 )) then
    	excnt = excnt+1
        go to 77
      elseif( zprf .lt. prf1 .or. zprf .gt. prf2 ) then
        excnt = excnt+1
c    	call f7( 'prf', 3, zprf )
        go to 77
      elseif( fixflt .and. fx .lt. oldfix ) then
        excnt = excnt+1
    	go to 77
      elseif( swpskp .gt. 0 .and. mod(swpcnt, swpskp) .eq. 0) then
        excnt = excnt+1
	goto 77
      elseif( bmskp .gt. 0 .and. mod(bmcnt, bmskp) .eq. 0) then
        excnt = excnt+1
	goto 77
      endif
c
    	oldfix = fx
c
        ttm(1) = kyear(butter)
        ttm(2) = kmonth(butter)
        ttm(3) = kday(butter)
        ttm(4) = khour(butter)
        ttm(5) = kminut(butter)
        ttm(6) = ksecnd(butter)
c
        do 47 i = 1, nxcl
            ixc = (i-1)*6 + 1
            if( icmptm( ttm, exct1(ixc)) .ge. 0 .and.
     +          icmptm( ttm, exct2(ixc)) .le. 0 ) then
c   exclude data in this time period
                    excnt = excnt+1
                    go to 77
            endif
  47    continue
c
c   elapsed time
	    itdif = 3600*ttm(4) + 60*ttm(5) + ttm(6) - tref
	    if( itdif .lt. -72000 ) then
c  assume crossed 24 hour boundary
		itm = itdif + 86400 
	    elseif( itdif .gt. 72000 ) then
		itm = itdif - 86400
	    else
		itm = itdif
	    endif
c
      az=azz*torad
c
c
c   set up the range information based on the first field
c
        call givrng( butter, mne1, range1, range2, maxgt, rg,
     1         numgt, rgate1, gatesp, changed, igate1 )
c
c
c  calculate xyz positions of gates relative to radar
c
c   caveat emptor!
c
c   the following algorithm assumes that the orientation of the positive
c   x-axis is north and the positive y-axis is west.  
c
c   this is corrected in the final subroutines dcopl or xyswap 
c   to x being east and y north.

c     (LJM 5/27/09) - diagnostic print
c
        if(diag_ljm)then
           print *,'Just before call xyz, salti,dalti=',salti,dalti
        endif

      call xyz( numgt, rg, fat(l4), fat(l5), fat(l6),
     a  cosel*cos( az ), -cosel*sin( az ), sin( el ),
     a  cosel, earthr, salti)
c     interesting values now in l4, l5, l6

      if( .false. .and. zok ) then
c      if( zok ) then
         call f9( 'az', 2, azz )
         call f9( 'el', 2, ell )
         if( sprint_xlt ) then
            call inslc( 'sprint_xlt' )
         endif
         call inslc( 'call xyz' )
         call dmplnb
         call typefe( fat(l4), 101, 115 )
         call typefe( fat(l5), 101, 115 )
         call typefe( fat(l6), 101, 115 )
      endif

      if( airpln ) then
c  corrections for airborn doppler
         call insoff( xoff, yoff, zoff,
     +        numgt, fat(l4), fat(l5), fat(l6))
         slong = ulongt( butter )
         slati = ulatit( butter )
         salti = uhight( butter )
         sinrla = sin( slati*torad )
         cosrla = cos( slati*torad )
         sinrlo = sin( slong*torad )
         cosrlo = cos( slong*torad )
         srad = earthr+salti
         s1 = sinrla*cosrlo
         s2 = cosrla*cosrlo
         s3 = sinrla*sinrlo
         s4 = cosrla*sinrlo
c      call f9( 'slat', 4, slati )
c      call f9( 'slon', 4, slong )
c      call f9( 'salt', 4, salti )
      endif
c
c  advection
        if( advect .eq. 1 ) then
c  single vector
          call advxyz( numgt, fat(l4), fat(l5), advx(1), advy(1),
     +          itm )
        elseif( advect .gt. 1 ) then
c  as a function of height
          call adhxyz( numgt, fat(l4), fat(l5), fat(l6),
     +          advx, advy, advz, itm, zmin, rcpdz, maxval, zok )
        endif
c
c  convert xyz values to earth surface coordinates.
c  ie. km. relative to 0 deg. lat., km. relative to 0 deg. lon., and
c  km. relative to earth center
c

c     (LJM 5/26/09) - diagnostic print
c
        if(diag_ljm)then
           print *,'sprint_xlt & .not. (sprint_xlt)=',sprint_xlt,
     +          .not.(sprint_xlt)
        endif

c     The default value of sprint_xlt is .false. so that routines
c     esurf and dxyz are called when SPRINT_XYZ is not included
c     in the user-specified input.
c
c     If this diag_ljm is included there is no error
c
        if( .not. sprint_xlt ) then
           if(diag_ljm)then
              print *,' '
              print *,'SPRINT_XYZ attribute WAS NOT included'
              print *,' '
              print *,'Before esurf: s1,s2,s3,s4=',s1,s2,s3,s4
           endif
           call esurf( numgt, fat(l4), fat(l5), fat(l6), srad,
     a          cosrlo, s1, s2, sinrlo, s3, s4, cosrla, sinrla,
     a          fat( l1 ), fat( l2 ), fat( l3 ))
c     values now in l1, l2, l3
c
c     
c     translate to the destination landmark and convert back to xyz
           call dxyz( numgt, fat(l1), fat(l2), fat(l3), drad,
     a          d1, d2, sindla, cosdlo, sindlo, d3, d4, cosdla,
     a          fat(l4), fat(l5), fat(l6), iadjust_z, salti, dalti,
     a          diag_ljm )
           if(diag_ljm)then
              print *,'  After dxyz: d1,d2,d3,d4=',d1,d2,d3,d4
           endif
c
c     values now in l4, l5, l6 again
c     
c     
c     rotate to the baseline
c     
           if( copln ) then
              call dcopl( numgt, fat(l1), fat(l2), fat(l3), dbaser,
     a             fat(l4), fat(l5), fat(l6), zok )
           else
              call xyswap( numgt, fat(l1), fat(l2), fat(l3),
     a             fat(l4), fat(l5), fat(l6), dbaser, zok )
           endif
c     values back to l1, l2, l3

        else
           if(diag_ljm)then
              print *,' '
              print *,'SPRINT_XYZ attribute WAS included'
              print *,' '
           endif
c     use the SPRINT translation calculations
c     for now we won't do coplane data with this option

           call xyswap_ljm( numgt, fat(l4), fat(l5) )
c
c     
c     (LJM 6/3/09) - Bug fix when SPRINT_XYZ is specified by the user.
c     Changed -slong to +slong and -dlong to +dlong since MMM xy2lldrv and
c     ll2xydrv routines are no longer being called.  The usual convention
c     is to have East longitude be (+) whereas MMM routines used the
c     convention of West longitude as (+).
c
           call sprint_xyz( numgt, fat(l4), fat(l5), fat(l6)
     a          , slati, +slong, salti, dlati, +dlong, dalti
     a          , fat(l1), fat(l2), fat(l3), dbase )

        endif
c
      if( .false. .and. zok ) then
c      if( zok ) then
         call inslc( 'after xlt' )
         call dmplnb
c         call typefe( fat(l4), 101, 115 )
c         call typefe( fat(l5), 101, 115 )
c         call typefe( fat(l6), 101, 115 )
         call typefe( fat(l1), 101, 115 )
         call typefe( fat(l2), 101, 115 )
         call typefe( fat(l3), 101, 115 )
      endif
c     
c   make sure this beam has gates inside the grid
c   x-axis
    	q1 = fat(l1)
    	q2 = fat(l1+numgt-1)
    	if(q1 .gt. q2) then
    	    if(q2 .gt. axmax(1) .or. q1 .lt. axmin(1)) then
    	    	goto 77
    	    endif
    	elseif(q1 .gt. axmax(1) .or. q2 .lt. axmin(1)) then
    	    goto 77
    	endif
c   y-axis
    	q1 = fat(l2)
   	q2 = fat(l2+numgt-1)
    	if(q1 .gt. q2) then
    	    if(q2 .gt. axmax(2) .or. q1 .lt. axmin(2)) then
            goto77 
    	    endif
    	elseif(q1 .gt. axmax(2) .or. q2 .lt. axmin(2)) then
    	    goto 77
    	endif
c z-axis
    	q1 = fat(l3)
    	q2 = fat(l3+numgt-1)
    	if(copln) then
    	    q1 = q1*unrad
    	    q2 = q2*unrad
    	endif
    	if(q1 .gt. q2) then
    	    if(q2 .gt. axmax(3) .or. q1 .lt. axmin(3)) then
    	    	goto 77
    	    endif
    	elseif(q1 .gt. axmax(3) .or. q2 .lt. axmin(3)) then
    	    goto 77
    	endif
c
c
      do 66 i=1,nflds
        mne(1:2) = fldmne(i*2-1:)
c  generate all the required fields
c        call i8( 'i', 1, i )
c        call inslc( mne(1:2))
c        call i8( 't', 1, fldtyp(i))
c        call i8( 'o', 1, fldoff(i))
c
        if( fldoff(i) .eq. inull .or. dupfld(i)) then
            go to 53
        endif
        go to( 51, 51, 53, 53, 53, 53, 51, 51, 51, 52 ), fldtyp(i)
c
  51    continue
c   grab this field
        spot = 1 + fldoff(i)
c                  
        call givgts( butter, mne(1:2), igate1, numgt
     1         , fat(spot), numgtx, badval)
c
        iend = spot+numgt-1
        if( airpln .and. grnd_out ) then
c   zap the ground!
    	    call out_ground( butter, mne(1:2), fat(spot), numgt
     +	    	, salti, ell
     +	    	, earthr, gbmwid, rg(1), rg(numgt), rscr
     +	    	, maxgt, badval)
    	endif
c
        do 511 j=spot, iend
            if( fat(j) .eq. badval ) then
                fat(j) = znull
            endif
 511    continue                  
c
        if( fldtyp(i) .eq. 2 ) then                           
c   convert the dbz field to z
            do 512 j = spot, iend
                if( fat(j) .ne. znull ) then
                   fat(j) = 10.**(.1*fat(j))
                endif
 512        continue
        endif
        go to 66
c
c
  52    continue
c   create a special field
        spot = 1 + fldoff(i)
        call fspecl( numgt, azz, ell, rg, slati, slong, salti,
     +      fat(l1), fat(l2), fat(l3), dlati, dlong, dalti,
     +      gnyqv, itm, fat(spot))
        go to 66
c
c
  53    continue
c   do nothing...this is a special field like time, az, el, or range
c   or a repeat source field
        go to 66
c
  66  continue
c
         numpnt = numpnt+numgt
c
c...mark
c
c   update tape ending time and volume ending time in the header
        ih1 = 27
        ih2 = 122
        do 70 i = 1, 6
            call put16(hedrec,inca(ih1,1),ttm(i))
            call put16(hedrec,inca(ih2,1),ttm(i))
  70    continue
c
    	fcount = fcount+1
c
c   now filter it to the grid
c
      call filter( fat(l1), fat(l2), fat(l3), fat, numgt,
     a  zok, azz, ell, itm, newvol, igskip, azrad, elrad, rgrad,
     a  iradfun, diag_ljm )
c
c 
  77  continue
c
        call scanfo( kswepn(butter), kswepm(butter), ttm, azz, ell, 
     +      ufixed(butter), .false., newvol, fcount )

c
c   next ray
      call setray(butter,1,error)
c
      if(error.gt.900)then
    	 final = .true.
c    	 write(*,*) 'final:', error, numbm
         goto 900
      elseif(error.ne.0)then
         knterr=knterr+1
         write(6,788)error,numbm
 788     format(' error code is ',i10,' at beam number = ',i10)
         if(knterr.gt.10)then
            call typstg( lunout(), 'too many errors^',1,0)
            goto 900
         else
            goto 77
         endif
      elseif( .not.span .and. 
     +      ( eovset() .or. volnum .ne. kvoln(butter))) then
    	 write(*,*) 'endit ', volnum, kvoln(butter)
         goto 900
      else
         numbm=numbm+1
         knterr=0
         goto 44
      endif
c 
c 
 900  continue
c   pass through the data is complete
c
        call scanfo( kswepn(butter), kswepm(butter), ttm, azz, ell, 
     +      ufixed(butter), .true., newvol, fcount )
c
        write(*,*) 'final error:', error, numbm
        call i8( 'total beams input', 0, numbm )
        call dmplnb
        call i8( 'total beams excluded', 0, excnt )
        call dmplnb
        call i8( 'total gates after exclusions', 0, numpnt )
        call dmplnb
        itc = namefx( 'TCOUNT', k )
        call i8( 'total gates thresholded out'
     +	    , 0, itc )
        call dmplnb
c
    	if(fcount .gt. 1) then
            call wrimud( butter, fat(l1), fat(l4), hedrec, syms, nsym,
     +      	acs, nacs )
    	else
    	    call typstg(lunout(), 'No gates within the grid!^',1,0)
    	endif
c
    	if( multiv .and..not. final ) then
c   process another volume
    	    fcount = 0
    	    go to 24
    	endif
c
c
        call typstg( lunout(), 'processing finished^',1,0)
c                    
 909  continue
    	call clsscr()
        call dmplin( 'stop ', 4 )
c        call histit
        return
        end
c
c-----------------------------------------------------------------------
        subroutine inidat( lun, acs, nacs, aaz, advec, advhgt
     a      , xoff, yoff, zoff, pt1, pinc, syms, nsym, az1, az2
     a      , exct1, exct2, nxcl, multiv, prf1, prf2, fixflt
     a	    , igskip, flee, azrad, elrad, rgrad, earthr
     a	    , gnyqv, grc, gbmwid, grnd_out, range1, range2
     a      , swpskp, bmskp, iadjust_z, sprint_xlt, twiddle
     a      , iradfun, diag_ljm)
c  obtain reo specifiec parameters from input file
c
        character acs*(*), syms*(*)
        dimension aaz(*), advec(*), advhgt(*)
        logical iadjust_z, sprint_xlt
        logical same
        logical diag_ljm
c
    	parameter( znull=-32768.)
        parameter( maxval=500, maxfld=7, maxf32=maxfld*8 )
    	parameter( maxxcl=132 )
c
        common  / datset /      slong,          slati,          salti
     +,         dlong,          dlati,          dalti,          dbase
     +,         axmin(3),       axmax(3),       radius(3),    delta(3)
     +,         deltai(3),      numval(3),      spacng(3)
     +,         tref,           values(maxval,3)
c 
	integer swpskp, bmskp
        integer tref
c
        integer     exct1(*),       exct2(*)
c
    	logical span, multiv, fixflt, grnd_out
        logical flee, copln, airpln, outflg
        integer c, c1, a1, aa1, ttm(6)
c
        data    c / 1 /
        data    nh, nsx / 2*0 /
        data    outflg  / .false. /
c
c
c
c       Read in all user-specified input line-by-line.  This
c       includes all comments, semicolon lines, and attributes
c       along with their parameters.  This input stream is
c       listed like the input followed by each input attribute
c       and its parameters, leaving out all semicolon and
c       comment (^) lines.
c     
        call idset( acs, nacs, flee )

c       LJM (6/15/09 diagnostic prints)
        if(diag_ljm)then
           print *,'After call idset: acs=attribute:parameter; stream'
           print *,'After call idset nacs=',nacs
           print *,'After call idset acs=',acs
           print *,' '
        endif
c
c       call insl( '*inidat*', 8 )
c       call i8( 'nacs', 4, nacs )
c       
        twiddle = 3
        c1 = c

        call fields( acs, c1, nacs, flee, diag_ljm )
c
c     subroutine fields( acs, a1, a2, flee, diag_ljm )
c     This routine extracts information from the attribute-parameter
c     input stream.  This includes the attribute (FIELDS:), followed 
c     the special mnemonic (designates the input field such as TI, AZ,
c     EL, ...) and output field name.
c
        call flush(6)
        if( flee ) then
            go to 90
        endif
c       
c
c   set up some defaults
c
	j = namefx('GECHO_GATE', i)
	call putfx(i, 3)
c
c
c       look for the first occurance of the OUTPUT or CEDRIC attributes
c       
    	i = indexa( acs, 1, nacs, 4, 'OUTPUT' )
    	if( i .gt. 0 ) then
    	    call attarg( acs, i, nacs, 1, a1, n )
    	    if( n .gt. 0 ) then
              	call putstr( 'OUTPUT', acs, a1, n )
              	outflg = .true.
    	    endif
    	endif
c       
    	i = indexa( acs, 1, nacs, 4, 'CEDRIC_FILE' )
    	if( i .gt. 0 ) then
    	    call attarg( acs, i, nacs, 1, a1, n )
    	    if( n .gt. 0 ) then
              	call putstr( 'CEDRIC', acs, a1, n )
              	outflg = .true.
    	    endif
    	endif
c       
    	i = indexa( acs, 1, nacs, 4, 'NETCDF_FILE' )
    	if( i .gt. 0 ) then
    	    call attarg( acs, i, nacs, 1, a1, n )
    	    if( n .gt. 0 ) then
              	call putstr( 'NETCDF', acs, a1, n )
              	outflg = .true.
    	    endif
    	endif
c       
    	i = indexa( acs, 1, nacs, 4, 'PROJECT' )
    	if( i .gt. 0 ) then
c       get the project name
    	    call attarg( acs, i, nacs, 1, a1, n )
    	    if( n .gt. 0 ) then
              	call putstr( 'PROJECT', acs, a1, n )
              	outflg = .true.
    	    endif
    	endif
c       
    	i = indexa( acs, 1, nacs, 4, 'SCIENTIST' )
    	if( i .gt. 0 ) then
c       get the scientist name
    	    call attarg( acs, i, nacs, 1, a1, n )
    	    if( n .gt. 0 ) then
              	call putstr( 'SCIENTIST', acs, a1, n )
              	outflg = .true.
    	    endif
    	endif
c       
    	i = indexa( acs, 1, nacs, 4, 'SEQUENCE' )
    	if( i .gt. 0 ) then
c       get the sequence number
    	    call attarg( acs, i, nacs, 1, a1, n )
    	    if( n .gt. 0 ) then
              	call putstr( 'SEQUENCE', acs, a1, n )
              	outflg = .true.
    	    endif
    	endif
c       
    	i = indexa( acs, 1, nacs, 4, 'EARTH_RADIUS' )
    	if( i .gt. 0 ) then
            call rargs( acs, i, nacs, rearth, zmin, zsec )
            if( rearth .gt. 5000. .and. rearth .lt. 7000. ) then
            	earthr = rearth
            endif
    	endif
c
    	a1 = indexa( acs, 1, nacs, 3, 'NYQUIST_VEL' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, gnyqv, rum, dum )
    	    zx = namefp('NYQUIST_VEL', i)
    	    call putfp(i, gnyqv)
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 7, 'RADAR_CONSTANT' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, grc, rum, dum )
    	    zx = namefp('RADAR_CONST', i)
    	    call putfp(i, grc)
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'ADJUST_Z' )
    	if( a1 .gt. 0 ) then
            iadjust_z = .true.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 6, 'SPRINT_XYZ' )
    	if( a1 .gt. 0 ) then
           twiddle = 4
            sprint_xlt = .true.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'GROUND_OUT' )
    	if( a1 .gt. 0 ) then
            grnd_out = .true.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 5, 'GND_ECHO_BMWIDTH' )
    	if( a1 .gt. 0 ) then
            grnd_out = .true.
            call attarg( acs, a1, nacs, 1, aa1, n )
    	    if(n .gt. 0) then
            	call rargs( acs, a1, nacs, gbmwid, rum, dum )
    	    	zx = namefp('BEAM_WIDTH', i)
    	    	call putfp(i, gbmwid)
    	    endif
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 7, 'GECHO_GATE_TWEAK' )
    	if( a1 .gt. 0 ) then
            grnd_out = .true.
            call attarg( acs, a1, nacs, 1, aa1, n )
            call dcode( acs, aa1, n, ityp, ix, zx )
	    j = namefx('GECHO_GATES', i)
	    call putfx(i, ix)
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 7, 'GECHO_ALT_TWEAK' )
    	if( a1 .gt. 0 ) then
            grnd_out = .true.
            call attarg( acs, a1, nacs, 1, aa1, n )
    	    if(n .gt. 0) then
            	call rargs( acs, a1, nacs, gtweak, rum, dum )
    	    	zx = namefp('GECHO_ALT', i)
    	    	call putfp(i, gtweak)
    	    endif
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 5, 'USE_GEOMETRIC_ALT' )
    	if( a1 .gt. 0 ) then
            grnd_out = .true.
	    j = namefx('GEO_ALT', i)
	    call putfx(i, 1)
    	endif
c       
c       Get user-specified latitude, longitude, and altitude
c       of the radar.
c          slati - latitude of the radar (North is +, South is -)
c          slong - longitude of the radar (East is +. West is -)
c          salti - altitude (km MSL) of the radar
c
    	a1 = indexa( acs, 1, nacs, 4, 'RLONGITUDE' )
    	if( a1 .gt. 0 ) then
            zmin = 0
            zsec = 0
            call rargs( acs, a1, nacs, slong, zmin, zsec )
            slong = slong + (zmin*60.+zsec)/3600.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'RLATITUDE' )
    	if( a1 .gt. 0 ) then
            zmin = 0
            zsec = 0
            call rargs( acs, a1, nacs, slati, zmin, zsec )
            slati = slati + (zmin*60.+zsec)/3600.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'RALTITUDE' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, salti, rum, dum )
    	endif
c       
c       Get user-specified longitude, latitude, and altitude of the
c       grid origin along with the baseline azimuth (normally 90 deg).
c
c          dlati - latitude of grid origin (North is +, South is -)
c          dlong - longitude of grid origin (East is +, West is -)
c          dalti - altitude (km MSL) of the grid origin
c          dbase - azimuth of the baseline (+X direction)
c
c       Note:  If the z output coordinate output is to be MSL, set
c       dalti to zero and salti for the radar to its MSL height.
c
    	a1 = indexa( acs, 1, nacs, 4, 'GLONGITUDE' )
    	if( a1 .gt. 0 ) then
            zmin = 0
            zsec = 0
            call rargs( acs, a1, nacs, dlong, zmin, zsec )
            dlong = dlong + (zmin*60.+zsec)/3600.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'GLATITUDE' )
    	if( a1 .gt. 0 ) then
            zmin = 0
            zsec = 0
            call rargs( acs, a1, nacs, dlati, zmin, zsec )
            dlati = dlati + (zmin*60.+zsec)/3600.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'GALTITUDE' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, dalti, rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'GBASELINE' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, dbase, rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'XMIN' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, axmin(1), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'XMAX' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, axmax(1), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'XSPACING' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, SPACNG(1), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'XRADIUS' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, radius(1), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'YMIN' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, axmin(2), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'YMAX' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, axmax(2), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'YSPACING' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, SPACNG(2), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'YRADIUS' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, radius(2), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'ZMIN' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, axmin(3), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'ZMAX' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, axmax(3), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'ZSPACING' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, SPACNG(3), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'ZRADIUS' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, radius(3), rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'AZRADIUS' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, azrad, rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'ELRADIUS' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, elrad, rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'RGRADIUS' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, rgrad, rum, dum )
    	endif

c***************************************************************
c     Determine the type of box dimensions to use
c        iradfun = 1 (Cartesian - Use fixed XYZ radii)
c        iradfun = 2 (Spherical - Use range-dependent RAE radii)
c        iradfun = 3 [Hybrid    - Use AMAX1(XYZ,RAE) radii]
c
        call flush(6)

c     Set the default BOX DIMENSION specification
c
        iradfun = 0
        print *,' '
        print *,'BOX DIMENSIONS: default iradfun=',iradfun
        call flush(6)
c     
c       function indexa( a, ith, jth, n, p )
        if(diag_ljm)then
           print *,'BOX DIM before indexa:  acs=',acs
           print *,'BOX DIM before indexa: nacs=',nacs
        endif
        a1 = indexa( acs, 1, nacs, 6, 'BOXDIM' ) 
        print *,'BOX DIM after indexa: a1=',a1
        call flush(6)
        if( a1 .gt. 0 ) then
            call attarg( acs, a1, nacs, 1, aa1, n )
            print *,'BOX DIM: pointer to begin attribute  aa1=',aa1
            print *,'BOX DIM: length of parameter           n=',n
            call flush(6)

c     Set the value of iradfun according to which weighting function
c     (parameter = CARTESIAN, SPHERICAL, or HYBRID) was
c     selected.  The logical function (foflib.F:same) does a comparison
c     between the parameter following the BOX DIM attribute and the 
c     character string in single quotes.
c
c     logical function same( a, i, n, b, j )
c
            if( same( acs, aa1, 6, 'CARTESIAN', 1 )) then
               iradfun = 1
            elseif( same( acs, aa1, 6, 'SPHERICAL', 1 )) then
               iradfun = 2
            elseif( same( acs, aa1, 6, 'HYBRID', 1 )) then
               iradfun = 3
            endif
         else
            print *,'***** BOX DIMENSIONS MUST BE SPECIFIED *****'
            print *,'** EITHER CARTESIAN, SPHERICAL, OR HYBRID **'
    	    call panic( '*********' )
         endif
         print *,'BOX DIMENSIONS: iradfun=',iradfun        
         call flush(6)
         call i8( 'box dim=',8,iradfun )
         print *,' '
         call flush(6)
c*******************************************************

c       
    	a1 = indexa( acs, 1, nacs, 4, 'XOFFSET' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, xoff, rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'YOFFSET' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, yoff, rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'ZOFFSET' )
    	if( a1 .gt. 0 ) then
            call rargs( acs, a1, nacs, zoff, rum, dum )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 5, 'GATESKIP' )
    	if( a1 .gt. 0 ) then
            call attarg( acs, a1, nacs, 1, aa1, n )
            call dcode( acs, aa1, n, ityp, ix, rum )
    	    if( ix .gt. 0 ) then
    	    	igskip = ix +1
    	    endif
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 5, 'BEAMSKIP' )
    	if( a1 .gt. 0 ) then
            call attarg( acs, a1, nacs, 1, aa1, n )
            call dcode( acs, aa1, n, ityp, ix, rum )
    	    if( ix .gt. 0 ) then
    	    	bmskp = ix +1
    	    endif
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 5, 'SWEEPSKIP' )
    	if( a1 .gt. 0 ) then
            call attarg( acs, a1, nacs, 1, aa1, n )
            call dcode( acs, aa1, n, ityp, ix, rum )
    	    if( ix .gt. 0 ) then
    	    	swpskp = ix +1
    	    endif
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'AZIMUTHS' )
    	if( a1 .gt. 0 ) then
    	    z1 = znull
    	    z2 = znull
            call rargs( acs, a1, nacs, z1, z2, dum )
    	    if( z1 .ne. znull ) then
    	    	az1 = z1
    	    endif
    	    if( z2 .ne. znull ) then
    	    	az2 = z2
    	    endif
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'RANGES' )
    	if( a1 .gt. 0 ) then
    	    z1 = znull
    	    z2 = znull
            call rargs( acs, a1, nacs, z1, z2, dum )
    	    if( z1 .ne. znull ) then
    	    	range1 = z1
    	    endif
    	    if( z2 .ne. znull ) then
    	    	range2 = z2
    	    endif
	    call e12('rng1', 4, range1)
	    call e12('rng2', 4, range2)
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 3, 'PRFS' )
    	if( a1 .gt. 0 ) then
    	    z1 = znull
    	    z2 = znull
            call rargs( acs, a1, nacs, z1, z2, dum )
    	    if( z1 .ne. znull ) then
    	    	prf1 = z1
    	    endif
    	    if( z2 .ne. znull ) then
    	    	prf2 = z2
    	    endif
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'REFERENCE TIME' )
    	if( a1 .gt. 0 ) then
            call attarg( acs, a1, nacs, 1, aa1, n )
            call itimeo( acs, aa1, n, ttm, 1 )
            tref = ttm(1)*3600 + ttm(2)*60 + ttm(3)
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'COPLANE' )
    	if( a1 .gt. 0 ) then
            copln = .true.
            iix = namefx( 'COPLANE', i )
            call putfx( i, 1 )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'AIRCRAFT' )
    	if( a1 .gt. 0 ) then
            airpln = .true.
    	    iix = namefx( 'AIRCRAFT', i )
    	    call putfx( i, 1 )
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'SPAN VOLUME' )
    	if( a1 .gt. 0 ) then
            span = .true.
            iix = namefx( 'SPANVOLS', i )
            call putfx( i, 1 )
   	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'ASCENDING' )
    	if( a1 .gt. 0 ) then
            fixflt = .true.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'MULTI-VOLUME' )
    	if( a1 .gt. 0 ) then
            multiv = .true.
    	endif
c       
    	a1 = indexa( acs, 1, nacs, 4, 'SYMBOLS' )
    	if( a1 .gt. 0 ) then
            call attarg( acs, a1, nacs, 1, aa1, nsym )
            if( nsym .gt. 1 ) then
                call copyc( acs, aa1, nsym, syms, 1 )
            endif
    	endif
c
c       
c       look for advection info
c       
        c = 2
  33    continue
c       
    	a1 = indexa( acs, c, nacs, 4, 'ADVECTION' )
    	if( a1 .gt. 0 ) then
            nh = nh+1
            if( nh .le. maxval ) then
                aaz(nh) = 0
                advec(nh) = 0
                advhgt(nh) = 0
                call rargs(acs,a1,nacs,aaz(nh),advec(nh),advhgt(nh))
                call i8( 'nh', 2, nh )
                call e9( 'az', 2, aaz(nh))
                call e9( 've', 2, advec(nh))
                call e9( 'ht', 2, advhgt(nh))
                c = indexc( acs, a1, nacs, 1, ';' )+1
                if( c .gt. 1 ) then
                    go to 33
                endif
            endif
    	endif
c
c       look for exclusions
c       
        c = 2
  44    continue
c       
    	a1 = indexa( acs, c, nacs, 4, 'EXCLUDE' )
    	if( a1 .gt. 0 ) then
            nxcl = nxcl+1
            ixc = (nxcl-1)*6 + 1
            call attarg( acs, a1, nacs, 1, b, n )
            if( n .gt. 0 ) then
                call idateo( acs, b, n, exct1(ixc), 1 )
            endif
            call attarg( acs, a1, nacs, 2, b, n )
            if( n .gt. 0 ) then
                call itimeo( acs, b, n, exct1(ixc), 4 )
            endif
    	    call i8( 'nx', 2, nxcl )
    	    call prtime( 'x1', 2, exct1(ixc))
            call attarg( acs, a1, nacs, 3, b, n )
            if( n .gt. 0 ) then
                call idateo( acs, b, n, exct2(ixc), 1 )
            endif
            call attarg( acs, a1, nacs, 4, b, n )
            if( n .gt. 0 ) then
                call itimeo( acs, b, n, exct2(ixc), 4 )
            endif
    	    call prtime( 'x2', 2, exct2(ixc))
            c = indexc( acs, a1, nacs, 1, ';' )+1
            if( nxcl .le. maxxcl/6 .and.c .gt. 1 ) then
                go to 44
            endif
    	endif
c       
  90    continue
    	call i8( 'igs', 3, igskip )
        if( .not. outflg ) then
            flee = .true.
            call typstg(lunout(), 'No output file specification ^',
     +              1,0)
        endif
c       
        return
        end
c-----------------------------------------------------------------------
        subroutine fields( acs, a1, a2, flee, diag_ljm )

c   qreo1.F:call fields( acs, c1, nacs, flee )
c   this routine analyses and accumulates the information about what
c   fields to interpolate and how to interpolate them, information about
c   what fields to output, and which output fields to BSCAN.
c
c   the routines incb and inca return the first argument before or after
c   being incremented by the second argument

c   Communicates parameters through common blocks named store,boxez and fldcom -DFF 3/17
c   Block definitions could be replaced with an include (like)
c
C
c
c   Input: acs - the compressed attribute-parameter stream
c          a2  - length of acs (nacs elsewhere in calls)
c
        implicit integer( a-y )
        implicit real( z )
    	parameter( YES=1, NO=0 )
        character acs*(*)
        logical same, anumbr, flee
        logical diag_ljm
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
        integer natfld, attyp, atptr
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
        integer noutf, comptr, oftyp
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
c   calculate the pointer to the corresponding 8 character output field name
        mnptr( i ) = 1 + (i-1)*8
c
c   initialize source field offset pointer
        data fo / mxg3 /
c
c
c     (LJM 6/15/09 - diagnostic prints)
c
        if(diag_ljm)then
           print *,'Fields has been called'
           print *,'FIELDS:   acs=',acs
           print *,'FIELDS: a1,a2=',a1,a2
        endif

        call insl( '**fields**  ', 10 )
        flee = .false.
c   determine the weighting function (default is CRESSMAN)
        wtfun = 1
c
        f1 = indexa( acs, a1, a2, 5, 'WEIGHTING' ) 
        if( f1 .gt. 0 ) then
            call attarg( acs, f1, a2, 1, aa, na)
            
            if(diag_ljm)then
               print *,'WEIGHTS: pointer to begin attribute f1=',f1
               print *,'WEIGHTS: pointer to begin parameter aa=',aa
               print *,'WEIGHTS: length of parameter        na=',na
            endif
            call flush(6)

c     Set the value of wtfun according to which weighting function
c     (parameter = CRESSMAN, EXPONENTIAL, CLOSEST, or UNIFORM) was
c     selected.  The logical function (foflib.F:same) does a comparison
c     between the parameter following the WEIGHTING attribute and the 
c     character string in single quotes.
c
c     logical function same( a, i, n, b, j )
c
           if( same( acs, aa, 5, 'CLOSEST', 1 )) then
                wtfun = 3
            elseif( same( acs, aa, 5, 'EXPONENTIAL', 1 )) then
                wtfun = 2
                if( anumbr( 2, acs, f1, a2, kint, zreal )) then
                    atten = zreal
                endif
            elseif( same( acs, aa, 5, 'UNIFORM', 1 )) then
                wtfun = 4
            endif
        endif
c
        call i8( 'wtf', 3, wtfun )
        call e12( 'atn', 3, atten )
c
c   loop through the input fields that are also to be output
        ae = a1-1
  22    continue 
        if( ae+1 .ge. a2 ) then
            go to 44
        endif

c     subroutine nxtatt( ab, ith, jth, a1, nch, ae )
c     Examines the ith-jth characters of ab to fi
c     the first non-blank character (a1)

        call nxtatt( acs, ae+1, a2, f1, nch, ae )

c     (LJM 06/15/09 diagnostic prints)
c
        if(diag_ljm)then
           print *,'qreo1.F:fields first call nxtatt'
           print *,'After call nxtatt:   ae,ae+1,a2=',ae,ae+1,a2
           print *,'After call nxtatt:       f1,nch=',f1,nch
           print *,'After call nxtatt: acs(ae,ae+1)=',acs(ae:ae+nch)
           print *,' '
        endif

c   get first argument info if there is one
        call attarg( acs, f1, ae, 1, aa, na )
        ff = nflds+1
c
c
        if( same( acs, f1, 5, 'FIELD', 1 )) then
            if( na .le. 0 ) then
                flee = .true.
                call typstg( lunout(),
     +                  'No source field name for ^', 1, 0 )
                call typstg( lunout(), acs, f1, nch )
            endif
c       determine the field type
            if( same( acs, aa, 2, 'TIME', 1 )) then
                fldtyp(ff) = 3
            elseif( same( acs, aa, 2, 'AZ', 1 )) then
                fldtyp(ff) = 4
            	unfptr(ff) = incb( lenbox, 1 )
            	space( unfptr(ff)) = znull
c   reserve a spot for the reference angle
            elseif( same( acs, aa, 2, 'EL', 1 )) then
                fldtyp(ff) = 5
            	unfptr(ff) = incb( lenbox, 1 )
            	space( unfptr(ff)) = znull
c   reserve a spot for the reference angle
            elseif( same( acs, aa, 2, 'RG', 1 )) then
                fldtyp(ff) = 6
            elseif( same( acs, aa, 2, 'XX', 1 )) then
c       field produced by routine FSPECL
                fldtyp(ff) = 10
            else
c       this is a real data field!
                fldtyp(ff) = 1
            endif
            call mnesrc( fldmne, nflds, acs, aa, 2, ith
     +              ,f1,nch,flee,NO)
            if( ith .gt. 0 .and. fldtyp(ith) .ne. 2 ) then
c       this source field already exists and is useable
                fldoff(ff) = fldoff(ith)
                dupfld(ff) = .true.
            else
c       offset to this field in the source field buffer
                fldoff(ff) = inca( fo, maxgt )
            endif
c
c
        elseif( same( acs, f1, 5, 'DBZFIELD', 1 )) then
            if( na .le. 0 ) then
                flee = .true.
                call typstg( lunout(),
     +	    	    'No source field name for ^', 1, 0 )
                call typstg( lunout(), acs, f1, nch )
            endif
            if( wtfun .eq. 3 ) then
                fldtyp(ff) = 1
            else
                fldtyp(ff) = 2
            endif
            fldoff(ff) = inca( fo, maxgt )
c
c
        elseif( same( acs, f1, 5, 'UNFOLD', 1 ) .or. 
     +	    	same( acs, f1, 5, 'KFIELD', 1 )) then
            if( na .le. 0 ) then
                flee = .true.
                call typstg( lunout(),
     +	    	    'No source field name for ^', 1, 0 )
                call typstg( lunout(), acs, f1, nch )
            endif
    	    if(same( acs, f1, 5, 'KFIELD', 1 )) then
            	fldtyp(ff) = 9
            elseif( same( acs, aa, 2, 'XX', 1 )) then
                fldtyp(ff) = 10
            else
                fldtyp(ff) = 1
            endif
c
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,f1,nch,flee,NO)
            if( ith .gt. 0 ) then
                fldoff(ff) = fldoff(ith)
                dupfld(ff) = .true.
            else
                fldoff(ff) = inca( fo, maxgt )
            endif
            unfflg(ff) = .true.
            unfptr(ff) = incb( lenbox, 1 )
            space( unfptr(ff)) = znull
c   reserve a spot for the reference velocity
c   set nyquist velocity pointer
            nyqptr(ff) = incb( nlards, 1 )
            zlard(nyqptr(ff)) = znull
c
        else           
            go to 22
        endif
c
c
c   set up the rest of the info and pointers
c
        nflds = nflds + 1
c   get the input field mnemonic
        call copyc( acs, aa, 2, fldmne, ff*2-1 )
c   bump output field count
        nx = incb( noutf, 1 )
c
c   put away the name of the output field
        call attarg( acs, f1, ae, 2, ab, nb )
        call bfill( mnames, mnptr(nx), 8 )
        if( nb .gt. 0 ) then
            call copyc( acs, ab, nb, mnames, mnptr(nx) )
        else
c   if no alias, just use the field mnemonic
            call bfill( mnames, mnptr(nx), 8 )
            call copyc( fldmne, ff*2-1, 2, mnames, mnptr(nx))
        endif
c
        if( wtfun .ne. 3 ) then
            natfld(ff) = 2
c   there will be 2 components in the output field--the weighted sum and
c   the sum of the weights (first attached field)
            attyp(1,ff) = 11
            atptr(1,ff) = incb( lenbox, 1 )
            comptr(1,nx) = atptr(1,ff)
            space( atptr(1,ff)) = 0
c  set up the weighted sum (second attached field)
            attyp(2,ff) = 1
            atptr(2,ff) = incb( lenbox, 1 )
            comptr(2,nx) = atptr(2,ff)
            space( atptr(2,ff)) = 0
c
            if( fldtyp(ff) .eq. 2 ) then
c   dBz field
                oftyp(nx) = 23
            else
                oftyp(nx) = 21
            endif
c
        else
c   only saving the closest point
            natfld(ff) = 2
            oftyp(nx) = 25
c   data
            attyp(1,ff) = 4
            atptr(1,ff) = incb( lenbox, 1 )
            space( atptr(1,ff)) = znull
            comptr(1,nx) = atptr(1,ff)
c   min radius
            attyp(2,ff) = 5
            atptr(2,ff) = incb( lenbox, 1 )
            space( atptr(2,ff)) = wmax
            comptr(2,nx) = atptr(2,ff)
        endif                        
c
        go to 22       
c
c
c
c...mark
c
  44    continue
c
c   analyse the data set for other information
        ae = a1-1
c
  45    continue
        if( ae+1 .ge. a2 ) then
            go to 66
        endif
        call nxtatt( acs, ae+1, a2, f1, nch, ae )

c     (LJM 06/15/09 diagnostic prints)
c
        if(diag_ljm)then
           print *,'qreo1.F:fields second call nxtatt'
           print *,'After call nxtatt:   ae,ae+1,a2=',ae,ae+1,a2
           print *,'After call nxtatt:       f1,nch=',f1,nch
           print *,'After call nxtatt: acs(ae,ae+1)=',acs(ae:ae+nch)
           print *,' '
        endif

c   first argument
        call attarg( acs, f1, ae, 1, aa, na )
c   second argument
        call attarg( acs, f1, ae, 2, ab, nb )
c
c
        if( same( acs, f1, 4, 'QUALITY', 1 ) .and. na .gt. 0 ) then
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
            nx = incb( noutf, 1 )
c   need to create a count field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 12, ith, 1, 0 )
c   need to create an sum field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 2, ith, 2, 0 )
c   need to create a sum of the squares field if not there
   	    call crefld( attyp(1,ith), natfld(ith), 3, ith, 3, 0 )
c   pointer to the nyquist velocity
            if( nyqptr(ith) .ne. inull ) then
                comptr(4,nx) = nyqptr( ith )
            else
                nyqptr(ith) = incb( nlards, 1 )
                zlard(nyqptr(ith)) = znull
                comptr(4,nx) = nyqptr( ith )
            endif
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
            call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 22
c
c
c
        elseif( same( acs, f1, 5, 'COUNT', 1 ) .and. na .gt. 0 ) then
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
            nx = incb( noutf, 1 )
c   need to create a count field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 12, ith, 1, 0 )
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 27
c
c
c
        elseif( same( acs, f1, 4, 'MINBIGR', 1 ) .and. na .gt. 0 ) then
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
            nx = incb( noutf, 1 )
c   need to create a min big R field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 15, ith, 1, wmax )
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 30
c
c
        elseif( same( acs, f1, 4, 'MAXBIGR', 1 ) .and. na .gt. 0 ) then
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
            nx = incb( noutf, 1 )
c   need to create a max big R field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 16, ith, 1, 0 )
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 30
c
c
c
        elseif( same( acs, f1, 4, 'AVERAGE', 1 ) .and. na .gt. 0 ) then
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
            nx = incb( noutf, 1 )
c   need to create a count field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 12, ith, 1, 0 )
c   need to create an sum field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 2, ith, 2, 0 )
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 2
c
c
c
        elseif( same( acs, f1, 4, 'SDEVIATION', 1 )) then
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
            nx = incb( noutf, 1 )
c   need to create a count field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 12, ith, 1, 0 )
c   need to create an sum field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 2, ith, 2, 0 )
c   need to create a sum of the squares field if not there
   	    call crefld( attyp(1,ith), natfld(ith), 3, ith, 3, 0 )
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 28
c
c
c
        elseif( same( acs, f1, 4, 'WFACTOR', 1 )) then
c   another quality parameter...sum of squared weights over the 
c   sum of the weights squared
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
            nx = incb( noutf, 1 )
c   need to create a weight field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 11, ith, 1, 0 )
c   need to create a weights squared field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 14, ith, 2, 0 )
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 24
c
c
c
        elseif( same( acs, f1, 4, 'RADIUS', 1 )) then
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
            nx = incb( noutf, 1 )
c   need to create a min radius field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 5, ith, 1, wmax )
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 30
c
c
c
        elseif( same( acs, f1, 4, 'VREFERENCE', 1 )) then 
c   output the reference velocity
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   search the unfold flags until the source fields match
            call unfsrc( unfflg, fldmne, nflds, acs, aa, 2, ith )
            if( ith .le. 0 ) then
c   error   
                flee = .true.
                call typstg( lunout(),
     +'No source field for ^', 1, 0 )
                call typstg( lunout(), acs, f1, nch )
    	    	go to 45
            endif
            nx = incb( noutf, 1 )
            comptr(1,nx) = unfptr(ith)
c   this field is treated similar to the closest point field
            oftyp(nx) = 25
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c
c
c
        elseif( same( acs, f1, 5, 'OCTANT', 1 )) then
    	    if( nb .lt. 1 ) then
    	    	go to 60
    	    endif
c   locate the source field
            call mnesrc( fldmne, nflds, acs, aa, 2, ith,
     +	    	f1,nch,flee,YES)
    	    if( flee ) then
    	    	go to 45
    	    endif
    	    nx = incb( noutf, 1 )
c   need to create an octant field if it's not already there
   	    call crefld( attyp(1,ith), natfld(ith), 13, ith, 1, 0 )
c   put away the name of the output field
            call bfill( mnames, mnptr(nx), 8 )
    	    call copyc( acs, ab, nb, mnames, mnptr(nx) )
c   output field type
            oftyp(nx) = 29
c
c
        endif            
        go to 45
c 
  60	continue
c   error!
    	flee = .true.
        call typstg( lunout(),
     +	    	'No output field name for ^', 1, 0 )
        call typstg( lunout(), acs, f1, nch )
    	go to 45
c
  66    continue
c
  88    continue
c
  90    continue
c
        call i8( 'nflds', 5, nflds )
        call i8( 'wtf', 3, wtfptr )
        call i8( 'mnr', 3, mnrptr )
        call i8( 'lenbox', 6, lenbox )
        call dmplnb
        call typstg( lunout(), fldmne, 1, nflds*2 )
c        call typefx( fldtyp, 1, nflds )
c        call typefx( fldoff, 1, nflds )
c        call typefe( thrval, 1, nflds )
c        call typefx( nyqptr, 1, nflds )
c        call typefx( natfld, 1, nflds )
        call i8( 'ngpps', 5, ngpps )
c        call dmplnb
c        call typefx( gpptyp, 1, ngpps )
c        call typefx( gppptr, 1, ngpps )
c        do 989 i = 1, nflds
c            call i8( 'f', 1, i )
c            call dmplnb                           
c            call typefx( attyp(1,i), 1, natfld(i))
c            call typefx( atptr(1,i), 1, natfld(i))
c 989    continue              
c        call i8( 'nlards', 6, nlards )
c        call dmplnb
c        call typefe( zlard, 1, nlards )
        call i8( 'noutf', 5, noutf )
c        call i8( 'ocp', 3, octptr )
c        call dmplnb
c        call typefx( oftyp, 1, noutf )
c        do 990 i = 1, noutf
c            call typefx( comptr(1,i), 1, mxcomp )
c 990    continue
        call typstg( lunout(), mnames, 1, 8*noutf )
        call dmplin( 'fieldsx  ', 8 )
c
c
        return
        end
c-------------------------------------------------------------------------
        subroutine typsrc( a, n, ityp, ith )
c   this routine searches "n" elements of array "a" for the number "ityp"
c   and returns its location in the array
c
        implicit integer( a-z )
        integer a(*)
c
        ith = -1
        do 11 i = 1, n
            if( a(i) .eq. ityp ) then
                ith = i
                go to 90
            endif
  11    continue
c
  90    continue
        return
        end
c-------------------------------------------------------------------------
        subroutine mnesrc( mne, nf, s, i, n, ith
     +	    , f1, nch, flee, verify )
c   this routine seaches an array of mnemonics "mne" for the "n" character
c   string beginning at character "i" of string "s" and returns the location
c   if found in "ith"
c
        implicit integer( a-z )
    	parameter( YES=1, NO=0 )
        character mne*(*), s*(*)
        logical flee
c
        ith = -1
        l = 1
        do 11 k = 1, nf
            if( mne(l:l+1) .eq. s(i:i+1)) then
                ith = k
                go to 22
            endif
            l = l+2
  11    continue
  22    continue
c
    	if( verify .eq. YES .and. ith .lt. 0 ) then
            flee = .true.
            call typstg( lunout(),
     +              'No source field for ^', 1, 0 )
            call typstg( lunout(), acs, f1, nch )
    	endif
c
  90    continue
        return
        end

c-------------------------------------------------------------------------
        subroutine unfsrc( unff, mne, nf, s, i, n, ith )
c   this routine is searching for the source of a unfolding information.
c   this routine seaches the array of unfolding flag "unff" and 
c   the array of mnemonics "mne" for the "n" character
c   string beginning at character "i" of string "s" and returns the location
c   if found in "ith"
c
        implicit integer( a-z )
        character mne*(*), s*(*)
        logical unff(*)
c
        ith = -1
        l = 1
        do 11 k = 1, nf
            if( unff(k) .and. mne(l:l+1) .eq. s(i:i+1)) then 
                ith = k
                go to 90
            endif
            l = l+2
  11    continue
c
  90    continue
        return
        end
c-------------------------------------------------------------------------
        subroutine crefld( a, n, ityp, ith, cpn, def )
c   this routine searches "n" elements of array "a" for the number "ityp"
c
        implicit integer( a-z )
        integer a(*)
    	real def
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
        integer natfld, attyp, atptr
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
        integer noutf, comptr, oftyp
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
        jth = -1
        do 11 i = 1, n
            if( a(i) .eq. ityp ) then
                jth = i
                go to 22
            endif
  11    continue
  22	continue
c
    	if( jth .gt. 0 ) then
                comptr(cpn,noutf) = atptr( jth, ith )
    	else
                at = incb( natfld(ith), 1 )
                attyp( at, ith ) = ityp
                atptr( at, ith ) = incb( lenbox, 1 )
                space( atptr(at,ith)) = def
                comptr(cpn,noutf) = atptr( at, ith )
    	endif
c
  90    continue
        return
        end

c----------------------------------------------------------------------
        subroutine scanfo( sno, stype, ttm, az, el, fixed, flush
     +	    , newvol, fcount )
c   this routine accumulates and prints scan information
c
        implicit integer( a-z )
        dimension ttm(*), ttmx(6)
        character*256 l
        real zreal, az, el, fixed, azdiff
        real fx, fxcw, fxcc, fxsum
        real va, vaold, vacw, vacc, dva, dvamx, dvamn, dvasum
    	real va1, va2
        logical flush, first, newvol
    	save l, c, count, ttmx
     + , s1, s2, t1, t2, f1, f2, f3, f4, v1, v2, d1, d2, d3, n1, fc1    
     + , fxcw, fxcc, fxsum, dvamx, dvamn, dvasum, vacw, vacc
     + , nb, fclast, va1, va2, vaold, snox, stypex

        data nb / -1 /
        data snox / -1 /
        data stypex / -1 /
        data first / .true. /
    	data count / 0 /
c
c
c
c
    	count = count+1
c
        if(count .eq. 1 ) then
            c = 1
c   place holder for the scan number
            s1 = inca( c, 6 )
c   place holder for the scan type
            s2 = inca( c, 6 )
c   place holder for start time
            t1 = inca( c, 9 )
c   place holder for stop time
            t2 = inca( c, 11 )
c   place holder for fixed angle
            f1 = inca( c, 8 )
c   place holder for minimum fixed angle
            f2 = inca( c, 7 )
c   place holder for maximum fixed angle
            f3 = inca( c, 7 )
c   place holder for average fixed angle
            f4 = inca( c, 10 )
c   place holder for minimum varying angle
            v1 = inca( c, 7 )
c   place holder for maximum varying angle
            v2 = inca( c, 9 )
c   place holder for minimum spacing
            d1 = inca( c, 6 )
c   place holder for maximum spacing
            d2 = inca( c, 7 )
c   place holder for average spacing
            d3 = inca( c, 9 )
c   place holder for the number of beams
            n1 = inca( c, 5 )
c   place holder for count of beams that intersect the grid
    	    fc1 = inca(c, 5)
c   print heading
            call lf( lunout())
            call lf( lunout())
            call bfill( l, 1, 128 )
            call copyc( 'SCAN INFO', 1, 9, l, s1 )
            call copyc( 'FIXED ANGLE INFO   ', 1, 16, l, f1 )
            call copyc( 'VARYING ANGLE INFO  ', 1, 13, l, v1 )
            call copyc( 'SPACING INFO  ', 1, 12, l, d1 )
            call copyc( 'BEAM ', 1, 4, l, n1 )
c    	    write(lunout(),789) l(1:c)
            call typlong( lunout(), l, 1, c )
c
            call bfill( l, 1, 128 )
            call copyc( 'NO. ', 1, 3, l, s1+1 )
            call copyc( 'TYPE ', 1, 4, l, s2-1 )
            call copyc( 'TIMES ', 1, 5, l, t1 )
            call copyc( 'FIXED ', 1, 5, l, f1 )
            call copyc( 'CCW ', 1, 3, l, f2+2 )
            call copyc( 'CW ', 1, 2, l, f3+3 )
            call copyc( 'MEAN', 1, 4, l, f4+1 )
            call copyc( 'CCW ', 1, 3, l, v1+1 )
            call copyc( 'CW ', 1, 2, l, v2+2 )
            call copyc( 'MIN. ', 1, 4, l, d1+1 )
            call copyc( 'MAX. ', 1, 4, l, d2+1 )
            call copyc( 'MEAN ', 1, 4, l, d3+1 )
            call copyc( 'COUNT ', 1, 5, l, n1 )
c    	    write(lunout(), 789 ) l(1:c)
            call typlong( lunout(), l, 1, c )
            call bfill( l, 1, 128 )
            call putc( l, t2-1, '-' )
        endif
c
c
c
c
c
c
        if(( flush .or. sno .ne. snox .or. stype .ne. stypex )
     +          .and. nb .gt. 0 ) then
c   dump the information for the last scan
c
c   antenna direction
            if( dvasum .lt. 0 ) then
                call putc( l, s2-1, '-' )
            else
                call putc( l, s2-1, ' ' )
            endif
c   stop time
            call ytime( ttmx(4), ttmx(5), ttmx(6), l, t2, nc )
c   minimum fixed angle
            call ncode( l, f2, 6, 2, x, fxcc, 2 )
c   maximum fixed angle
            call ncode( l, f3, 6, 2, x, fxcw, 2 )
c   average fixed angle
    	    zreal = fxsum/nb
            call ncode( l, f4, 6, 2, x, zreal, 2 )
c   counterclockwise limit
            call ncode( l, v1, 5, 2, x, vacc, 1 )
c            call ncode( l, v1, 5, 2, x, va1, 1 )
c   clockwise limit
            call ncode( l, v2, 5, 2, x, vacw, 1 )
c            call ncode( l, v2, 5, 2, x, vaold, 1 )
c   minimum delta
            call ncode( l, d1, 5, 2, x, dvamn, 2 )
c   maximum delta
            call ncode( l, d2, 5, 2, x, dvamx, 2 )
c   average delta
            if( nb .gt. 1 ) then
    	    	zreal = dvasum/(nb-1)
                call ncode( l, d3, 5, 2, x, zreal, 2 )
            else
                call ncode( l, d3, 5, 2, x, 0, 2 )
            endif
c   the number of beams
            call ncode( l, n1, 4, 1, nb, x, 2 )
c   gridded beams
            call ncode( l, fc1, 4, 1, fcount-fclast, x, 2 )
c    	    write( lunout(), 789 ) l(1:c)
            call typlong( lunout(), l, 1, c )
c
        elseif( flush ) then
            go to 90                                     
        endif
c
c
c
c
c
        if( stype .eq. 3 ) then
c   rhi scan
            fx = az
c   fixed angle
            va = el
c   varying angle
        else
            fx = el
            va = az
        endif
c
c
c
        if( sno .ne. snox .or. stype .ne. stypex ) then
c   new scan!
            snox = sno
            stypex = stype
    	    va1 = va
    	    va2 = va
            vacc = va
            vacw = va
            vaold = va
            dvasum = 0
            dvamx = -400
            dvamn = 400
c
            fxcw = fx
            fxcc = fx
            fxsum = fx
            nb = 1
    	    fclast = fcount
            ttmx(4) = ttm(4)
            ttmx(5) = ttm(5)
            ttmx(6) = ttm(6)
c   encode the scan no.
            call ncode( l, s1, 4, 1, sno, x,x)
c   insert the scan type
            call scntyp( stype, l, s2, nc )
c   start time
            call ytime( ttmx(4), ttmx(5), ttmx(6), l, t1, nc )
c   encode the fixed angle
            call ncode( l, f1, 5, 2, x, fixed, 1 )
            go to 90
        endif
c
c
c
c
c
c   accumulate info
        ttmx(4) = ttm(4)
        ttmx(5) = ttm(5)
        ttmx(6) = ttm(6)
c
        if( azdiff( fxcc, fx ) .lt. 0 ) then
c   new angle is further counterclockwise than the old
            fxcc = fx
        endif
c
        if( azdiff( fxcw, fx ) .gt. 0 ) then
c   new angle is further clockwise than the old angle
            fxcw = fx
        endif
c
        if( azdiff( vacc, va ) .lt. 0 ) then
c   new angle is further counterclockwise than the old
            vacc = va
        endif
c
        if( azdiff( vacw, va ) .gt. 0 ) then
c   new angle is further clockwise than the old angle
            vacw = va
        endif
c
        dva = azdiff( vaold, va )
c                
        if( abs( dva ) .lt. dvamn ) then
            dvamn = abs( dva )
        endif
        if( abs( dva ) .gt. dvamx ) then
            dvamx = abs( dva )
        endif
c
        fxsum = fxsum + fx
        dvasum = dvasum + dva
        vaold = va      
        nb = nb + 1
        go to 90
c
c
c
c
  90    continue
        return
 789	format( ' ', a )
        end
c-----------------------------------------------------------------------
        subroutine fspecl( n, az, el, r, rlat, rlon, ralt, 
     +      x, y, z, glat, glon, galt, vnyq, itm, field )
c
c   this routine is for creating the special field "XX" of radar data where
c
c   n       =   the number of gates in the beam
c   az      =   the azimuth of the beam
c   el      =   the elevation of the beam
c   r       =   is an array of the ranges associated with each gate 
c                   in the beam
c   rlat    =   the radar latitude
c   rlon    =   the radar longitude
c   ralt    =   the altitude of the radar
c   x       =   the x coordinate of the gate relative to the grid origin
c   y       =   the y coordinate of the gate relative to the grid origin
c   z       =   the z coordinate of the gate relative to "galt"
c   glat    =   the grid origin latitude
c   glon    =   the grid origin longitude
c   galt    =   the grid origin altitude
c   vnyq    =   the Nyquist velocity          
c   itm     =   the elapsed time in seconds relative to the reference time
c   field   =   the destination of the created radar data field
c
c
        real r(*), x(*), y(*), z(*), field(*)
c
c
        do 11 i = 1, n
            field(i) = ranf()
  11    continue
c   this is supposed to be the vector form of the random number generator
c
        do 22 i = 1, n
            field(i) = field(i)*2.*vnyq - vnyq
  22    continue
c
c
        return
        end
c----------------------------------------------------------------------
    	subroutine setced( nv, acs, nacs )
c   this routine grabs and stores the current output volume path name
    	implicit integer( a-z )
    	logical ok
c
    	ok = .false.
    	call delstr( 'OUTPUT', nd )
c   get pointer to output attribute in access control string
    	a = namefx( 'OUTPUT', i )
    	if( a .gt. 0 ) then
    	    call attarg( acs, a, nacs, nv, b, n )
    	    if( n .gt. 0 ) then
    	    	call putstr( 'OUTPUT', acs, b, n )
    	    	ok = .true.
    	    endif
    	endif
c
    	call delstr( 'CEDRIC', nd )
    	a = namefx( 'CEDRIC', i )
    	if( a .gt. 0 ) then
    	    call attarg( acs, a, nacs, nv, b, n )
    	    if( n .gt. 0 ) then
    	    	call putstr( 'CEDRIC', acs, b, n )
    	    	ok = .true.
    	    endif
    	endif
c
    	if( .not. ok ) then
    	    write(*,*) 'No output file name for volume ', nv
    	    call panic( '*********' )
    	endif
c
    	return
    	end
c----------------------------------------------------------------------
    	subroutine out_ground( kbuf, name, field, ng, alt_, elev
     +	    , earthr, bmwidth_, rmin, rmax, scr, m, badval)
c   try to remove the ground contamination
    	character*(*) name
    	parameter(rcprad=0.017453292)
    	real field(*)
    	logical changed

	data fudge / 1. /

    	if((elev) .gt. -0.1) then
    	    return
    	endif
c	call e12('alt', 3, alt)
	elev_limit = -0.00001
        alt = alt_
        bmwidth = bmwidth_
c
	if(namefx('GEO_ALT', i) .ne. 0) then
	    alt = ugealt(kbuf)
	endif
c	call e12('alt', 3, alt)

        b = namefp('BEAM_WIDTH', i)
	if( b .gt. 0) then
           bmwidth = b
	endif
c
	relev = elev * rcprad
	bmwr = rcprad * bmwidth
	d = rmax * rbmwidth
	d = rmax * bmwr
        d = d - alt

	if(d .gt. 0) then
	    elev_limit = atan2(d, rmax)
	endif
c
	if(elev .gt. elev_limit) then
	    return
	endif

	if(d .ge. 0 .and. relev .gt. -fudge * bmwr) then
	    ig1 = 1
c	    call e12('galt', 4, alt)
c	    call e12('ele', 3, elev)
c	    call i8('iig1', 4, ig1)
	else
	    tan_elev = tan(relev)
c   calculate the intersection of the ray and the ground
	    gndxn = (-(alt)/sin(relev))*
     +		(1.+alt/(2.*earthr*tan_elev*tan_elev))
	    if(gndxn .ge. rmax) then
		return
	    endif
c	call e12('bmw', 3, bmwidth)
c	call e12('ele', 3, elev)
c	call e12('xn', 2, gndxn)
c
c   now calculate the footprint due to beam spreading
	    ftprnt = abs(gndxn * bmwr /tan_elev)
	    r1 = gndxn -(.5*ftprnt)
c
	    if(r1 .ge. rmax) then
		return
	    endif
c
c	call e12('r1', 2, r1)
c
	    if(r1 .lt. rmin) then
		r1 = rmin
	    endif
c
c   get the first gate number of the footprint
c
	    rx = r1 +1.
	    call givrng(kbuf, name, r1, rx, m, scr, n, r0, gs
     +		, changed, ig1)   	
	endif
c
c	call i8('ig1', 3, ig1)
c
c   fudge the starting gate slightly
c
    	ig1 = ig1 + namefx('GECHO_GATES', i)
	if(ig1 .lt. 1) then
	    ig1 = 1
	endif
c	call i8('ig1', 3, ig1)
c	call i8('ng', 2, ng)

    	do 22 i = ig1, ng
    	    field(i) = badval
  22	continue

    	return
    	end
c----------------------------------------------------------------------



