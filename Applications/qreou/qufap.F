c  file:ufap.for
c  $Revision: 1.1.1.1 $ $Date: 2002/08/07 16:03:06 $
c   this file contains routines (fortran functions, not
c   subroutines).
c
c   	givgts
c   	givrng
c       givfld
c       sclit
c       clrbt
c       givflg
c       sfldid
c       kfldss
c       kfld1
c       kfwdat
c       kfwman
c       kfwopt
c       kfwloc
c       kfwfld
c       kintgd
c       kintgr
c       kchar
c       kword
c       kdelf
c       kradar
c       ksite
c       kproj
c       kgenrn
c       kdate
c       ktzone
c       kgendt
c       kfldtn
c       ktapen
c       ktime
c       uazim
c       uelev
c       ufixed
c       uswprt
c       ubasel
c       ubasaz
c       uhight
c       ulatit
c       ulongt
c       krecln
c       kvoln
c       kprnv
c       klrect
c       kprnry
c       kswepn
c       kswepm
c       kraynv
c       kyear
c       kmonth
c       kday
c       khour
c       kminit
c       kminut
c       ksecnd
c       kgenyr
c       kgenmo
c       kgenda
c       khourv
c       kminv
c       ksecv
c       kgtflg
c       knfldr
c       knrecr
c       urzero
c       urone
c       udepth
c       ugtdel
c       uhbwid
c       uvbwid
c       ubandw
c       uwavel
c       uthval
c       uprtim
c       uazim2
c       uelev2
c       uvenyq
c       kveflg
c       kthfld
c       keditc
c       kpolar
c       knsamp
c       knvol
c       kscale
c       udmrc
c       udmpw
c       udmrgn
c       udmppw
c       udmagn
c       udmpdr
c       udmmds
c       kwordd
c       ktoint
c       givint
c   	scatyp
c       setcat
c       setpsn
c       setray
c       ufeof
c       setnxr
c       nxrset
c       present
c       pflist
c   	rufscr
c   	wufscr
c
c----------------------------------------------------------------------
c
c  this set of subroutines is to access common doppler exchange format
c  tapes, commonly known as universal format.
c
c  the code consists of functions to return various parameters such
c  as azimuth, elevation, etc. and subroutines for unpacking the
c  data (subroutine  givfld).
c
c  this code uses some machine dependent utility routines:
c
c       mtread  reads a tape record into the designated buffer area
c
c       mtwait  returns the status of the read
c
c       iget16  returns the "ith" 16 bit word from 
c               an array containing 16 bit data.  the word
c               should be returned as a full size sign extended
c               number in the current host machine
c
c       iget8   returns the "ith" 8 bit byte.  this should not
c               be sign extended.
c
c       upkfld  unpacks the two complement 16 bit gate values into an 
c               integer variable array with sign extension if necessary.
c
c       upkflg  unpack the low order bit of each gate value into the
c               low order bit of an integer variable array
c
c
c----------------------------------------------------------------------
        subroutine givgts(kbuf,name,ig1,m,vector,lenout,badval)
c       
c       return the values for the "name" field converted to scientific units
c       starting at gate "ig1" for "lenout" gates.
c       
        common / scr / iscr( 2048 )
        integer kbuf(*)
        dimension vector(*)
        character*2 name, id
c       
c       
c       determine the source id of the "name" field
        call sfldid( kbuf, name, id )
        if( id(1:2) .eq. '  ' ) then
            go to 90
        endif
c       
c       get location of the first word in the field header and the
c       length of the field header
        call kfwfld(kbuf,id,loc,len)
c       no. of gates
        ng=knvol(kbuf,id)
c       
        lenout = ng - (ig1-1)
        if( m .lt. 1 ) then
c       assume this is just a request for the number of possible gates
            go to 90
        endif
c       
c       no. gates to unpack
        lenout = min0(lenout,m)
c       where to begin unpacking data
        locval=kintgd(kbuf,loc,0)+ig1-1
c       
        scale=1./float(kintgd(kbuf,loc+1,1))
c       delete flag
        badval=kdelf(kbuf)*scale
c       unpack the field into vector
    	call upkfld( kbuf, vector, locval, lenout )
c       scale the data to scientific units
        call sclit( vector, vector, m, lenout, scale )
c       
  90    continue
        return
        end
c----------------------------------------------------------------------
        subroutine givrng
     1          (kbuf, name, r1, r2, m, vector, lenout
     +	    	, rzero, gs, changed, ig1 )
c       
c       the purpose of givrng is to return the number of the first gate
c       and the number of gates following to cover the ranges "r1" and "r2";
c       (this info can be used with "givgts")
c       to fill vector with the range values corresponding to these
c       particular gates;
c       and to return a flag indicating that the range information will
c       be different from the information passed in in vector
c       
c       It is done this way to accommodate variable gate spacing in other
c       access packages
c       
        integer kbuf(*)
        dimension vector(*)
        logical changed
        character*2 name, id
c       
c       
c       determine the source id of the "name" field
        call sfldid( kbuf, name, id )
        if( id(1:2) .eq. '  ' ) then
            go to 90
        endif
c       
c       gate spacing
        gs=ugtdel(kbuf,id)
c       no. of gates
        ng=knvol(kbuf,id)
c       
c       range to the first gate
        r0x=urzero(kbuf,id)
        ig1=max0(1,int((r1-r0x)/gs+2.))
        ig2=min0(ng,int((r2-r0x)/gs+1.))
c       range to the first gate returned
        rzero=r0x+(ig1-1)*gs
        lenout = ig2-ig1+1
        if( m .lt. 1 ) then
c       m is the space available in vector
c       assume this is just a request for range and gate info
            go to 90
        endif
c       
        lenout = min0(lenout,m)
    	changed = rzero .ne. vector(1) .or. rzero +(lenout-1)*gs 
     +          .ne. vector(lenout)
    	if( changed ) then
    	    zrng = rzero
    	    do 11 igt = 1, lenout
    	    	vector(igt) = zrng
    	    	zrng = zrng +gs
  11	    continue
    	endif
c       
c       
  90    continue
        return
        end
c----------------------------------------------------------------------
c----------------------------------------------------------------------
      subroutine givfld
     1  (kbuf,name,r1,r2,m,vector,lenout,rzero,gs,badval)
c----------------------------------------------------------------------
c                            
c     return the values for the "name" field converted to scientific units
c     between the ranges "r1" and "r2".
c
      common / scr / iscr( 2048 )
      integer kbuf(*)
      dimension vector(*)
        logical same
        character*2 name, id, kflg
c
c            
c
        lenout=0
c  determine the source id of the "name" field
        call sfldid( kbuf, name, id )
      if( id(1:2) .eq. '  ' ) then
        go to 90
      endif
c
c  get location of the first word in the field header and the
c  length of the field header
      call kfwfld(kbuf,id,loc,len)
c
c  gate spacing
      gs=ugtdel(kbuf,id)
c  no. of gates
      mx=knvol(kbuf,id)
c
c     exit if zero gate spacing for no data
      if(gs.eq.0..or.mx.eq.0.or.len.eq.0)then
        call typstg( 6, id, 1, 2 )
        call typstg( 6, 'data is garbage ^', 1, 0 )
        go to 90
      endif
c
      if( r1 .gt. r2 ) then
         call typstg( 6, 'r1 is greater than r2 ^', 1, 0 )
      endif
c
c  range to the first gate
      r0x=urzero(kbuf,id)
      ig1=max0(1,int((r1-r0x)/gs+2.))
      ig2=min0(mx,int((r2-r0x)/gs+1.))
c  range to the first gate returned
      rzero=r0x+(ig1-1)*gs
      lenx = ig2-ig1+1
      if( m .lt. 1 ) then
c   assume this is just a request for range and gate info
        lenout = lenx
        go to 90
      endif
c
c  no. gates to unpack
      lenout = min0(lenx,m)
c  where to begin unpacking data
      locval=kintgd(kbuf,loc,0)+ig1-1
c
      scale=1./float(kintgd(kbuf,loc+1,1))
c  delete flag
      badval=kdelf(kbuf)*scale
c
      call kveflg(kbuf,id,kflg)
c                                            
      if( name(1:2) .eq. 'FL' .and. id(1:2) .ne. 'FL' )then
c  unpack only the low order bit
            call upkflg( kbuf, vector, locval, lenout )
            call sclit( vector, vector, m, lenout, 1. )
      else
            if( kflg(1:2) .eq. 'FL' ) then
c  if this is a flagged field, need to clear low order bit
            	call upkfldf( kbuf, vector, locval, lenout )
    	    else
            	call upkfld( kbuf, vector, locval, lenout )
            endif
c
c  scale the data to scientific units
            call sclit( vector, vector, m, lenout, scale )
      endif
c
      if( name(1:2) .eq. 'DZ' .and. id(1:2) .eq. 'DM' ) then
c  convert dm to dz field.
         rcs=udmrc(kbuf,id)
         rx=rzero
         do 210 k=1,lenout
            if(vector(k).eq.badval)then
                continue
            else
                vector(k)=rcs+vector(k)+20.*alog10(rx)
            endif
            rx=rx+gs
  210    continue
c       
        elseif(( name(1:2) .eq. 'VE' .and. 
     +              id(1:2) .eq. 'VF' .and. 
     +              kflg(1:2) .eq. 'FL' )   .or.
     +         ( name(1:2) .eq. 'VF' .and. 
     +              kflg(1:2) .eq. 'FL' ))then
c   this field is to be thresholded on the flag in the low order bit
c   first unpack the low order bit
            call upkflg( kbuf, iscr, locval, lenout )
            do 220 igt = 1, lenout
                if( iscr( igt ) .eq. 0 ) then
                    vector( igt ) = badval
                endif
 220        continue
        endif
c
c
        do 33 igt = lenout+1, m
            vector(igt) = badval
  33    continue
c
c
 90   continue
      return
      end
c----------------------------------------------------------------------
      subroutine sclit ( ivec, vec, m, len, scl )
c----------------------------------------------------------------------
c  this routine hides the integer to floating conversion in the same array
c
      dimension ivec( m ), vec( m )
c
      do 200 i=1,len
        vec(i)=float( ivec( i ))*scl
  200 continue
      return
      end
c----------------------------------------------------------------------
      subroutine clrlbt ( ivec, m, nval )
c----------------------------------------------------------------------
c  this routine clears the low order bit
      dimension ivec( m )
c
      do 11 i = 1, nval
        ivec( i ) = ( ivec(i)/2 )*2
 11   continue
      return
      end
c----------------------------------------------------------------------
      subroutine givflg(kbuf,name,r1,r2,m,vector,lenout,rzero,gs,
     1     kbad)
c----------------------------------------------------------------------
c     return the flag field
      integer kbuf(*)
      integer vector(*)
      integer kfl
      logical same
        character name*(*)
        character*2 id, kflg
c
c  determine the source id of the "name" field
c
      call sfldid( kbuf, name, id )
      lenout=0
      if( id .eq. '  ' ) then
        go to 90
      endif
c
      call kfwfld(kbuf,id,loc,len)
      gsx=ugtdel(kbuf,id)
      r0x=urzero(kbuf,id)
      mx=knvol(kbuf,id)
      lenout=0

c     exit is zero gate spacing or no data
      if(gsx.eq.0..or.mx.eq.0.or.len.eq.0)return

c     first and last gates
      ig1=max0(1,ifix((r1-r0x)/gsx+2.))
      ig2=min0(mx,ifix((r2-r0x)/gsx+1.))
      if( m .lt. 1 ) then
        go to 90
      endif
c
c     location of data
      locval=kintgd(kbuf,loc,0)+ig1-1

c     number of data
      lenout=min0(ig2-ig1+1,m)
      call kveflg(kbuf,id,kflg)
c
      if( kflg .eq. 'FL' ) then
        call upkflg( kbuf, vector, locval, lenout )
      else
c   this field is not flagged
        do 22 i = 1, lenout
            vector(i) = 1
  22    continue
      endif
c
      gs=gsx
      rzero=r0x+(ig1-1)*gs
      kbad=0
c
  90  continue
      return
      end
c----------------------------------------------------------------------
      subroutine sfldid (kbuf,name,id)
c----------------------------------------------------------------------
c  return the field id that contains the information implied by "name"
c
        implicit integer( a-z )
        dimension kbuf(*)
        logical same, dprset
        character*2 kflg
        character*66 list
        character   id*(*), name*(*)
        data rec1sv, rec1 / -1111, -2222 /
c
c
c        call insl( '**sfldid** ', 10 )
c        call insl( name, 2 )
c        call insl( '*', 1 )
c
        loop = 0
c   blank the id
        id(1:2) = '  '
c  get a list of the fields present
c
  11    continue
        loop = loop+1
c   look in the current record
        call kfldss(kbuf,list,n)
c
        do 101 l = 1, 2*n, 2
          if( list(l:l+1) .eq. name(1:2)) then
c  field requested is present in the record
                id(1:2) = list(l:)
                go to 90
          endif
 101    continue
c
        if( name(1:2) .eq. '  ' ) then
c  return the first field
            id(1:2) = list(1:2)
            go to 90
c
        elseif( name(1:2) .eq. 'DZ' ) then
c  look for the dbz to dbm possibility
          do 111 l = 1, 2*n, 2
                if( list(l:l+1) .eq. 'DM' ) then
                    id(1:2) = list(l:)
                    go to 90
                endif
 111      continue
c
        elseif( name(1:2) .eq. 'FL' ) then
c  just trying to extract the flag
          do 121 l = 1, 2*n, 2
                call kveflg( kbuf, list(l:l+1), kflg )
                if( kflg .eq. 'FL' ) then
                    id(1:2) = list(l:)
                    go to 90
                endif
 121      continue
c
        elseif( name(1:2) .eq. 'VE' .or. name(1:2) .eq. 'VR' ) then
c  look for the possibility of flagged velocities 
          do 131 l = 1, 2*n, 2
                if( list(l:l+1) .eq. 'VF' ) then
                    id(1:2) = list(l:)
                    go to 90
                endif
 131      continue
c
        endif
c
c
c
c   see if there are more records
        call kfwdat( kbuf, loc, len )
        trec = iget16( kbuf, loc+1 )
c
c   no field has been found in this record
        if( dprset()) then
          crec = iget16( kbuf, 9 )
          write( lunout(), 212 ) name(1:2), crec, trec
 212      format( ' ** field ', a, ' does not exist in rec',
     a    i2, ' of', i2, ' **' /
     a    ' the following fields do exist! ' )
          do 231 l = 1, 2*n, 2
            write( lunout(), 213 ) list(l:l+1)
 213        format( ' field ', a )
 231      continue
        endif
c
        if( trec .gt. 1 ) then
c   a scratch file is used to contain the more than one record in the ray
            crec = iget16( kbuf, 9 )
            if( crec .eq. 1 ) then
                rec1 = iget16( kbuf, 6 )
            endif
            if( rec1 .ne. rec1sv ) then
c   first time this ray has been examined
                if( crec .eq. 1 ) then
c   rewind the scratch file
                    call rewusc( ierr )
                endif
c   write this record to the scratch file
                call wufscr( kbuf, ierr )
                if( crec .eq. trec ) then
c   all the records in the ray have been saved
                    rec1sv = rec1
                endif
            endif
c   get the next record
            if( crec .eq. trec ) then
                call rewusc( ierr )
            endif
            if( rec1 .eq. rec1sv ) then
c   read from scratch file
                call rufscr( kbuf, ierr )
            else
c   read the next record in the ray
                call setnxr( .false. )
                call setray( kbuf, 1, ierr )
                call setnxr( .true. )
            endif
            if( loop .lt. trec ) then
                go to 11
            endif
        endif
c
c
c
 90     continue
        return
        end
c----------------------------------------------------------------------
      subroutine kfldss (kbuf,list,n)
c----------------------------------------------------------------------
        implicit integer( a-z )
        character list*(*)
        character*2 mne
        dimension kbuf(*)
c
c  return a list of the fields present in this record
c
        call kfwdat(kbuf,loc,len)
        l = 1
c
        n=(len-3)/2
        loc=loc+3
        do 10 i=1,n
            call get16c( kbuf, loc, 2, mne)
            list(l:) = mne(1:2)
            loc=loc+2
            l = l+2
   10   continue
      return
      end
c-----------------------------------------------------------------------
      subroutine kfld1(kbuf,name)
c  return the name of the first field
        dimension   kbuf(*)
      character name*(*)
c
      call kfwdat( kbuf, loc, len )
      call get16c( kbuf, loc+3, 2, name )
      return
      end
c----------------------------------------------------------------------
      subroutine kfwdat (kbuf,loc,len)
      dimension kbuf(*)
c  return the location of first word of the data header
      loc=iget16(kbuf,5)
      k2=iget16(kbuf,loc+2)
      len=2*k2+3
      return
      end
c----------------------------------------------------------------------
      subroutine kfwman (kbuf,loc,len)
c  return length of the manual header
        dimension   kbuf(*)
      loc=1
      k1=iget16(kbuf,3)
      len=k1-1
      return
      end
c----------------------------------------------------------------------
      subroutine kfwopt (kbuf,loc,len)
c  pointer and length for the optional header
        dimension   kbuf(*)
      k1=iget16(kbuf,3)
      k2=iget16(kbuf,4)
      loc=k1
      len=k2-k1
      return
      end
c----------------------------------------------------------------------
      subroutine kfwloc (kbuf,loc,len)
c return the location of first word of the local use header
        dimension   kbuf(*)
      k1=iget16(kbuf,4)
      k2=iget16(kbuf,5)
      loc=k1
      len=k2-k1
      return
      end
c----------------------------------------------------------------------
        subroutine kfwfld( kbuf, name, loc, len )
c
c  return a pointer to the first word of the field header and the
c  length of the header in 16 bit words
c
        dimension kbuf(*)
        logical same
        character*2 id
        character   name*(*)
c
c
c  search the data header
      call kfwdat(kbuf,lfw,len)
      nfld=iget16( kbuf, lfw+2 )
c
      lfld = lfw+3
      do 10 i=1,nfld
        call get16c( kbuf, lfld, 2, id )
        if( id .eq. name(1:2)) then
            go to 30
        endif
        lfld = lfld+2
   10 continue
c
      lfld = lfw+3
c
      if( name(1:2) .eq. '  ' )then
c   a blank name indicates that the first field should be returned.
        goto 30
      endif
c
      loc=0
      len=0
      return
c
   30 continue
      loc=iget16(kbuf,lfld+1)
      len=iget16(kbuf,loc)-loc
      return
      end
c----------------------------------------------------------------------
      function kintgd (kbuf,n,kd)
c----------------------------------------------------------------------
c  this routine returns the "nth" 16 bit word from "kbuf" sign extended
c  unless it contains the delete flag and then the default value "kd"
c  is substituted
c
      dimension kbuf(*)
c
      kintgd = iget16(kbuf,n)
      if( kintgd .eq. iget16( kbuf, 45 )) then
        kintgd = kd
      endif
      return
      end
c----------------------------------------------------------------------
      function kintgr (kbuf,n)
c----------------------------------------------------------------------
c  return the nth 16 bit word (no default)
      dimension kbuf(*)
      kintgr = iget16( kbuf, n )
      return
      end
c----------------------------------------------------------------------
      function kword (kbuf,n)
c----------------------------------------------------------------------
      dimension kbuf(*)
      kword = iget16( kbuf, n )
      return
      end
c----------------------------------------------------------------------
      function kdelf (kbuf)
c----------------------------------------------------------------------
c  return the delete flag
      dimension kbuf(*)
      kdelf=iget16(kbuf,45)
      return
      end
c----------------------------------------------------------------------
      subroutine kradar ( kbuf, kf )
c----------------------------------------------------------------------
      dimension kbuf(*)
      character kf*(*)
c  radar name starts a word 11 
        if((iget16(kbuf,11).ne.iget16(kbuf,45)))then
            call get16c( kbuf, 11, 8, kf )
        else
            kf(1:8) = ' '
        endif
        return
        end
c----------------------------------------------------------------------
      subroutine ksite ( kbuf, kf )
      dimension kbuf(*)
      character kf*(*)
c  site name starts at word 15
    	iw1 = iget16(kbuf,15)
        if(iw1.ne.iget16(kbuf,45).or.iw1.gt.0)then
            call get16c( kbuf, 15, 8, kf )
        else
            kf(1:8) = ' '
        endif
        return
        end
c----------------------------------------------------------------------
      subroutine kproj ( kbuf, kf )
      dimension kbuf(*)
      character kf*(*)
c  project name is at the beginning of the optional header
      call kfwopt(kbuf,loc,len)
      if( len .ge. 4 .and. iget16( kbuf, loc ) .ne.
     a    iget16( kbuf, 45 )) then
            call get16c( kbuf, loc, 8, kf )
        else
            kf(1:8) = ' '
        endif
        return
        end
c----------------------------------------------------------------------
      subroutine kgenrn ( kbuf, kf )
      dimension kbuf(*)
      character kf*(*)
c  facility where this tape was written
      if((iget16(kbuf,41).ne.iget16(kbuf,45)))then
            call get16c( kbuf, 41, 8, kf )
        else
            kf(1:8) = ' '
        endif
        return
        end
c----------------------------------------------------------------------
      subroutine kdate ( kbuf, kf )
      dimension kbuf(*)
      character kf*(*)
c  date of the data returned as a character string
c
      k1=kyear(kbuf)
      k2=kmonth(kbuf)
      k3=kday(kbuf)
    	write(kf,17) k2, k3, k1
c      encode(8,17,kf)k2,k3,k1
   17 format(i2,'/',i2,'/',i2)
      call zrofil( kf, 1, 8 )
      return
        end
c----------------------------------------------------------------------
      subroutine ktzone( kbuf,kf )
      dimension kbuf(*)
      character kf*(*)
c  time zone(kbuf)
        call get16c( kbuf, 32, 2, kf )
      return
        end
c----------------------------------------------------------------------
      subroutine kgendt ( kbuf, kf )
      dimension kbuf(*)
      character kf*(*)
c  date this tape was written...date returned as a character string
c
      k1=kgenyr(kbuf)
      k2=kgenmo(kbuf)
      k3=kgenda(kbuf)
    	write(kf, 17 ) k2, k3, k1
c      encode(8,17,kf)k2,k3,k1
   17 format(i2,'/',i2,'/',i2)
      call zrofil( kf, 1, 8 )
      return
        end
c----------------------------------------------------------------------
      subroutine kfldtn(kbuf,kf)
      dimension kbuf(*)
      character kf*(*)
c     return the field tape name
      call kfwopt(kbuf,loc,len)
      if(len.ge.13.and.(iget16(kbuf,loc+9).ne.iget16(kbuf,45)))then
            call get16c( kbuf, loc+9, 8, kf )
c      else
            kf(1:8) = '        '
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine ktapen(kbuf,kf)
      dimension kbuf(*)
      character kf*(*)
c   return the field tape name
      kf(1:8) = ' '
      call getstr( 'TAPE_NAME', kf, 1, n )
      return
      end
c----------------------------------------------------------------------
      subroutine ktime (kbuf,kf)
      dimension kbuf(*)
      character kf*(*)
      character*2 k4
c  time
c     return 10 character values. the time zone is tacked onto the end
      k1=khour(kbuf)
      k2=kminit(kbuf)
      k3=ksecnd(kbuf)
      call ktzone(kbuf, k4 )
    	write(kf,7)k1,k2,k3,k4
c      encode(10,7,kf)k1,k2,k3,k4
    7 format(i2,':',i2,':',i2,a2)
      call zrofil( kf, 1, 8 )
      return
      end
c----------------------------------------------------------------------
      function uazim (kbuf)
      dimension kbuf(*)
c azimuth
      uazim=float(kintgd(kbuf,33,0))/64.
      return
        end
c----------------------------------------------------------------------
      function uelev (kbuf)
      dimension kbuf(*)
c  elevation
      uelev=float(kintgd(kbuf,34,0))/64.
      return
        end
c----------------------------------------------------------------------
      function ufixed (kbuf)
      dimension kbuf(*)
c  fixed angle
      ufixed=float(kintgd(kbuf,36,0))/64. 
      return
        end
c----------------------------------------------------------------------
      function uswprt (kbuf)
      dimension kbuf(*)
c  sweep rate
      uswprt=float(kintgd(kbuf,37,0))/64.
      return
        end
c----------------------------------------------------------------------
      function ubasel (kbuf)
      dimension kbuf(*)
c  coplane baseline elevation resides in the optional header
c
      call kfwopt(kbuf,loc,len)
      if(len.eq.0)then
      ubasel=0.
      else
      ubasel=float(kintgd(kbuf,loc+5,0))/64.
      endif
      return
        end
c----------------------------------------------------------------------
      function ubasaz (kbuf) 
      dimension kbuf(*)
c  coplane baseline azimuth
c
      call kfwopt(kbuf,loc,len)
      if(len.eq.0)then
      ubasaz=0.
      else
      ubasaz=float(kintgd(kbuf,loc+4,0))/64.
      endif
      return
        end
c----------------------------------------------------------------------
      function ugealt (kbuf)
      dimension kbuf(*)
c  elevation of the radar above msl
c
c  put in the missing data flag
      ugealt=float(kintgd(kbuf,45,0))
      return
        end
c----------------------------------------------------------------------
      function uhight (kbuf)
      dimension kbuf(*)
c  elevation of the radar above msl
c
      uhight=float(kintgd(kbuf,25,0))/1000.
      return
        end
c----------------------------------------------------------------------
      function ulatit ( kbuf )
      dimension kbuf(*)
c  latitude of the radar
c
      ulatit=float(kintgd(kbuf,19,0))+float(kintgd(kbuf,20,0))/60.
     1     +float(kintgd(kbuf,21,0))/(60.*60.*64.)
      return
        end
c----------------------------------------------------------------------
      function ulongt (kbuf)
      dimension kbuf(*)
c  longitude of the radar
c
      ulongt=float(kintgd(kbuf,22,0))+float(kintgd(kbuf,23,0))/60.
     1     +float(kintgd(kbuf,24,0))/(60.*60.*64.)
      return
      end
c----------------------------------------------------------------------
      function klrect(kbuf)
      dimension kbuf(*)
c  record no. relative to the start of the volume
      klrect=kintgd(kbuf,6,0)
      return
        end
c----------------------------------------------------------------------
      function krecln (kbuf)
      dimension kbuf(*)
c  length of the record in 16 bit words
      krecln=kintgd(kbuf,2,0)
      return
        end
c----------------------------------------------------------------------
      function kvoln (kbuf)
      dimension kbuf(*)
c  volume or file no.
      kvoln=kintgd(kbuf,7,0)
      return
        end
c----------------------------------------------------------------------
      function kprnv (kbuf)
      dimension kbuf(*)
c  record no. relative to the start of the volume
      kprnv=kintgd(kbuf,6,0)
      return
        end
c----------------------------------------------------------------------
      function kprnry (kbuf)
      dimension kbuf(*)
c  record no. relative to the start of the ray.  this is usually 1 but
c  permits an extremely large ray to be broken up into several records.
      kprnry=kintgd(kbuf,9,0)
      return
        end
c----------------------------------------------------------------------
      function kswepn (kbuf)
      dimension kbuf(*)
c  sweep no.
      kswepn=kintgd(kbuf,10,0)
      return
        end
c----------------------------------------------------------------------
      function kswepm (kbuf)
      dimension kbuf(*)
c  sweep mode (1=ppi,2=cop,3=rhi,4=vth, etc.)
      kswepm=kintgd(kbuf,35,7)
      return
        end
c----------------------------------------------------------------------
      function kraynv (kbuf)
      dimension kbuf(*)
c  ray no. relative to the start of the volume
      kraynv=kintgd(kbuf,8,0)
      return
        end
c----------------------------------------------------------------------
      function kyear (kbuf)
      dimension kbuf(*)
      kyear=kintgd(kbuf,26,0)
    	if( kyear .gt. 1900 ) then
    	    kyear = kyear -1900
        elseif( kyear .lt. 70 ) then
           kyear = kyear +100
    	endif
      return
        end
c----------------------------------------------------------------------
      function kmonth (kbuf)
      dimension kbuf(*)
      kmonth=kintgd(kbuf,27,0)
      return
        end
c----------------------------------------------------------------------
      function kday (kbuf)
      dimension kbuf(*)
      kday=kintgd(kbuf,28,0)
      return
        end
c----------------------------------------------------------------------
      function khour (kbuf)
      dimension kbuf(*)
      khour=kintgd(kbuf,29,0)
      return
        end
c----------------------------------------------------------------------
      function kminit (kbuf)
      dimension kbuf(*)
      kminit=kintgd(kbuf,30,0)
      return
        end
c----------------------------------------------------------------------
      function kminut (kbuf)
      dimension kbuf(*)
      kminut=kintgd(kbuf,30,0)
      return
        end
c----------------------------------------------------------------------
      function ksecnd (kbuf)
      dimension kbuf(*)
      ksecnd=kintgd(kbuf,31,0)
      return
        end
c----------------------------------------------------------------------
      function kgenyr (kbuf)
      dimension kbuf(*)
c  the year this tape was written
      kgenyr=kintgd(kbuf,38,0)
      return
        end
c----------------------------------------------------------------------
      function kgenmo (kbuf)
      dimension kbuf(*)
c  the month this tape was written
      kgenmo=kintgd(kbuf,39,0)
      return
        end
c----------------------------------------------------------------------
      function kgenda (kbuf)
      dimension kbuf(*)
c  the day this tape was written
      kgenda=kintgd(kbuf,40,0)
      return
        end
c----------------------------------------------------------------------
      function khourv (kbuf)
      dimension kbuf(*)
c  volume start hour (in the options header)
      call kfwopt(kbuf,loc,len)
      if(len.lt.7)then
        khourv=0
      else
        khourv=kintgd(kbuf,loc+6,0)
      endif
      return
        end
c----------------------------------------------------------------------
      function kminv (kbuf)
      dimension kbuf(*)
c  volumne start minute
      call kfwopt(kbuf,loc,len)
      if(len.lt.8)then
        kminv=0
      else
        kminv=kintgd(kbuf,loc+7,0)
      endif
      return
        end
c----------------------------------------------------------------------
      function ksecv (kbuf)
      dimension kbuf(*)
c  volume start second
      call kfwopt(kbuf,loc,len)
      if(len.lt.9)then
        ksecv=0
      else
        ksecv=kintgd(kbuf,loc+8,0)
      endif
      return
        end
c----------------------------------------------------------------------
      function kgtflg (kbuf)
      dimension kbuf(*)
c  range data consistancy flag
      call kfwopt(kbuf,loc,len)
      if(len.lt.14)then
        kgtflg=0
      else
        kgtflg=kintgd(kbuf,loc+13,0)
      endif
      return
        end
c----------------------------------------------------------------------
      function knfldr (kbuf)
      dimension kbuf(*)
c     return the number of fields in the current ray
      call kfwdat(kbuf,loc,len)
      knfldr=kintgd(kbuf,loc,0)
      return
        end
c----------------------------------------------------------------------
      function knrecr (kbuf)
      dimension kbuf(*)
c     return the number of records in the current ray
      call kfwdat(kbuf,loc,len)
      knrecr=kintgd(kbuf,loc+1,0)
      return
      end
c----------------------------------------------------------------------
      function urzero (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  range to the first gate in the "name" field
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        urzero=0.
      else
        urzero = 
     +    float(kintgd(kbuf,loc+2,0))+float(kintgd(kbuf,loc+3,0))/1000.
      endif
      return
        end
c----------------------------------------------------------------------
      function urone(kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  range to the first gate in the "name" field
    	urone = urzero( kbuf, name )
      return
        end
c----------------------------------------------------------------------
      function udepth (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  depth of the pulse volume
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        udepth=0.
      else
        udepth=float(kintgd(kbuf,loc+6,0))/1000.
      endif
      return
        end
c----------------------------------------------------------------------
      function ugtdel (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  gate spacing (distance between the centers of the sample volumes)
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        ugtdel=0.
      elseif( len .ge. 30 ) then
        ugtdel=float(kintgd(kbuf,loc+4,0))*.001 +
     a    float(kintgd( kbuf, loc+29, 0 ))*.000001
      else
        ugtdel=float(kintgd(kbuf,loc+4,0))*.001 
      endif
      return
        end
c----------------------------------------------------------------------
      function uhbwid (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  horizontal beam width in degrees
      call kfwfld(kbuf,name,loc,len)
      if(loc.eq.0)then
        uhbwid=0.
      else
        uhbwid=float(kintgd(kbuf,loc+7,0))/64.
      endif
      return
        end
c----------------------------------------------------------------------
      function uvbwid (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  vertical beam width in degrees
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        uvbwid=0.
      else
        uvbwid=float(kintgd(kbuf,loc+8,0))/64.
      endif
      return
        end
c----------------------------------------------------------------------
      function ubandw (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  bandwidth in megahertz
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        ubandw=0.
      else
        ubandw=float(kintgd(kbuf,loc+9,0))
      endif
      return
        end
c----------------------------------------------------------------------
      function uwavel (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  wave length in cm.
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        uwavel=0.
      else
        uwavel=float(kintgd(kbuf,loc+11,0))/64.
      endif
      return
        end
c----------------------------------------------------------------------
      function uthval (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  value at which the "name" field was thresholded
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        uthval=-32767. 
      else
        uthval=
     +    float(kintgd(kbuf,loc+14,-32767))/float(kintgd(kbuf,loc+1,1))
      endif
      return
        end
c----------------------------------------------------------------------
      function udmprf (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c
c  pulse repetition frequency
      call kfwfld(kbuf,name,loc,len)
c
      if(len.eq.0)then
        zprt=0.
      else
        zprt=float(kintgd(kbuf,loc+17,0))/1000000.
      endif
c
    	if( zprt .gt. 1.1e-11 ) then
    	    udmprf = 1./zprt
    	else
    	    udmprf = 0
    	endif
      	return
        end
c----------------------------------------------------------------------
      function uprtim (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  pulse repetition time returned as seconds
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        uprtim=0.
      else
        uprtim=float(kintgd(kbuf,loc+17,0))/1000000.
      endif
      return
        end
c----------------------------------------------------------------------
      function uazim2 (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  azimuth adjusted for this field
      call kfwfld(kbuf,name,loc,len)
      if(len.lt.28)then
        uazim2=uazim( kbuf )
      else
        uazim2=uazim( kbuf )+float(kintgd(kbuf,loc+27,0))*.001
      endif
      return
        end
c----------------------------------------------------------------------
      function uelev2 (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  elevation adjusted for this field
      call kfwfld(kbuf,name,loc,len)
      if(len.lt.29)then
        uelev2=uelev( kbuf )
      else
        uelev2=uelev( kbuf )+float(kintgd(kbuf,loc+28,0))*.001
      endif
      return
      end
c----------------------------------------------------------------------
      function uvenyq (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  nyquist velocity
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        uvenyq=0.
      else
        uvenyq=float(kintgd(kbuf,loc+19,0))/float(kintgd(kbuf,loc+1,1))
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine kveflg (kbuf,name,kf)
      dimension kbuf(*)
      character name*(*)
      character kf*(*)
c  returns the contents of the velocity flag word
c  returns a blank if the field does not exist or the word contains the
c  delete flag
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        kf(1:2) = '  '
      else
        call get16c( kbuf, (loc+20), 2, kf )
      endif
      return
        end
c----------------------------------------------------------------------
      subroutine kthfld (kbuf,name,kf)
      dimension kbuf(*)
      character name*(*)
      character kf*(*)
c  returns the name of the threshold field
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
            kf(1:2)='  '
      else
            call get16c( kbuf, loc+13, 2, kf )
      endif
      return
        end
c----------------------------------------------------------------------
      subroutine keditc (kbuf,name,kf)
      dimension kbuf(*)
      character name*(*)
      character kf*(*)
c  returns the edit code
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
            kf(1:2)='  '
      else
            call get16c( kbuf, loc+16, 2, kf )
      endif
      return
      end
c----------------------------------------------------------------------
      function kpolar (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  polarization
      call kfwfld(kbuf,name,loc,len)
      if(len.lt.11)then
        kpolar=2
      else
        kpolar=kintgd(kbuf,loc+10,2)
      endif
      return
        end
c----------------------------------------------------------------------
      function knsamp(kbuf,name)
      dimension kbuf(*)
      character name*(*)
c     return the number of samples used
      call kfwfld(kbuf,name,loc,len)
      knsamp=0
      if(len.lt.13)return
      knsamp=kintgd(kbuf,loc+12,0)
      return
        end
# ifdef notyet
# endif
c----------------------------------------------------------------------
      function knvol(kbuf,name)
      dimension kbuf(*)
      character name*(*)
c     return the number of volumes (gates) available in a given beam
      call kfwfld(kbuf,name,loc,len)
      knvol=0
      if(len.lt.6)return
      knvol=kintgd(kbuf,loc+5,0)
      return
        end
c----------------------------------------------------------------------
      function kscale(kbuf,name)
      dimension kbuf(*)
      character name*(*)
c     return the scale number used for a given field
      kscale=1
      call kfwfld(kbuf,name,loc,len)
      if(len.le.1)return
      kscale=kintgd(kbuf,loc+1,1)
      return
      end
c----------------------------------------------------------------------
      function udmrc (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  radar constant necessary to convert dbm to dbz( if it is a power field)
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        udmrc=0.
      else
        udmrc=float(kintgd(kbuf,loc+19,0))/float(kintgd(kbuf,loc+1,1))
      endif
      return
        end
c----------------------------------------------------------------------
      function udmnpw (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  noise power for this field (if it is a power field)
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        udmnpw=0.
      else
        udmnpw=float(kintgd(kbuf,loc+20,0))/float(kintgd(kbuf,loc+1,1))
      endif
      return
        end
c----------------------------------------------------------------------
      function udmrgn (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c receiver gain
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        udmrgn=0.
      else
        udmrgn=float(kintgd(kbuf,loc+21,0))/float(kintgd(kbuf,loc+1,1))
      endif
      return
        end
c----------------------------------------------------------------------
      function udmppw (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  peak power
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        udmppw=0.
      else
        udmppw=float(kintgd(kbuf,loc+22,0))/float(kintgd(kbuf,loc+1,1))
      endif
      return
        end
c----------------------------------------------------------------------
      function udmagn (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  antenna gain
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        udmagn=0.
      else
        udmagn=float(kintgd(kbuf,loc+23,0))/float(kintgd(kbuf,loc+1,1))
      endif
      return
        end
c----------------------------------------------------------------------
      function udmpdr (kbuf,name)
      dimension kbuf(*)
      character name*(*)
c  pulse duration returned as seconds
      call kfwfld(kbuf,name,loc,len)
      if(len.eq.0)then
        udmpdr=0.
      else
        udmpdr=float(kintgd(kbuf,loc+24,0))/(64.*1000000.)
      endif
      return
      end
c----------------------------------------------------------------------
      function udmmds (kbuf,name)
c  minimum detectable signal that must be range corrected
        dimension   kbuf(*)
      character name*(*)
      udmmds=udmnpw(kbuf,name)+udmrc(kbuf,name)
      return 
      end
c
c----------------------------------------------------------------------
      function kwordd (kbuf,n,kd)
      dimension kbuf(*)
      k1=kdelf(kbuf)
      kwordd=iget16(kbuf,n)
      if(k1.eq.kwordd)then
        kwordd = kd
      endif
      return
      end
c----------------------------------------------------------------------
      function ktoint (k)
      k1=k
      if(k1.gt.32767)k1=k1-65538
      ktoint=k1
      return
      end
c----------------------------------------------------------------------
      subroutine givint
     1  (kbuf,name,r1,r2,m,ivec,lenout,rzero,gs,ibad)
c----------------------------------------------------------------------
c     return the integer values for a field
      integer kbuf(*)
      dimension ivec(m)
      integer kfl,kfldid
      character name*(*)
c
      call kfwfld(kbuf,name,loc,len)
      gsx=ugtdel(kbuf,name)
      r0x=urzero(kbuf,name)
      mx=knvol(kbuf,name)
      lenout=0

c  exit if zero gate spacing or no data.
      if(gsx.eq.0..or.mx.eq.0.or.len.eq.0)return

c  first and last gates of data
      ig1=max0(1,int((r1-r0x)/gsx+2.))
      ig2=min0(mx,int((r2-r0x)/gsx+1.))

c  location of data values
      locval=kintgd(kbuf,loc,0)+ig1-1
c  number of values
      lenout=min0(ig2-ig1+1,m)
c  delete flag
      ibad=kdelf(kbuf)
c
      call upkfld( kbuf, vector, locval, lenout )
      rzero=r0x+(ig1-1)*gsx
      gs=gsx
      return
      end
c-----------------------------------------------------------------------
      subroutine scatyp(istr,kmode)
        character istr*(*)
c     return the code for the scan type of istr
c
      if(istr(1:3).eq.'CAL')then
         kmode=0
      elseif(istr(1:3).eq.'PPI')then
         kmode=1
      elseif(istr(1:3).eq.'COP')then
         kmode=2
      elseif(istr(1:3).eq.'RHI')then
         kmode=3
      elseif(istr(1:3).eq.'VER')then
         kmode=4
      elseif(istr(1:3).eq.'TAR')then
         kmode=5
      elseif(istr(1:3).eq.'MAN')then
         kmode=6
      elseif(istr(1:3).eq.'IDL')then
         kmode=7
      elseif(istr(1:3).eq.'SUR')then
         kmode=8
      else
         write(6,7)istr(1:3)
    7    format(' unknown scan type = ',a)
         kmode=7
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine setcat
     1  ( buffer, lenbf, lundx, lucat, ludat, uics, lens, ierr )
c
c  this routine scans an input control string for attributes
c  that specify which tapes to mount and where to start and stop
c  reading data.
c  if all necessary information is found, then on return from this
c  routine the first useable record should be in the record buffer.
c
        implicit        integer( a-z )
        dimension       buffer(*)
        character       uics*(*)
c
        common  / ufapu /       ncs
     a,         taptim( 6 ),    ccstm1( 6 ),    ccstm2( 6 )   
     a,         lenbuf,         lundat,         nfiles
     a,         intpt,          intno,          ivol,           iscan
     a,         irec,           krec,           ueof
        common / ufapc /        ufcs
        character*2222 ufcs
        integer taptim, ccstm1, ccstm2
c
c
    	character*77	str
        logical         first
        data            first   / .true. /
c
c
    	ierr = 0
c   print out the access version
        call accver()
        nfiles = 0
        ivol = 0
        iscan = 0
        lenbuf = lenbf
    	ix = namefx( 'REC_LEN', i )
    	call putfx( i, nwds16(lenbuf))
        lundat = ludat
c
        if( lens .lt. 1 ) then
          call typstg( 6, 'no ufap input control string  ^', 1, -1 )
          ierr = 999
          go to 90
        endif
c
c  copy and compress the input control string
        ncs = 1
        ufcs(1:1) = ';'
        do 11 i = 1, lens
            if( uics(i:i) .ne. ' ' ) then
                ncs = ncs+1
                ufcs(ncs:ncs) = uics(i:i)
            endif
 11     continue
        ncs = ncs+1
        ufcs(ncs:ncs) = ';'
c
c
        ia = indexa( ufcs, 1, ncs, 4, 'SHOW' )
        if( ia .gt. 0 ) then
            iarg = 1
  22        continue
            call attarg( ufcs, ia, ncs, iarg, ia1, na )
            if( na .gt. 0 ) then
                call copyc( ufcs, ia1, na, str, 1 )
                ix = namefx( str(1:na), i )
                call putfx( i, 1 )
                iarg = iarg+1
                go to 22
            endif
        endif
c
c  search the string for an input tape list
        intpt = indexa( ufcs, 1, ncs, 6, 'INPUT:' )
        if( intpt .le. 0 ) then
          call typstg
     a          (6,'no list of input tapes   ^', 1, -1)
          ierr = 999
          go to 90
        endif
c
c  see if skip files
        ifile = indexa( ufcs, 1, ncs, 4, 'NFILES  ' )
        if( ifile .gt. 0 ) then
          call attarg( ufcs, ifile, ncs, 1, ipfc, nc )
          call dcode( ufcs, ipfc, nc, itype, nfiles, zreal )
        endif
c
c  extract the start and stop times
        call cdatmz( ufcs, ncs, ccstm1, ccstm2, ierr )
        if( ierr .ne. 0 ) then
          go to 90
        endif
c
c  first input tape
        intno = 1
c  mount the first tape
        call mountu( ierr )
        if( ierr .ne. 0 ) then
          go to 90
        endif
c
c  position to the first record
        call setpsn( buffer, 1, ierr )
        if( ierr  .ne. 0 ) then
          go to 90
        endif
c
c
 90     continue
        return
        end
c----------------------------------------------------------------------
      subroutine setpsn( kbuf, kdir, ierr )
c
c  this routine positions to the first record of a scan that is
c  greater than or equal to the start time.
c
        dimension   kbuf(*)
c
        common  / ufapu /       ncs
     a,         taptim( 6 ),    ccstm1( 6 ),    ccstm2( 6 )   
     a,         lenbuf,         lundat,         nfiles
     a,         intpt,          intno,          ivol,           iscan
     a,         irec,           krec,           ueof
        common / ufapc /        ufcs
        character*2222 ufcs
        integer taptim, ccstm1, ccstm2
    	logical ueof
c
        character*8 tmp, insn
        logical     inst
c
c
c
        inst = .false.
        in = indexa( ufcs, 1, ncs, 4, 'INSTRUMENT' )
        if( in .gt. 0 ) then
            call attarg( ufcs, in, ncs, 1, i1, ni )
            inst = ni .gt. 0
            if( inst ) then
                if( ni .gt. 8 ) then
                    ni = 8
                endif
                call copyc( ufcs, i1, ni, insn, 1 )
            endif
        endif
    	ierr = 0
        jscan = -1
    	jvol = -1
c
 11     continue
        call setray( kbuf, 1, ierr )
        if( ierr .ne. 0 ) then
          go to 90
        endif
c
    	if( ueof ) then
            jscan = -1
    	    jvol = -1
    	endif
c
        if( iscan .eq. jscan .and. ivol .eq. jvol ) then
          go to 11
        endif
c
        jscan = iscan
    	jvol = ivol
        if( inst ) then
            call kradar( kbuf, tmp )
            if( indexc( tmp, 1, 8, ni, insn ) .le. 0 ) then
                go to 11
            endif
        endif
c
        if( icmptm( taptim, ccstm1 ) .lt. 0 ) then
c  still to early
          go to 11
        endif
c
        if( nwds16( lenbuf ) .lt. krecln( kbuf )) then
          call sslu( 6 )
          call typstg( 6, '*** warning ***       ^', 1, -1 )
          call typstg( 6,
     a'tape record is longer than the input buffer  ^', 1, -1 )
          call typstg( 6, ' ', 1, 1 )
        endif
c
 90     continue
    	call dmplnb
        return
        end
c----------------------------------------------------------------------
      subroutine setray (kbuf, kdir, ierr)
c
c     move to the next ray
c     ignore kdir --- always go forward
c
        implicit        integer( a-z )
        dimension       kbuf(*)
c
        common  / ufapu /       ncs
     a,         taptim( 6 ),    ccstm1( 6 ),    ccstm2( 6 )   
     a,         lenbuf,         lundat,         nfiles
     a,         intpt,          intno,          ivol,           iscan
     a,         irec,           krec,           ueof
        common / ufapc /        ufcs
        character*2222 ufcs
        integer taptim, ccstm1, ccstm2
        logical ueof
c 
        logical nxrset
        dimension       dt( 6 )
c
c
      ierr = 0
      kount=0
      ueof = .false.
      call seteov( .false. )
c 
   10 continue
      krec = krec+1
      call mtread(lundat,kbuf,nwds16(lenbuf))
      call mtwait(lundat,state,nwds)
c
      if(state.eq.2)then
c  an error or an end of file
         kount=kount+1
         if(kount.gt.10)then
c  too many read errors
            write(6,7)
 7          format(' more than 10 input tape read errors.')
            stop
         endif
         goto 10 
c
      elseif(state.eq.3.or.(ueof.and.state.eq.1))then
c  see if there is another tape
        call mountu( ierr )
        if( ierr .eq. 0 ) then
          kount = 0
          go to 10
        endif
        go to 90
c
      elseif(state.eq.1)then
        ueof = .true.
        call seteov( ueof )
        kount = kount + 1
        go to 10
c 
      elseif(state.eq.0)then
c        good read
         ierr=0
      else
         stop 111
      endif
c
        if( nxrset() .and. iget16( kbuf, 9 ) .ne. 1 ) then
c   looking for the first record in the ray
            go to 10
        endif
c
c  see if at the beginning of a scan and tape time is greater than
c  or equal to the stop time
        taptim( 1 ) = kyear( kbuf )
        taptim( 2 ) = kmonth( kbuf )
        taptim( 3 ) = kday( kbuf )
        taptim( 4 ) = khour( kbuf )
        taptim( 5 ) = kminit( kbuf )
        taptim( 6 ) = ksecnd( kbuf )
c
        jscan = kswepn( kbuf )
        if( iscan .ne. jscan .and. icmptm( taptim, 
     a          ccstm2 ) .ge. 0 ) then
            ierr = 999
        endif
        iscan = jscan
        ivol = kvoln( kbuf )
        irec = kprnv( kbuf )
c
 90   continue
      return
      end
c----------------------------------------------------------------------
      logical function ufeof(kbuf)
c----------------------------------------------------------------------
c  the purpose of this routine is to return ufeof as .true. if during
c  the last read (call setray) an end of file was encountered
c
        dimension   kbuf(*)
c
        common  / ufapu /       ncs
     a,         taptim( 6 ),    ccstm1( 6 ),    ccstm2( 6 )   
     a,         lenbuf,         lundat,         nfiles
     a,         intpt,          intno,          ivol,           iscan
     a,         irec,           krec,           ueof
        common / ufapc /        ufcs
        character*2222 ufcs
        logical ueof
        integer taptim, ccstm1, ccstm2
c
        ufeof = ueof
        return
        end
c-----------------------------------------------------------------------
        subroutine setnxr( flag )
c   this routine set a flag that indicates whether to look for the first
c   record of a multi-record ray
c
        common / curayf / rayflg, dprint
        logical rayflg, dprint, flag
        data rayflg / .true. /
        data dprint / .false. /
c       
        rayflg = flag
        return
        end
c-----------------------------------------------------------------------
        logical function nxrset()
c   this routine gets the flag that indicates whether to look for the first
c   record of a multi-record ray
c
        common / curayf / rayflg, dprint
        logical rayflg, dprint
c
        nxrset = rayflg 
        return
        end
c-----------------------------------------------------------------------
        subroutine setdpr( flag )
c   this routine set a flag that indicates diagnostic print out 
c
        common / curayf / rayflg, dprint
        logical rayflg, dprint, flag
c       
        dprint = flag
        return
        end
c----------------------------------------------------------------------
        logical function present( kbuf, mne )
c   this routine determines if the source field for this mnemonic is present
c   in the input data.
c
        implicit integer( a-z )
        dimension   kbuf(*)
        character   mne*(*)
        character*2 id
c
c
        call sfldid( kbuf, mne, id )
        if( id(1:2) .eq. '  ' ) then
            present = .false.
        else
            present = .true.
        endif
c
        return
        end
c----------------------------------------------------------------------
        logical function prznt( kbuf, mne )
c   this routine determines if the source field for this mnemonic is present
c   in the input data.
c
        implicit integer( a-z )
        dimension   kbuf(*)
        character   mne*(*)
        character*2 id
c
c
        call sfldid( kbuf, mne, id )
        if( id(1:2) .eq. '  ' ) then
            prznt = .false.
        else
            prznt = .true.
        endif
c
        return
        end
c----------------------------------------------------------------------
c----------------------------------------------------------------------
      subroutine pflist( kbuf )
c----------------------------------------------------------------------
c   this routine prints a list of the available fields
c
        implicit integer( a-z )
        dimension kbuf(*)
        character*66 list
        logical same
c
        call kfldss( kbuf, list, n )
        call dmplnb
        call inslc(
     +'The following fields are present in this record' )
        do 11 i = 1, n*2, 2
            call inslc( list(i:i+1) )
  11    continue
        return
        end
c----------------------------------------------------------------------
        subroutine rufscr( kbuf, ierr )
c   this routine reads a ufap scratch file record
c
        implicit integer( a-z )
        logical first
    	save lun
        data first / .true. /
c
        if( first ) then
            first = .false.
            lun = lunque( 'UFSC' )
    	    len = namefx( 'REC_LEN', i )
        endif
c
c
        call mtread( lun, kbuf, len )
        call mtwait( lun, state, lenth )
        return
        end
c----------------------------------------------------------------------
        subroutine wufscr( kbuf, ierr )
c   this routine writes a ufap scratch file record
c
        implicit integer( a-z )
        logical first
        data first / .true. /
c
        if( first ) then
            first = .false.
            lun = lunque( 'UFSC' )
    	    len = namefx( 'REC_LEN', i )
        endif
c
c
        call mtwrit( lun, kbuf, len )
        call mtwait( lun, state, lenth )
        return
        end
c----------------------------------------------------------------------
