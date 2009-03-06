c---------------------------------------------------------------------
c   $Revision: 1.1.1.1 $ $Date: 2002/08/07 16:03:06 $
c   this file contains the following FOF library routines
c   the indented files do not contain character manipulation
c
c   	ascdmp
c       anumbr
c       attarg
c           azdiff
c           aznsyd
c       bfill
c       cdatmz
c       cfill
c           clearw
c       copyb
c       copyc
c       copycs
c       copycn
c           copyfp
c       copync
c       ctype
c       dcode
c       delstr
c       dmplin
c       dmplnb
c           dprset
c       eform
c           eovset
c       e7
c       e9
c       e12
c       fform
c           flgset
c       f5
c       f7
c       f9
c   	fpak16
c   	fpak16f
c   	fzap
c           getfp
c           igetfx
c       getstr
c           gettim
c       get16c
c   	gnxatt
c   	hdset
c       hexdmp
c    	hgnxatt
c           icmptm
c           idlun
c       inca
c       incb
c       idateo
c       idset
c       indexa
c       indexc
c       insl
c       inslc
c       inslx
c   	istrlen
c       itimeo
c       i8
c       kw16c
c   	kzap
c       lblnk
c   	lc
c       letter
c       lf
c           linfit
c       ltblnk
c       lunque
c           lunout
c       namefp
c       namefx
c       nameti
c       ncode
c       ndatt
c       nxtatt
c       nullc
c           page
c       praset
c       prtime
c       popstr
c       prtstr
c       pshstr
c       putc
c           putfp
c           putfx
c       putstr
c           puttim
c       put16c
c       rargs
c   	readln
c       remcom
c       resstr
c       same
c       scntyp
c           setdpr
c           seteov
c           setflg
c       slipstr
c           ss
c           sslu
c       sumtim
c       typefe
c       typefp
c       typefx
c       type16
c       typlin
c       typstg
c       uc
c   	upkfld
c   	upkfldf
c   	upkflg
c       ydate
c       ytime
c       zeroc
c       zipstr
c       znz
c       zncode
c       zrofil
c
c-----------------------------------------------------------------------
    	subroutine ascdmp( kbuf, w1, w2 )
c       dump info as ascii characters w1 and w1 are the 16-bit word limits
c       print 64 bytes or 32 words per line
    	implicit integer( a-z )
    	character*88 line
    	character*1 char
c       
c       
    	do 33 i = w1, w2, 32
            j = i+31
            if( j .gt. w2 ) then
                j = w2
            endif
            n = 0
            do 22 k = i*2-1, j*2
    	    	n = n+1
                c = iget8( kbuf, k )
                if( c .ge. 32 .and. c .le. 122 ) then
                    line(n:) = char( c )
                else
                    line(n:) = '.'
                endif
  22        continue
            write(6,789) i, line(1:n)
  33	continue
c       
    	return
  789   format( ' ', i5, ') ', a )
    	end
c--------------------------------------------------------------------
        logical function anumbr( iarg, s, ith, jth, kint, zreal )
c   this routine tries to decode the "iarg" argument as a number
c   but does not change "kint" or "zreal" if it cannot be decoded.
c
        character s*(*)
c
        itype = 0
        call attarg( s, ith, jth, iarg, ia, na )
        if( na .gt. 0 ) then
            call dcode( s, ia, na, itype, kintx, zrealx )
            if( itype .gt. 0 ) then
                kint = kintx
                zreal = zrealx
            endif
        endif
        anumbr = itype .gt. 0
        return
        end
c---------------------------------------------------------------------
      subroutine attarg( ibuf, ith, jth, nth, c1, nchar )
c   this routine returns a pointer to the first character of the nth
c   argument and the length of the argument in nchar.
c   c1 = 0 if the nth argument cannot be located
c
      implicit integer( a-z )
        character ibuf*(*)
c
c
      c1 = 0
      nchar = 0
      isemi = index( ibuf(ith:jth), ';' )
      if( isemi .eq. 0 ) then
    	    isemi = jth+1
      else
c   remember isemi is relative to ith as is icolon and icomma
            isemi = ith+isemi-1
      endif
c     find the colon
      icolon = index( ibuf(ith:isemi), ':' )
      if( icolon .eq. 0 ) then
    	kpt = ith
      else
      	kpt = ith + icolon
      endif
c
      do 22 iarg = 1, nth
      icomma = index( ibuf(kpt:isemi), ',' )
      if( icomma .eq. 0 .and. iarg .eq. nth ) then
c     argument found as the last argument
         c1 = kpt
         nchar = isemi -kpt
         go to 33
      elseif( icomma .eq. 0 ) then
         go to 90
      elseif( iarg .eq. nth ) then
         c1 = kpt
         nchar = icomma -1
         go to 33
      else
         continue
      endif
      kpt = kpt + icomma 
   22 continue
      go to 90
c
  33    continue
c   remove leading and trailing blanks
        j = c1
        k = j+nchar-1
        do 35 i = j, k
            if( ibuf(i:i) .eq. ' ' ) then
                c1 = c1+1
                nchar = nchar-1
            else
                go to 36
            endif
  35    continue
  36    continue
        k = c1+nchar-1
        do 37 i = k, c1+1, -1
            if( ibuf(i:i) .eq. ' ' ) then
                nchar = nchar-1
            else
                go to 38
            endif
  37    continue
  38    continue
c
   90 continue
      return
      end
c----------------------------------------------------------------------
      function azdiff( az1, az2 )
c     this function computes the azimuth difference in degrees
c
      azdiff= az2 -az1
      if( azdiff .gt. 180. ) azdiff = azdiff -360.
      if( azdiff .lt. -180. ) azdiff = 360. +azdiff
c
      return
      end
c----------------------------------------------------------------------
      logical function aznsyd( az, azmin, azmax )
c     this function is true if the azimuth is within
c     the specified limits
c
      if( azmin .gt. azmax ) then
         aznsyd = az .ge. azmin .or. az .le. azmax
      else
         aznsyd = az .ge. azmin .and. az .le. azmax
      endif
      return
      end
c---------------------------------------------------------------------
      subroutine bfill( ibuf, ith, nch )
c   put blanks from the ith to the jth character inclusive
        character   ibuf*(*)
        jth = ith+nch-1
        do 22 i = ith, jth
            ibuf(i:i) = ' '
  22    continue
        return
        end
c---------------------------------------------------------------------
      subroutine cdatmz( ccs, ncs, ccstm1, ccstm2, ierr )
c  this routine extracts the start and stop times from the catalog
c  control string
c
      implicit integer( a-z )
        character ccs*(*)
        dimension ccstm1(*), ccstm2(*)
c
c
c  unpack the times from the catalog control string 
c
        do 5 i = 1, 6
          ccstm1(i) = 0
          ccstm2(i) = 999
 5      continue
c
      i = index( ccs(1:ncs), ';START' )
      call attarg( ccs, i+1, ncs, 1, a, n )
      if( i .le. 0 .or. n .lt. 1 ) then
c  bad start date
        write( lunout(), 988 )
     +      ' BAD START DATE IN CATALOG CONTROL STRING '
        write( lunout(), 988 ) ccs(1:ncs)
        ierr = 999
        return
      endif
      if( n .gt. 0 ) then
        call idateo( ccs, a, n, ccstm1, 1 )
      endif
c
      call attarg( ccs, i+1, ncs, 2, a, n )
c
      if( n .gt. 0 ) then
        call itimeo( ccs, a, n, ccstm1, 4 )
      endif
c
      i = index( ccs(1:ncs), ';STOP' )
      if( i .gt. 0 ) then
        call attarg( ccs, i+1, ncs, 1, a, n )
        if( n .gt. 0 ) then
          call idateo( ccs, a, n, ccstm2, 1 )
        endif
        call attarg( ccs, i+1, ncs, 2, a, n )
        if( n .gt. 0 ) then
          call itimeo( ccs, a, n, ccstm2, 4 )
        endif
      endif
c
      return
 988    format( ' ', a )
      end
c----------------------------------------------------------------------
      subroutine cfill( ibuf, ith, nchar, jbuf )
c   this routine fills ibuf from the ith character for nchar
c   characters with the first character in jbuf
c
        character   ibuf*(*), jbuf*(*)
c
        jth = ith +nchar -1
        do 22 ich = ith, jth
            ibuf(ich:ich) = jbuf(1:1)
  22    continue
        return
        end
c----------------------------------------------------------------------
        subroutine clearw( b, i, j )
c   this routine zeroes the array "b" between "i" and "j"
        integer b(*)
c
        do 11 k = i, j
            b(k) = 0
  11    continue
        return
        end
c---------------------------------------------------------------------
      subroutine copyb( s, ith, n, d, jth )
c  this routine copies n from the ith character of s to the
c  jth character of d only this copy starts with the last
c  characters in the string
        character   s*(*), d*(*), t*1
c
      l = jth+n
      do 22 k = ith+n-1, ith, -1
        l = l-1
        t(1:1) = s(k:k)
        d(l:l) = t(1:1)
  22  continue
      return
      end
c-----------------------------------------------------------------------
        subroutine copyc( a, ith, n, b, jth )
c   this routine copies n characters of string "a" beginning at the "ith"
c   character into string "b" beginning at the "jth" character.
c
        character a*(*), b*(*)
        character*1     t
c
        if( n .lt. 1 .or. ith .lt. 1 .or. jth .lt. 1 ) then
            go to 90
        endif
c
        j = jth
        do 11 i = ith, ith+n-1
            t(1:1) = a(i:i)
            b(j:j) = t(1:1)
            j = j+1
  11    continue
c
  90    continue
        return
        end
c-----------------------------------------------------------------------
        subroutine copycs( a, ith, n, b, jth )
c   this routine copies n characters of string "a" beginning at the "ith"
c   character into string "b" beginning at the "jth" character.
c
        character a*(*), b*(*)
c
        if( n .lt. 1 .or. ith .lt. 1 .or. jth .lt. 1 ) then
            go to 90
        endif
c
        b(jth:) = a(ith:ith+n-1)
c
  90    continue
        return
        end
c---------------------------------------------------------------------
        subroutine copync( kbuf, ith, n, chs, jth )
c   this routine extracts n characters beginning at the ith character
c   of a noncharacter buffer and converts them to character data.
c
        character chs*(*), char*1
c
        j = jth
        do 11 i = ith, ith+n-1
            chs(j:j) = char( iget8( kbuf, i ))
            j = j+1
  11    continue
c
  90    continue
        return
        end
c-------------------------------------------------------------------
        subroutine copyfp( a, i, n, b, j )
c   this routine copies floating point data from "a" to "b"
c
        real a(*), b(*)
c
        ii = i
c
        do 11 jj = j, j+n-1
            b(jj) = a(ii)
            ii = ii+1
  11    continue
c
        return
        end
c---------------------------------------------------------------------
        subroutine copycn( chs, ith, n, kbuf, jth )
c   this routine copies and converts character data to noncharacter data
c
        character chs*(*)
        integer kbuf(*)
c
        j = jth
        do 11 i = ith, ith+n-1
            call put8( kbuf, j, ichar( chs(i:i)))
            j = j+1
  11    continue
c
  90    continue
        return
        end
c---------------------------------------------------------------------
        function ctype( source, ith, nchar )
c  this routine classifies the string as one of the following
c
c       ctype = 0       a string that cannot be decoded as an
c                       integer or real number.
c
c       ctype = 1       a string that can be decoded as an integer
c
c       ctype = 2       a string that can be decoded as a real no.
c
c
        implicit        integer( a-z )
        character       source*(*), ch*1
        logical         dot, dash, plus, digit, lge, lle
c
c
        ctype = -1
        j = ith
        dot = .false.
        dash = .false.
        plus = .false.
c
        do 11 i = 1, nchar
        ch = source(j:j)
        j = j+1
c
        if( lge( ch, '0' ) .and. lle( ch, '9' )) then
c  digit
          if( ctype .eq. 1 .or. ctype .eq. 2 ) then
            continue
          elseif( ctype .eq. -1 ) then
            ctype = 1
          else
            continue
          endif
        elseif( ch .eq. ' ' ) then
          continue
        elseif( ch .eq. '.' .and..not. dot .and. ctype .ne. 0 ) then
          ctype = 2
          dot = .true.
        elseif( ch .eq. '-' .and..not. dash .and. ctype .lt. 0 ) then
          ctype = 1
          dash = .true.
        elseif( ch .eq. '+' .and..not. plus .and. ctype .lt. 0 ) then
          ctype = 1
          plus = .true.
        else
          ctype = 0
        endif
 11     continue
c
        if( ctype .lt. 0 ) then
          ctype = 0
        endif
c
        return
        end
c---------------------------------------------------------------------
        subroutine dcode( ss, ith, nchar, itype, fixd, zreal )
c  this routine decodes 'nchar' characters from the source string 'ss'
c  and returns the type as well as the result of the decode
c
c       itype = 0       string cannot be decoded as a number and
c                       fixd = zreal = 0
c
c       itype = 1       string was decoded as an integer and the
c                       result is in both 'fixd' and 'zreal'.
c
c       itype = 2       string was decoded as a real no. and the
c                       result is in 'zreal' and 'fixd' if possible
c
c
        implicit        integer( a-t )
        character       ch*1, ss*(*)
        logical         flip
c
c
        ncharx = nchar
        azero = ichar( '0' )
        fixd = 0
        zreal = 0
        if( ncharx .le. 0 ) then
          itype = 1
        else
          itype = ctype( ss, ith, ncharx )
        endif
c
        if( itype .eq. 0 ) then
          go to 90
        elseif( itype .eq. 2 ) then
          go to 33
        else
c  decode as an integer
        endif
c
        s = 1
        j = ith
c
        do 11 i = 1, ncharx
          ch = ss(j:j)
          j = j+1
          if( ch .eq. ' ' ) then
            continue
          elseif( ch .eq. '-' ) then
            s = -1
          elseif( ch .eq. '+' ) then
            continue
          else
            fixd = fixd*10 +ichar( ch ) -azero
          endif
 11     continue
        fixd = s*fixd
        zreal = fixd
        go to 90
c
c
 33     continue
c  real value
        flip = .false.
        zs = 1.
        zf = .1
        j = ith
c
        do 44 i = 1, ncharx
          ch = ss(j:j)
          j = j+1
          if( ch .eq. ' ' ) then
            continue
          elseif( ch .eq. '.' ) then
            flip = .true.
          elseif( ch .eq. '-' ) then
            zs = -1
          elseif( ch .eq. '+' ) then
            continue
          elseif( flip ) then
            zreal = zreal +( ichar( ch )-azero )*zf
            zf = .1*zf
          else
            zreal = zreal*10. +ichar( ch )-azero
          endif
 44     continue
c
        zreal = zs*zreal
        if( abs( zreal ) .lt. 2.e9 ) then
          fixd = zreal
        endif
c
 90     continue
        return
        end
c------------------------------------------------------------------------
        subroutine delstr( id, l )
c   this routine deletes an entry from ids and names
c
        implicit integer( a-z )
        parameter( maxstr=88 )
        character*1 t
        character   id*(*)
c
        character*3200  cs
        common / strnm0 / cs
        common / strnm1 / n, pi(maxstr+1),  pn(maxstr)
c

        ni = len( id )
        do 11 i = n, 1, -1
            a = pi(i)
            b = a+ni-1
            if( ni .ne. pn(i)-pi(i) ) then
                continue
            elseif( id(1:ni) .eq. cs(a:b)) then
c   ids match...return length
                l = pi(i+1) - pn(i)
                p = i
                go to 22
            endif
  11    continue
        l = 0
        go to 90
c
  22    continue
        if( p .eq. n ) then
            n = n-1
            go to 90
        endif
c        
        c = pi(p)
        d = pi(p+1)
        m = d - c
c   adjust the pointers
        do 33 i = p, n-1
            pn(i) = pn(i+1) - m
            pi(i+1) = pi(i+2) - m
  33    continue
c   move the remaining characters
        do 44 i = d, pi(n+1)-1
            t(1:1) = cs(i:i)
            cs(c:c) = t(1:1)
            c = c+1
  44    continue
        n = n-1
c
  90    continue
        return
        end
c---------------------------------------------------------------------
        subroutine dmplin( a, m )
c  this routine inserts an 'n' character string into the diagnostic line
c  buffer and forces a print
c
        character*80 b
        common / dlb / nc
        common / dlb0 / b
        character a*(*)
c
        common / diag / bypass
        logical bypass
        data nc / 0 /
c
c
        if( bypass ) then
            return
        endif
c
        if( m .lt. 1 ) then
            n = len( a )
        else
            n = m
        endif
c
        if( nc+n+1 .gt. 80 ) then
            call dmplnb
        elseif( n .lt. 1 ) then
            call dmplnb
            return
        endif
        b(nc+1:) = ' '//a(1:n)
        nc = nc+n+1
        call dmplnb
        return
        end
c---------------------------------------------------------------------
        subroutine dmplnb
c  this routine forces a print of the diagnostic line buffer
c
        character*80 b
        common / dlb / nc
        common / dlb0 / b
c
        common / diag / bypass
        logical bypass
c
        if( bypass ) then
            return
        endif
c
        if( nc .le. 0 ) then
            return
        endif
c
        write( lunout(), 123 ) b(1:nc)
 123    format( a )
        nc = 0
        return
        end
c---------------------------------------------------------------------
        subroutine eform( a, n, y, nh )
c  this routine encodes a number into the dlb along with identifier
c  string 'a'
c
        implicit integer( a-z )
        parameter( llim=80 )
        character*80 b
        common / dlb / nc
        common / dlb0 / b
        character a*(*)
        character e*13, l*32
        common / diag / bypass
        logical bypass
        real y, aby
c
        if( bypass ) then
            return
        endif
c
        if( n .lt. 1 ) then
            m = len( a )
        else
            m = n
        endif
c
        l(1:) = ' '//a(1:m)//'='
        c = m+3
c
        if( y .eq. 0 ) then
            l(c:c) = '0'
            go to 88
        endif
c  sign
        if( y .lt. 0 ) then
            l(c:c) = '-'
            c = c+1
        endif
c
    	write( e, 122 ) y
c        encode( 13, 122, e ) y
 122    format( e13.6 )
c
        l(c:) = '.'//e(4:nh-3)//'e'
        c = c+nh-4
c
        aby = abs( y )
        if( aby .le. 1.e-10 ) then
c  two negative digits in the exponent
            l(c:) = e(11:13)
            c = c+2
        elseif( aby .lt. 1. ) then
c  one negative digit in the exponent
            l(c:) = '-'//e(13:13)
            c = c+1
        elseif( aby .lt. 1.e10 ) then
c  one positive digit in the exponent
            l(c:c) = e(13:13)
        else
c  two positive digits in the exponent
            l(c:) = e(12:13)
            c = c+1
        endif
c
 88     continue
        if( nc+c .gt. llim ) then
            call dmplnb
        endif
c
        b(nc+1:) = l(1:c)
c
        nc = nc+c
        return
        end
c-----------------------------------------------------------------------
        logical function dprset()
c   this routine returns the diagnostic print out flag
c
        common / curayf / rayflg, dprint
        logical rayflg, dprint
c
        dprset = dprint
        return
        end
c----------------------------------------------------------------------
        logical function eovset()
c  this routine gets eof flag 
c
        common / ceov / eov
        logical eov
c
        eovset = eov
        return
        end
c---------------------------------------------------------------------
        subroutine e7( a, n, y )
c  this routine encodes an e7.1 number into the dlb along with identifier
c  string 'a'
c
        character a*(*)
        real y
c
        call eform( a, n, y, 7 )
c
        return
        end
c---------------------------------------------------------------------
        subroutine e9( a, n, y )
c  this routine encodes an e9.3 number into the dlb along with identifier
c  string 'a'
c
        character a*(*)
        real y
c
        call eform( a, n, y, 9 )
c
        return
        end
c---------------------------------------------------------------------
        subroutine e12( a, n, y )
c  this routine encodes an e12.6 number into the dlb along with identifier
c  string 'a'
c
        character a*(*)
        real y
c
        call eform( a, n, y, 12 )
c
        return
        end
c---------------------------------------------------------------------
        subroutine fform( a, n, y, nh, nf )
c  this routine encodes an f5.1 number into the dlb along with identifier
c  string 'a'
c
        parameter( llim=80 )
        character*80 b
        common / dlb / nc
        common / dlb0 / b
        character a*(*)
        common / diag / bypass
        logical bypass
        real y
c
        if( bypass ) then
            return
        endif
c
        if( n .lt. 1 ) then
            m = len( a )
        else
            m = n
        endif
c
        if( nc+m+nh+2 .gt. llim ) then
            call dmplnb
        endif
c
        b(nc+1:) = ' '//a(1:m)//'='
        call ncode( b, nc+m+3, nh, 2, ix, y, nf )
        call lblnk( b, nc+m+3, nh, nhc )
        nc = nc+m+nhc+2
        return
        end
c-----------------------------------------------------------------------
        logical function flgset( n )
c  this routine returns the logical value of flag "n".
c
        parameter( nfl=55 )
        common / flags / flag( nfl )
        logical flag
        data flag / nfl*.false. /
c
        if( n .le. nfl ) then
                flgset = flag( n )
        else
                flgset = .false.
        endif
        return
        end
c---------------------------------------------------------------------
        subroutine f5( a, n, y )
c  this routine encodes an f5.1 number into the dlb along with identifier
c  string 'a'
c
        character a*(*)
        real y
c
        call fform( a, n, y, 5, 1 )
c
        return
        end
c---------------------------------------------------------------------
        subroutine f7( a, n, y )
c  this routine encodes an f7.2 number into the dlb along with identifier
c  string 'a'
c
        character a*(*)
        real y
c
        call fform( a, n, y, 7, 2 )
c
        return
        end
c---------------------------------------------------------------------
        subroutine f9( a, n, y )
c  this routine encodes an f9.3 number into the dlb along with identifier
c  string 'a'
        character a*(*)
        real y
c
        call fform( a, n, y, 9, 3 )
c
        return
        end
c----------------------------------------------------------------------
        subroutine fpak16( field, ngts, mbuf, ipt, fscl, badval )
c  this routine packs the 'field' into 16 bit words
c
        common / scr / array( 2048 )
        dimension       field( * )
        integer         array
c
c
    	izap = kzap()
c
        do 22 ig = 1, ngts
            if( field( ig ) .eq. badval ) then
            	array( ig ) = izap
            else
            	array( ig ) = field( ig )*fscl
            endif
 22     continue
c
    	call pak16( mbuf, array, ipt-1, ngts )
c
        return
        end
c----------------------------------------------------------------------
        subroutine fpak16f( field, fieldf, ngts, mbuf, ipt, fscl )
c  this routine packs the 'field' into 16 bit words
c
        common / scr / array( 2048 )
    	common / scr2 / flag( 2048 )
        dimension       mbuf( 1 ), field( 1 ), fieldf( 1 )
        integer         array, flag
c
c
    	do 11 ig = 1, ngts
    	    array(ig) = int( field( ig )*fscl )
  11	continue
c   clear out flag bit
    	call andmsk( array, 1, ngts, 1 )
c
    	do 33 ig = 1, ngts
            if( fieldf( ig ) .gt. 0 ) then
              	flag( ig ) = 1
    	    else
              	flag( ig ) = 0
            endif
  33	continue
c
c   merge flag bit
    	call orit( array, flag, ngts )
    	call pak16( mbuf, array, ipt-1, ngts )
c
        return
        end
c---------------------------------------------------------------------
    	integer function fzap()
c   the definitive source for the floating version of the missing flag
c
    	fzap = -32768
    	return
    	end
c---------------------------------------------------------------------------
        real function getfp( n )
c   this function returns the nth floating point item from the list
        parameter( maxfp=55, znull=-32768. )
        common / fpcom / fp(maxfp)
c
        if( n .lt. 1 .or. n .gt. maxfp ) then
            getfp = znull
        else
            getfp = fp(n)
        endif
        return
        end
c---------------------------------------------------------------------------
        integer function igetfx( n )
c   this function returns the nth fixed point item from the list
        parameter( maxfx=128, inull=-32768 )
        integer fx
        common / fxcom / fx(maxfx)
c
        if( n .lt. 1 .or. n .gt. maxfx ) then
            igetfx = inull
        else
            igetfx = fx(n)
        endif
        return
        end
c------------------------------------------------------------------------
        subroutine getstr( id, name, ith, l )
c   this routine associates ids and names
c   with a max of 55 entries
c
        implicit integer( a-z )
        parameter( maxstr=88 )
        character   id*(*), name*(*)
c
        character*3200  cs
        common / strnm0 / cs
        common / strnm1 / n, pi(maxstr+1),  pn(maxstr)
c

        ni = len( id )
        do 11 i = n, 1, -1
            a = pi(i)
            b = a+ni-1
            if( ni .ne. pn(i)-pi(i) ) then
                continue
            elseif( id(1:ni) .eq. cs(a:b)) then
c   ids matched...return name and length
                l = pi(i+1) - pn(i)
                name(ith:) = cs(pn(i):pn(i)+l-1)
                go to 90
            endif
  11    continue
c
        l = 0
  90    continue
        return
        end
c---------------------------------------------------------------------------
        subroutine gettim( n, t )
c   this function returns the nth time from the list
        integer t(*)
        parameter( maxti=55, inull=-32768 )
        common / ticom / ti(6,maxti)
        integer ti
c
        if( n .lt. 1 .or. n .gt. maxti ) then
            do 11 i = 1, 6
                t(i) = inull
  11        continue
        else
            do 22 i = 1, 6
                t(i) = ti(i,n)
  22        continue
        endif
        return
        end
c---------------------------------------------------------------------
        subroutine get16c( kbuf, ith, n, chs )
c   this routine extracts n characters beginning at the ith 16 bit word 
c   of a buffer and converts them to character data.
c
        character chs*(*), char*1
        dimension   kbuf(*)
c
        j = ith*2-1
        do 11 i = 1, n
            chs(i:i) = char( iget8( kbuf, j ))
            j = j+1
  11    continue
c
  90    continue
        return
        end
c----------------------------------------------------------------------
    	subroutine gnxatt( lui, str, ith, jth, eof, flee )
c   this routine returns the next attribute from the input stream
    	implicit integer( a-z )
    	character str*(*)
    	character*132 card
    	character*1 cc, char
    	logical flee, quote, eof
        save card, ii, ncd
    	data quote / .false. /
    	data ii, ncd / 1, 0 /
c
    	jth = ith
  11	continue
c   if there are no characters left in the input buffer read in the
c   next card
        if( ii .gt. ncd ) then
            ii = 1
            call readln( lui, card, ncd, eof, flee )
            if( flee .or. eof ) then
                go to 90
            endif
    	    if( ncd .lt. 1 ) then
            	write( lunout(), 988 ) ' '
    	    else
            	write( lunout(), 988 ) card(1:ncd)
    	    endif
        endif
c
  22	continue
c   loop to next ; or end of card
    	if( ii .gt. ncd ) then
    	    go to 11
    	endif
    	cc(1:1) = card(ii:ii)
    	c = ichar(cc(1:1))
    	ii = ii+1
c
    	if( quote .and. cc(1:1) .eq. '"' ) then
    	    quote = .false.
    	elseif( quote ) then
    	    str(jth:jth) = cc(1:1)
    	    jth = jth+1
    	elseif( cc(1:1) .eq. '"' ) then
    	    quote = .true.
    	elseif( cc(1:1) .eq. ';' ) then
c   do not pass an argument consisting of only one ';'
    	    if( jth .gt. ith ) then
    	    	str(jth:jth) = cc(1:1)
    	    	jth = jth+1
    	    	go to 90
    	    endif
    	elseif( c .gt. 32 .and. c .lt. 127 ) then
c   ignore blanks and non-printable characters and change to upper case
    	    call uc( cc, 1 )
    	    str(jth:jth) = cc(1:1)
    	    jth = jth+1
    	endif
    	go to 22
c
  90	continue
    	str(jth:jth) = char(0)
    	return
c
 988    format( ' ', a )
    	end
c----------------------------------------------------------------------
        subroutine hdset( instr, nchar, flee )
c       the purpose of this routine is to compress all the input
c       into a compact string and list the input attributes
c       
    	implicit integer( a-z )
    	parameter( YES=1, NO=0 )
        logical flee, eof, same, ignore
        character instr*(*)
        data lsem / 0 /
c       
	lui = namefx( 'HDSET', i )
      	luo = lunout()
c       put a semicolon in the first character
        instr(1:1) = ';'
        nchar = 1
        ith = nchar+1
c       
  11 	continue
c       get the next attribute
      	call hgnxatt( lui, instr, ith, jth, eof, flee )
    	kth = ith
      	if( flee ) then
	    write(6,*) ' Error during hdset input'
	    go to 90
      	elseif( eof .or.
     +	    	indexc( instr, kth, jth-1, 4, '\\EOF' ) .gt. 0 .or.
     +          indexc( instr, kth, jth-1, 4, 'BEOF' ) .gt. 0
     +          ) then
    	    flee = .true.
    	    write(6,*) '****** HDSET--EOF ******'
	    go to 66
      	else
            nchar = jth-1
            ith = nchar+1
    	    if( indexc( instr, kth, jth-1, 4, 'QUIT' ) .gt. 0 ) then
    	    	go to 66
	    else
	    	go to 11
	    endif
        endif
c
  66    continue
        if( instr(nchar:nchar) .ne. ';' ) then
            nchar = nchar +1
            instr(nchar:nchar) = ';'
        endif
c       
  90	continue
        return
c       
  988   format( ' ', a )
        end
c----------------------------------------------------------------------
        subroutine hexdmp( a, ith, jth )
c  this routine displays the data between the ith and jth 16 bit word
c  in hex format.
c
        implicit        integer( a-z )
        dimension       a(*)
        character*16    hex 
        character*132   l
        real            zreal
c
c
c
        lu = lunout()
        hex(1:) = '0123456789ABCDEF'
c
c
        do 22 i = ith, jth, 10
          j = i+9
          if( j .gt. jth ) then
                j = jth
          endif
c
          l(1:79) = ' '
          c = 2
          call ncode( l, inca(c,5), 5, 1, i, zreal, 0 )
          l(c:c) = ')' 
c
          c = 8
          h = i*4-3
          do 21 k = i, j
            do 20 n = 1, 4
                hc = iget4( a, h )+1 
                l(c:c) = hex(hc:hc)
                c = c+1
                h = h+1
 20         continue
            c = c+1
 21       continue
          write( lu, 987 ) l(1:79)
c
 22     continue
c
        return
 987    format( a )
        end
c----------------------------------------------------------------------
    	subroutine hgnxatt( lui, str, ith, jth, eof, flee )
c       this routine returns the next attribute from a file
    	implicit integer( a-z )
    	character str*(*)
    	character*256 card
    	character*1 cc, char
        save card, ii, ncd
    	logical flee, eof
    	data ii, ncd / 1, 0 /
c       
c       
    	jth = ith
    	nc = 0
c
  11	continue
c       if there are no characters left in the input buffer read in the
c       next card
        if( ii .gt. ncd ) then
            ii = 1
            call readcd( lui, card, ncd, eof, flee )
            if( flee .or. eof ) then
                go to 90
            endif
c   ignore text after the last semicolon on the card
    	    jth = ith
c    	    write(6,*) 'ncd ', ncd
c    	    write(6,*) card(1:ncd)
        endif
c       
  22	continue
c       loop to next ; or end of card
    	if( ii .gt. ncd ) then
    	    go to 11
    	endif
    	cc(1:1) = card(ii:ii)
    	c = ichar(cc(1:1))
    	ii = ii+1
c       
    	if( c .gt. 32 .and. c .lt. 123 ) then
c       ignore blanks and non-printable characters
    	    call uc( cc(1:1), 1 )
    	    str(jth:) = cc(1:1)
    	    jth = jth+1
    	endif
c
    	if( cc(1:1) .ne. ';' ) then
    	    go to 22
    	endif
c       
  90	continue
    	str(jth:) = char(0)
    	return
c       
    	end
c...mark
c---------------------------------------------------------------------
        integer function icmptm( t1, t2 )
c  this routine compares two 6 element arrays of date and time and returns
c  a -1 is t1 is less than t2, 0 is t1 equals t2, or 1 it t1 is greater
c  than t2.
c
        implicit integer( a-z )
        dimension t1(*), t2(*)
c
        do 11 i = 1, 6
          if( t1(i) .lt. t2(i) ) then
                icmptm = -1
                go to 90
          elseif( t1(i) .gt. t2(i) ) then
                icmptm = 1
                go to 90
          endif
 11     continue
        icmptm = 0
c
 90     continue
        return
        end
c----------------------------------------------------------------------
        integer function idlun()
c  this routine returns the logical unit of the input control string
        logical first
        save first, lun
        data    first   / .true. /
c
        if( first ) then
            first = .false.
            lun =  namefx( 'IDLUN', i )
            if( lun .eq. 0 ) then
                lun = 5
            endif
        endif
        idlun = lun
        return
        end
c------------------------------------------------------------------------
        integer function inca( i, n )
c  increments i after using it
        inca = i
        i = i+n
        return
        end
c------------------------------------------------------------------------
        integer function incb( i, n )
c  increments i before using it
        i = i+n
        incb = i
        return
        end
c----------------------------------------------------------------------
        subroutine idateo( ds, ith, nch, d, kth )
c  this routine recodes the date into 'd'
c
        implicit        integer( a-z )
        character       ds*(*)
        character*3     month(12)
        dimension       d(*)
        data            month / 'JAN', 'FEB', 'MAR', 'APR', 'MAY',
     a                  'JUN','JUL','AUG','SEP','OCT','NOV','DEC'/
        real            x
c
        d(kth) = 0
        d(kth+1) = 0
        d(kth+2) = 0
c
        if( ith .le. 0 .or. nch .lt. 8 ) then
          go to 90
        endif
        jth = ith+nch-1
c                           
        d1 = index( ds(ith:jth), '-' )
        if( d1 .le. 0 ) then
          go to 77
        endif
c   set the absolute character address
        d1 = ith+d1-1
        d2 = index( ds(d1+1:jth), '-' )
        if( d2 .le. 0 ) then
          go to 77
        endif
c   set the absoulte character address
        d2 = d1+d2
        ncyr = jth-d2
        if( ncyr .lt. 2 ) then
          go to 77   
        endif
        ncda = d1-ith
        if( ncda .lt. 1 ) then
          go to 77
        endif
c  year
        call dcode( ds, d2+1, jth-d2, ty, yy, x)
        if( yy .lt. 70 ) then
           d(kth) = yy +100
        elseif( yy .gt. 1900 ) then
           d(kth) = yy -1900
        else
           d(kth) = yy
        endif
c
c  day
        if( ncda .ge. 2 ) then
          call dcode( ds, d1-2, 2, ty, d(kth+2), x)
        else
          call dcode( ds, ith, 1, ty, d(kth+2), x)
        endif
c
c  month
        ncmo = d2-d1-1
        if( ncmo .lt. 3 ) then
          go to 77
        endif
        do 11 m = 1, 12
          if( ds( d1+1:d1+3) .eq. month( m )) then
            d(kth+1) = m
            go to 90
          endif      
 11     continue
c
 77     continue
        write( lunout(), 988 ) ds(ith:ith+nch-1)
        write( lunout(), 988 ) 'is an illegal date        '
c
 90     continue  
        return
 988    format( ' ', a )
        end
c----------------------------------------------------------------------
        subroutine idset( instr, nchar, flee )
c   the purpose of this routine is to compress all the input
c   into a compact string and list the input attributes
c   
        implicit integer( a-z )
        logical flee, eof, same
        character instr*(*)
        data lsem / 0 /
c   
        lui = namefx( 'IDSET', i )
        if( lui .eq. 0 ) then
            lui = 5
        endif
        luo = lunout()
c   put a semicolon in the first character
        instr(1:) = ';'
        nchar = 1
        ith = nchar+1
        write( luo, 988 )
     a          '---------------------- input -----------------------'
c   
  11    continue
c   get the next attribute
        call gnxatt( lui, instr, ith, jth, eof, flee )
        if( flee ) then
            go to 90
        elseif( eof ) then
            go to 66
        elseif( same( 'QUIT', 1, 4, instr, ith )) then
            eof = .true.
        endif
c   
c   remove comments
        if(     same( '^', 1, 1, instr, ith ) .or.
     a          same( 'COMM', 1, 4, instr, ith )) then
            continue
        else
c   include this attribute
            nchar = jth-1
            ith = nchar+1
        endif
c   
        if( eof ) then
            continue
        else
            go to 11
        endif
c   
  66    continue
        write( luo, 988 ) 
     a          '+++++++++++++++++ input attributes +++++++++++++++++'
c   
        if( instr(nchar:nchar) .ne. ';' ) then
            nchar = nchar +1
            instr(nchar:nchar) = ';'
        endif
c   
        if( nchar .le. 2 ) then
            write( luo, 988 ) 'no input data '
            flee = .true.
        else
c   print the whole input set attribute by attribute
            call praset( luo, instr, 2, nchar )
        endif
c   
        write( luo, 988 ) 
     a          '===================================================='
c   
  90    continue
        return
c   
  988   format( ' ', a )
        end
c---------------------------------------------------------------------
      function indexa( a, ith, jth, n, p )
c   this routine insures that the string found is indeed at the
c   beginning of an attribute
c
        character a*(*), p*(*)
c
      i = ith
   22 continue
      indexa = index( a(i:jth), p(1:n))
      if( indexa .eq. 0 ) then
         go to 90
      endif
c   adjust to the absolute character postition
      indexa = i+indexa-1
c
      if( a(indexa-1:indexa-1) .eq. ';' ) then
         go to 90
      else
         i = indexa +n
         if( i .lt. jth ) then
            go to 22
         endif
      endif
c
   90 continue
      return
      end
c---------------------------------------------------------------------
      function indexb( a, ith, jth, n, p )
c   old style backwards string search for the pattern in p
c   returns the absolute character no.
        implicit integer( a-z )
        character a*(*), p*(*)
c
        do 22 i = jth-n+1, ith, -1
            b = i
            do 11 c = 1, n
                if( a(b:b) .eq. p(c:c)) then
                    continue
                else
                    go to 22
                endif
                b = b+1
  11        continue
            indexb = i
            go to 90
  22    continue
        indexb = 0
c
  90    continue
        return
        end
c---------------------------------------------------------------------
      function indexc( a, ith, jth, n, p )
c   old style string search returning the absolute character no.
c
        character a*(*), p*(*)
c
      indexc = index( a(ith:jth), p(1:n))
      if( indexc .eq. 0 ) then
         go to 90
      endif
c   adjust to the absolute character postition
      indexc = ith+indexc-1
c
   90 continue
      return
      end
c---------------------------------------------------------------------
        subroutine insl( a, n )
c  this routine inserts an 'n' character string into the diagnostic
c  line buffer
c
        character a*(*)
c
        call inslx( a, 1, n )
        return
        end
c---------------------------------------------------------------------
        subroutine inslc( a )
c  this routine inserts an 'n' character string into the diagnostic
c  line buffer
c
        character a*(*)
c
        call inslx( a, 1, len(a))
        return
        end
c---------------------------------------------------------------------
        subroutine inslx( a, i, n )
c  this routine inserts an 'n' character string into the diagnostic
c  line buffer
c
        character*80 b
        common / dlb / nc
        common / dlb0 / b
        character a*(*)
        common / diag / bypass
        logical bypass
c
        if( bypass ) then
            return
        endif
c
        if( nc+n+1 .gt. 80 ) then
          call dmplnb
        elseif( n .lt. 1 ) then
            return
        endif
        b(nc+1:) = ' '//a(i:i+n-1)
        nc = nc+n+1
        return
        end
c---------------------------------------------------------------------
    	integer function istrlen( str )
    	implicit integer( a-z )
c
    	do 11 i = 1, 256
    	    if( iget8( str, i ) .eq. 0 ) then
    	    	istrlen = i-1
    	    	return
    	    endif
  11	continue
    	istrlen = 0
    	return
    	end
c---------------------------------------------------------------------
c----------------------------------------------------------------------
        subroutine itimeo( ts, ith, nch, t, kth )
c  this routine recodes the time string into the array 't' at the
c  'kth' element
c
        implicit        integer( a-z )
        dimension       t(*)
        character       ts*(*)
        real            x
c
c
        t(kth) = 0
        t(kth+1) = 0
        t(kth+2) = 0
c
        if( ith .le. 0 .or. nch .le. 0 ) then
          go to 90
        endif
c  remove leading blanks and trailing blanks
        call ltblnk( ts, ith, nch, a, n )
c
        e = a+n-1
        c = index( ts(a:e), ':' )
c
        if( c .le. 0 ) then
c  no colons
          if( n .ge. 2 ) then
c  decode the first two characters into the hours position
                call dcode( ts, a, 2, ty, t(kth), x)      
                n = n-2
                if( n .ge. 2 ) then
c  decode then next two characters into the minutes position
                  call dcode( ts, a+2, 2, ty, t(kth+1), x)      
                  n = n-2
c  decode then next two characters into the seconds position
                  call dcode( ts, a+4, min0(n,2),ty,t(kth+2),x)      
                else
                  call dcode( ts, a+2, n, ty, t(kth+1), x)      
                endif
          else                                             
            call dcode( ts, a, n, ty, t(kth), x)      
          endif
          go to 90
        endif
c
        c = a+c-1
c
c  hours
        if( c-a .gt. 1 ) then
                call dcode( ts, c-2, 2, ty, t(kth), x)      
        else     
                call dcode( ts, a, c-a, ty, t(kth), x)      
        endif
c
        if( c .eq. e ) then
          go to 90
        endif
        a = c+1
        d = index( ts(a:e), ':' )
c           
        if( d .le. 0 ) then
c  no second colon
          n = e-c
                if( n .ge. 2 ) then
c  decode then next two characters into the minutes position
                  call dcode( ts, a, 2, ty, t(kth+1), x)      
                  n = n-2
c  decode then next two characters into the seconds position
                  call dcode( ts, a+2, min0(n,2),ty,t(kth+2),x)      
                else
                  call dcode( ts, a, n, ty, t(kth+1), x)      
                endif               
          go to 90
        endif
c
        d = a+d-1
c
c  minutes
        if( d-a .gt. 1 ) then
                call dcode( ts, d-2, 2, ty, t(kth+1), x)      
        else     
                call dcode( ts, a, d-a, ty, t(kth+1), x)      
        endif
c
        if( d .eq. e ) then
          go to 90
        endif
c
c  seconds
        if( e-d .gt. 1 ) then
                call dcode( ts, e-1, 2, ty, t(kth+2), x)      
        else      
                call dcode( ts, d+1, e-d, ty, t(kth+2), x)      
        endif    
        go to 90
c
 90     continue
        return
        end
c---------------------------------------------------------------------
        subroutine i8( a, n, ix )
c  this routine encodes an i4 integer into the dlb along with identifier
c  string 'a'
c
        parameter( llim=80 )
        character*80 b
        common / dlb / nc
        common / dlb0 / b
        character a*(*)
        common / diag / bypass
        logical bypass
c
        if( bypass ) then
            return
        endif
        ln = 8
        if( n .lt. 1 ) then
            m = len( a )
        else
            m = n
        endif
c
        if( nc+n+ln+2 .gt. llim ) then
            call dmplnb
        endif
        nc = nc+1
        b(nc:nc) = ' '
        nc = nc+1
        if( m .gt. 0 ) then
            b(nc:nc+m-1) = a(1:)
            nc = nc+m
            b(nc:nc) = '='
            nc = nc+1
        endif
        call ncode( b, nc, ln, 1, ix, y, iz )
        call lblnk( b, nc, ln, nhc )
        nc = nc+nhc-1
        return
        end
c-----------------------------------------------------------------
        subroutine kw16c( kbuf, ith, ch2, def )
c   this routine extracts two characters from the ith 16 bit word of a buffer
c   and converts them to character data.
c
        character*2 ch2*(*), def
        character char
c
c        if( iget16( kbuf, ith ) .eq. iget16( kbuf, 45 )) then
c            ch2 = def
c            go to 90
c        endif
c
        j = ith*2-1
        ch2(1:1) = char( iget8( kbuf, j ))
        ch2(2:2) = char( iget8( kbuf, j+1 ))
c
  90    continue
        return
        end
c---------------------------------------------------------------------
    	integer function kzap()
c   the definitive source for the integer version of the missing flag
c
    	kzap = -32768
    	return
    	end
c---------------------------------------------------------------------
        subroutine lblnk( id, ith, nchar, mchar )
c  this routine removes the leading blanks from the string and returns
c  the new no. of characters
c
        implicit        integer( a-z )
        character       id*(*)
        character*1     t
c
        j = ith
        mchar = nchar
        do 11 i = 1, nchar
          if( id(j:j) .ne. ' ' ) then
            if( j .eq. ith ) then
                return
            else
                go to 22
            endif
          endif
          mchar = mchar-1
          j = j+1
 11     continue
        go to 90
c
 22     continue
c   move string forward
        call copyc( id, j, mchar, id, ith )
c  fill the remainder of the area with blanks
        id(ith+mchar:ith+nchar-1) = ' '
c
 90     continue
        return
        end
c----------------------------------------------------------------------
        subroutine lc( s, nc )          
c  this routine replaces upper case characters with lower case characters
c
        implicit        integer( a-z )
        character s*(*)
        character*1 ua, uz, la, lz, char
        logical first
        save first, chua, chuz, chla, chlz, bias
c
        data    ua, uz, la, lz / 'A', 'Z', 'a', 'z' /
        data    first   / .true. /
c
        if( first ) then
            first = .false.
            chua = ichar( 'A' )
    	    chuz = ichar( 'Z' )
            chla = ichar( 'a' )
            chlz = ichar( 'z' )
            bias = chua-chla
        endif
c
        do 11 i = 1, nc
          ch = ichar( s(i:i) )
          if( ch .ge. chua .and. ch .le. chuz ) then
            s(i:i) = char( ch-bias )
          endif
 11     continue
c
        return
        end
c---------------------------------------------------------------------
        logical function letter( s, i )
c   this routine determines if the ith character of s is a
c   letter
c
        implicit integer( a-z )
        character s*(*)
c
        letter = ( lge(s(i:i),'a') .and. lle(s(i:i),'z')) .or. 
     +      ( lge(s(i:i),'A') .and. lle(s(i:i),'Z'))
        return
        end
c----------------------------------------------------------------------
      subroutine lf( lun )
c  this routine sends a line feed to the output unit
      write( lun, 111 )
 111  format( ' ' )
      return
      end
c---------------------------------------------------------------------
      subroutine linfit( y, ith, jth, zap )
c     this routine does a piecewise linear interpolation of the array 'y'
c
      dimension y( 1 )
c
      kount = 0
      do 11 i = ith, jth
   11 if( y( i ) .ne. zap ) kount = kount +1
      if( kount .ge. 2 ) go to 22
c
      write( 6, 123 )
  123 format( ' less than two data points in this array.', 2x,
     a   'no linear fit attempted ' )
      go to 90
c
   22 continue
c     find the first two data points
      do 25 i = ith, jth
   25 if( y( i ) .ne. zap ) go to 26
   26 continue
      ii = i+1
      do 27 j = ii, jth
   27 if( y( j ) .ne. zap ) go to 28
   28 continue
c
      if( i .eq. ith ) go to 38
c
c     else extrapolate down
      a = ( y( j ) -y( i ) )/float( j -i )
      b = y( i ) -a*i
      jj = i-1
      do 35 k = 1, jj
   35 y( k ) = a*k +b
c
   38 continue
c     interpolate between the next two data points
      if( i .ge. jth ) go to 90
      ii = i+1
      do 41 j = ii, jth
   41 if( y( j ) .ne. zap ) go to 44
c     no more valid points
      go to 55
c
   44 continue
      if( j .eq. i+1 ) go to 50
      a = ( y( j ) -y( i ) )/float( j -i )
      b = y( i ) -a*i
      ii = i+1
      jj = j-1
      do 48 k = ii, jj
   48 y( k ) = a*k +b
c
   50 continue
      isav = i
      i = j
      go to 38
c
   55 continue
c     extrapolate to the end
      a = ( y( i ) -y( isav ) )/float( i -isav )
      b = y( isav ) -a*isav
      ii = i+1
      do 58 k = ii, jth
   58 y( k ) = a*k +b
c
c
   90 continue
      return
      end
c---------------------------------------------------------------------------
        subroutine ltblnk( s, j, m, i, n )
c   this routine removes leading and trailing blanks
        character s*(*)
c
        i = j
        n = m
c
        do 11 k = j, j+m-1 
            if( s(k:k) .ne. ' ' ) then
                go to 12
            endif
            i = i+1
            n = n-1
  11    continue
  12    continue
c
        do 22 k = j+m-1, i+1, -1
            if( s(k:k) .ne. ' ' ) then
                go to 23
            endif
            n = n-1
  22    continue
  23    continue
c
        return
        end
c-------------------------------------------------------------------------
        function lunque( name )
        implicit integer( a-z )
        character   name*(*)
c   this routine assigns or returns a logical unit number 
c   for the given name
        parameter( maxlu=89, bias=10 )
        character*1111  cs
        dimension p(maxlu+1)
    	save cs, p, n
        data n, p(1) / 0, 1 /       
c
        l = len( name )
        do 11 i = n, 1, -1
            if( l .ne. p(i+1)-p(i)) then
                continue
            elseif( name(1:l) .eq. cs(p(i):p(i)+l-1)) then
                lunque = i + bias
                go to 90
            endif
  11    continue
c
        n = n+1
        if( n .gt. maxlu .or. p(n)+l .gt. 1111 ) then
            call dmplin( 'name/logical unit list exceeded', 0 )
            call fofdie
        endif
c
        cs(p(n):) = name(1:l)
        p(n+1) = p(n) + l
        lunque = n + bias
c
  90    continue
        return
        end
c----------------------------------------------------------------------
      integer function lunout()
c  this routine returns the logical unit of the input control string
c
      lunout = 6
      return
      end
c-------------------------------------------------------------------------
        real function namefp( name, number )
        implicit integer( a-z )
        character   name*(*)
c   this routine assigns or returns an index number for a floating point
c   value for the given name and initializes it to zero
        parameter( maxfp=55 )
        character*555  cs
        dimension p(maxfp+1)
    	real getfp, zero
    	save cs, p, n
        data n, p(1) / 0, 1 /       
        data zero / 0 /
c
        l = len( name )
        do 22 i = 1, n
            if( l .ne. p(i+1)-p(i)) then
                continue
            else
    	    	if( name(1:l) .eq. cs(p(i):p(i)+l-1)) then
                    number = i
    	    	    namefp = getfp(i)
                    go to 90
    	    	endif
            endif
  22    continue
c
        n = n+1
        if( n .gt. maxfp .or. p(n)+l .gt. 555 ) then
            call typstg( lunout(), 
     +	    	'name/floating number list exceeded', 1,0 )
            call fofdie
        endif
c
        cs(p(n):) = name(1:l)
        p(n+1) = p(n) + l
        number = n
        call putfp( n, zero )
    	namefp = 0
c
  90    continue
        return
        end


c-------------------------------------------------------------------------
        integer function namefx( name, number )
        implicit integer( a-z )
        character   name*(*)
c   this routine assigns or returns an index number for a fixed point
c   value for the given name and initializes it to zero
        parameter( maxfx=128 )
        character*1540  cs
        dimension p(maxfx+1)
    	save cs, p, n
        data n, p(1) / 0, 1 /       
c
        l = len( name )
        if( l .lt. 1 ) then
            number = -1
            go to 90
        endif
c
        do 11 i = 1, n
            if( l .ne. p(i+1)-p(i)) then
                continue
            elseif( name(1:l) .eq. cs(p(i):p(i)+l-1)) then
                number = i
    	    	namefx = igetfx( i )
                go to 90
            endif
  11    continue
c
        n = n+1
        if( n .gt. maxfx .or. p(n)+l .gt. 1111 ) then
            call i8( 'n', 1, n )
            call i8( 'l', 1, l )
            call i8( 'p', 1, p(n)-1 )
            call insl( name, 11 )
            call dmplin( 'name/fixed number list exceeded', 0 )
            call typstg( lunout(), cs, 1, p(n)-1 )
            call fofdie
        endif
c
        cs(p(n):) = name(1:l)
        p(n+1) = p(n) + l
        number = n
        call putfx( n, 0 )
    	namefx = 0
c
c
  90    continue
        return
        end
c-------------------------------------------------------------------------
        subroutine nameti( name, number )
        implicit integer( a-z )
        character   name*(*)
c   this routine assigns or returns an index number for the given name
        parameter( maxti=55 )
        character*555  cs
    	save cs, p, n
        dimension p(maxti+1)
        data n, p(1) / 0, 1 /       
c
        l = len( name )
        do 11 i = 1, n
            if( l .ne. p(i+1)-p(i)) then
                continue
            elseif( name(1:l) .eq. cs(p(i):p(i)+l-1)) then
                number = i
                go to 90
            endif
  11    continue
c
        n = n+1
        if( n .gt. maxti .or. p(n)+l .gt. 555 ) then
            call dmplin( 'name/fixed number list exceeded', 0 )
            call fofdie
        endif
c
        cs(p(n):) = name(1:l)
        p(n+1) = p(n) + l
        number = n
c
  90    continue
        return
        end
c---------------------------------------------------------------------
        subroutine ncode( ds, ith, nchar, itype, fixd, zrealx, nfrac )
c  this routine encodes for
c
c       itype = 1       the integer 'fixd' with an i'nchar' format
c                       into string 'ds' at the 'ith' character
c                       (e.g. if nchar=10, then fixd is encoded i10)
c
c       itype = 2       the real no. 'zreal' is encoded f'nchar'.'nfrac'
c                       (e.g. if nchar=10 and nfrac=2, then zreal is
c                       encoded f10.2)
c
c
        implicit        integer( a-t )
        character       ds*(*)
        character*1     char
        jwd( ii ) = ( ii -1 )/8 + 1
c
c
        if( nchar .le. 0 ) then
          go to 90
        endif
c
        azero = ichar( '0' )
c  fill the space with blanks first
        call bfill( ds, ith, nchar )
c
        if( itype .eq. 2 ) then
          go to 33
        elseif( itype .eq. 1 ) then
          continue
        else
          call cfill( ds, ith, nchar, '*' )
          go to 90
        endif
c
c  encode as an integer
        if( fixd .lt. 0 ) then
          maxch = nchar-1
        else
          maxch = nchar
        endif
c
        j = ith+nchar-1
        fx = iabs( fixd )
c
        do 11 i = 1, maxch
          rem = mod( fx, 10 )
c
          if( fx .gt. 0 ) then
            ds(j:j) = char( rem+azero )
          elseif( i .eq. 1 ) then
            ds(j:j) = '0'
            j = j-1
            go to 12
          else
            go to 12
          endif
        kwd = jwd( j )
c
c
          fx = fx/10
          j = j-1
 11     continue
 12     continue
c
        if( fx .gt. 0 ) then
          call cfill( ds, ith, nchar, '*' )
        elseif( fixd .lt. 0 ) then
          ds(j:j) = '-'
        else
          continue
        endif
        go to 90
c
c
 33     continue
c  real no.
        j = ith+nchar-nfrac-2
        zreal = zrealx +sign( .5*10.**(-nfrac), zrealx )
        fx = abs( zreal )
        zf = abs( zreal ) -fx
c
c
        if( zreal .lt. 0 ) then
          maxch = nchar-nfrac-2
        else
          maxch = nchar-nfrac-1
        endif
c
        if( fx .eq. 0 .and. maxch .gt. 0 ) then
c  put a zero 
          ds(j:j) = '0'
          j = j-1
          go to 45
        endif
c
        do 44 i = 1, maxch
          rem = mod( fx, 10 )
          if( fx .gt. 0 ) then
            ds(j:j) = char( rem+azero )
          else
            go to 45
          endif
          fx = fx/10
          j = j-1
 44     continue
 45     continue
c
        if( fx .gt. 0 ) then
          call cfill( ds, ith, nchar, '*' )
          go to 90
        elseif( zreal .lt. 0 ) then
          ds(j:j) = '-'
        else
          continue
        endif
c  fractional part
        j = ith+nchar-nfrac-1
        ds(j:j) = '.'
c
        do 55 i = 1, nfrac
          j = j+1
          fx = zf*10.
          zf = zf*10. -fx
          if( fx .gt. 0 ) then
            ds(j:j) = char( fx+azero )
          else
            ds(j:j) = '0'
          endif
 55     continue
c
 90     continue
        return
        end
c----------------------------------------------------------------------
      function ndatt( is, ith, jth, nchr )
c  this routine returns a pointer the the end of the attribute beginning
c  with the 'ith' character of 'is' and the number of characters in the
c  attribute name in 'nchr'
c
c
        logical nosemi
        character   is*(*)
c
c
c  search for the next semicolon
        ndatt = index( is(ith:jth), ';' )
        if( ndatt .eq. 0 ) then
          nosemi = .true.
          ndatt = jth
        else
          nosemi = .false.
          ndatt = ith+ndatt-1
        endif
c
c  next find the colon
        icolon = index( is(ith:ndatt), ':' )
        if( icolon .eq. 0 .and. nosemi ) then
          nchr = ndatt -ith +1
        elseif( icolon .eq. 0 ) then
          nchr = ndatt -ith
        else
          nchr = icolon -1
        endif
c
   90   continue
        return
        end
c---------------------------------------------------------------------
        subroutine nxtatt( ab, ith, jth, a1, nch, ae )
c  this routine examines the string between the 'ith' and 'jth' characters
c  and returns the location of the first non blank character 'a1' the
c  no. of characters minus leading and trailing blanks 'nch' and the
c  end of the attribute 'ae' (usually the semicolon).
c
c
        implicit        integer( a-z )
        character       ab*(*)
c
c
        c1 = ith
        a1 = 0
        nch = 0
        ae = index( ab(ith:jth), ';' )
        if( ae .le. 0 ) then
          ae = jth
          c2 = jth
        else
          ae = ith+ae-1
          c2 = ae-1
        endif
c
c  c2 points to the last nonsemicolon character in the attribute
        if( ae .eq. ith ) then
c  a legal attribute must contain more than one character
          go to 90
        endif
c
        nch = c2-c1+1   
        ncx = nch
c  count the leading blanks
        do 11 k = 1, ncx
          if( ab(c1:c1) .ne. ' ' ) then
            go to 12
          endif
          nch = nch-1
          c1 = c1+1
 11     continue
 12     continue
c
        if( nch .le. 0 ) then
          go to 90
        endif
c
        a1 = c1
        ncx = c2-c1+1
c  count trailing blanks
        do 22 k = 1, ncx
          if( ab(c2:c2) .ne. ' ' ) then
            go to 23
          endif
          nch = nch-1
          c2 = c2-1
 22     continue
 23     continue
c
 90     continue
        return
        end
c---------------------------------------------------------------------
        subroutine nullc( s, i, n )
c   this routine fills the character array "s" with hard zeros
c
        character s*(*), char*1
c
        do 11 j = i, i+n-1
            s(j:j) = char( 0 )
  11    continue
        return
        end
c----------------------------------------------------------------------
        subroutine page( lun )
c  this routine sends a page eject to the output unit
        write( lun, 111 )
 111    format( '1' )
        return
        end
c----------------------------------------------------------------------
      	subroutine praset( lunitp, is, ith, jth )
c   this routine prints the attributes between the 'ith' and the
c   'jth' characters one attribute per line.
c
        character is*(*)
c
        ipt = ith
        if( ith .ge. jth ) then
          go to 90
        endif
c
   11   continue
        nd = index( is(ipt:jth), ';' )
        if( nd .le. 0 ) then
          nd = jth
        else
          nd = ipt+nd-1
        endif
    	len = nd-ipt+1
    	if( len .le. 128 ) then
            write( lunitp, 988 ) is(ipt:nd)
    	else
    	    call prtargs( lunitp, is, ipt, nd )
    	endif
        ipt = nd+1
        if( ipt .le. jth ) then
          go to 11
        endif
c                
 90     continue
        return
 988    format( ' ', a )
        end
c----------------------------------------------------------------------
      	subroutine prtargs( lunitp, is, ith, jth )
c   this routine prints the arguments of the attribute 
c   between the 'ith' and the 'jth' characters 
c
    	implicit integer( a-z )
        character is*(*)
c
        if( ith .ge. jth ) then
          go to 90
        endif
        nd = index( is(ith:jth), ';' )
        if( nd .le. 0 ) then
          nd = jth
        else
          nd = ith+nd-1
        endif
    	i = ith
    	arg = 1
    	call attarg( is, ith, jth, arg, a, n )
    	if( a .le. 0 ) then
            write( lunitp, 988 ) is(ith:nd)
    	    go to 90
    	endif
c
   11   continue
        write( lunitp, 988 ) is(i:a-1)
    	arg = arg+1
    	i = a
    	call attarg( is, ith, jth, arg, a, n )
    	if( a .gt. 0 ) then
    	    go to 11
    	endif
        write( lunitp, 988 ) is(i:jth)
c                
 90     continue
        return
 988    format( ' ', a )
        end
c-----------------------------------------------------------------------
        subroutine prtime( s, n, t )
c  this routine prints the 6 element time array t
        implicit integer( a-z )
        character*2 u
        character s*(*)
        integer t(*)
        real zreal
c
        call insl( s, n )
        do 11 i = 1, 6
          call ncode( u, 1, 2, 1, t(i), zreal, xx )
          call insl( u, 2 )
 11     continue
        return
        end
c------------------------------------------------------------------------
        subroutine popstr( name, ith, l )
        character   name*(*)
c   this routine removes the last entry from the stack of ids and names
c
        call getstr( 'STACK', name, ith, l )
        call delstr( 'STACK', nx )
        return
        end
c------------------------------------------------------------------------
        subroutine prtstr()
c   this routine prints all ids and names
c
        implicit integer( a-z )
        parameter( maxstr=88 )
c
        character*3200  cs
        common / strnm0 / cs
        common / strnm1 / n, pi(maxstr+1),  pn(maxstr)
c
        if( n .lt. 1 ) then
            call dmplin( 'str list empty', 0 )
            go to 90
        endif
        call dmplin( 'str entry list', 0 )
        do 11 i = 1, n
            a = pi(i)
            b = pn(i)-1
            c = pn(i)
            d = pi(i+1)-1
            call inslc( cs(a:b))
            call dmplin( cs(c:d), 0 )
  11    continue
c
  90    continue
        return
        end
c------------------------------------------------------------------------
        subroutine pshstr( name, ith, l )
        character   name*(*)
c   this routine pushes an entry onto the stack of ids and names
c
        call putstr( 'STACK', name, ith, l )
        return
        end
c----------------------------------------------------------------------
        subroutine putc( a, ith, c )
c  this routine inserts the leftmost character of c into the ith position
c  of the array a.
c
        character a*(*), c*(*)
c
        a(ith:ith) = c(1:1)
        return
        end
c---------------------------------------------------------------------------
        subroutine putfp( n, rval )
c   this function puts the nth floating point item into the list
        parameter( maxfp=55, znull=-32768. )
        real rval, fp
        common / fpcom / fp(maxfp)
c
        if( n .lt. 1 .or. n .gt. maxfp ) then
            call dmplin( 'floating list full', 0 )
            call fofdie
        else
            fp(n) = rval
        endif
        return
        end
c---------------------------------------------------------------------------
        subroutine putfx( n, ival )
c   this function puts the nth fixed point item into the list
        parameter( maxfx=128, inull=-32768 )
        integer ival, fx
        common / fxcom / fx(maxfx)
c
        if( n .lt. 1 .or. n .gt. maxfx ) then
            call dmplin( 'fixed list full', 0 )
            call fofdie
        else
            fx(n) = ival
        endif
        return
        end
c------------------------------------------------------------------------
        subroutine putstr( id, name, ith, l )
c   this routine associates ids and names
c   with a max of 55 entries
c
        implicit integer( a-z )
        parameter( maxstr=88 )
        character   id*(*), name*(*)
c
        character*3200  cs
        common / strnm0 / cs
        common / strnm1 / n, pi(maxstr+1),  pn(maxstr)
c   pointers to the id and string start for each entry
        data pi(1) / 1 /
c   pi(n+1) always points to the next available character
        data n / 0 /
c
        ni = len( id )
        n = n+1
        if( l .lt. 1 ) then
            nc = len( name )
            i = 1
        else
            nc = l
            i = ith
        endif
c
        if( n .gt. maxstr .or. pi(n)+ni+nc .gt. 3200 ) then
    	    call inslc( id )
            call dmplin( ' Capacity of put or get str exceeded', 0 )
    	    write(*,*) 'n=', n, ' full=', pi(n), ' str=', nc, ni
            call fofdie
        endif
c
        cs(pi(n):) = id(1:ni)
        pn(n) = pi(n) + ni
        cs(pn(n):) = name(i:i+nc-1)
        pi(n+1) = pn(n) + nc
c
  90    continue
        return
        end                        
c---------------------------------------------------------------------------
        subroutine puttim( n, t )
c   this function inserts the nth time from the list
        integer t(*)
        parameter( maxti=55, inull=-32768 )
        common / ticom / ti(6,maxti)
        integer ti
c
        if( n .lt. 1 .or. n .gt. maxti ) then
        else
            do 22 i = 1, 6
                ti(i,n) = t(i)
  22        continue
        endif
        return
        end
c---------------------------------------------------------------------
        subroutine put16c( kbuf, ith, n, chs )
c   this routine inserts n characters beginning at the ith 16 bit word 
c   of a buffer 
c
        character chs*(*), char
c
        j = ith*2-1
        do 11 i = 1, n
            call put8( kbuf, j, ichar(chs(i:i)))
            j = j+1
  11    continue
c
  90    continue
        return
        end
c----------------------------------------------------------------------
        subroutine rargs( ccs, ith, nc, arg1, arg2, arg3 )
c  this routine extracts the first three arguments of the attribute
c  and tries to interpret them as floating point numbers
c
        implicit        integer( a-z )
        real            arg1, arg2, arg3, z1, z2, z3
        character       ccs*(*)
c
        call attarg( ccs, ith, nc, 1, a1, nca)
        if( nca .gt. 0 ) then
          call dcode( ccs, a1, nca, itype, kint, z1 )
            if( itype .gt. 0 ) then
                arg1 = z1
            endif
        endif
c
        call attarg( ccs, ith, nc, 2, a1, nca)
        if( nca .gt. 0 ) then
          call dcode( ccs, a1, nca, itype, kint, z2 )
            if( itype .gt. 0 ) then
                arg2 = z2
            endif
        endif
c
        call attarg( ccs, ith, nc, 3, a1, nca )
        if( nca .gt. 0 ) then
          call dcode( ccs, a1, nca, itype, kint, z3 )
            if( itype .gt. 0 ) then
                arg3 = z3
            endif
        endif
c
        return
        end
c----------------------------------------------------------------------
      subroutine readln( lun, line, n, zend, zerr )
c  this routine reads a card/line from an input file
        character line*(*)
      logical zend, zerr
c
      nc = 0
      zend = .false.
      zerr = .false.
      read( lun, 987, err=77, end=66 ) line
  987 format( a )
      	nd = len(line)
    	n = nd
c
    	do 22 i = nd, 1, -1
    	    if( line(i:i) .ne. ' ' ) then
    	    	go to 23
    	    endif
    	    n = n-1
  22	continue
  23 	continue
c
      return
c
 66   continue
      zend = .true.
      return
c
 77   continue
    	write( lunout(), 789 )
 789	format( ' Error in readln ' )
      zerr = .true.
      return
      end
c----------------------------------------------------------------------
        subroutine remcom( is, n1, a, ls, nc )
c  this routine removes comments and unwanted attributes (^) if possible
c  between the last semi-colon and the end of the string.  except for labels
c  all blanks are removed.
c  where "is" is the input string of "n1" characters, "a" is the output
c  string, "ls" is the location of the last semicolon, and "ns" is the length
c  of the output string.
c
        implicit integer( a-z )
        character is*(*), a*(*)
        character*1 lc, cc
c
c  copy the current string into the array with restricted blank removal
c  and if it does not contain a label attribute, remove extra blanks. 
c
c        write( lunout(), 987 ) 'n1, ls, nc', n1, ls, nc
 987    format( ' ', a, 3i8 )
        ncx = nc
        do 10 i = 1, n1
          lc = a(ncx:ncx)
          cc = is(i:i)
c          cv = ichar( cc )
          if( cc .eq. ';' .and. lc .eq. ' ' ) then
c  remove blank before semicolon
            a(ncx:ncx) = cc 
          elseif( cc .eq. ';' .and. lc .eq. ';' ) then
c  remove consecutive semicolons
            continue
          elseif( cc .eq. ' ' .and. lc .eq. ' ' ) then
c  remove consecutive blanks
            continue
          elseif( cc .eq. ' ' .and. lc .eq. ';' ) then
c  remove blanks after the semicolon
            continue
c          elseif( cv .lt. 32 .or. cv .gt. 127 ) then
c   remove nonprintable characters
c            continue
          else
            ncx = ncx+1
            a(ncx:ncx) = cc 
          endif
 10     continue
c
c
 11     continue
        ii = ls+1
c  search for the next semicolon
        ns = index( a(ii:ncx), ';' )
        if( ns .le. 0 ) then
                go to 90
        endif
c   ns is relative to ii...make it absolute
        ns = ii+ns-1
c
c   is this a label
        if( a(ii:ii+2).eq.'LAB' .or. a(ii:ii+2).eq.'lab' ) then
c  leave things alone
          ls = ns 
        elseif( a(ii:ii+2).eq.'COM' .or.
     +          a(ii:ii+2).eq.'com' .or.
     +          a(ii:ii).eq.'^' ) THEN
c  eliminate this comment or unwanted attribute
          call copyc( a, ns+1, ncx-ns, a, ii )
          ncx = ncx - (ns-ls)
        else
c  compress the remaining blanks out of this attribute
          jj = ii
          do 22 i = jj, ns
            cc = a(i:i)
            if( cc .ne. ' ' ) then
              a(ii:ii) = cc
              ii = ii+1
            endif
 22       continue
          ls = ii-1
          if( ls .ne. ns ) then
c  move the remainder of the string up
            call copyc( a, ns+1, ncx-ns, a, ls+1 )
            ncx = ncx - (ns-ls)
          endif
        endif
c
        go to 11
c
c
 90     continue
        nc = ncx
        return
 988    format( ' ', a )
        end
c------------------------------------------------------------------------
        subroutine resstr()
c   this routine resets or zeroes out ids and names
c
        implicit integer( a-z )
        parameter( maxstr=88 )
        common / strnm1 / n, pi(maxstr+1),  pn(maxstr)
c
        n = 0
        return
        end
c---------------------------------------------------------------------
      logical function same( a, i, n, b, j )
c   this routine compares two n-character strings
      character a*(*), b*(*)
c
      if( n .gt. 0 ) then
        same = a(i:i+n-1) .eq. b(j:j+n-1)
      else
        same = .false.
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine scntyp( mode, id, ith, nchr )
c  this routine encodes the scan mode mnemonic for 'mode' into the
c  'ith' character of 'id' and returns the number of characters
c  in the mnemonic.
c
        character id*(*)
c
        nchr = 3
c
        if( mode .lt. 0 .or. mode .gt. 9 ) then
c         encode asterisks
          id(ith:) = '***'
          go to 90
        endif
        go to( 10,11,12,13,14,15,16,17,18,19 ), mode+1
c
   10   continue
        id(ith:) = 'CAL'
        go to 90
C
   11   continue
        id(ith:) = 'PPI'
        go to 90
c
   12   continue
        id(ith:) = 'COP'
        go to 90
c
   13   continue
        id(ith:) = 'RHI'
        go to 90
c
   14   continue
        id(ith:) = 'VER'
        go to 90
c
   15   continue
        id(ith:) = 'TAR'
        go to 90
c
   16   continue
        id(ith:) = 'MAN'
        go to 90
c
   17   continue
        id(ith:) = 'IDL'
        go to 90
c
   18   continue
        id(ith:) = 'SUR'
        go to 90
c
   19   continue
        id(ith:) = 'AIR'
        go to 90
c
   90   continue      
        return
        end
c----------------------------------------------------------------------
        subroutine seteov( eovx )
c  this routine sets eov flag 
c
        common / ceov / eov
        logical eov, eovx
c
        eov = eovx
        return
        end
c-----------------------------------------------------------------------
        subroutine setflg( n, flg )
c  this routine sets flag "n" to "flg"
c
        parameter( nfl=55 )
        common / flags / flag( nfl )
        logical flg, flag
c
        if( n .le. nfl ) then
                flag( n ) = flg
        endif
        return
        end
c---------------------------------------------------------------------
        subroutine slipstr( s, d, jth )
c  this routine copies the source string 's' up to to a ^ or hard zero
c  into string d at the jth character and adds the number of characters
c  copied to the pointer.
c
        implicit integer( a-z )
        character   s*(*), d*(*)
c
c
        n = len( s )
        h = index( s(1:n), '^' )
        if( h .gt. 0 ) then
            n = h-1
        endif
c
        d(jth:) = s(1:n)
c
        return
        end
c---------------------------------------------------------------------
        subroutine ss
c  this routine does a line feed
        write( 6, 111 )
 111    format( ' ' )
        return
        end
c---------------------------------------------------------------------
        subroutine sslu( lun )
c  this routine does a line feed
        write( lun, 111 )
 111    format( ' ' )
        return
        end
c---------------------------------------------------------------------------
        subroutine sumtim( t1, t2, t3 )
c   this routine sums the first two times and puts the result in t3
        implicit integer( a-z)
        logical leap
        dimension t1(*), t2(*), t3(*), mlen(12)
        data mlen / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
c
        sum = t1(6) + t2(6)
        if( sum .ge. 0 )then
            carry = sum/60
        else
            carry = -1
        endif
        t3(6) = sum - carry*60
c
        sum = t1(5) + t2(5) + carry
        if( sum .ge. 0 )then
            carry = sum/60
        else
            carry = -1
        endif
        t3(5) = sum - carry*60
c
        sum = t1(4) + t2(4) + carry
        if( sum .ge. 0 )then
            carry = sum/24
        else
            carry = -1
        endif
        t3(4) = sum - carry*24
c
        if( carry .lt. 0 ) then
            m = t1(2)-1
            if( m .eq. 0 ) then
                m = 12 
            endif
        else
            m = t1(2)
        endif
        lm = mlen( m )
c
        if( m .eq. 2 .and. mod( t1(1), 4 ) .eq. 0 ) then
c   leap year!
            lm = 29
        endif
        sum = t1(3) + t2(3) + carry
        if( sum .ge. 1 .and. sum .le. lm ) then
            t3(3) = sum
            carry = 0
        elseif( sum .gt. lm ) then
            t3(3) = sum - lm
            carry = 1
        else
c   back into previous month
            if( m .eq. 1 ) then
                m = 12
            else
                m = m-1
            endif
            if( m .eq. 2 .and. mod( t1(1), 4 ) .eq. 0 ) then
                ml = 29
            else
                ml = mlen( m )
            endif
            t3(3) = sum + ml
            carry = -1
        endif
c
        sum = t1(2) + t2(2) + carry
        if( sum .ge. 1 .and. sum .le. 12 ) then
            carry = 0
            t3(2) = sum
        elseif( sum .gt. 12 ) then
            carry = 1
            t3(2) = sum - 12
        else
            carry = -1
            t3(2) = sum + 12
        endif
c
        t3(1) = t1(1) + t2(1) + carry
        if( t3(1) .le. 0 ) then
            t3(1) = t3(1) + 100
        endif
c
        return
        end
c-----------------------------------------------------------------------
        subroutine typefe( sa, ith, jth )
c  this routine prints 'sa' from the 'ith' word to the 'jth' word as
c  floating point numbers in e format
c
        dimension       sa( * )
        character*132   l
        character*9     n
        integer         c
        common / diag / bypass
        logical bypass
c
        if( bypass ) then
          go to 90
        endif
c
        lu = lunout()
c
        do 22 i = ith, jth, 15
          j = i+14
          if( j .gt. jth ) j = jth
          l(1:128) = ' '
          c = 2
          call ncode( l, inca(c,5), 5, 1, i, zreal, ix )
          l(c:c) = ')'
          c = c+2
c
          do 21 k = i, j
    	    write( n, 111 ) sa(k)
c            encode( 9, 111, n ) sa( k )
 111        format( e9.2 )
c   get the sign
            l(c:c) = n(1:1)
            c = c+1
            call copyc( n, 3, 7, l, inca(c,7))
 21       continue
          call typlin( lu, l, 128 )
c
 22     continue
c
 90     continue
        return
        end
c-----------------------------------------------------------------------
        subroutine typefp( sa, ith, jth )
c  this routine prints 'sa' from the 'ith' word to the 'jth' word as
c  floating point numbers
c
        dimension       sa( 1 )
        common / diag / bypass
        logical bypass
c
        if( bypass ) then
          go to 90
        endif
c
        lu = lunout()
c
        do 22 i = ith, jth, 15
          j = i+14
          if( j .gt. jth ) j = jth
          write( lu, 123 ) i, ( sa( k ), k = i, j )
 123      format( 1x, i5, ')', 15( 1x, f7.1 ))
 22     continue
c
 90     continue
        return
        end
c-----------------------------------------------------------------------
        subroutine typefx( sa, ith, jth )
c  this routine prints 'sa' from the 'ith' word to the 'jth' word as
c  integers
c
        common / diag / bypass
        logical bypass
        integer sa(*)
c
        if( bypass ) then
          go to 90
        endif
c
        lu = lunout()
c
        do 22 i = ith, jth, 15
          j = i+14
          if( j .gt. jth ) j = jth
          write( lu, 123 ) i, ( sa( k ), k = i, j )
 123      format( 1x, i5, ')', 15( i8 ))
 22     continue
c
 90     continue
        return
        end
c---------------------------------------------------------------------
        subroutine type16( sa, ith, jth )
c  this routine unpacks and displays the elements of a 16 bit array
c
        implicit        integer( a-z )
        common / diag / bypass
        logical bypass
        dimension       lbuf( 20 )
c
c
        if( bypass ) then
          go to 90
        endif
c
        lu = lunout()
c
c        write( lu, 987 ) ( i, i = 1, 10 )
 987    format( 7x, 10( 4x, i2 ))
c        write( lu, 988 )
 988    format( 7x, 10( 5x, 'V' ))

        do 22 i = ith, jth, 10
          j = i+9
          if( j .gt. jth ) j = jth
          c = 0
          do 11 k = i, j
            c = c+1
            lbuf( c ) = iget16( sa, k )
 11       continue
          write( lu, 123 ) i-1, ( lbuf( l ), l = 1, c )
 123      format( 1x, i5, ')', 20i6 )
c
 22     continue
c
 90     continue
        return
        end
c---------------------------------------------------------------------
      subroutine typlin( lun, line, nchar )                          
c     this routine prints an nchar length line                         
c                                                                       
      character line*(*)
c                                                                       
      write( lun, 111 ) line(1:nchar)
  111 format( a )
      return                                                            
      end                                                               
c---------------------------------------------------------------------
      subroutine typstg( lun, is, ith, nchr )
c  this routine prints an arbitrary string of one line or less to
c  logical unit 'lun'.
c
        implicit integer( a-z )
    	parameter( LLIM=78 )
        character is*(*)
c
c
        i = ith
        n = nchr
c
        if( n .lt. 1 ) then
c  search the next 256 characters for a "^"
            h = index( is(i:i+255), '^' )
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
c
        if( n .gt. 0 ) then
          i = j+1
          go to 11
        endif
c
 90     continue
        return
        end
c----------------------------------------------------------------------
        subroutine uc( s, nc )          
c  this routine replaces lower case characters with upper case characters
c
        implicit        integer( a-z )
        character s*(*)
        character*1 ua, uz, la, lz, char
        logical first
        save first, chua, chla, chlz, bias
c
        data    ua, uz, la, lz / 'A', 'Z', 'a', 'z' /
        data    first   / .true. /
c
        if( first ) then
            first = .false.
            chua = ichar( 'A' )
            chla = ichar( 'a' )
            chlz = ichar( 'z' )
            bias = chua-chla
        endif
c
        do 11 i = 1, nc
          ch = ichar( s(i:i) )
          if( ch .ge. chla .and. ch .le. chlz ) then
            s(i:i) = char( ch+bias )
          endif
 11     continue
c
        return
        end
c---------------------------------------------------------------------
        subroutine zrofilc( id, ith, nchar )
c  this routine examines 'nchar' characters from array 'id' beginning
c  with the 'ith' character and substitutes a zero for every blank
c  encountered
c
        character id*(*)
c
        jth = ith+nchar-1
        do 11 i = ith, jth
          if( id(i:i) .eq. ' ' ) then
            id(i:i) = '0'
          endif
 11     continue
c
        return
        end
c----------------------------------------------------------------------
    	subroutine upkfld( kbuf, vec, ith, len )
c   this routine unpacks a 16 bit integer field and does sign extension
    	implicit integer( a-z )

    	call upk16s( kbuf, vec, ith-1, len )
    	return
    	end
c----------------------------------------------------------------------
    	subroutine upkfldf( kbuf, vec, ith, len )
c   this routine unpacks a 16 bit integer flagged field and
c   does sign extension
    	implicit integer( a-z )

    	call upk16s( kbuf, vec, ith-1, len )
c   clear out the flag bit
    	call andmsk( vec, 1, len, 1 )
    	return
    	end
c----------------------------------------------------------------------
    	subroutine upkflg( kbuf, vec, ith, len )
c   this routine unpacks a 16 bit integer flagged field and
c   just returns the flag
    	implicit integer( a-z )

    	call upk16( kbuf, vec, ith-1, len )
c   just the flag bit
    	call andmsk( vec, 1, len, 0 )
    	return
    	end
c----------------------------------------------------------------------
      subroutine ydate( iyr, imo, ida, id, ith, nchr )
c  this routine encodes the date into 'id' beginning at the
c  'ith' character
c
        character id*(*)
  123   format( i10 )
c
        nchr = 10
        if( ida .lt. 1 .or. ida .gt. 31 ) then
          call copyc( 'xx', 1, 2, id, ith )
        else
          call ncode( id, ith, 2, 1, ida, x, ix )
        endif
c
        call putc( id, ith+2, '-' )
        jth = ith+3
c
        if( imo .eq. 1 ) then
          call copyc( 'JAN', 1, 3, id, jth )
        elseif( imo .eq. 2 ) then
          call copyc( 'FEB', 1, 3, id, jth )
        elseif( imo .eq. 3 ) then
          call copyc( 'MAR', 1, 3, id, jth )
        elseif( imo .eq. 4 ) then
          call copyc( 'APR', 1, 3, id, jth )
        elseif( imo .eq. 5 ) then
          call copyc( 'MAY', 1, 3, id, jth )
        elseif( imo .eq. 6 ) then
          call copyc( 'JUN', 1, 3, id, jth )
        elseif( imo .eq. 7 ) then
          call copyc( 'JUL', 1, 3, id, jth )
        elseif( imo .eq. 8 ) then
          call copyc( 'AUG', 1, 3, id, jth )
        elseif( imo .eq. 9 ) then
          call copyc( 'SEP', 1, 3, id, jth )
        elseif( imo .eq. 10 ) then
          call copyc( 'OCT', 1, 3, id, jth )
        elseif( imo .eq. 11 ) then
          call copyc( 'NOV', 1, 3, id, jth )
        elseif( imo .eq. 12 ) then
          call copyc( 'DEC', 1, 3, id, jth )
        else
          call copyc( 'XXX', 1, 3, id, jth )
        endif
c
        call putc( id, jth+3, '-' )
c
        if( iyr .lt. 0 .or. iyr .gt. 100 ) then
          call copyc( 'xxx', 1, 3, id, jth+4 )
        else
          call ncode( id, jth+4, 3, 1, iyr, x, ix )
        endif
c
        call zrofil( id, ith, nchr )
c
        return
        end
c----------------------------------------------------------------------
      subroutine ytime( ihr, imin, isec, id, ith, nchr )
c  this routine encodes the time beginning with the 'ith' character of
c  array 'id'.
c
        character id*(*)
c
  123   format( i10 )
c
        nchr = 8
c
        call ncode( id, ith, 2, 1, ihr, x, ix )
        call putc( id, ith+2, ':' )
        call ncode( id, ith+3, 2, 1, imin, x, ix )
        call putc( id, ith+5, ':' )
        call ncode( id, ith+6, 2, 1, isec, x, ix )
        call zrofil( id, ith, nchr )
c
        return
        end
c---------------------------------------------------------------------
        subroutine zeroc( s, i, n )
c   this routine fills the character array "s" with hard zeros
c
        character s*(*), char*1
c
        do 11 j = i, i+n-1
            s(j:j) = char( 0 )
  11    continue
        return
        end
c---------------------------------------------------------------------
        subroutine zipstr( s, d, jth )
c  this routine copies the source string 's' up to to a ^ or hard zero
c  into string d at the jth character and adds the number of characters
c  copied to the pointer.
c
        implicit integer( a-z )
        character   s*(*), d*(*)
c
c
        n = len( s )
        h = index( s(1:n), '^' )
        if( h .gt. 0 ) then
            n = h-1
        endif
c
        d(jth:) = s(1:n)
        jth = jth+n
c
        return
        end
c---------------------------------------------------------------------
        subroutine znz( a, n, x )
c  this routine returns a 0 if x=0 and 'nz' if not
c
        character*80 b
        common / dlb / nc
        common / dlb0 / b
        character a*(*)
        common / diag / bypass
        logical bypass
c
        if( bypass ) then
            return
        endif
c
        if( nc+n+4 .gt. 80 ) then
            call dmplnb
        endif
        if( x .eq. 0 ) then
            b(nc+1:) = ' '//a(1:n)//'=0'
            nc = nc+n+3
        else
            b(nc+1:) = ' '//a(1:n)//'=NZ'
            nc = nc+n+4
        endif
c
        return
        end
c---------------------------------------------------------------------
        subroutine zncode( ds,ith,nchar,itype,fixd,zrealx,nfrac )
c  same as ncode but bumps the position pointer
        character   ds*(*)
        integer fixd
c
        call ncode( ds, ith, nchar, itype, fixd, zrealx, nfrac )
        ith = ith+nchar
        return
        end
c---------------------------------------------------------------------
        subroutine zrofil( id, ith, nchar )
c  this routine examines 'nchar' characters from array 'id' beginning
c  with the 'ith' character and substitutes a zero for every blank
c  encountered
c
        character   id*(*)
c
        jth = ith+nchar-1
        do 11 i = ith, jth
          if( id(i:i) .eq. ' ' ) then
            id(i:i) = '0' 
          endif
 11     continue
c
        return
        end
c----------------------------------------------------------------------
# ifdef notyet
# endif
