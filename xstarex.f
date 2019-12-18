      SUBROUTINE xstarex( buf, lbuf, ibuf, nready )
c****  The Edgetech XSTAR SEG-Y conversion module.
c  (C) The Regents of the University of California, 2001
c  Written by Paul Henkart, Scripps Institution of Oceanography August 2001
c  Funded by NSF
c
c  mod 7 Sept 01 - Add warning if diskin parameter FORMAT was not used
c  mod 17 Oct 01 - Add lunxyz, delta, lprint, depth
c  mod 24 Oct 01 - Add lunbin
c  mod 29 Oct 01 - Add luntmp for interpolating the water depths
c  mod 10 Jan 02 - Add a message about water depth/time on the first shot
c                  when doing XYZ file stuff.
c  mod 16 Jan 02 - Use the more precise lat/long in header 21 & 23
c  mod 20 Feb 02 - nready not ser correctly when depth not given.
c  mod 30 Apr 02 - If it only has 1 trace, it's a GeoStar, so don't
c                  stack or insist on two ducers.
c  mod 3 May 02 - GeoStar sets the sample interval to 1 way travel
c                 time (we think), so multiple it by 2.
c  mod 18 Nov 02 - Creation of the xyz binary file wrote over the
c                  first 3 data samples on the first trace.
c  mod 17 Dec 02 - Add ftr/ltr.
c                - Account for 1 trace XSTAR as well as 2 trace XSTAR.
c                - gstar is same as 1 trace xstar but doesn't need
c                  diskin parameter format edgetech is gstar2segy is used.
c mod 19 Feb 03 - Add parameter DUMMIES (preset = yes)
c mod 21 Feb 03 - Always set ntrcs to 1
c mod 14 Mar 03 - Add type 3, Triton
c mod 1 Apr 03 - Make sure traces to be summed have the same
c                sample interval
c mod 20 Jun 03 - Previous check was bad if the first trace is not 1.
c               - Clarify message when only 1 ducer on 2 ducer system.
c mod 1 Jul 03 - Zero out segy binary header words 151, 152, 153
c mod 12 Feb 04 - Change some of the warning messages.
c mod 17 Nov 04 - Don't abort when the sample interval changes in the
c                 same ping.
c mod 1 Dec 04 - Scalar on first trace might be 0 due to temp = 0 statement.
c              - Add MKREAL
c mod 11 Jan 05 - Change dummies into 0, 1, 2 from yes/no and add 2 to
c                 mean use the ping if 1 of the ducers is missing.
c               - Divide summation by the number of traces added.
c mod 10 Feb 05 - In MKREAL, zero sample 1601 - numdat
c mod 27 Apr 05 - Redo the type 2 buffers
c               - Redo the logic of missing traces and traces out
c                 of order.
c mod 25 Aug 05 - Add type 5, Xstar version 5. (Martin Jakobsson's)
c mod 30 Jun 06 - Add type 4 (unsummed two trace Xstar).
c mod 14 Aug 07 - g95 doesn't allow type declaration to set the value.
c
c********   start JSTAR documentation
c Here's what I know about a new (Nov 2004) Edgetech A/D called:    JSTAR
c (I suspect another Jason Sara format.  Remember he invented the .jsf
c format which is readable by his pc program only).
c recording the data in 5 formats:
c    type 0 - 1 value per sample - envelope data
c    type 1 - 2 values per sample - stored as real(i), imag(1)
c    type 2 - 1 value per sample - before matched filter
c    type 3 - 1 value per sample - real part analytical signal
c    type 4 - 1 value per sample - pixel data / ceros data
c fish depth in byte 105-106 of the trace header in milliseconds (integer)
c and the microsecond integer value in bytes 181-182.  The static offset for
c the trace is then the millisecond value concatenated with a decimal point
c and the byte 181-182 value
c******   end JSTAR
c******  The *.jsf format is Jason Sera's proprietary binary format.

      DIMENSION buf(1111), lbuf(1111)
      INTEGER*2 ibuf(1111), packetno
      REAL*8 deglat, deglong, dtemp, dlong, dlat, deltad, dbuf(1),
     &     mlat, mlong, distance, deglatlast, deglonglast, dbub(3),
     &     depth_lat, depth_long, boatlat, boatlong, deltalat, deltalong
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, ntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      COMMON /apmem/a(5000000)
      EQUIVALENCE (a(1),dbuf(1))
c******      why   stkindx AND stkindex   ??????????????????????
      INTEGER ftr, type, dummies, stkindx, stkindex
      COMMON /xstar/ lunxyz, delta, lprint, depth, wireout, lunbin,
     &               ftr, ltr, type, weights(2), dummies, mkreal
      COMMON /SEGY/ header(60)
      COMMON /sioln2/ ICHAR, NCHARS, iprint, lunpo
      CHARACTER*80 token
      LOGICAL first, first_xyz
      DATA first/.TRUE./, first_xyz/.TRUE./
      SAVE
      DATA iwarn/0/, iwarn1/0/, idepth/0/, mlat/0./, distance/99999.D0/,
     &    dlatlast/0./, luntmp/0/, nsaved/0/, lastpingno/0/, lundead/0/
      DATA noffset/1000/                                                ! the maximum number of pings in the offset
c
c
      iout = 1
      CALL inap( buf(numhdr+1), numdat )
c****
c****  Treat Triton totally separately from Edgetech
c****
      IF( type .EQ. 3 ) THEN
c****     trace 1 is envelope data
          IF( lbuf(4) .EQ. 1 ) RETURN
          IF( first ) THEN
              first = .FALSE.
              stkindx = nextad
              nextad = nextad + numdat
          ENDIF
          IF( lbuf(4) .EQ. 2 ) THEN
              DO i = 1, numdat
                 a(nextad + i - 1 ) = a(numhdr+i)
              ENDDO
          ENDIF
      ENDIF
c**** If Edgetech, always output just 1 trace per shot/ping
      ntrcs = 1
c**** Insist the user give parameter FORMAT EDGETECH in process DISKIN,
c**** which takes care of the trace number and shot time BS, but more
c**** importantly, DISKIN and INPUT have aleady clobbered long words 45-47
      IF( lbuf(3) .NE. lbuf(1) .AND. iwarn .LT.10.OR.lbuf(4).EQ.0) THEN
          iwarn = iwarn + 1
          PRINT *,
     &    ' ***  WARNING  ***  Use DISKIN parameter FORMAT EDGETECH'
      ENDIF
      nready = 0
      itrno = lbuf(4)
      IF( lbuf(4) .LT. ftr .OR. lbuf(4) .GT. ltr .AND. type .NE. 5) THEN
          IF( type .NE. 2 .AND. type .NE. 4 .AND. iwarn .LT. 20 ) THEN
              PRINT *,' ***  WARNING  ***  XSTAR TYPE 2 should be used.'
              iwarn = iwarn + 1
          ENDIF
          RETURN
      ENDIF
      lbuf(6) = 0
      nopackets = ibuf(12)
      packetno = lbuf(4)
      lbuf(7) = 0
      scalar = buf(51)
      buf(51) = 0
      temp = scalar / 32767. * 1.41E-14
c**** DISKIN and gstar2xstar already moved the date to standard SEGY
c      ibuf(79) = ibuf(100)
c      ibuf(80) = ibuf(99)
c      ibuf(81) = ibuf(94)
c      ibuf(82) = ibuf(95)
c      ibuf(83) = ibuf(96)
      IF( itrno .EQ. 1 ) THEN
          isi1 = ibuf(59)
      ELSE
          IF( isi1 .NE. ibuf(59) .AND. ftr .NE. ltr .AND.isi1.GT.0) THEN
              PRINT *,
     &    ' Dropping trace 2 due to different sample intervals.'
c****     NO stack or summing of traces
             DO i = 0, numdat - 1
                a(in + i) = temp *
     &                SQRT(a(in+j)*a(in+j) + a(in+j+1)*a(in+j+1) )
                j = j + 2
             ENDDO
             IF( weights(itrno) .NE. 1. ) THEN
                 DO i = 0, numdat - 1
                    a(in + i) = a(in + i) * weights(itrno)
                 ENDDO
             ENDIF
             lastpingno = lbuf(3)
             lasttr = lbuf(4)
             nready = 1
             GOTO 800
          ENDIF
      ENDIF
c****
c****  Convert to Envelope and apply trace scale Factor
c****  Do the work in the array processor so we can use the memeory.
c****  Hopefully this isn't too cute.  As a reminder, inap puts
c****  the trace at ap(in).  ap(nextad) is a scratch area, so reserving
c****  some space is accomplished by remembering where nextad is, then
c****  incrementing it so the next person doesn't use ours.
c****
c**** remember the output is half as long as the input.
      numdat = numdat / 2
      ibuf(58) = numdat
c**** Account for the complex modulus in the sample interval, but do
c**** 41 differently because it was really 41.6666
      IF( ibuf(59) .EQ. 41 ) THEN
          buf(49) = 1./12000.
          ibuf(59) = 83
      ELSE
          ibuf(59) = ibuf(59) * 2
          buf(49) = buf(49) * 2.
      ENDIF
      temp = scalar / 32767. * 1.41E-14
c****
      IF( mkreal .EQ. 1 ) GOTO 5000
c**** Leave the trace number alone, especially if not stacking.  diskin corrected it
c      lbuf(4) = 1
c****
c**** Stack (add) trace 1 and 2.  Watch out for shots with only
c**** one trace.  Prepare for either trace being missing.
c**** We've already renumber the traces  so they are 1 & 2
c****
      IF( first ) THEN
          first = .FALSE.
          stkindex = nextad
          IF( type .EQ. 5 ) THEN
              nextad = nextad + 8*numdat
          ELSE
              nextad = nextad + numdat
          ENDIF
          icount = 0
          IF( wireout .NE. 0 ) THEN
              idepaddr = nextad
              nextad = nextad + noffset
              lataddr = nextad
              nextad = nextad + noffset
              longaddr = nextad
              nextad = nextad + noffset
              noffdone = 0
          ENDIF
          ltemp = 0.
          CALL podiscb( ihunit, 0, 3500 )
          DO i = 1, 3
c****        zero out binary header words 151-153
             CALL wrdiscb( ihunit, ltemp, 2 )
          ENDDO
c****     sometimes the first trace in a file is not trace 1
          IF( itrno .NE. 1 .AND. ftr .NE. ltr ) THEN
              PRINT *,' Dropping first ping - not trace 1'
              nmissing = 1
              GOTO 2000
          ENDIF
      ENDIF
      j = 0
      nmissing = 0
      IF( ifakeout .NE. 0 ) lastpingno = lastpingno + 1
      ifakeout = 0
      IF( lbuf(3) .GT. lastpingno + 1 .AND. lastpingno .NE. 0 .AND.
     &    dummies .NE. 0 ) THEN
          nmissing = lbuf(3) - lastpingno - 1
          PRINT *,'Before ping ',lbuf(3),nmissing,
     &           ' missing pings will be replaced by dead ones.'
      ENDIF
c****
c**** Do the complex modulus without summing the traces.  GeoStar
c**** and some Xstar system only have 1 trace.  ftr & ltr were set
c**** by xstared according to parameter TYPE.
c****
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' Edgetech shot ',lbuf(1),lbuf(3),' last=',lastpingno
	     PRINT *,' ftr=',ftr,' ltr=',ltr,' itrno=',itrno
          PRINT *,' scalar=',scalar,temp,' weight=',weights(itrno)
      ENDIF
c**** Geostar (type 0) is already in envelope and only has 1 trace
c**** (does it have a scalar?)
      IF( type .EQ. 0 ) GOTO 800
c****
c****  Martin Jakobsson's weird Xstar version 5
c      5120 bytes/tr - 240 segy header = 4880 bytes
c      4880 - 80 bytes of zeroes = 2400 samples per trace
c      packets 2-8, 7 * 2400 = 16,800 samples per trace
c****  The documenation says there are 8*3976 words or
c      (8192-240 bytes = 7952 bytes or 3976 words)
c      packet #1 is missing!
c****
      IF( type .EQ. 5 ) THEN
          nready = 0
          IF( lastpingno .EQ. 0 .AND. packetno .NE. 2 ) RETURN
          lastpingno = lbuf(3)
          indx = (packetno-1) * 2400 + stkindex - 1
          DO i = 1, 2400
             a(indx+i) = a(in+i)
          ENDDO
          IF( packetno .NE. 8 ) RETURN
          idelay = lbuf(2)
          jdelay = ibuf(55)
          joke1 = ibuf(58)
          numdat = 7 * 2400
          DO i = 0, numdat-1
             a(in+i) = a(stkindx+i)
          ENDDO
          numdat = numdat / 2
          ibuf(58) = numdat
      ENDIF
      DO i = 0, numdat - 1
         a(in + i) = temp * SQRT(a(in+j)*a(in+j) + a(in+j+1)*a(in+j+1) )
         j = j + 2
      ENDDO
      IF( weights(itrno) .NE. 1. ) THEN
          DO i = 0, numdat - 1
             a(in + i) = a(in + i) * weights(itrno)
          ENDDO
      ENDIF
      IF( ftr .EQ. ltr .OR. type .EQ. 4 .OR. type .EQ. 5 ) THEN
          lasttr = 1
          nready = 1
          GOTO 800
      ENDIF
c**** The traces may not come in order, so see if it's the same ping no.
      IF( lbuf(3) .EQ. lastpingno ) THEN
          IF( icount .EQ. 0 ) THEN
              PRINT *,' impossible - icount =0 and its the same ping no'
              STOP
          ENDIF
c*****    stack this in
           DO i = 0, numdat-1
              a(stkindex+i) = a(stkindex+i) + a(in+i)
           ENDDO
           icount = icount + 1
           IF( icount .EQ. type ) THEN
               DO i = 0, numdat-1
                  a(in+i) = a(stkindex+i) / icount
               ENDDO
               nready = 1
               icount = 0
           ENDIF
           GOTO 800
      ENDIF
c**** This ping is different from the last
      IF( type .EQ. 1 .OR. type .EQ. 4 ) GOTO 800
      IF( icount .EQ. 0 ) THEN
           DO i = 0, numdat-1
              a(stkindex+i) = a(in+i)
           ENDDO
           icount = 1
           GOTO 800
      ENDIF
      IF( icount .NE. type ) THEN
c         different ping, previous ping is missing trace(s).
          PRINT *,' ping ',lastpingno,' only had ',icount,' traces.'
c****     fake out the ping numbering
          ifakeout = 1
          lastpingno = lbuf(3)
          lbuf(3) = lastpingno - 1
          IF( dummies .LT. 2 ) THEN
              PRINT *,' Killing partial pings.'
              nmissing = nmissing + 1
              DO i = 0, numdat-1
                 a(stkindex+i) = a(in+i)
              ENDDO
              icount = 1
              GOTO 800
          ENDIF
      ENDIF
c**** The last ping is in the stack buffer.
c**** save the stack in nextad, put this trace in the stack, and
c**** put nextad (the stack) into the output.
      DO i = 0, numdat-1
         a(nextad+i) = a(stkindex+i) / icount
         a(stkindex+i) = a(in+i)
         a(in+i) = a(nextad+i)
      ENDDO
      icount = 1
      nready = 1
c****
c**** If we get this far, the trace has been processed.
c****
  800 CONTINUE
      lastpingno = lbuf(3)
c      IF( lunxyz + lunbin .EQ. 0 ) nready = 1
      IF( lunxyz .NE. 0 .AND. lunbin .EQ. 0 ) THEN
          token = 'xyz.bin'
          PRINT *,' ***   Creating binary XYZ file ',token
          CALL getfil ( 3, lunbin, token, istat )
  900     CALL rline( lunxyz )
          IF( nchars .GT. 0 ) THEN
              CALL getoke( token, nchars_token )
              CALL ddcode( token, nchars_token, dbub(1), istat )
              CALL getoke( token, nchars_token )
              CALL ddcode( token, nchars_token, dbub(2), istat )
              CALL getoke( token, nchars_token )
              CALL ddcode( token, nchars_token, dbub(3), istat )
              CALL wrdiscb( lunbin, dbub, 24 )
              GOTO 900
           ENDIF
      ENDIF
      IF( lunbin .NE. 0 .AND. ibuf(45) .EQ. 2 ) THEN
          IF( luntmp .EQ. 0 ) CALL getfil ( 1, luntmp, token, istat )
c****     Position is in tenths of arc seconds.  Is NMEA in tenths?
c          dtemp = lbuf(19)
c          deglong = dtemp / 3600.
c          dtemp = lbuf(20)
c          deglat = dtemp / 3600.
c****     Position is .0001 min of arc
          IF( ibuf(45) .NE. 2 )
     &        PRINT *,' ***  WARNING  ***  lat/long switch not 2.'
          dtemp = lbuf(21)
          deglong = dtemp / 60.D+04
          dtemp = lbuf(22)
          deglat = dtemp / 60.D+04
          IF( IAND(lprint,4) .NE. 0 )
     &            PRINT *,' Boat lat/long is ',deglat,deglong
          IF( deglat .EQ. deglatlast .AND. deglong .EQ.deglonglast) THEN
              nready = 0
              CALL wrdisc( luntmp, ibuf, numhdr )                       ! save the trace header
c****    the trace is in a(in).  save it so we can interpolate the depth
              CALL wrdisc( luntmp, a(in), numdat )
              nsaved = nsaved + 1
              lastdepth = idepth
c****         was RETURN rather than GOTO 2000
              GOTO 2000
          ENDIF
          deltad = delta
          IF( mlat .EQ. 0. ) CALL dlendeg( deglat, mlat, mlong )
          CALL podiscb( lunbin, 0, 0 )
          difflat = 360.
          difflong = 360.
          distance = 999999.
 1000     index = nextad/2 + 1
c****     dbuf is equivalenced to a
          CALL rddiscb( lunbin, dbuf(index), 24000, istat )
          n = 1000
          IF( istat .LT. 0 ) n = istat / 24
          DO i = 1, n
             dlong = dbuf(index)
             dlat = dbuf(index+1)
             dtemp=DSQRT(((dlong-deglong)*mlong)*((dlong-deglong)*mlong)
     &         +((dlat-deglat)*mlat)*((dlat-deglat)*mlat))
             IF( dtemp .LT. distance ) THEN
                 distance = dtemp
                 depth = -dbuf(index+2)
                 idepth = NINT(depth)
                 depth_lat = dlat
                 depth_long = dlong
             ENDIF
              index = index + 3
          ENDDO
          IF( n .EQ. 1000 ) GOTO 1000
          IF( distance .GT. delta )
     &        PRINT *,' ***  WARNING  ***  Boat lat/long is ',deglat,
     &          deglong, ' depth of ',idepth,' is from ',depth_lat,
     &          depth_long,' or ',distance,' (m away) on shot ',lbuf(3)
          IF( IAND(lprint,8) .NE. 0 )
     &        PRINT *,' Using water depth of ',idepth,' from ',
     &            depth_lat,depth_long,' or ',distance,' (m away).'
          deglatlast = deglat
          deglonglast = deglong
 1500     lbuf(16) = idepth
          buf(50) = depth / 750.
          IF( first_xyz ) THEN
              first_xyz = .FALSE.
              PRINT *,' The first water depth is ',lbuf(16),' time ',
     &             buf(50)
          ENDIF
c****
c****     Interpolate the depths
c****
          IF( nsaved .NE. 0 ) THEN
              CALL wrdisc( luntmp, ibuf, numhdr )
              CALL wrdisc( luntmp, a(in), numdat )
              CALL podisc( luntmp, 0, 0 )
              CALL rddisc( luntmp, ibuf, numhdr, istat )
              numdat = ibuf(58)
              CALL rddisc( luntmp, a(in), numdat, istat )
              depthinc = FLOAT(idepth - lastdepth ) / FLOAT(nsaved)
              depth = FLOAT(lastdepth) + depthinc
              lbuf(16) = NINT(depth)
              buf(50) = depth / 750.
              ndone = 1
              nready = nsaved + 1
          ELSE
              nready = 1
          ENDIF
      ENDIF
c****
c****
 2000 IF( nmissing .NE. 0 .AND. dummies .GT. 0 ) THEN
          nmissing = nmissing - 1
          nready = nready + nmissing
          DO i = 0, numdat-1
             a(in+i) = 0.
          ENDDO
          lbuf(3) = lastpingno - nmissing
          ibuf(15) = 2
      ENDIF
      IF( type .EQ. 4 .AND. lbuf(4) .EQ. 2 ) lbuf(51) = -1
      RETURN
c****
c****     MKREAL   - Make a real time series
c**** The XSTAR data is in pseudo analytical form.  Pseudo because the
c**** imaginary is not quite the Hilbert transform of the reals because
c**** negative time was discarded.
c**** Dan Lizarralde's analysis:
c**** Let A be the analytic signal, where A = (Ar,iAi)
c****     T be the real trace, where T = (Tr,iTi) = (Tr,0)
c****     F(A) is Fourier transform of A, which "has power only at real
c****         frequencies" because Edgetech threw the negatives away.
c**** What we have is:
c****     F(A) = 2*F(Tr), f>0
c****     F(A)=F(Tr), f=0
c****     F(A) = 0, f<0
c**** So, in order to get the true real trace:
c**** 1) FFT back to the frequency domain.  (2048 samples)
c**** 2) Set:   F(Tr) = .5 * F(Ar + i*Ai), for f>0
c****           F(T) = F(A), for F=0
c****           F(T) = .5*(FAr - i*FAi), for f<0
c**** 3) This is now 4096 samples.  FFT back to the time domain.
 5000 CONTINUE
c**** xstar has 1600 complex samples, but we're only interested in the
c**** reals.  (segy header says 3976.  There are 448 complex zeroes at
c**** the end.  1600*2 + 448*2 = 3200 + 776 = 3976).
      nfft = 1024
      ipowr2 = 10
c**** numdat was divided by 2 earlier, so there are numdat complex samples
      DO i = 1, 10
         IF( nfft .GT. numdat ) GOTO 5010
         nfft = nfft + nfft
         ipowr2 = ipowr2 + 1
      ENDDO
 5010 CONTINUE
      nfft2 = nfft + nfft
      nffto2 = nfft / 2
      numdat2 = numdat * 2
      fftscalar = 1. / FLOAT(nfft)
      DO i = 0, numdat2-1
         a(nextad+i) = a(in+i)
      ENDDO
      DO i = numdat2, nfft2-1
         a(nextad+i) = 0.
      ENDDO
      CALL fftfwd( a(nextad), ipowr2 )
c**** -nyqust to 0 to nyqust-deltaf, complex.
      DO i = 1, nfft
         a(nextad+i-1) = .5 * a(nextad+i-1)
      ENDDO
c**** complex conjugate of positive frequencies
      DO i = 1, nfft-1, 2
         a(nextad+i) = -a(nextad+i)
      ENDDO
      DO i = 3, nfft-2
         a(nextad+nfft+i-1) = .5 * a(nextad+nfft+i-1)
      ENDDO
      CALL fftinv( a(nextad), ipowr2 )
      k = 0
      DO i = 0, nfft2-1, 2
         a(in+k) = a(nextad+i) * fftscalar
         k = k + 1
      ENDDO
c**** get rid of the chatter at the end of the trace
      DO i = 1600, numdat-1
         a(in+i) = 0.
      ENDDO
c**** Now account for the Edgetech scalar
      temp = scalar / 32767. * 1.41E-14
      DO i = 0, numdat - 1
         a(in+i) = a(in+i) * temp
      ENDDO
c**** We haven't taken care of the two ducer stack.  Should we?
      nready = 1
      RETURN
c****
c****
      ENTRY GETXSTARTR( buf, lbuf, ibuf, nready )
c**** Called only if nready was > 1
c**** Called if multiple out due to wireout or xyz stuff OR missing pings
      IF( nmissing .GT. 0 ) THEN
          lastpingno = lastpingno + 1
          lbuf(3) = lbuf(3) + 1
          ibuf(15) = 2
          nready = nready - 1
          nmissing = nmissing - 1
          DO i = 0, numdat-1
             a(in+i) = 0.
          ENDDO
      ENDIF
      IF( luntmp .EQ. 0 ) RETURN
c
      CALL rddisc( luntmp, ibuf, numhdr, istat )
      numdat = ibuf(58)
c**** put the data in the ap and signal that it's in the ap!
      in = 1
      CALL rddisc( luntmp, a(in), numdat, istat )
      ndone = ndone + 1
      depth = FLOAT(lastdepth) + depthinc * FLOAT(ndone)                ! this is the interpolated depth
c****
c**** The fish is way behind the boat, so the depth for
c**** this trace was calculated much earlier.  Save the depths in
c**** the ap and cycle the buffer when done
      IF( wireout .NE. 0 ) THEN
          IF( noffdone .LT. noffset ) THEN
              noffdone = noffdone + 1
          ELSE
              DO i = 1, noffset
                 a(idepaddr+i-1) = a(idepaddr+i)
                 a(lataddr+i-1) = a(lataddr+i)
                 a(longaddr+i-1) = a(longaddr+i)
              ENDDO
          ENDIF
          a(idepaddr + noffdone - 1) = depth
c          a(lataddr + noffdone - 1) = FLOAT(lbuf(20))
c          a(longaddr + noffdone - 1) = FLOAT(lbuf(19))
          a(lataddr + noffdone - 1) = FLOAT(lbuf(22))
          a(longaddr + noffdone - 1) = FLOAT(lbuf(21))
          fdepth = a(idepaddr)
c****
c****  Now find the fish.  Search for the "closest" lat/long
c****  by using the user given fish offset (cable out) and last
c****  fish depth.  But process header and wbt figured that out
c****  on the last trace, which is gone and not accessible. F.
c****
c       use header l13 = r59 * 750  (assuming r59 is the shift)
c       process header saved the trace header in common, so use it.
          IF( lbuf(13) .EQ. 0 ) buf(13) = header(13)
          fishdepth = lbuf(13)
          IF( fishdepth .LE. 0 )
     &        PRINT *,' ***  WARNING  ***  Source depth of ',
     &         lbuf(13),' may lead to bad xyz location.'
          IF( fishdepth .GT. wireout ) THEN
              PRINT *,' ***  HUH  ***  fishdepth greater than wireout.'
              STOP
          ENDIF
          offset = SQRT( wireout * wireout - fishdepth * fishdepth )
c****   find the lat/log of the fish
          dtemp= lbuf(21)
          boatlong = dtemp / 60.D+04
          dtemp = lbuf(22)
          boatlat  = dtemp / 60.D+04
          index = -1
c****     Use the depth of the first position further than the fish
c****     Go through the xyz buffer to find the first position further
c****     than the fish and use that water depth.
          DO i = 0, noffdone - 1
             IF( index .LT. 0 ) THEN
                 dtemp = a(lataddr + i )
                 dlat = dtemp / 60.D+04
                 dtemp = a(longaddr + i )
                 dlong = dtemp / 60.D+04
                 deltalat = (boatlat-dlat) * mlat
                 deltalong = (boatlong-dlong) * mlong
                 deltad = DSQRT(deltalat*deltalat +deltalong*deltalong)
                 IF( offset - deltad .GE. 0 ) index = i
             ENDIF
          ENDDO
          IF( index .LT. 0 ) index = 0
          fdepth = a(idepaddr+index)
      ENDIF
      lbuf(16) = NINT(fdepth)                                           ! water depth at source/receiver
      buf(50) = depth / 750.                                            ! water depth at boat's GPS
      nsaved = nsaved - 1
      IF( nsaved .EQ. 0 ) CALL podisc( luntmp, 0, 0 )
      lbuf(4) = 1
      ibuf(58) = numdat
      nready = 1
      RETURN

      END

