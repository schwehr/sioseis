      SUBROUTINE ssmiex( buf, lbuf, ibuf, scr, lscr, iscr, istop,
     *           nready )
c  Split-Step migration by Dan Lizarralde
c  Copyright (C) by Woods Hole Oceanographic Institution, 1995
c
c  mod Apr 96 by gmk for:
c    smooth the time data
c    smooth the slowness in depth
c    write SEGY velocity file
c  mod May 96 by pch to error out if nx * nz > iapsiz because Velocities
c      are stored there.
c  mod 9 Apr 09 - Use unsigned int for header number of samples
c
      PARAMETER (iapsiz = 5000000)
      PARAMETER (isize = 262144 )                                       ! 512 x 512

      PARAMETER ( maxvel = 55 )
      DIMENSION vels(maxvel), vels1(maxvel), vels2(maxvel)
      DIMENSION buf(111), lbuf(111), ibuf(111),
     &          scr(111), lscr(111), iscr(111)
      INTEGER*2 ibuf, iscr
      COMMON /readt/ itunit, numhdr, numdat, ihunit, ireeln, jntrcs,
     *               ifmt, nskip, secs, lrenum, isrcf, idtype,
     *               nfskip, jform, itxsi, itxdel, nfktrc, norigtr
      COMMON /sioap/ iasgnd, irelse, in, iout, nextad, lapsiz, ifree,
     *     iuseap
      COMMON /segyptr/ llsegptr, lrseqptr, lshotptr, lshtrptr, lrpnptr,
     *                 lrptrptr, itridptr, ldisptr,  lwbdptr,  lsxcoptr,
     *                 lrxcoptr, idelmptr, istmptr,  iendmptr, isampptr,
     *                 isiptr,   iyrptr,   idayptr,  ihrptr,   iminptr,
     *                 isecptr,  igmtptr,  ldelsptr, lsmusptr, lemusptr,
     *                 lsisptr,  lwbtsptr, lgatptr,  lssmsptr, lesmsptr,
     *                 lsbptr,   ifoldptr, icvleptr, lespnptr
      COMMON /ssmigr/ junit, nlists, nwrds

      REAL*4 APDATA(0:iapsiz)
      COMMON /apmem/ apdata

      COMMON /transp/ t(isize)

      INTEGER fno, bpad, epad, ref, firstno, fno1, fno2, vintpl,
     &        nvsmth, mtap
      LOGICAL big, FIRSTSEGY
      DATA mlists/0/, ntraces/0/, tbig/0./, lunt/0/, big/.FALSE./,
     &     ndone/0/, firstsegy/.TRUE./
      SAVE
c
      IF( istop .EQ. -1 ) GOTO 90                                       ! means no trace
      nready = 0
   10 IF( mlists .EQ. 0 ) THEN
          twopi = 2.*3.141592653589
          CALL podisc( junit, 1, 0 )                                    ! get the first parameter list from disk
          CALL rddisc( junit, lscr, nwrds, istat )
          mlists = mlists + 1
          lprint = lscr(1)
          fno = lscr(2)
          lno = lscr(3)
          nvels = lscr(4)
          bpad = lscr(5)
          epad = lscr(6)
          zbig = scr(7)
          fmin = scr(8)
          fmax = scr(9)
          deltax = scr(10)
          deltaz = scr(11)
          edepth = scr(12)
          lunscr = lscr(13)
          IF( lunscr .EQ. 0 ) CALL getfil( 1, lunscr, scr, istat )
          lunvel = lscr(14)
          IF( lunvel .EQ. 0 ) CALL getfil( 1, lunvel, scr, istat )
          vbig = scr(15)
          ref = lscr(16)
          vintpl = lscr(17)
          nvsmth = lscr(18)
          twinlen = scr(19)
          mtap = lscr(20)
          lunsegy = lscr(21)
          CALL getfil( 1, lunhdr, scr, istat )                          ! get a file for the trace headers
          CALL getfil( 1, lunslice, scr, istat )                        ! get a file for sspost2's slices
          nz = int(edepth/deltaz + 0.0001) + 1                          ! check for underflow
      ENDIF
      IF( lbuf(7) .EQ. 0 ) THEN
          no = lbuf(3)
          itrno = lbuf(4)
      ELSE
          no = lbuf(6)
          itrno = lbuf(7)
      ENDIF
      dt = buf(lsisptr)
c      nsamps = ibuf(isampptr)
      nsamps = numdat
      delay = buf(ldelsptr)
      wbt = buf(lwbtsptr)                                               ! water bottom time in seconds
      IF( tbig .EQ. 0. ) tbig = delay
      IF( tbig .NE. delay ) THEN
          PRINT *,' ***  ERROR  ***  SSMIGR must have a constant delay.'
          STOP
      ENDIF
      IF( IAND(lprint,2) .NE. 0 ) THEN
          PRINT *,' no= ',no
          PRINT *,' fno= ',fno,' lno= ',lno,' nvels= ',nvels,' bpad= ',
     &         bpad,' epad= ',epad
          PRINT *,' fmin= ',fmin,' fmax= ',fmax,
     &         ' deltax= ',deltax,' deltaz= ',deltaz
          PRINT *,' edepth= ',edepth,' lunscr= ',lunscr,' lunvel= ',
     &         lunvel,' ref= ',ref,' vintpl=',vintpl
      ENDIF
      IF( ntraces .EQ. 0 ) THEN
          nt = nsamps                                                   ! this must be a constant!
          firstno = no
          lastno = no - 1
      ENDIF
      IF( no .NE. lastno + 1 ) THEN
          PRINT *,' ***  WARNING  ***  SSMIGR assumes monotonically ',
     &       'increasing shot/rp numbers.'
      ENDIF
      lastno = no
      CALL wrdisc( lunhdr, buf, numhdr )
      iout = 0                                                          ! force the data out of the ap
      CALL rlseap( buf(numhdr+1), nt )
      IF( ntraces .EQ. 0 ) THEN
          CALL powtwo( nt, nfft )
          dw = twopi / (dt * FLOAT(nfft-1))
          IF( fmax .EQ. 0. ) fmax = 1. / (2. * dt) / 2.
c****  calculate nw EXACTLY the same way sspost2 does (roundoff!)
          temp = twopi * fmin
          indexfw = INT( temp / dw ) + 1                        ! Dan index to first frequency
          temp = twopi * fmax
          indexlw = INT( temp / dw ) + 1
c          nw = (INT( twopi*(fmax-fmin)/dw) + 1)
          nw = indexlw - indexfw + 1
          IF( IAND(lprint,4) .NE. 0 ) PRINT *,' dw=',dw,' nw=',nw,
     &        ' fmin=',fmin,' fmax=',fmax, ' indexfw=',indexfw,
     &        ' nt=',nt,' nfft=',nfft
      ENDIF
c***  Time taper trace before pad and fft
      spt = twinlen
      nspt = 1+int(spt/dt)
      call get_Ttaper(Nt,nspt,scr)

      do 40 it=1,Nt
        buf(numhdr+it)=buf(numhdr+it)*scr(it)
   40 continue

c***  Zero Arrays
      DO  50 i = nt+1, nfft
   50 buf(numhdr+i) = 0.
      DO 60 i = 1, nfft
   60 scr(i) = 0.
c     fastf( reals, imaginaries, number which is a power of 2)
      CALL fastf( buf(numhdr+1), scr, nfft )
      CALL wrdisc( lunscr, buf(numhdr+indexfw), nw )                    ! write the reals
      CALL wrdisc( lunscr, scr(indexfw), nw )                           ! write the imaginaries
      ntraces = ntraces + 1
      IF( istop .EQ. 0 ) RETURN
c****
c****   got a panel
c****
   90 CALL powtwo( (ntraces+bpad+epad), nxmig )
      nx = ntraces
      IF( nx * nz .GT. iapsiz ) THEN
          PRINT *,' ***  ERROR  ***  Too many depths and traces.'
          PRINT *,' Use sioseis.BIG.  Max nx * nz is ',iapsiz
	     PRINT *,' nx=',nx,' nz=',nz
          STOP
      ENDIF
      CALL podiscb( lunvel, 1, 0 )
      CALL rddisc( lunvel, scr, 20, istat )
      IF( istat .NE. 20 ) THEN
          PRINT *,' CREATING VELOCITY-DEPTH FILE.'
          CALL podiscb( junit, 1, 0 )
          mlists = 0
          CALL rddisc( junit, lscr, nwrds, istat )
          mlists = mlists + 1
          fno1 = lscr(2)
          lno1 = lscr(3)
          nvels1 = lscr(4)
          CALL rddisc( junit, vels1, nvels1, istat )
          fno2 = 0
  100     IF( mlists .LT. nlists ) THEN
              CALL rddisc( junit, lscr, nwrds, istat )
              mlists = mlists + 1
              fno2 = lscr(2)
              lno2 = lscr(3)
              nvels2 = lscr(4)
              CALL rddisc( junit, vels2, nvels2, istat )
          ELSE
              lno1 = 99999999
          ENDIF
          DO 500 no = firstno, lastno
  150        CONTINUE
             IF( no .LE. lno1 ) THEN
                 DO 160 i = 1, nvels1
  160            vels(i) = vels1(i)
                 nvels = nvels1
                 GOTO 400
             ENDIF
             IF( no .GE. fno2 ) THEN
                 IF( mlists .LT. nlists ) THEN
                     fno1 = fno2
                     lno1 = lno2
                     nvels1 = nvels2
                     DO 200 i = 1, nvels1
  200                vels1(i) = vels2(i)
                     CALL rddisc( junit, lscr, nwrds, istat )
                     mlists = mlists + 1
                     fno2 = lscr(2)
                     lno2 = lscr(3)
                     nvels2 = lscr(4)
                     CALL rddisc( junit, vels2, nvels2, istat )
                     GOTO 150
                 ELSE
                     DO 210 i = 1, nvels2
  210                vels(i) = vels2(i)
                     nvels = nvels2
                     GOTO 400
                 ENDIF
             ENDIF
             IF( vintpl .EQ. 1 ) THEN
c***             Velocity variation is by iso-velocity
                 DO 310 i = 1, nvels1, 2
                    v = vels1(i)
                    d = vels1(i+1)
                    DO 290 j = 1, nvels2, 2
                       vnext = vels2(i)
                       dnext = vels2(i+1)
                       IF( v .EQ. vnext ) THEN
                           IF( j .EQ. 1 ) GOTO 295
                           dnext = (v-vels2(j+2))/(vels2(j)-vels2(j+2))*
     &                        (vels2(j+1)-vels2(j+3))+vels2(j+3)
                           GOTO 300
                       ENDIF
  290               CONTINUE
  295               v = (vnext-v)/(fno2-lno1) * (no - lno1) + v
  300               CONTINUE
                    vels(i) = v
                    vels(i+1) = (dnext-d)/(fno2-lno1)*(no-lno1)+d
  310            CONTINUE
                 nvels = nvels1
             ELSEIF( vintpl .EQ. 2 ) THEN
c****            Velocity interpolation by iso-depth
                 IF( nvels2 .NE. nvels1 ) THEN
                     PRINT *,' ***  ERROR  ***  Must have the same ',
     &                   'of velocity pairs.'
                     STOP
                 ENDIF
                 ratio = FLOAT(no-lno1) / FLOAT(fno2-lno1)
                 DO 330 i = 1, nvels1, 2
                    v = vels1(i)
                    d = vels1(i+1)
                    vnext = vels2(i)
                    dnext = vels(i+1)
                    vels(i) = (vnext-v) * ratio + v
                    vels(i+1) = (dnext-d) * ratio + d
  330            CONTINUE
             ENDIF
  400        CONTINUE
             index = (no - firstno) * nz + 1
             IF( IAND(lprint,4) .NE. 0 ) PRINT *,' no ',no,
     &          ' vdp ',(vels(i),i=1,nvels),' nz=',nz
             CALL ivelt( vels, apdata(index), 0., deltaz, nz )
c
c*** Smooth slowness vertically
c
             do 410 iz=1,nz
                buf(iz)=1./apdata(index+iz-1)
  410        continue
             do 420 iv=1,nvsmth
                do 430 iz=2,nz-1
                   scr(iz)=(buf(iz-1)+buf(iz)+buf(iz+1))/3.
  430           continue
                scr(1) = buf(1)
                scr(nz) = buf(nz)
                do 440 iz=1,nz
                   buf(iz)=scr(iz)
  440           continue
  420        continue
             do 450 iz=1,nz
                apdata(index+iz-1)=1./buf(iz)
  450        continue
C***GMK Write out smoothed velocity file as segy file for external purposes
             IF (lunsegy .ne. 0) THEN
               IF (FIRSTSEGY) THEN
                 FIRSTSEGY = .FALSE.
                 CALL podisc( lunhdr, 1, 0 )
C***GMK Clean scratch array
                 do 460 ic = 1, 800
                  buf(ic) = 0
  460            continue
                  CALL WRDISC(lunsegy, buf, 800)
                  ibuf(9)  = deltaz * 1000
                  ibuf(10) = deltaz * 1000
                  ibuf(11) = nz
                  ibuf(12) = nz
                  ibuf(13) = 5
                  ibuf(15) = 1
C***GMK WRITE OUT TAPE ID HEADER WITH APPROPRIATE VAULES FOR VELOCITY FILE
                  CALL WRDISC(lunsegy, buf, 100)
               ENDIF
               CALL rddisc( lunhdr, buf, numhdr, istat )
C***GMK Change appropriate header values for velocity file
                ibuf(idelmptr) = 0
                buf(ldelsptr) = 0.0
                ibuf(isiptr) = deltaz * 1000.                                     ! sample interval in micrometers
                buf(lsisptr) = deltaz / 1000.
c                ibuf(isampptr) = nz
               CALL long2ushort( nx, ibuf(isampptr) )
               CALL wrdisc( lunsegy, buf, numhdr, istat )
               CALL wrdisc( lunsegy, apdata(index), nz, istat )
             ENDIF
  500     CONTINUE
          CALL podiscb( lunvel, 1, 0 )
          DO 700 i = 1, nz
             index = i
             DO 600 j = 1, nx
c****           transpose and convert from meters to kilometers
                scr(j) = apdata(index) / 1000.
                index = index + nz
  600        CONTINUE
             CALL wrdisc( lunvel, scr, nx, istat )
  700     CONTINUE
      ENDIF
      PRINT *,' Finished creating velocity depth file.'
c***
c***  The velocity file is ready
c***
 1000 CONTINUE
      IF( zbig .NE. 0. .AND. vbig .EQ. 0. ) vbig = vels(1)
      IF( zbig .GT. 99 ) THEN
          zbig = zbig / 1000.                                           ! did the user give meters or kilometers?
          vbig = vbig / 1000.
      ENDIF
      dx_km = deltax / 1000.
      dz_km = deltaz / 1000.
      DO 1010 i = 1, nx
         CALL wrdisc( lunslice, scr, nz )
 1010 CONTINUE
      CALL sspost2( lunscr, lunslice, lunvel, lunt, fmin, fmax, delay,
     &     zbig, vbig, ref, nx, dx_km, bpad, epad, mtap,
     &     nt, dt, nz, dz_km, buf, scr, scr )
c**** The data now sits on disk as REAL depth slices.
      CALL frefil( 3, lunscr, istat )                                   ! release lunscr
      nready = nx
      CALL podisc( lunhdr, 1, 0 )
      CALL podisc( lunslice, 1, 0 )
      IF( nx * nz .GT. isize ) THEN
          big = .TRUE.
c***GMK If lunt is not opened in sspost2 (since big is defined differently) open it now
          IF (lunt .eq. 0) CALL getfil( 1, lunt, scr, istat )
      ENDIF
      istop = 0
      CALL transinit                                                    ! initialize the transpose subroutines
      DO i = 1, nz
         CALL rddisc( lunslice, scr, Nx, istat )
         IF( i .EQ. nz ) istop = 1
         IF( .NOT. big ) THEN
             CALL trans1( scr, nx, nz, istop )
         ELSE
             CALL trans2( scr, nx, nz, lunt, istop )
         ENDIF
      ENDDO
      IF( big ) CALL podisc( lunt, 1, 1 )
      CALL frefil( 3, lunslice, istat )
      RETURN
c
      ENTRY getnext( buf, lbuf, ibuf, scr )
      CALL rddisc( lunhdr, buf, numhdr, istat )
      ibuf(idelmptr) = int(zbig*1000.0)                                 ! delay in meters
      ibuf(istmptr) = 0                                                 ! start mute time
      ibuf(iendmptr) = 0                                                ! end mute time
      nbigz = NINT(zbig/dz_km)
      ibuf(isampptr) = nz-nbigz                                         ! number of samples
      numdat = nz-nbigz
      CALL long2ushort( numdat, ibuf(isampptr) )
      ibuf(isiptr) = deltaz * 1000.                                     ! sample interval in micrometers
      buf(ldelsptr) = zbig*1000.0
      buf(lsisptr) = deltaz / 1000.
      IF( .NOT. big ) THEN
          index = ndone * nz
          DO iz = nbigz+1, nz
             buf(numhdr+iz-nbigz) = t(index+iz)
          ENDDO
      ELSE
          CALL podisc( lunt, 2, nbigz )                                 ! GMK, skip unwanted front stuff
          CALL rddisc( lunt, buf(numhdr+1), nz-nbigz, istat )
      ENDIF
      ndone = ndone + 1

      RETURN
      END

c--------------------------------------------------------------------
c
      subroutine get_Ttaper(Nt,nspt,ttap)

      real*4 ttap(Nt)

      rtap = float(nspt)

      do i=1,Nt
         ttap(i)=1.
      end do

      do i=1,nspt
         r=float(i)
         arg = -1.*(0.08*(rtap-r))**2.
         ttap(Nt+1-i) = exp(arg)
      end do
c
      return
      end
c
